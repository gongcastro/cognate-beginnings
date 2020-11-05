#### vocabulary: Analyse vocabulary sizes ######################################

#### set up ####################################################################

# load packages
library(tidyverse)
library(data.table)
library(brms)
library(tidybayes)
library(here)

# set params
set.seed(888)

#### import data ###############################################################
dat <- fread(here("Data", "vocabulary.csv")) %>% 
    as_tibble() %>% 
    mutate_at(vars(type, lp, item_dominance), as.factor) %>% 
    mutate(age_center = age-mean(.$age)) %>% 
    filter(type %in% "Comprehensive")

contrasts(dat$lp) <- c(0.5, -0.5)
contrasts(dat$item_dominance) <- c(0.5, -0.5)

#### fit models ################################################################
priors <- c(prior(normal(0, 1), class = "Intercept"),
            prior(normal(0.5, 0.4), class = "b", coef = "age_center"))

fitp <- brm(sum | trials(n) ~ age_center,
            data = dat,
            family = binomial("logit"),
            prior = priors,
            sample_prior = "only",
            file = here("Results", "vocabulary_p.rds"),
            cores = 4)

fit0 <- update(fitp, sample_prior = "no",
               newdata = dat, cores = 4,
               save_all_pars = TRUE, file = here("Results", "vocabulary_0.rds"))
fit1 <- update(fit0, . ~ . -age_center + age_center*item_dominance,
               newdata = dat, cores = 4,
               save_all_pars = TRUE, file = here("Results", "vocabulary_1.rds"))
fit2 <- update(fit1, . ~ . -age_center*item_dominance + age_center*item_dominance*lp,
               newdata = dat, cores = 4,
               save_all_pars = TRUE, file = here("Results", "vocabulary_2.rds"))

#### model comparison ##########################################################
loo0 <- loo(fit0, moment_match = TRUE)
loo1 <- loo(fit1, moment_match = TRUE)
loo2 <- loo(fit2, moment_match = TRUE)
loo_comp <- loo_compare(loo0, loo1, loo2)

#### examine prior #############################################################
prior_dist <- gather_draws(fitp, `b_.*`, regex = TRUE) %>% 
    mutate(.chain = as.factor(.chain))

# traceplots
ggplot(prior_dist, aes(.iteration, .value, colour = .chain)) +
    facet_wrap(~.variable, scales = "free_y") +
    geom_line() +
    scale_color_brewer(palette = "BrBG") +
    theme_minimal() +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "top")

# distributions
ggplot(prior_dist, aes(.value, fill = .variable)) +
    facet_wrap(~.variable, scales = "free_x") +
    stat_halfeye() +
    scale_fill_brewer(palette = "BrBG") +
    theme_minimal() +
    labs(x = "Iteration", y = "Value", fill = "Chain") +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "top")

# prior predictive distribution
prior_preds <- expand.grid(age_center = seq(min(dat$age_center), max(dat$age_center), by = 0.1),
                           n = 1) %>% 
    add_fitted_draws(fitp, n = 50)
ggplot(prior_preds, aes(age_center, .value, group = .draw)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Iteration", y = "Value", fill = "Chain") +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "top")

#### examine posterior #########################################################
posterior_dist <- gather_draws(fit1, `b_.*`, regex = TRUE) %>% 
    mutate(.chain = as.factor(.chain))

# traceplots
ggplot(posterior_dist, aes(.iteration, .value, colour = .chain)) +
    facet_wrap(~.variable, scales = "free_y") +
    geom_line() +
    scale_colour_manual(values = c("#543005", "#BF812D", "#80CDC1", "#01665E")) +
    theme_minimal() +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "top")

# distributions
ggplot(posterior_dist, aes(.value, fill = .variable)) +
    facet_wrap(~.variable, scales = "free_x") +
    stat_halfeye() +
    scale_fill_manual(values = c("#543005", "#BF812D", "#80CDC1", "#01665E")) +
    theme_minimal() +
    labs(x = "Iteration", y = "Value", fill = "Chain") +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "top")

# prior predictive distribution
posterior_preds <- expand.grid(age_center = seq(min(dat$age_center),
                                                max(dat$age_center), by = 0.1),
                               item_dominance = c("L1", "L2"),
                               lp = c("Monolingual", "Bilingual"),
                               n = 1) %>% 
    add_fitted_draws(fit2, n = 50)
ggplot(posterior_preds, aes(age_center, .value, colour = lp)) +
    facet_wrap(~item_dominance) +
    geom_point(data = dat, aes(y = vocab_size), shape = 1, stroke = 1, alpha = 0.5) +
    geom_line(aes(group = interaction(lp, .draw)), size = 0.5, alpha = 0.5) +
    scale_colour_manual(values = c("#01665E", "#BF812D")) +
    theme_minimal() +
    labs(x = "Age (months)", y = "vocabulary size (%)", colour = "Group") +
    theme(legend.position = "top", 
          legend.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA))



