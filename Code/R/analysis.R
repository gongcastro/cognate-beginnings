#### analysis: Analyse vocabulary data ##########################################

#### set up #####################################################################

# load packages
library(tidyverse)
library(data.table)
library(brms)
library(tidybayes)
library(here)

# set params
set.seed(888)
bins <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- bins[-c(1, length(bins))]
options(loo.cores = 4, mc.cores = 4)

##### import data and set params ###############################################
dat <- fread(here("Data", "preprocessed.csv"), na.string = c("", "NA")) %>% 
    as_tibble() %>% 
    filter(type %in% "Comprehensive") %>% 
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1,
           cognate = as.factor(cognate),
           item_dominance = as.factor(item_dominance)) %>%
    arrange(type, item, te, age_bin) %>% 
    select(item, te, item_dominance, cognate, age_bin, successes, n, proportion) %>% 
    filter(te %in% sample(unique(.$te), 15))

contrasts(dat$cognate) <- c(0.5, -0.5) 
contrasts(dat$item_dominance) <- c(0.5, -0.5) 

#### fit model #################################################################
priors <- c(prior(normal(-4.23, 5), class = "Intercept"),
            prior(exponential(2), class = "sd"),
            prior(normal(0.5, 0.01), class = "b", coef = "age_bin"),
            prior(lkj(2), class = "cor"),
            prior(normal(0, 5), class = "b", coef = "item_dominance"),
            prior(normal(0, 5), class = "b", coef = "age_bin:item_dominance"),
            prior(normal(0, 5), class = "b", coef = "age_bin:cognate"),
            prior(normal(0, 5), class = "b", coef = "item_dominance:cognate"),
            prior(normal(0, 5), class = "b", coef = "age_bin:item_dominance:cognate"))

fitp <- brm(successes | trials(n) ~ age_bin + (age_bin | te),
            family = binomial("logit"),
            data = dat,
            prior = priors[1:4,],
            sample_prior = "only",
            file = here("Results", "fitp.rds"),
            chains = 4,
            refresh = 0,
            cores = 4)

fit0 <- update(fitp, sample_prior = "no",
               file = here("Results", "fit0.rds"))
fit1 <- update(fit0, . ~ . - age_bin + age_bin*item_dominance - (age_bin | te) + (age_bin*item_dominance | te),
               prior = priors[5:6,],
               newdata = dat, file = here("Results", "fit1.rds"))
fit2 <- update(fit1, . ~ . - age_bin*item_dominance + age_bin*item_dominance*cognate,
               newdata = dat,
               file = here("Results", "fit2.rds"))


#### model comparison ##########################################################
loo0 <- loo(fit0)
loo1 <- loo(fit1)
loo2 <- loo(fit2)
loo_comp <- loo_compare(loo0, loo1, loo2)

#### examine prior #############################################################
prior_dist <- gather_draws(fitp, `b_.*`, regex = TRUE) 
ggplot(prior_dist, aes(.value)) +
    facet_wrap(~.variable, scales = "free") +
    stat_slab()

prior_preds <- expand_grid(age_bin = seq(min(dat$age_bin), max(dat$age_bin), by = 0.1),
                          n = 1) %>% 
    add_fitted_draws(., fitp, n = 100, re_formula = NA, scale = "response")
ggplot(prior_preds, aes(age_bin, .value, group = as.factor(.draw))) +
    #stat_lineribbon(.width = 0.95, alpha = 0.5) +
    geom_line(aes(group = interaction(.draw)), show.legend = FALSE)

#### examine posterior #########################################################
post <- gather_draws(fit1, `b_.*`, regex = TRUE) %>% 
    mutate(.chain = as.factor(.chain))

ggplot(post, aes(x = .iteration, y = .value, colour = .chain)) +
    facet_wrap(~.variable, scales = "free_y") +
    geom_line() +
    scale_colour_manual(values = c("#543005", "#BF812D", "#80CDC1", "#01665E")) +
    theme_minimal() +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "top")

ggplot(post, aes(.value, fill = .variable)) +
    facet_wrap(~.variable, scales = "free") +
    stat_slab() +
    labs(x = "Value", y = "Probability density", fill = "Parameter") +
    scale_fill_manual(values = c("#543005", "#BF812D", "#80CDC1", "#01665E")) +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA),
          legend.title = element_text(face = "bold"),
          legend.position = "top")    

post_preds <- expand_grid(age_bin = seq(min(dat$age_bin), max(dat$age_bin), by = 0.1),
                          item_dominance = c("L1", "L2"),
                          n = 1) %>% 
    add_fitted_draws(., fit1, n = 50, re_formula = NA, scale = "response") 
ggplot(post_preds, aes(age_bin, .value, colour = item_dominance)) +
    #stat_lineribbon(.width = 0.95, alpha = 0.5)
    geom_line(aes(group = interaction(.draw, item_dominance))) +
    stat_summary(data = dat, aes(y = proportion), fun.data = mean_se, geom = "pointrange") +
    scale_colour_manual(values = c("#BF812D", "#01665E")) +
    labs(x = "Age (months)", y = "P(1|age_bin)", colour = "Item dominance") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA),
          legend.title = element_text(face = "bold"),
          legend.position = "top") 

#### estimated AOAs ############################################################
aoa <- gather_draws(fit1, r_te[te, param]) %>% 
    pivot_wider(names_from = param, values_from = .value) %>%
    mutate(Intercept = Intercept + fixef(fit1)[1,1],
           age_bin = age_bin+fixef(fit1)[2,1]) %>% 
    rowwise() %>% 
    mutate(.value = -Intercept/age_bin) %>% 
    ungroup() %>% 
    select(te, .value) %>% 
    group_by(te) %>% 
    #mean_qi() %>% 
    left_join(distinct(dat, te, cognate)) 

ggplot(aoa, aes(as.factor(te), .value, colour = cognate)) +
    stat_pointinterval() +
    coord_flip()
