#### 05_analysis-prod: Bayesian non-linear model ##############
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(truncnorm)
library(brms)
library(modelr)
library(janitor)
library(tidybayes)
library(ggplot2)
library(ggmcmc)
library(mcmcplots) 
library(here)

# create/load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins <- c("14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")
options(contrasts = c("contr.sum", "contr.poly"))

#### import data ##########################################
dat <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    filter(type=="Productive",
           lp=="Bilingual") %>%
    select(item, meaning, age_bin, item_dominance, cognate, proportion, frequency) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins, ordered = TRUE)),
           item_dominance = factor(item_dominance, levels = c("L2", "L1")),
           frequency = scale(frequency)[,1],
           cognate = factor(cognate, levels = c("Non-cognate", "Cognate"))) %>%
    arrange(item, meaning, age_bin)

dat_priors <- fread(here("Data", "05_priors-wordbank.csv"))

#### fit models ###########################################

# load in case models are already fitted
fit0 <- readRDS(here("Results", "prod-L1_fit0.rds"))
fit1 <- readRDS(here("Results", "prod-L1_fit1.rds"))

# model 0
fit0 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
                         asym ~ 1,
                         mid ~ 1 + item_dominance + frequency + (1 | meaning),
                         steep ~ 1, 
                         phi ~ 1,
                         nl = TRUE,
                         family = zero_one_inflated_beta),
            prior = c(prior(normal(0.7903355, 0.05), nlpar = "asym", coef = "Intercept"),
                      prior(normal(2.5164560, 1), nlpar = "mid", coef = "Intercept"),
                      prior(normal(1.7925281, 0.6), nlpar = "steep", coef = "Intercept"),
                      prior(normal(0, 5), class = "b", nlpar = "mid", coef = "item_dominance1"),
                      prior(normal(2, 1), dpar = "phi", class = "Intercept")),
            data = dat,
            cores = 4,
            save_all_pars = TRUE,
            save_model = here("Code", "Stan", "prod_fit0.stan"))

# model 1
fit1 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
                         asym ~ 1,
                         mid ~ 1 + item_dominance*cognate + frequency + (1 | meaning),
                         steep ~ 1, 
                         phi ~ 1,
                         nl = TRUE,
                         family = zero_one_inflated_beta),
            prior = c(prior(normal(0.7903355, 0.05), nlpar = "asym", coef = "Intercept"),
                      prior(normal(2.5164560, 1), nlpar = "mid", coef = "Intercept"),
                      prior(normal(1.7925281, 0.6), nlpar = "steep", coef = "Intercept"),
                      prior(normal(0, 5), class = "b", nlpar = "mid", coef = "item_dominance1"),
                      prior(normal(0, 5), class = "b", nlpar = "mid", coef = "cognate1"),
                      prior(normal(2, 1), dpar = "phi", class = "Intercept")),
            data = dat,
            cores = 4,
            save_model = here("Code", "Stan", "prod_fit1.stan"),
            stan_model_args = list(save_dso = TRUE))

# export fit
saveRDS(fit0, here("Results", "prod_fit0.Rds"))
saveRDS(fit1, here("Results", "prod_fit1.Rds"))

#### check convergence ######################

# caterpillar: If convergence is good, plots should look like funny fat catterpillars
posterior <- list("Model 0" = ggs(fit0) %>% clean_names() %>% filter(iteration >= 1000),
                  "Model 1" = ggs(fit1) %>% clean_names() %>% filter(iteration >= 1000)) %>%
    bind_rows(.id = "model") %>%
    mutate(chain = as.factor(chain))

ggplot(posterior, aes(iteration, value, colour = chain)) +
    facet_grid(parameter~model, scales = "free") +
    geom_line(alpha = 0.7) +
    scale_colour_brewer(palette = "Spectral") +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    theme_custom +
    theme(legend.position = "top") +
    ggsave(here("Figures", "07_analysis_prod-convergence.png"), height = 6, width = 5.4)

# Gelman-Rubin diagnostic (Rhat) should be close to 1.0
fit0$fit 
fit1$fit

# autocorrelation across chains (should be low after some lags)
autocorr.diag(as.mcmc(fit0)[, 1:5], lags = c(0, 1, 2, 3, 4, 5, 10, 50))
autocorr.diag(as.mcmc(fit1)[, 1:5], lags = c(0, 1, 2, 3, 4, 5, 10, 50))

#### posterior predictive checks ##############################
posterior_check <- expand_grid(age_bin = unique(dat$age_bin),
                               item_dominance = c("L2", "L1"),
                               cognate = c("Non-cognate", "Cognate"),
                               frequency = seq_range(dat$frequency, 4)) %>%
    add_fitted_draws(model = fit1, n = 100, value = "proportion", scale = "linear") %>%
    ungroup() %>%
    mutate(.draw = as.character(.draw))
dat_posterior_pcheck <- dat %>% group_by(age_bin, cognate) %>%
    summarise(prop = mean(proportion, na.rm = TRUE),
              n = n(),
              std = sd(proportion, na.rm = TRUE),
              sem = std/sqrt(n),
              .groups = "drop") %>%
    mutate(lp = ifelse(lp==-0.5, "Monolingual", "Bilingual"),
           cognate = ifelse(cognate==-0.5, "Non-cognate", "Cognate"))

ggplot(posterior_check, aes(age_bin, proportion, colour = cognate, linetype = cognate)) +
    facet_grid(~frequency) +
    stat_lineribbon(aes(group = cognate), size = 0.5, colour = "#44546A",
                    .width = c(0.11, 0.5, 0.89, 0.95)) +
    #geom_point(data = dat_ppcheck, aes(x = age_bin, y = prop, colour = cognate)) +
    #geom_errorbar(data = dat_ppcheck, aes(x = age_bin, y = prop, ymin = prop-sem, ymax = prop+sem, colour = cognate), width = 0) +
    labs(x = "Age (months)", y = "Proportion", fill = "Credible interval",
         group = "Cognate", linetype = "Cognate", colour = "Cognate") +
    scale_x_continuous(breaks = seq(0, 11), labels = bins) +
    scale_fill_brewer(palette = "Oranges") +
    theme_custom +
    theme(panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
          legend.position = "right",
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          axis.text.x = element_text(size = 7, angle = 300)) +
    ggsave(here("Figures", "07_analysis_prod_posterior-checks.png"))

# coefficients
intervals <- posterior %>% 
    mutate(parameter = as.character(parameter)) %>%
    filter(model %in% "Model 1",
           parameter %in% c("b_mid_frequency", "b_mid_cognate")) %>% 
    group_by(parameter) %>%
    median_qi(value, .width = c(0.95, 0.89, 0.50)) %>%
    mutate(parameter = case_when(parameter=="b_mid_frequency" ~ "Frequency\n(Centered)",
                                 parameter=="b_mid_cognate" ~ "Cognateness \n(NC/C, centered)"))

posterior %>%
    mutate(parameter = as.character(parameter)) %>%
    filter(model %in% "Model 1",
           parameter %in% c("b_mid_frequency", "b_mid_cognate")) %>%
    mutate(parameter = case_when(parameter=="b_mid_frequency" ~ "Frequency\n(Centered)",
                                 parameter=="b_mid_cognate" ~ "Cognateness \n(NC/C, centered)")) %>% 
    ggplot(aes(y = parameter, x = value)) +
    stat_slabh(fill = "#44546A", colour = NA) +
    geom_intervalh(data = intervals, position = position_nudge(y = -0.3), alpha = 0.7) +
    geom_pointintervalh(position = position_nudge(y = -0.1), size = 0.1, data = intervals) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Value", y = "Parameter",
         fill = "Credible Interval", colour = "Credible Interval",
         title = "Sampling the posterior of the predictors' coefficients",
         subtitle = "Negative coefficients decrease age of acquisition\nPositive coefficients increase age of acquisition") +
    scale_colour_brewer(palette = "YlOrBr") +
    theme_custom +
    theme(legend.position = c(0.3, 0.9),
          legend.direction = "horizontal",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.y = element_line(colour = "grey"),
          axis.title.y = element_blank()) +
    ggsave(here("Figures", "07_analysis_prod-coefs.png"), width = 7)


#### compare models #########################
loo0 <- add_criterion(fit0, "loo", file = here("Results", "prod-L1_fit0"))
loo1 <- add_criterion(fit1, "loo", file = here("Results", "prod-L1_fit1"))
loo_comp <- loo_compare(loo0, loo1)
comp <- list(loo_comp)

#### export results #########################
fits <- list(fit0, fit1)
saveRDS(fits, here("Results", "prod-L1_fits.rds"))
saveRDS(prod, here("Results", "prod-L1_selection.rds"))
