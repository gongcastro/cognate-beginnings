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
library(wesanderson)
library(here)

# create/load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins <- c("18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32")

#### import data ##########################################
dat <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    arrange(meaning, age_bin, lp) %>%
    select(meaning, type, age_bin, lp, successes, n, cognate, frequency) %>%
    filter(type=="Productive")

dat_priors <- fread(here("Data", "05_priors.csv")) %>%
    as_tibble()

#### priors ###############################################
prior_asym  <- dat_priors$estimate[dat_priors$term=="asym"] %>% inv_logit_scaled()
prior_mid   <- dat_priors$estimate[dat_priors$term=="mid"] 
prior_steep <- dat_priors$estimate[dat_priors$term=="steep"] 

#### fit models ###########################################

# model 0
formula0 <- bf(successes | trials(n) ~ 0.6789804 * inv(1 + exp((mid - age_bin) * exp(steep))),
               steep ~ 1, mid ~ 1 + lp + frequency,
               nl = TRUE, family = binomial(link = "logit"))

priors0 <- c(prior(normal(6, 1), class = "b", nlpar = "mid", coef = "Intercept"),
             prior(normal(3.28, 1.5),class = "b", nlpar = "steep", coef = "Intercept"),
             prior(normal(0, 5), class = "b", nlpar = "mid", coef = "lp"))

fit0 <- brm(formula = formula0,
            data = dat,
            prior = priors0,
            save_model = here("Code", "Stan", "prod_fit0.stan"),
            stan_model_args = list(save_dso = TRUE),
            control = list(adapt_delta = 0.9, max_treedepth = 15))

# model 1
formula1 <- bf(successes | trials(n) ~ 0.6789804 * inv(1 + exp((mid - age_bin) * exp(steep))),
               steep ~ 1, mid ~ 1 + frequency + lp*cognate,
               nl = TRUE, family = binomial(link = "logit"))

priors1 <- c(prior(normal(6, 1), nlpar = "mid", coef = "Intercept"),
             prior(normal(3.28, 1.5), nlpar = "steep", coef = "Intercept"),
             prior(normal(0, 5), class = "b", nlpar = "mid", coef = "cognate"),
             prior(normal(0, 5), class = "b", nlpar = "mid", coef = "lp"),
             prior(normal(0, 5), class = "b", nlpar = "mid", coef = "lp:cognate"))

fit1 <- brm(formula = formula1,
            data = dat,
            prior = priors1,
            save_model = here("Code", "Stan", "prod_fit1.stan"),
            stan_model_args = list(save_dso = TRUE),
            control = list(adapt_delta = 0.9, max_treedepth = 15))

fit1_prior <- brm(formula = formula1,
                  data = dat,
                  prior = priors1,
                  sample_prior = "only",
                  save_model = here("Code", "Stan", "prod_fit1-prior.stan"),
                  stan_model_args = list(save_dso = TRUE),
                  control = list(adapt_delta = 0.9, max_treedepth = 15))

#### compare models #########################
loo0 <- add_criterion(fit0, "loo", file = here("Results", "prod_fit0.rds"))
loo1 <- add_criterion(fit1, "loo", file = here("Results", "prod_fit1.rds"))
loo_comp <- loo_compare(loo0, loo1)
bayes_comp <- bayes_factor(loo0, loo1)

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
    ggsave(here("Figures", "07_analysis-productive-convergence.png"), height = 6, width = 5.4)

# Gelman-Rubin diagnostic (Rhat) should be close to 1.0
fit0$fit 
fit1$fit

# autocorrelation across chains (should be low after some lags)
autocorr.diag(as.mcmc(fit0)[, 1:5], lags = c(0, 1, 2, 3, 4, 5, 10, 50))


#### check posterior #########################
ggplot(posterior, aes(x = value, fill = model)) +
    facet_wrap(~parameter, scales = "free") +
    geom_density(alpha = 0.5) +
    labs(x = "Value", y = "Density", fill = "Model") +
    scale_fill_brewer(palette = "Set1") +
    theme_custom +
    ggsave(here("Figures", "07_analysis-productive-posterior.png"), height = 6, width = 5.4)

# coefficients
intervals <- posterior %>% 
    mutate(parameter = as.character(parameter)) %>%
    filter(model %in% "Model 1",
           parameter %in% c("b_mid_frequency", "b_mid_lp", "b_mid_cognate", "b_mid_lp:cognate")) %>% 
    group_by(parameter) %>%
    median_qi(value, .width = c(0.95, 0.89, 0.50))


posterior %>%
    mutate(parameter = as.character(parameter)) %>%
    filter(model %in% "Model 1",
           parameter %in% c("b_mid_frequency", "b_mid_lp", "b_mid_cognate", "b_mid_lp:cognate")) %>%
    ggplot(aes(y = parameter, x = value)) +
    stat_slabh(fill = "grey30", colour = NA) +
    geom_intervalh(data = intervals, position = position_nudge(y = -0.3), alpha = 0.7) +
    geom_pointintervalh(position = position_nudge(y = -0.1), size = 0.1, data = intervals) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Value", y = "Parameter", fill = "Credible Interval") +
    scale_colour_brewer(palette = "Blues") +
    theme_custom +
    theme(legend.position = "top",
          panel.grid.major.y = element_line(colour = "grey")) +
    ggsave(here("Figures", "07_analysis-productive-coefs.png"), width = 5)

