#### 05_analysis: Bayesian non-linear model ##############
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(wesanderson)
library(data.table)
library(truncnorm)
library(brms)
library(modelr)
library(tidybayes)
library(here)

# create/load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
age_bins <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "30-32")

#### import data ##########################################
dat <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    mutate(age_bin = as.numeric(factor(age_bin), levels = age_bins, ordered = TRUE)-1,
           lp = ifelse(lp=="Monolingual", -0.5, 0.5),
           cognateness = ifelse(cognate_rater1=="Cognate", 0.5, -0.5),
           frequency = frequency-mean(.$frequency, na.rm = TRUE)) %>%
    arrange(meaning, age_bin, lp) %>%
    select(meaning, type, age_bin, lp, successes, n, cognateness, frequency) 

dat_priors <- fread(here("Data", "05_priors.csv")) %>%
    as_tibble()

#### priors ###############################################
prior_asym  <- dat_priors$estimate[dat_priors$term=="asym"] 
prior_mid   <- dat_priors$estimate[dat_priors$term=="mid"] 
prior_steep <- dat_priors$estimate[dat_priors$term=="steep"] 

#### fit models ###########################################

# model 0
formula0 <- bf(successes | trials(n) ~ inv_logit(prior_asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
               steep ~ 1,
               mid ~ 1 + lp + frequency,
               nl = TRUE, family = binomial(link = "logit"))

priors0 <- c(prior(normal(0.749, 0.1), nlpar = "asym", coef = "Intercept"),
             prior(normal(6, 1), nlpar = "mid", coef = "Intercept"),
             prior(normal(3.28, 1.5), nlpar = "steep", coef = "Intercept"))

fit0 <- brm(formula = formula0,
            data = dat,
            prior = priors0,
            save_model = here("Stan", "fit0.stan"),
            control = list(adapt_delta = 0.9, max_treedepth = 15))

# model 1
formula1 <- bf(successes | trials(n) ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
               asym ~ 1, steep ~ 1, mid ~ 1 + frequency + item_dominance*lp,
               nl = TRUE, family = binomial(link = "logit"))

priors1 <- c(prior(normal(0.749, 0.1), nlpar = "asym", coef = "Intercept"),
             prior(normal(6, 1), nlpar = "mid", coef = "Intercept"),
             prior(normal(3.28, 1.5), nlpar = "steep", coef = "Intercept"))

fit1 <- brm(formula = formula1,
            data = dat,
            prior = priors1,
            save_model = here("Stan", "fit1.stan"),
            control = list(adapt_delta = 0.9, max_treedepth = 15))

# model 2
formula2 <- bf(successes | trials(n) ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
               asym ~ 1, steep ~ 1,  mid ~ 1 + frequency + item_dominance*lp*cognateness,
               nl = TRUE, family = binomial(link = "logit"))

priors2 <- c(prior(normal(0.749, 0.1), nlpar = "asym", coef = "Intercept"),
             prior(normal(6, 1), nlpar = "mid", coef = "Intercept"),
             prior(normal(3.28, 1.5), nlpar = "steep", coef = "Intercept"),
             prior(normal(0, 1), nlpar = "mid", coef = "cognateness"))

fit12 <- brm(formula = formula2,
             data = dat,
             prior = priors2,
             save_model = here("Stan", "fit2.stan"),
             control = list(adapt_delta = 0.9, max_treedepth = 15))
