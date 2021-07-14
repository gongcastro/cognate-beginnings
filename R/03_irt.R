# IRT

# set up ----
library(tidyverse)
library(brms) # for Bayesian models
library(tidybayes) # for posterior draws
library(job) # for running code in the background
library(here) # for reproducible file paths

# set params
options(mc.cores = 4)

# import data ----
responses <- readRDS(here("Data", "responses.rds"))

# models ----
# set contrasts
contrasts(responses$cognate) <- c(0.5, -0.5)
contrasts(responses$lp) <- c(0.5, -0.5)

# formula
formulas <- list(
    f_0 = bf(understands ~ 1 + age + frequency_center + (1 + age | te) + (1 + frequency_center | id), family = bernoulli("logit")),
    f_1 = bf(understands ~ 1 + age + frequency_center + lp + (1 + age + lp | te) + (1 + frequency_center | id), family =  bernoulli("logit")),
    f_2 = bf(understands ~ 1 + age + frequency_center + lp*cognate + (1 + age + lp | te) + (1 + frequency_center + cognate | id), family =  bernoulli("logit"))
)

# priors (derived from prior samples)
priors <- c(
    # model 0
    prior(normal(0, 0.1), class = "Intercept"),
    prior(normal(0.75, 0.1), class = "b", coef = "age"),
    prior(normal(0, 0.1), class = "b", coef = "frequency_center"),
    prior(normal(0.2, 0.1), class = "sd", group = "te"),
    prior(constant(0.2), class = "sd", group = "id"),
    # model 1
    prior(lkj(7), class = "cor"),
    prior(normal(0, 0.1), class = "b", coef = "lp1"),
    # model 2
    prior(normal(0, 0.1), class = "b", coef = "cognate1"),
    prior(normal(0, 0.1), class = "b", coef = "lp1:cognate1")
)

# fit model
job(
    title = "Fit models",
    fits = {
        options(mc.cores = 4)
        
        fit_0 = brm(
            formula = formulas$f_0, data = responses, prior = priors[1:6,],
            chains = 4, iter = 2000, inits = 0,
            backend = "cmdstanr", save_pars = save_pars(all = TRUE),
            save_model = here("Stan", "irt_0.stan"), file = here("Results", "irt_0.rds")
        ) 
        
        fit_1 = brm(
            formula = formulas$f_1, data = responses, prior = priors[1:7,],
            chains = 4, cores = 4, iter = 2000,
            save_pars = save_pars(all = TRUE),
            inits = 0,
            save_model = here("Stan", "irt_1.stan"),
            file = here("Results", "irt_1.rds")
        ) 
        
        fit_2 = brm(
            formula = formulas$f_2, data = responses, prior = priors,
            chains = 4, cores = 4, iter = 2000,
            save_pars = save_pars(all = TRUE),
            inits = 0,
            save_model = here("Stan", "irt_2.stan"),
            file = here("Results", "irt_2.rds")
        ) 
        
        export(c(fit_0, fit_1, fit_2))
    }, import = c(responses, priors, formulas) 
)

fits <- as.list(fits)

# compare models
job(
    title = "Compare models",
    loo = {
        loo = loo_compare(map(fits, loo_subsample))
        export(loo)
    }, import = c(fits) 
)
saveRDS(loo, here("Results", "loo.rds"))

