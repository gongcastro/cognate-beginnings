# simulations for deriving priors

# set up ---
library(tidyverse) # for basically everything
library(brms) # for Bayesian models
library(tidybayes) # for posterior draws
library(job) # for running code in the background
library(here) # for reproducible file paths

# set params
options(mc.cores = 4)

# import data ----
responses <- readRDS(here("Data", "responses.rds"))

# prior predicte checks ----
# set contrasts
contrasts(responses$cognate) <- c(0.5, -0.5)
contrasts(responses$lp) <- c(0.5, -0.5)

# formula
formulas <- list(
    f_0 = bf(understands ~ 1 + age + frequency_center + (1 + age | te) + (1 + frequency_center | id), family = bernoulli("logit")),
    f_1 = bf(understands ~ 1 + age + frequency_center + lp + (1 + age + lp | te) + (1 + frequency_center | id), family =  bernoulli("logit")),
    f_2 = bf(understands ~ 1 + age + frequency_center + lp*cognate + (1 + age + lp | te) + (1 + frequency_center + cognate | id), family =  bernoulli("logit"))
)

# priors
priors <- c(
    # model 0
    prior(normal(0, 0.1), class = "Intercept"),
    prior(normal(0.75, 0.1), class = "b", coef = "age"),
    prior(normal(0, 0.1), class = "b", coef = "frequency_center"),
    prior(normal(0.2, 0.1), class = "sd", group = "te"),
    prior(constant(0.2), class = "sd", group = "id"),
    # model 1
    prior(lkj(10), class = "cor"),
    prior(normal(0, 0.1), class = "b", coef = "lp1"),
    # model 2
    prior(normal(0, 0.1), class = "b", coef = "cognate1"),
    prior(normal(0, 0.1), class = "b", coef = "lp1:cognate1")
)

# fit model
job(
    title = "Simulate from prior",
    
    brms_result = {

    fit_0_prior = brm(
        formula = formulas$f_0, data = responses, prior = priors[1:5,],
        chains = 4, cores = 4, iter = 1000,
        save_pars = save_pars(all = TRUE),
        sample_prior = "only",
        save_model = here("Stan", "irt_0_prior.stan"),
        file = here("Results", "irt_0_prior.rds")
    ) 
    
    fit_1_prior = brm(
        formula = formulas$f_1, data = responses, prior = priors[1:7,],
        chains = 4, cores = 4, iter = 4000,
        save_pars = save_pars(all = TRUE),
        sample_prior = "only",
        save_model = here("Stan", "irt_1_prior.stan"),
        file = here("Results", "irt_1_prior.rds")
    )
    
    fit_2_prior = brm(
        formula = formulas$f_2, data = responses, prior = priors,
        chains = 4, cores = 4, iter = 4000,
        save_pars = save_pars(all = TRUE),
        sample_prior = "only",
        save_model = here("Stan", "irt_2_prior.stan"),
        file = here("Results", "irt_2_prior.rds")
    )
    
    
    export(c(fit_0_prior, fit_1_prior, fit_2_prior))
}, import = c(responses, priors, formulas) 

)

