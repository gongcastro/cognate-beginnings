# fit models

fit_models_comp <- function(
    ... # arguments passed to brms::brm()
){
    
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
        prior(lkj(10), class = "cor"),
        prior(normal(0, 0.1), class = "b", coef = "lp1"),
        # model 2
        prior(normal(0, 0.1), class = "b", coef = "cognate1"),
        prior(normal(0, 0.1), class = "b", coef = "lp1:cognate1"),
        prior(normal(0, 0.1), class = "b", coef = "cognate2"),
        prior(normal(0, 0.1), class = "b", coef = "lp1:cognate2")
    )
    
    # fit models
    fit_0 <- brm(
        formula = formulas$f_0, prior = priors[1:6,],
        save_model = here("Stan", "irt_comp_0.stan"),
        file = here("Results", "irt_comp_0.rds"),
        ...
    ) 
    fit_1 <- brm(
        formula = formulas$f_1, prior = priors[1:7,],
        save_model = here("Stan", "irt_comp_1.stan"),
        file = here("Results", "irt_comp_1.rds"),
        ...
    ) 
    fit_2 <- brm(
        formula = formulas$f_2, prior = priors,
        save_model = here("Stan", "irt_comp_2.stan"),
        file = here("Results", "irt_comp_2.rds"),
        ...
    ) 
    
    fits <- list(fit_0 = fit_0, fit_1 = fit_1, fit_2 = fit_2)
    
    return(fits)   
}


# compare models
compare_models <- function(
    fits
){
    loo <- loo_compare(map(fits, loo_subsample))
    saveRDS(loo, here("Results", "loo.rds"))
}


# fit models

fit_models_prod <- function(
    ... # arguments passed to brms::brm()
){
    
    # formula
    formulas <- list(
        f_0 = bf(produces ~ 1 + age + frequency_center + (1 + age | te) + (1 + frequency_center | id), family = bernoulli("logit")),
        f_1 = bf(produces ~ 1 + age + frequency_center + lp + (1 + age + lp | te) + (1 + frequency_center | id), family =  bernoulli("logit")),
        f_2 = bf(produces ~ 1 + age + frequency_center + lp*cognate + (1 + age + lp | te) + (1 + frequency_center + cognate | id), family =  bernoulli("logit"))
    )
    
    # priors (derived from prior samples)
    priors <- c(
        # model 0
        prior(normal(0, 0.1), class = "Intercept"),
        prior(normal(1.5, 0.1), class = "b", coef = "age"),
        prior(normal(0, 0.1), class = "b", coef = "frequency_center"),
        prior(normal(0.2, 0.1), class = "sd", group = "te"),
        prior(constant(0.2), class = "sd", group = "id"),
        # model 1
        prior(lkj(10), class = "cor"),
        prior(normal(0, 0.1), class = "b", coef = "lp1"),
        # model 2
        prior(normal(0, 0.1), class = "b", coef = "cognate1"),
        prior(normal(0, 0.1), class = "b", coef = "lp1:cognate1"),
        prior(normal(0, 0.1), class = "b", coef = "cognate2"),
        prior(normal(0, 0.1), class = "b", coef = "lp1:cognate2")
    )
    
    # fit models
    fit_0 <- brm(
        formula = formulas$f_0, prior = priors[1:6,],
        save_model = here("Stan", "irt_prod_0.stan"),
        file = here("Results", "irt_prod_0.rds"),
        ...
    ) 
    fit_1 <- brm(
        formula = formulas$f_1, prior = priors[1:7,],
        save_model = here("Stan", "irt_prod_1.stan"),
        file = here("Results", "irt_prod_1.rds"),
        ...
    ) 
    fit_2 <- brm(
        formula = formulas$f_2, prior = priors,
        save_model = here("Stan", "irt_prod_2.stan"),
        file = here("Results", "irt_prod_2.rds"),
        ...
    ) 
    
    fits <- list(fit_0 = fit_0, fit_1 = fit_1, fit_2 = fit_2)
    
    return(fits)   
}


