# models

# fit comprehension models
fit_models_comp <- function(
    ... # arguments passed to brms::brm()
){
    
    # formula
    formulas <- list(
        f_0 = bf(
            understands ~ 1 + age_center + frequency_center +
                (1 + age_center | te) +
                (1 + frequency_center | id),
            family = bernoulli("logit")
        ),
        f_1 = bf(
            understands ~ 1 + age_center + frequency_center + doe_center +
                (1 + age_center + doe_center | te) +
                (1 + frequency_center | id),
            family =  bernoulli("logit")
        ),
        f_2 = bf(
            understands ~ 1 + age_center + frequency_center + doe_center*cognate +
                (1 + age_center + doe_center*cognate | te) +
                (1 + frequency_center + doe_center | id),
            family =  bernoulli("logit")
        )
    )
    
    # priors (derived from prior samples)
    priors <- c(
        # model 0
        prior(normal(0.5, 0.1), class = "Intercept"),
        prior(normal(0.75, 0.1), class = "b", coef = "age_center"),
        prior(normal(0, 0.1), class = "b", coef = "frequency_center"),
        prior(normal(0.2, 0.1), class = "sd", group = "te"),
        prior(constant(0.2), class = "sd", group = "id"),
        # model 1
        prior(lkj(10), class = "cor"),
        prior(normal(0, 0.1), class = "b", coef = "doe_center"),
        # model 2
        prior(normal(0, 0.1), class = "b", coef = "cognate1"),
        prior(normal(0, 0.1), class = "b", coef = "doe_center:cognate1"),
        prior(normal(0, 0.1), class = "b", coef = "cognate2"),
        prior(normal(0, 0.1), class = "b", coef = "doe_center:cognate2")
    )
    
    # fit models
    fit_0 <- brm(
        formula = formulas$f_0, prior = priors[1:6,],
        save_model = here("Stan", "irt_comp-0.stan"),
        file = here("Results", "irt_comp-0.rds"),
        ...
    ) 
    fit_1 <- brm(
        formula = formulas$f_1, prior = priors[1:7,],
        save_model = here("Stan", "irt_comp-1.stan"),
        file = here("Results", "irt_comp-1.rds"),
        ...
    ) 
    fit_2 <- brm(
        formula = formulas$f_2, prior = priors,
        save_model = here("Stan", "irt_comp-2.stan"),
        file = here("Results", "irt_comp-2.rds"),
        ...
    ) 
    
    fits <- list(fit_0 = fit_0, fit_1 = fit_1, fit_2 = fit_2)
    
    return(fits)   
}

# fit production models

fit_models_prod <- function(
    ... # arguments passed to brms::brm()
){
    
    # formula
    formulas <- list(
        f_0 = bf(
            produces ~ 1 + age_center + frequency_center +
                (1 + age_center | te) +
                (1 + frequency_center | id),
            family = bernoulli("logit")
        ),
        f_1 = bf(
            produces ~ 1 + age_center + frequency_center + doe_center +
                (1 + age_center + doe_center | te) +
                (1 + frequency_center | id),
            family =  bernoulli("logit")
        ),
        f_2 = bf(
            produces ~ 1 + age_center + frequency_center + doe_center*cognate +
                (1 + age_center + doe_center*cognate | te) +
                (1 + frequency_center + doe_center | id),
            family =  bernoulli("logit")
        )
    )
    
    # priors (derived from prior samples)
    priors <- c(
        # model 0
        prior(normal(0.5, 0.1), class = "Intercept"),
        prior(normal(0.75, 0.1), class = "b", coef = "age_center"),
        prior(normal(0, 0.1), class = "b", coef = "frequency_center"),
        prior(normal(0.2, 0.1), class = "sd", group = "te"),
        prior(constant(0.2), class = "sd", group = "id"),
        # model 1
        prior(lkj(10), class = "cor"),
        prior(normal(0, 0.1), class = "b", coef = "doe_center"),
        # model 2
        prior(normal(0, 0.1), class = "b", coef = "cognate1"),
        prior(normal(0, 0.1), class = "b", coef = "doe_center:cognate1"),
        prior(normal(0, 0.1), class = "b", coef = "cognate2"),
        prior(normal(0, 0.1), class = "b", coef = "doe_center:cognate2")
    )
    
    # fit models
    fit_0 <- brm(
        formula = formulas$f_0, prior = priors[1:6,],
        save_model = here("Stan", "irt_prod-0.stan"),
        file = here("Results", "irt_prod-0.rds"),
        ...
    ) 
    fit_1 <- brm(
        formula = formulas$f_1, prior = priors[1:7,],
        save_model = here("Stan", "irt_prod-1.stan"),
        file = here("Results", "irt_prod-1.rds"),
        ...
    ) 
    fit_2 <- brm(
        formula = formulas$f_2, prior = priors,
        save_model = here("Stan", "irt_prod-2.stan"),
        file = here("Results", "irt_prod-2.rds"),
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
}
