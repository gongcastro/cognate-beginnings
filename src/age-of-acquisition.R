get_aoa_data <- function(responses) 
{
    
    aoa_data <- responses |>
        select(id, age, age_std, te, item, response, exposure_std) |>
        mutate(comprehension = response != "No",
               production = responses == "Understands & Says")
    
    return(aoa_data)
}

get_aoa_model_fit <- function(aoa_data, type, ...)
{
    if (type=="comprehension") {
        model_formula <- bf(
            comprehension ~ 1 * inv(1 + exp((mid - age_std) * exp(scale))),
            mid ~ 1 + exposure_std + (1 + exposure_std | te),
            scale ~ 1 + exposure_std + (1 + exposure_std | te),
            nl = TRUE,
            family = bernoulli("logit")
        )
    } else if(type=="production") {
        model_formula <- bf(
            comprehension ~ 1 * inv(1 + exp((mid - age_std) * exp(scale))),
            mid ~ 1 + exposure_std + (1 + exposure_std | te),
            scale ~ 1 + exposure_std + (1 + exposure_std | te),
            nl = TRUE,
            family = bernoulli("logit")
        )
    } else {
        cli_abort("{.code type} must be 'comprehension' or 'production', \
                  not '{type}'")
    }
    
    fit <- brm(model_formula,
               data = aoa_data,
               iter = 2000,
               chains = 4,
               cores = 4,
               init = 1,
               control = list(adapt_delta = 0.9, 
                              max_treedepth = 15),
               seed = 20211014,
               file = glue("results/aoa_{type}.rds"),
               file_refit = "on_change",
               backend = "cmdstanr",
               ...
    )
    
    return(fit)
}

get_aoa_posterior <- function(x, aoa_data)
{
    
    epreds <- aoa_data %>%
        expand(age_std = scale(seq(0, 50, 0.1),
                               mean(aoa_data$age),
                               sd(aoa_data$age)),
               te = unique(aoa_data$te),
               exposure_std = c(-1, 0, 1)) %>%
        add_epred_draws(fit, ndraws = 25) |>
        mutate(.epred = qlogis(.epred),
               exposure_std = as.factor(exposure_std))
    
    aoa_posterior <- spread_draws(x,
                                  b_mid_Intercept,
                                  b_mid_exposure_std,
                                  r_te__mid[te, .param]) |> 
        median_hdi() |> 
        distinct(te, .param, .keep_all = TRUE) |> 
        select(-c(.width, .interval)) |> 
        pivot_wider(names_from = .param,
                    values_from = is.numeric) |>
        mutate(intercept = b_mid_Intercept_Intercept + r_te__mid_Intercept,
               intercept.lower = b_mid_Intercept.lower_Intercept + r_te__mid.lower_Intercept,
               intercept.upper = b_mid_Intercept.upper_Intercept + r_te__mid.upper_Intercept,
               exposure_std = b_mid_exposure_std_exposure_std + r_te__mid_exposure_std,
               exposure_std.lower = b_mid_exposure_std.lower_exposure_std + r_te__mid.lower_exposure_std,
               exposure_std.upper = b_mid_exposure_std.upper_exposure_std + r_te__mid.upper_exposure_std) |> 
        mutate(`-1.median` = intercept - exposure_std,
               `-1.lower` = intercept.lower - exposure_std.lower,
               `-1.upper` = intercept.upper - exposure_std.upper,
               `0.median` = intercept,
               `0.lower` = intercept.lower,
               `0.upper` = intercept.upper,
               `1.median` = intercept + exposure_std,
               `1.lower` = intercept.lower + exposure_std.lower,
               `1.upper` = intercept.upper + exposure_std.upper) |> 
        select(te, `-1.median`:`1.upper`) |> 
        pivot_longer(`-1.median`:`1.upper`,
                     names_to = c("exposure_std", ".summary"),
                     names_sep = "\\.",
                     values_to = ".value",
                     names_transform = as.factor) |> 
        pivot_wider(names_from = .summary,
                    values_from = .value,
                    names_prefix = ".") |> 
        rename(.value = .median)
    
    return(aoa_posterior)
}

