get_aoa  <- function(model, data, te, ...) {
    posterior_draws_re <- get_posterior_draws_re(model)
    
    aoa <- posterior_draws_re |> 
        filter(te %in% .env$te,
               .variable %in% c("age_std", "Intercept", "n_phon_std", "exposure_std", "age_std:exposure_std")) |> 
        mutate(.variable = paste0(.variable, " / ", type)) |> 
        pivot_wider(id_cols = c(te, .draw), 
                    names_from = .variable,
                    values_from = .value_rescaled) |> 
        drop_na() |> 
        clean_names() |> 
        mutate(aoa_comprehension = -(intercept_comprehension + n_phon_std_both + exposure_std_both + age_std_exposure_std_both)/(age_std_both),
               aoa_production = -(intercept_comprehension_and_production + n_phon_std_both + exposure_std_both + age_std_exposure_std_both)/(age_std_both),
               aoa_comprehension_rescaled = rescale_variable(aoa_comprehension,
                                                             mean(.env$data$age),
                                                             sd(.env$data$age)),
               aoa_production_rescaled = rescale_variable(aoa_production,
                                                          mean(.env$data$age),
                                                          sd(.env$data$age))) |> 
        select(te, .draw = draw, starts_with("aoa_")) 
}




predictions_te |>
    filter(te %in% .env$te, exposure == "Exposure: mean") |>
    mutate(.category = ifelse(.category=="Understands", 
                              "Comprehension",
                              "Comprehension and Production"),
           te = as.factor(te)) |> 
    ggplot(aes(age, .value, colour = te)) +
    facet_wrap(~.category) +
    stat_summary(aes(group = te), fun = median, geom = "line", linewidth = 1) +
    
    aoa |> 
    select(te, .draw, ends_with("_rescaled")) |> 
    pivot_longer(starts_with("aoa_"),
                 names_to = ".category",
                 values_to = ".value") |> 
    mutate(.category = ifelse(.category=="aoa_comprehension_rescaled", 
                              "Comprehension",
                              "Comprehension and Production"),
           te = as.factor(te)) |> 
    ggplot(aes(.value, te, fill = te)) + 
    facet_wrap(~.category, ncol = 2) +
    stat_slab(colour = "white") + 
    stat_pointinterval() + 

    plot_layout(ncol = 1) &
    scale_x_continuous(limits = c(10, 50))
