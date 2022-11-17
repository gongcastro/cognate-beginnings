library(brms)
library(tidybayes)
library(ggplot2)

fit_0 <- brm(
    rating ~ treat + period + carry + (1 | subject), 
    data = inhaler, 
    family = cumulative(), 
    prior = set_prior("normal(0,5)"),
    backend = "cmdstanr"
)


fit_1 <- brm(
    rating ~ treat + period + (1 | subject), 
    data = inhaler, 
    family = cumulative(), 
    prior = set_prior("normal(0,5)"),
    backend = "cmdstanr"
)


fit_1$data %>% 
    # sample_n(10) %>% 
    add_predicted_draws(fit_1) %>% 
    mutate(
        correct = .prediction==rating,
        rating = as.factor(rating)
    ) %>% 
    count(rating, treat, .prediction) %>% 
    ggplot(aes(.prediction, n, fill = .prediction)) +
    facet_grid(rating~treat) +
    geom_col() +
    theme_custom() +
    scale_fill_d3()
