#### simulate: simulate data to check priors and degub Stan code ###############

#### set up ####################################################################

# load packages
library(tidyverse)
library(brms)
library(tidybayes)
library(here)

# set params

#### create data ###############################################################
log_curve <- function(x, Asym = 1, xmid = 24, scal = 1){
    y <- Asym / (1 + exp((xmid - x) / scal))
    return(y)
}

dat <- expand_grid(x = 1:10, item = as.factor(1:10)) %>%
    group_by(item) %>% 
    
    mutate(xmid = rnorm(1, 4.5, 1)) %>% 
    ungroup() %>% 
    mutate(y = log_curve(x, xmid = xmid))

#### fit model #################################################################
priors <- c(prior(normal(4.5, 1), nlpar = "mid", coef = "Intercept"),
            prior(normal(0.5, 1), nlpar = "scale", coef = "Intercept"),
            prior(normal(2, 1), dpar = "phi", class = "Intercept"),
            prior(normal(0, 1), nlpar = "mid", coef = "b"),
            prior(exponential(1), class = "sd"),
            prior(lkj(2), class = "cor"))


inv_logit <- function(x) 1 / (1 + exp(-x))

model <- bf(proportion ~ 1 * inv(1 + exp((mid - age_bin) * exp(scale))),
            mid ~ 1,
            scale ~ 1,
            phi ~ 1,
            nl = TRUE, 
            family = zero_one_inflated_beta(link = "identity"))

fit0 <- brm(model, data = dat,
            prior = priors[1:3,],
            iter = 2000, chains = 1, cores = 1,
            file = here("Results", "fit0.rds"),
            control = list(adapt_delta = 0.9, max_treedepth = 15))

#### visualise data ############################################################
ggplot(dat, aes(x, y, group = item)) +
    geom_line() +
    labs(x = "Age (months)", y = "P(understands|age)") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"))
