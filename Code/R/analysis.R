#### Analysis: Analyse vocabulary data ##########################################

#### set up #####################################################################

# load packages
library(tidyverse)
library(data.table)
library(brms)
library(tidybayes)
library(here)

# create/load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

##### import data and set params ###############################################
dat <- fread(here("Data", "preprocessed.csv"), na.string = c("", "NA")) %>% 
    as_tibble() %>% 
    filter(type %in% "Comprehensive") %>% 
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1,
           age_center = age-mean(age, na.rm = TRUE),
           cognate = as.factor(cognate),
           item_dominance = as.factor(item_dominance),
           response = as.numeric(response)) %>%
    arrange(type, item, te, age_center) %>% 
    select(item, te, item_dominance, cognate, age_center, response)

contrasts(dat$cognate) <- c(0.5, -0.5) 
contrasts(dat$item_dominance) <- c(0.5, -0.5) 

#### fit model #################################################################
priors <- c(prior(normal(0, 0.1), class = "Intercept"),
            prior(normal(0, 0.5), class = "b"),
            prior(exponential(2.5), class = "sd"),
            prior(lkj(2), class = "cor"))

fit0 <- brm(response ~ age_center + (age_center | te), 
            family = bernoulli("logit"),
            data = dat,
            prior = priors,
            file = here("Results", "fit0.rds"),
            chains = 1)

fit1 <- brm(response ~ age_center*item_dominance + (age_center*item_dominance | te), 
            family = bernoulli("logit"),
            data = dat,
            prior = priors,
            file = here("Results", "fit1.rds"),
            chains = 1)

fit2 <- brm(response ~ age_center*item_dominance*cognate + (age_center*item_dominance | te), 
            family = bernoulli("logit"),
            data = dat,
            prior = priors,
            file = here("Results", "fit2.rds"),
            chains = 1)

#### compare models ############################################################
loo0 <- loo(fit0)
loo1 <- loo(fit1)
loo2 <- loo(fit2)
comp <- loo_compare(loo(fit0), loo(fit1), loo(fit2))

#### examine posterior #########################################################
post <- gather_draws(fit2, `b_.*`, regex = TRUE)
post_preds <- distinct(dat, te, cognate) %>%
    left_join(., expand_grid(te = unique(dat$te),
                             age_bin_center = seq(min(dat$age_bin_center),
                                                  max(dat$age_bin_center),
                                                  by = 0.1),
                             item_dominance = c("L1", "L2"),
                             n = 1)) %>% 
    add_fitted_draws(fit1, n = 10) %>% 
    mutate(te = as.factor(te))

#### visualise data ############################################################
# posterior distributions
ggplot(post, aes(.value, .variable)) +
    geom_vline(xintercept = 0) +
    stat_dotsinterval(quantiles = seq(0, 1, by = 0.005)) 

# joint posterior distribution
spread_draws(fit1, `b_.*`, regex = TRUE) %>% 
    ggplot(aes(b_Intercept, b_age_bin_center)) +
    stat_density_2d_filled()

# posterior predictive predictions
ggplot(dat, aes(age_bin_center, proportion)) +
    facet_wrap(te~cognate) +
    geom_line(data = post_preds, aes(y = .value, group = interaction(.draw, item_dominance),
                                     colour = item_dominance)) +
    theme_minimal() +
    theme(legend.position = "none")



