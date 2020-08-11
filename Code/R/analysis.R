#### analysis ########################

#### set up ##########################

# load packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(data.table)
library(brms)
library(tidybayes)
library(BayesFactor)
library(janitor)
library(here)
# load functions

# set params
set.seed(888)
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

#### import data #####################

dat <- fread(here("Data", "preprocessed.csv"), na.strings = "", stringsAsFactors = TRUE)

contrasts(dat$cognate) <- contr.sum(c("Non-cognate", "Cognate"))/2
contrasts(dat$item_dominance) <- contr.sum(c("L2", "L1"))/2

dat_priors <- fread(here("Data", "05_priors-wordbank.csv")) %>%
    mutate(estimate_scaled = (estimate_scaled-14)/2)

inv_logit <- function(x) 1 / (1 + exp(-x))

#### comprehension ###################
# set priors 
comp_priors <- c(prior(normal(0.7857192, 0.5), nlpar = "asym", coef = "Intercept"),
                 prior(normal(-1.7576520, 0.5), nlpar = "steep", coef = "Intercept"),
                 prior(normal(4.369435, 1), nlpar = "mid", coef = "Intercept"),
                 prior(normal(0, 5), class = "b", nlpar = "mid"),
                 prior(cauchy(0, 5), class  = "sd", nlpar = "mid"))

# `model 0
comp_fit0 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * steep)),
                              asym ~ 1,
                              steep ~ 1,
                              mid ~ 1 + (1 | meaning),
                              nl = TRUE),
                 prior = comp_priors,
                 data = filter(dat, type=="Comprehensive"),
                 chains = 1,
                 iter = 2000,
                 file = here("Results", "comp_fit0.rds"),
                 save_model = here("Code", "Stan", "comp_fit0.stan"),
                 control = list(adapt_delta = 0.95, max_treedepth = 15))

# model 1
comp_fit1 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * steep)),
                              asym ~ 1,
                              steep ~ 1,
                              mid ~ 1 + item_dominance + (1 + item_dominance | meaning),
                              nl = TRUE),
                 prior = c(comp_priors, prior(lkj(2), class = "cor")),
                 data = filter(dat, type=="Comprehensive"),
                 chains = 1,
                 iter = 6000,
                 file = here("Results", "comp_fit1.rds"),
                 save_model = here("Code", "Stan", "comp_fit1.stan"),
                 control = list(adapt_delta = 0.95, max_treedepth = 15))

# model 2
comp_fit2 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * steep)),
                              asym ~ 1,
                              steep ~ 1,
                              mid ~ 1 + item_dominance*cognate + (1 + item_dominance*cognate | meaning),
                              nl = TRUE),
                 prior = c(comp_priors, prior(lkj(2), class = "cor")),
                 data = filter(dat, type=="Comprehensive"),
                 chains = 1,
                 iter = 6000,
                 file = here("Results", "comp_fit2.rds"),
                 save_model = here("Code", "Stan", "comp_fit2.stan"),
                 control = list(adapt_delta = 0.95, max_treedepth = 15))

# compare models
comp_loo <- loo(comp_fit0, comp_fit1, comp_fit2)
saveRDS(comp_loo, here("Results", "comp_loo.rds"))

#### production ###############################

prod_priors <- c(prior(normal(0.7857192, 0.5), nlpar = "asym", coef = "Intercept"),
                 prior(normal(-1.7576520, 1), nlpar = "steep", coef = "Intercept"),
                 prior(normal(5.369435, 1), nlpar = "mid", coef = "Intercept"),
                 prior(normal(0, 5), class = "b", nlpar = "mid"),
                 prior(cauchy(0, 5), class  = "sd", nlpar = "mid"))

# `model 0
prod_fit0 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * steep)),
                         asym ~ 1,
                         steep ~ 1,
                         mid ~ 1 + (1 | meaning),
                         nl = TRUE),
            prior = prod_priors,
            data = filter(dat, type=="Productive"),
            chains = 1,
            iter = 2000,
            file = here("Results", "prod_fit0.rds"),
            save_model = here("Code", "Stan", "prod_fit0.stan"),
            control = list(adapt_delta = 0.95, max_treedepth = 15))

# model 1
prod_fit1 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * steep)),
                         asym ~ 1,
                         steep ~ 1,
                         mid ~ 1 + bilingualism + (1 + item_dominance | meaning),
                         nl = TRUE),
            prior = c(prod_priors, prior(lkj(2), class = "cor")),
            data = filter(dat, type=="Productive"),
            chains = 1,
            iter = 6000,
            file = here("Results", "prod_fit1.rds"),
            save_model = here("Code", "Stan", "prod_fit1.stan"),
            control = list(adapt_delta = 0.95, max_treedepth = 15))

# model 2
prod_fit2 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * steep)),
                         asym ~ 1,
                         steep ~ 1,
                         mid ~ 1 + item_dominance*cognate + (1 + item_dominance*cognate | meaning),
                         nl = TRUE),
            prior = c(prod_priors, prior(lkj(2), class = "cor")),
            data = filter(dat, type=="Productive"),
            chains = 1,
            iter = 6000,
            file = here("Results", "prod_fit2.rds"),
            save_model = here("Code", "Stan", "prod_fit2.stan"),
            control = list(adapt_delta = 0.95, max_treedepth = 15))

# compare models
prod_loo <- loo(prod_fit0, prod_fit1, prod_fit2)
saveRDS(prod_loo, here("Results", "prod_loo.rds"))

#### analyse random effects ####################
random <- spread_draws(comp_fit2,
                       `b_mid_.*`,
                       `sd_.*`,
                       r_meaning__mid[meaning,param], regex = TRUE) %>% 
    median_qi(.width = 0.5) %>% 
    select(-matches("lower|upper|point|interval|width")) %>% 
    ungroup() %>% 
    pivot_wider(names_from = param, values_from = r_meaning__mid) %>% 
    rename(r_intercept = Intercept) %>% 
    clean_names() %>% 
    rowwise() %>% 
    mutate(r_intercept_scaled = b_mid_intercept + r_intercept*sd_meaning_mid_intercept,
           r_item_dominance1_scaled = b_mid_item_dominance1 + item_dominance1*sd_meaning_mid_item_dominance1,
           r_cognate1_scaled = b_mid_cognate1 + cognate1*sd_meaning_mid_cognate1,
           r_item_dominance1_cognate1_scaled = b_mid_item_dominance1_cognate1 + item_dominance1_cognate1*sd_meaning_mid_item_dominance1_cognate1)

corr <- spread_draws(comp_fit2, `sd_.*`, `cor_.*`, regex = TRUE) %>% 
    mean_hdi(.width = 0.5) %>% 
    select(-matches("point|interval|width")) %>% 
    clean_names()

saveRDS(random, here("Results", "ranef.rds"))
saveRDS(corr, here("Results", "corr.rds"))

#### anova ##############################
dat_random <- left_join(dat, random, by = "meaning")
anova <- anovaBF(r_intercept_scaled ~ cognate, data = 
)

ggplot(dat_random, aes(cognate, r_intercept_scaled)) +
    geom_violin() +
    geom_point()


    






