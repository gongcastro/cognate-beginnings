#### 05_gca-prod: Bayesian non-linear model ##############
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(tidyverse)
library(data.table)
library(truncnorm)
library(brms)
library(modelr)
library(janitor)
library(tidybayes)
library(here)

# create/load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins <- c("14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")
options(contrasts = c("contr.sum", "contr.poly"))

#### import data ##########################################
dat <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    filter(type=="Productive",
           lp=="Bilingual") %>%
    select(item, meaning, age_bin, item_dominance, cognate, proportion, frequency) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins, ordered = TRUE)),
           item_dominance = as.factor(item_dominance),
           frequency = scale(frequency)[,1],
           cognate = as.factor(cognate)) %>%
    arrange(item, meaning, age_bin)

contrasts(dat$item_dominance) <- contr.sum(c("L2", "L1"))/2
contrasts(dat$cognate) <- contr.sum(c("Non-cognate", "Cognate"))/2

dat_priors <- fread(here("Data", "05_priors-wordbank.csv")) %>%
    mutate(estimate_scaled = (estimate_scaled-14)/2)

#### fit models ###########################################

# load in case models are already fitted
fit_prior <- readRDS(here("Results", "prod_fit-prior.rds"))
fit0 <- readRDS(here("Results", "prod_fit0.rds"))
fit1 <- readRDS(here("Results", "prod_fit1.rds"))

# model 0
fit0 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
                         asym ~ 1,
                         mid ~ 1 + item_dominance + frequency + (1 | meaning),
                         steep ~ 1, 
                         phi ~ 1,
                         nl = TRUE,
                         family = zero_one_inflated_beta),
            prior = c(prior(normal(0.7857192, 0.1), nlpar = "asym", coef = "Intercept"),
                      prior(normal(4.497546, 1), nlpar = "mid", coef = "Intercept"),
                      prior(normal(1.7576520, 0.8), nlpar = "steep", coef = "Intercept"),
                      prior(normal(0, 1), class = "b", nlpar = "mid", coef = "item_dominance1"),
                      prior(normal(0, 1), class = "b", nlpar = "mid", coef = "frequency"),
                      prior(normal(1.5, 1), dpar = "phi", class = "Intercept")),
            data = dat,
            save_all_pars = TRUE,
            save_model = here("Code", "Stan", "prod_fit0.stan"))

# model 1
fit1 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
                         asym ~ 1,
                         mid ~ 1 + item_dominance*cognate + frequency + (1 | meaning),
                         steep ~ 1, 
                         phi ~ 1,
                         nl = TRUE,
                         family = zero_one_inflated_beta),
            prior = c(prior(normal(0.7857192, 0.1), nlpar = "asym", coef = "Intercept"),
                      prior(normal(4.497546, 1), nlpar = "mid", coef = "Intercept"),
                      prior(normal(1.7576520, 0.8), nlpar = "steep", coef = "Intercept"),
                      prior(normal(0, 1), class = "b", nlpar = "mid", coef = "item_dominance1"),
                      prior(normal(0, 1), class = "b", nlpar = "mid", coef = "cognate1"),
                      prior(normal(0, 1), class = "b", nlpar = "mid", coef = "item_dominance1:cognate1"),
                      prior(normal(0, 1), class = "b", nlpar = "mid", coef = "frequency"),
                      prior(normal(1.5, 1), dpar = "phi", class = "Intercept")),
            data = dat,
            save_all_pars = TRUE,
            save_model = here("Code", "Stan", "prod_fit1.stan"))
# export fit
saveRDS(fit0, here("Results", "prod_fit0.Rds"))
saveRDS(fit1, here("Results", "prod_fit1.Rds"))

#### extract posterior samples ###################################3
posterior <- fit1 %>%
    gather_draws(b_asym_Intercept,
                 b_steep_Intercept,
                 b_mid_Intercept,
                 b_mid_item_dominance1,
                 b_mid_cognate1,
                 b_mid_frequency,
                 `b_mid_item_dominance1:cognate1`,
                 Intercept_phi,
                 b_phi_Intercept,
                 zoi,
                 coi,
                 r_meaning__mid[meaning, term],
                 sd_meaning__mid_Intercept) %>% 
    mutate(.label = case_when(.variable %in% "b_asym_Intercept" ~ "Asymptote\n(Intercept)",
                              .variable %in% "b_steep_Intercept" ~ "Steepness\n(Intercept)",
                              .variable %in% "b_mid_Intercept" ~ "Mid-point\n(Intercept)",
                              .variable %in% "b_mid_item_dominance1" ~ "Dominance\n(L2/L1)",
                              .variable %in% "b_mid_cognate1" ~ "Cognateness\n(Non-cognate/Cognate)",
                              .variable %in% "b_mid_item_dominance1:cognate1" ~ "Dominance \U000D7 Cognateness\n",
                              .variable %in% "b_mid_frequency" ~ "Frequency\n(Zipf, std.)",
                              .variable %in% "phi_Intercept" ~ "\U03C6\n(Intercept)",
                              .variable %in% "b_phi_Intercept" ~ "\U03C6\n(Slope)",
                              .variable %in% "zoi" ~ "ZOI",
                              .variable %in% "coi" ~ "COI",
                              .variable %in% "sd_meaning__mid_Intercept" ~ "SD Meaning\n(Intercept)",
                              TRUE ~ "Meaning\n(Intercept)"),
           class = case_when(.label %in% c("Asymptote\n(Intercept)", "Steepness\n(Intercept)", "Mid-point\n(Intercept)") ~ "Logistic\n(Intercept, fixed)",
                             .label %in% c("Dominance\n(Slope)", "Cognateness\n(Slope)", "Dominance \U000D7 Cognateness\n(Slope)", "Frequency\n(Slope)") ~ "Linear",
                             .label %in% c("b_phi_Intercept", "\U03C6\n(Intercept)", "\U03C6\n(Slope)", "ZOI", "COI") ~ "Distributional",
                             TRUE ~ "Random"),
           .chain = factor(.chain, levels = 1:4, ordered = TRUE))


#### check convergence ######################

# traceplot: If convergence is good, plots should look like funny fat catterpillars
ggplot(posterior, aes(.iteration, .value, colour = .chain)) +
    facet_wrap(class~.label, scales = "free") +
    geom_line(alpha = 0.7) +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    scale_colour_brewer(palette = "Oranges") +
    theme_custom +
    theme(legend.position = "top") +
    ggsave(here("Figures", "07_gca_prod-convergence.png"), height = 6, width = 7)

#### coefficients ##############################################
intervals <- posterior %>% 
    filter(.variable %in% c("b_mid_item_dominance1", "b_mid_cognate1", "b_mid_item_dominance1:cognate1", "b_mid_frequency")) %>% 
    group_by(class, .variable, .label) %>%
    mean_qi(.value, .width = c(0.95, 0.89, 0.50))

posterior %>%
    filter(.variable %in% c("b_mid_item_dominance1", "b_mid_cognate1", "b_mid_item_dominance1:cognate1", "b_mid_frequency")) %>% 
    mutate(.label = str_remove(.label, "\n(Slope)")) %>% 
    ggplot(aes(x = .value, y = .label)) +
    stat_slabh(fill = "#44546A", colour = NA, show.legend = FALSE) +
    geom_intervalh(data = intervals, position = position_nudge(y = -0.15), alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    annotate(geom = "text", x = 1.75, y = 4.5, label = "Increases AoA", size = 3) +
    annotate(geom = "text", x = -1.75, y = 4.5, label = "Reduces AoA", size = 3) +
    annotate(geom = "segment", x = 3, xend = 3.5, y = 4.5, yend = 4.5, arrow = arrow(ends = "last", length = unit(0.1, units = "cm"))) +
    annotate(geom = "segment", x = -3.5, xend = -3, y = 4.5, yend = 4.5, arrow = arrow(ends = "first", length = unit(0.1, units = "cm"))) +
    labs(x = "Estimate", y = "Parameter",
         fill = "Credible Interval", colour = "Credible Interval",
         title = "Fixed coefficients of the extended model (M1)",
         caption = "Frequency scores were extracted from SUBTLEX-ESP [1] and SUBTLEX-CAT [2]") +
    scale_fill_manual(values = c("#44546A", "orange")) +
    scale_colour_brewer(palette = "YlOrBr") +
    theme_custom +
    theme(plot.title = element_text(size = 14),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot",
          legend.position = "top",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.y = element_line(colour = "grey"),
          axis.title.y = element_blank()) +
    ggsave(here("Figures", "07_gca_prod-coefs.png"), width = 6, height = 4)

#### posterior predictive checks ##############################

posterior_check <- expand_grid(age_bin = unique(dat$age_bin),
                               item_dominance = c("L2", "L1"),
                               cognate = c("Non-cognate", "Cognate"),
                               meaning = unique(dat$meaning),
                               frequency = seq_range(dat$frequency, 4)) %>%
    add_fitted_draws(model = fit1, n = 100, value = "proportion", scale = "linear") %>% 
    ungroup() %>%
    mutate(frequency = cut(frequency, breaks = seq_range(dat$frequency, 5), labels = paste("Frequency:", c("Q1", "Q2", "Q3", "Q4")), include.lowest = TRUE),
           .draw = as.character(.draw))

ggplot(posterior_check, aes(age_bin, proportion, colour = cognate, linetype = cognate)) +
    facet_grid(~frequency) +
    stat_lineribbon(aes(group = cognate), size = 0.5, colour = "#44546A",
                    .width = c(0.11, 0.5, 0.89, 0.95)) +
    #geom_point(data = dat_ppcheck, aes(x = age_bin, y = prop, colour = cognate)) +
    #geom_errorbar(data = dat_ppcheck, aes(x = age_bin, y = prop, ymin = prop-sem, ymax = prop+sem, colour = cognate), width = 0) +
    labs(x = "Age (months)", y = "Proportion", fill = "Credible interval",
         group = "Cognate", linetype = "Cognate", colour = "Cognate") +
    scale_x_continuous(breaks = seq(0, 11), labels = bins) +
    scale_fill_brewer(palette = "Oranges") +
    theme_custom +
    theme(panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
          legend.position = "right",
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          axis.text.x = element_text(size = 7, angle = 300)) +
    ggsave(here("Figures", "07_gca_prod_posterior-checks.png"))

#### compare models #########################
loo0 <- add_criterion(fit0, "loo", file = here("Results", "prod-L1_fit0"))
loo1 <- add_criterion(fit1, "loo", file = here("Results", "prod-L1_fit1"))
loo_comp <- loo_compare(loo0, loo1)
comp <- list(loo_comp)

#### export results #########################
fits <- list(fit0, fit1)
saveRDS(fits, here("Results", "prod_fits.rds"))
saveRDS(comp, here("Results", "prod_selection.rds"))
