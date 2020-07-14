#### 05_gca-comp: Bayesian non-linear model ##############
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
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")
options(contrasts = c("contr.sum", "contr.poly"))

#### import data ##########################################
dat <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    filter(type=="Comprehensive",
           item_dominance=="L2") %>%
    select(item, meaning, category, age_bin, bilingualism, cognate, proportion) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1,
           bilingualism = scale(bilingualism)[,1],
           cognate = as.factor(cognate)) %>%
    arrange(item, meaning, age_bin)

contrasts(dat$cognate) <- contr.sum(c("Non-cognate", "Cognate"))/2

dat_priors <- fread(here("Data", "05_priors-wordbank.csv")) %>%
    mutate(estimate_scaled = (estimate_scaled-14)/2)

#### fit models ###########################################

# load in case models are already fitted
fit_prior <- readRDS(here("Results", "comp_fit-prior.rds"))

# set priors 
priors <- c(prior(normal(0.7857192, 0.1), nlpar = "asym", coef = "Intercept"),
            prior(normal(5.369435, 1), nlpar = "mid", coef = "Intercept"),
            prior(normal(1.7576520, 0.8), nlpar = "steep", coef = "Intercept"),
            prior(normal(0, 5), class = "b", nlpar = "mid"),
            prior(normal(1.5, 1), dpar = "phi", class = "Intercept"),
            prior(beta(10, 10), class = "coi"),
            prior(beta(10, 10), class = "zoi"),
            prior(HalfCauchy(0, 5), class  = "sd"))
# model 0
fit0 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
                         asym ~ 1,
                         mid ~ 1 + bilingualism + (1 | meaning),
                         steep ~ 1,
                         phi ~ 1,
                         nl = TRUE,
                         family = zero_one_inflated_beta),
            prior = priors,
            data = dat,
            chains = 1,
            iter = 4000,
            cores = 
            file = here("Results", "comp_fit0.rds"),
            save_model = here("Code", "Stan", "comp_fit0.stan"),
            control = list(adapt_delta = 0.95, max_treedepth = 15))

# model 1
fit1 <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
                         asym ~ 1,
                         mid ~ 1 + bilingualism*cognate + (1 | meaning),
                         steep ~ 1,
                         phi ~ 1,
                         nl = TRUE,
                         family = zero_one_inflated_beta),
            prior = priors,
            data = dat,
            chains = 1,
            iter = 4000,
            file = here("Results", "comp_fit1.rds"),
            save_model = here("Code", "Stan", "comp_fit1.stan"),
            control = list(adapt_delta = 0.95, max_treedepth = 15))


#### extract posterior samples ###################################
posterior_groups <- fit1 %>%
    spread_draws(b_asym_Intercept, b_mid_bilingualism, b_steep_Intercept, b_mid_Intercept, r_meaning__mid[meaning, term]) %>% 
    median_qi(.width = 0.5) %>% 
    arrange(meaning) %>% 
    select(meaning, term, mid = b_mid_Intercept, asym = b_asym_Intercept, steep = b_steep_Intercept, bilingualism = b_mid_bilingualism, mid_group = r_meaning__mid) %>% 
    pivot_wider(names_from = term, values_from = mid_group) %>% 
    left_join(expand_grid(meaning = unique(dat$meaning), age_bin = unique(dat$age_bin))) %>% 
    rename(mid_group = Intercept) %>% 
    rowwise() %>% 
    mutate(proportion_noncognate = asym/(1 + exp((((mid+mid_group)*-0.5*cognate1)-age_bin)*steep)),
           proportion_cognate = asym/(1 + exp((((mid+mid_group)*0.5*cognate1)-age_bin)*steep))) %>% 
    pivot_longer(c(proportion_noncognate, proportion_cognate), names_to = "cognateness", values_to = "proportion") %>% 
    mutate(cognateness = ifelse(str_detect(cognateness, "noncognate"), "Non-cognate", "Cognate"))

ggplot(posterior_groups, aes(age_bin, proportion, colour = cognateness)) +
    stat_summary(data = dat, aes(age_bin, proportion, colour = cognate), fun = "mean", geom = "point") +
    geom_line(aes(group = meaning), size = 0.5, alpha = 0.5) +
    labs(x = "Age (months)", y = "Proportion", colour = "Cognateness") +
    theme_classic() +
    theme(legend.position = "top") +
    ggsave(here("Figures", "06_gca_comp-category.png"), height = 4, width = 7)



posterior <- fit1 %>%
    recover_types(dat) %>% 
    gather_draws(c(`b_.*`, `sd_.*`, coi, zoi), regex = TRUE) %>% 
    mutate(.label = pred_labels(.variable),
           class = case_when(.variable %in% c("b_asym_Intercept") ~ "Asymptote",
                             str_detect(.variable, "mid") ~ "Mid-point",
                             str_detect(.variable, "steep") ~ "Steepness",
                             .variable %in% c("phi_Intercept", "b_phi_Intercept", "zoi", "coi") ~ "Distributional"),
           .chain = factor(.chain, levels = 1:4, ordered = TRUE))


#### check convergence ######################

# traceplot: If convergence is good, plots should look like funny fat catterpillars
ggplot(posterior, aes(.iteration, .value)) +
    facet_wrap(class~.label, scales = "free") +
    geom_line(colour = "Orange") +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    scale_colour_brewer(palette = "Oranges") +
    theme_classic() +
    theme(legend.position = "top") +
    ggsave(here("Figures", "07_gca_comp-convergence.png"), height = 5, width = 9)

#### coefficients ##############################################
intervals <- posterior %>% 
    filter(class %in% "Mid-point") %>% 
    group_by(class, .variable, .label) %>% 
    mean_qi(.value, .width = c(0.95, 0.89, 0.50)) 


posterior %>%
    filter(class %in% "Mid-point") %>% 
    ggplot(aes(x = .value, y = .label)) +
    stat_slabh(fill = "#44546A", colour = NA, show.legend = FALSE) +
    geom_intervalh(data = intervals, position = position_nudge(y = -0.2), alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    annotate(geom = "text", x = 2, y = 6.5, label = "Increases AoA", size = 3) +
    annotate(geom = "text", x = -2, y = 6.5, label = "Reduces AoA", size = 3) +
    annotate(geom = "segment", x = 3, xend = 3.5, y = 6.5, yend = 6.5, arrow = arrow(ends = "last", length = unit(0.1, units = "cm"))) +
    annotate(geom = "segment", x = -3.5, xend = -3, y = 6.5, yend = 6.5, arrow = arrow(ends = "first", length = unit(0.1, units = "cm"))) +
    labs(x = "Estimate", y = "Parameter",
         fill = "Credible Interval", colour = "Credible Interval",
         subtitle = "Model coefficients: What is the contribution of each predictor?") +
    scale_fill_manual(values = c("#44546A", "orange")) +
    scale_colour_brewer(palette = "YlOrBr") +
    theme_classic() +
    theme(plot.title = element_text(size = 14),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot",
          legend.position = "top",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.y = element_line(colour = "grey"),
          axis.title.y = element_blank()) +
    ggsave(here("Figures", "07_gca_comp-coefs.png"), width = 10, height = 5)

#### posterior predictive checks ##############################

posterior_check <- expand_grid(age_bin = unique(dat$age_bin),
                               bilingualism = seq(0, 1, by = 0.1),
                               cognate = c("Non-cognate", "Cognate")) %>%
    add_fitted_draws(model = fit1, n = 100, value = "proportion", scale = "linear", re_formula = NA) %>% 
    ungroup()

posterior_check %>%
    mutate(cognate = factor(cognate, levels = c("Non-cognate", "Cognate"), ordered = TRUE),
           bilingualism = cut_interval(bilingualism, n = 2, labels = c("Bilingualism below median", "Bilingualism above median")),
           .draw = as.character(.draw)) %>%  
    ggplot(aes(age_bin, proportion, colour = cognate, fill = cognate)) +
    facet_grid(~bilingualism) +
    stat_lineribbon(.width = 0.95, alpha = 0.25, colour = "NA", show.legend = FALSE) +
    stat_summary(fun = "median", geom = "line", na.rm = TRUE, size = 1) +
    labs(x = "Age (months)", y = "Proportion",
         group = "Cognate", colour = "Cognateness",
         subtitle = "Posterior predictive checks: What does our model predict?",
         caption = "Lines represent the median of the marginal posterior distribution of fitted values.\nShaded areas represent 95% credible intervals.") +
    scale_colour_manual(values = c("#44546A", "orange")) +
    scale_fill_manual(values = c("#44546A", "orange")) +
    scale_x_continuous(breaks = seq(0, 10), labels = bins_interest) +
    theme_classic() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot",
          plot.title.position = "plot") +
    ggsave(here("Figures", "07_gca_comp_posterior-checks.png"), height = 4)

# 3D graph
posterior_check %>%
    mutate(cognate = factor(cognate, levels = c("Non-cognate", "Cognate"), ordered = TRUE)) %>% 
    ggplot(aes(age_bin, bilingualism, fill = proportion)) +
    facet_wrap(~cognate) +
    geom_raster() +
    labs(x = "Age (months)", y = "% L2 exposure", fill = "Proportion") +
    scale_fill_viridis_c(option = "plasma") +
    theme_classic() +
    ggsave(here("Figures", "07_gca_comp-surface.png"))
    
#### compare models #########################
loo0 <- add_criterion(fit0, "loo", file = here("Results", "comp_fit0"))
loo1 <- add_criterion(fit1, "loo", file = here("Results", "comp_fit1"))
loo_comp <- loo_compare(loo0, loo1)

#### export results #########################
fwrite(posterior, here("Results", "comp_posterior.csv"), sep = ",", dec = ".")


library(nlmer)

