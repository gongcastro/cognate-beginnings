#### 05_gca-comp: Bayesian non-linear model ##############
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(data.table)
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
nchains <- 4
ncores <- 4
niter <- 2000
control <- list(adapt_delta = 0.95, max_treedepth = 15))


#### import data ##########################################
dat <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    filter(type=="Comprehensive",
           item_dominance=="L2") %>%
    select(item, meaning, category, age_bin, sex, bilingualism, cognate, n, successes, proportion) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1,
           bilingualism = scale(bilingualism)[,1],
           bilingualism_cat = cut_interval(bilingualism, n = 2, labels = c("Completely bilingual", "Completely monolingual")),
           cognate = as.factor(cognate),
           sex = factor(sex, levels = c("Female", "Male"))) %>%
    arrange(item, meaning, age_bin)

contrasts(dat$cognate) <- contr.sum(c("Non-cognate", "Cognate"))/2
contrasts(dat$sex) <- contr.sum(c("Female", "Male"))/2

dat_priors <- fread(here("Data", "05_priors-wordbank.csv")) %>%
    mutate(estimate_scaled = (estimate_scaled-14)/2)

#### fit models ###########################################
inv_logit <- function(x) 1 / (1 + exp(-x))

# set priors 
priors <- c(prior(normal(0.7857192, 0.5), nlpar = "asym", coef = "Intercept"),
            prior(normal(-1.7576520, 1), nlpar = "steep", coef = "Intercept"),
            prior(normal(4.369435, 1), nlpar = "mid", coef = "Intercept"),
            prior(normal(0, 5), class = "b", nlpar = "mid"),
            prior(cauchy(0, 5), class  = "sd", nlpar = "mid"))

# model 0
fit0 <- brm(formula = bf(proportion ~ asym * inv(1 + exp((mid - age_bin) * steep)),
                         asym ~ 1,
                         steep ~ 1,
                         mid ~ 1 + (1 | meaning),
                         nl = TRUE),
            prior = priors,
            data = dat,
            chains = nchains,
            cores = ncores,
            iter = niter,
            file = here("Results", "comp_fit0.rds"),
            save_model = here("Code", "Stan", "comp_fit0.stan"),
            control = control)

# model 1
fit1 <- brm(formula = bf(proportion ~ asym * inv(1 + exp((mid - age_bin) * steep)),
                         asym ~ 1,
                         steep ~ 1,
                         mid ~ 1 + bilingualism + (1 + bilingualism | meaning),
                         nl = TRUE),
            prior = c(priors, prior(lkj(2), class = "cor")),
            data = dat,
            chains = nchains,
            cores = ncores,
            iter = niter,
            file = here("Results", "comp_fit1.rds"),
            save_model = here("Code", "Stan", "comp_fit1.stan"),
            control = control)

# model 2
fit2 <- brm(formula = bf(proportion ~ asym * inv(1 + exp((mid - age_bin) * steep)),
                         asym ~ 1,
                         steep ~ 1,
                         mid ~ 1 + bilingualism*cognate + (1 + bilingualism*cognate | meaning),
                         nl = TRUE),
            prior = c(priors, prior(lkj(2), class = "cor")),
            data = dat,
            chains = nchains,
            iter = ncores,
            file = here("Results", "comp_fit2.rds"),
            save_model = here("Code", "Stan", "comp_fit2.stan"),
            control = control)


#### compare models #########################
loo_comp <- loo(fit0, fit1, fit2)

fixed_coefs <- summary(fit2)$fixed %>%
    as.data.frame() %>% 
    rownames_to_column("term") %>% 
    as_tibble() %>% 
    clean_names()

#### posterior ##############################
posterior <- fit2 %>%
    recover_types(dat) %>% 
    gather_draws(`b_.*`, regex = TRUE) %>% 
    mutate_at(vars(.variable, .chain), as.factor) %>%
    filter(!str_detect(.variable, "Intercept"))

ggplot(posterior, aes(.value, .variable)) +
    stat_slab(fill = "black") +
    stat_interval(position = position_nudge(y = -0.2), point_interval = "mean_hdi") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Value", y = "Coefficient", colour = "HDI") +
    scale_color_brewer() +
    theme_classic() +
    theme(panel.grid.major.y = element_line(colour = "grey")) +
    ggsave(here("Figures", "06_gca_comp-coefs.png"), width = 6)

#### check convergence ######################

# traceplot: If convergence is good, plots should look like funny fat catterpillars
ggplot(posterior, aes(.iteration, .value, colour = .chain)) +
    facet_wrap(~.variable, scales = "free") +
    geom_line(colour = "black") +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    theme_classic() +
    theme(legend.position = "top") +
    ggsave(here("Figures", "07_gca_comp-convergence.png"), height = 5, width = 9)

#### coefficients ##############################################

#### posterior predictive checks ##############################

posterior_check <- expand_grid(n = 1,
                               age_bin = seq_range(dat$age_bin, n = 50),
                               bilingualism = c(-1, 1),
                               cognate = c("Non-cognate", "Cognate")) %>%
    add_fitted_draws(model = prod_fit2, newdata = ., n = 100, scale = "response", re_formula = NA) %>% 
    ungroup() %>% 
    mutate(cognate = factor(cognate, levels = c("Non-cognate", "Cognate"), ordered = TRUE),
           bilingualism_cat = factor(bilingualism, levels = c(-1, 1), labels = c("Completely bilingual", "Completely monolingual")),
           .draw = as.character(.draw))

ggplot(posterior_check, aes(age_bin, .value, colour = cognate, fill = cognate)) +
    facet_grid(~bilingualism_cat) +
    stat_lineribbon(.width = 0.95, colour = NA, alpha = 0.5) +
    stat_summary(fun = "mean", geom = "line") +
    stat_summary(data = dat, aes(y = proportion), fun = "mean", geom = "point", shape = 1) +
    labs(x = "Age (months)", y = "Proportion",
         colour = "Cognateness", fill = "Cognateness", shape = "Cognateness") +
    scale_fill_brewer(palette = "Set1") +
    scale_colour_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(0, 10), labels = bins_interest) +
    theme_classic() +
    theme(legend.position = "top", 
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle = 270),
          legend.title = element_blank(),
          plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot",
          plot.title.position = "plot") +
    ggsave(here("Figures", "07_gca_comp_posterior-checks.png"))

# 3D graph
posterior_check_cat <- expand_grid(n = 1,
                               age_bin = seq_range(dat$age_bin, n = 50),
                               bilingualism = seq_range(dat$bilingualism, n = 50),
                               cognate = c("Non-cognate", "Cognate")) %>%
    add_fitted_draws(model = fit2, newdata = ., n = 100, scale = "response", re_formula = NA) %>% 
    ungroup() %>% 
    mutate(cognate = factor(cognate, levels = c("Non-cognate", "Cognate"), ordered = TRUE),
           .draw = as.character(.draw))

posterior_check %>%
    mutate(cognate = factor(cognate, levels = c("Non-cognate", "Cognate"), ordered = TRUE)) %>% 
    ggplot(aes(age_bin, bilingualism, fill = .value)) +
    facet_wrap(~cognate) +
    geom_raster(interpolate = TRUE) +
    annotate(geom = "text", colour = "white", label = "Very bilingual", size = 2.5,
             x = median(posterior_check$age_bin), y = max(posterior_check$bilingualism)-0.1) +
    annotate(geom = "text", colour = "white", label = "Very monolingual", size = 2.5,
             x = median(posterior_check$age_bin), y = min(posterior_check$bilingualism)+0.1) +
    labs(x = "Age (months)", y = "L2 exposure", fill = "Proportion") +
    scale_fill_viridis_c(option = "plasma") +
    scale_x_continuous(breaks = seq_range(dat$age_bin, n = length(bins_interest)),
                       labels = bins_interest) +
    scale_y_continuous(breaks = seq_range(dat$bilingualism, n = 6),
                       labels = paste0(seq(0, 50, by = 10), "%")) +
    theme_classic() +
    theme(legend.position = "right",
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle = 270)) +
    ggsave(here("Figures", "07_gca_comp-surface.png"))


#### export results #########################
fwrite(posterior_check, here("Results", "comp_posterior_check.csv"), sep = ",", dec = ".")
fwrite(posterior_check_cat, here("Results", "comp_posterior_check_cat.csv"), sep = ",", dec = ".")
