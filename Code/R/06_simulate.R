#### 05_simulations: Simnulate data #######################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra
#### set up ###############################################

# load packages
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(truncnorm)
library(brms)
library(modelr)
library(tidybayes)
library(ggplot2)
library(patchwork)
library(here)

# create/load functions
source(here("Code", "R", "functions.R"))

# set params
bins <- c("14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

#### import data #########################################
dat <- fread(here("Data", "04_prepared.csv")) %>%
  as_tibble() %>% 
  filter(type=="Comprehensive",
         lp=="Bilingual") %>%
  select(item, meaning, age_bin, item_dominance, cognate, proportion, frequency) %>%
  mutate(age_bin = as.numeric(factor(age_bin, levels = bins, ordered = TRUE)),
         item_dominance = as.factor(item_dominance),
         frequency = scale(frequency)[,1],
         cognate = as.factor(cognate)) %>%
  arrange(item, meaning, age_bin) %>%
  drop_na(age_bin)

contrasts(dat$item_dominance) <- contr.sum(c("L2", "L1"))/2
contrasts(dat$cognate) <- contr.sum(c("Non-cognate", "Cognate"))/2

dat_priors <- fread(here("Data", "05_priors-wordbank.csv"))

#### generate data #######################################
asym      <- dat_priors$estimate[dat_priors$term=="asym"]
asym_sd   <- 0.1
mid       <- dat_priors$estimate[dat_priors$term=="mid"]+1
mid_sd    <- 1
steep     <- dat_priors$estimate[dat_priors$term=="steep"]
steep_sd  <- 0.8
b_cognate <- 0
n         <- 1000

item <- factor(1:n)
age <- factor(seq(1, length(age_bins)), ordered = TRUE)

dat_sim <- expand_grid(item, age) %>%
  mutate(cognate = rep(c(-0.5, 0.5), each = nrow(.)/2),
         cognate_label = ifelse(cognate==-0.5, "Non-cognate", "Cognate"),
         age_bin = as.numeric(age)-1) %>%
  group_by(item) %>%
  mutate(asym = rtruncnorm(n = 1, mean = asym, sd = asym_sd, a = 0),
         mid = rtruncnorm(n = 1, mean = mid, sd = mid_sd, a = 0) + b_cognate*cognate,
         steep = rtruncnorm(n = 1, mean = steep, sd = steep_sd, a = 0),
         proportion = asym / (1 + exp((mid - age_bin) * steep))) %>%
  arrange(item, age_bin, cognate) %>%
  drop()


#### sample prior ###########################################
fit_prior <- readRDS(here("Results", "comp_fit-prior.rds"))

fit_prior <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
                              asym ~ 1,
                              mid ~ 1 + item_dominance*cognate + frequency + (1 | meaning),
                              steep ~ 1, 
                              phi ~ 1,
                              nl = TRUE,
                              family = zero_one_inflated_beta),
                 prior = c(prior(normal(0.7857192, 0.1), nlpar = "asym", coef = "Intercept"),
                           prior(normal(3.497546, 1), nlpar = "mid", coef = "Intercept"),
                           prior(normal(1.7576520, 0.8), nlpar = "steep", coef = "Intercept"),
                           prior(normal(0, 1), class = "b", nlpar = "mid", coef = "item_dominance1"),
                           prior(normal(0, 1), class = "b", nlpar = "mid", coef = "cognate1"),
                           prior(normal(0, 1), class = "b", nlpar = "mid", coef = "item_dominance1:cognate1"),
                           prior(normal(0, 1), class = "b", nlpar = "mid", coef = "frequency"),
                           prior(normal(1.5, 1), dpar = "phi", class = "Intercept")),
                 data = dat,
                 sample_prior = "only",
                 save_all_pars = TRUE,
                 save_model = here("Code", "Stan", "comp_fit-prior.stan"))

saveRDS(fit_prior, here("Results", "comp_fit-prior.rds"))

#### prior predictive checks ##############################
posterior <- fit_prior %>%
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
                               .variable %in% "b_mid_item_dominance1" ~ "Dominance\n(Slope)",
                               .variable %in% "b_mid_cognate1" ~ "Cognateness (Slope)",
                               .variable %in% "b_mid_item_dominance1:cognate1" ~ "Dominance \U000D7 Cognateness\n(Slope)",
                               .variable %in% "b_mid_frequency" ~ "Frequency\n(Slope)",
                               .variable %in% "phi_Intercept" ~ "\U03C6\n(Intercept)",
                               .variable %in% "b_phi_Intercept" ~ "\U03C6\n(Slope)",
                               .variable %in% "zoi" ~ "ZOI",
                               .variable %in% "coi" ~ "COI",
                               .variable %in% "sd_meaning__mid_Intercept" ~ "SD Meaning\n(Intercept)",
                               TRUE ~ "Meaning\n(Intercept)"),
         class = case_when(.variable %in% c("b_asym_Intercept", "b_steep_Intercept", "b_mid_Intercept") ~ "Logistic\n(Intercept, fixed)",
                           .variable %in% c("b_mid_item_dominance1", "b_mid_cognate1", "b_mid_item_dominance1:cognate1", "b_mid_frequency") ~ "Linear",
                           .variable %in% c("phi_Intercept", "b_phi_Intercept", "zoi", "coi") ~ "Distributional",
                           TRUE ~ "Random"),
         .chain = factor(.chain, levels = 1:4, ordered = TRUE))


intervals <- posterior %>% 
  #filter(.variable %in% c("b_mid_item_dominance1", "b_mid_cognate1", "b_mid_item_dominance1:cognate1", "b_mid_frequency")) %>% 
  group_by(class, .variable, .label) %>%
  median_qi(.value, .width = c(0.95, 0.89, 0.50))

posterior %>%
  #filter(.variable %in% c("b_mid_item_dominance1", "b_mid_cognate1", "b_mid_item_dominance1:cognate1", "b_mid_frequency")) %>% 
  ggplot(aes(x = .value, y = .label)) +
  facet_wrap(~class, scales = "free") +
  stat_slabh(fill = "#44546A", colour = NA) +
  geom_intervalh(data = intervals, position = position_nudge(y = -0.3), alpha = 0.7) +
  geom_pointintervalh(position = position_nudge(y = -0.1), size = 0.1, data = intervals) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Value", y = "Parameter",
       fill = "Credible Interval", colour = "Credible Interval",
       title = "Sampling the posterior of the predictors' coefficients",
       caption = "Negative coefficients decrease age of acquisition. Positive coefficients increase age of acquisition") +
  scale_colour_brewer(palette = "YlOrBr") +
  theme_custom +
  theme(legend.position = "right",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major.y = element_line(colour = "grey"),
        axis.title.y = element_blank()) +
  ggsave(here("Figures", "06_gca_comp-prior-coefs.png"), height = 5)
