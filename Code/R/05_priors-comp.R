#### 05_priors-comp: compute priors from Worbank data
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(data.table)
library(tidyverse)
library(janitor)
library(broom)
library(modelr)
library(brms)
library(tidybayes)
library(patchwork)
library(here)

# load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins_wordbank <- c("< 16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "> 40")
breaks_wordbank <- c(0, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 100)
bins_interest_wordbank <- c("16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36")
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

#### import data ############################################

# wordbank data
dat_wordbank <- list.files(here("Data", "Wordbank"), full.names = TRUE) %>%
  map(., fread) %>%
  set_names(str_remove(list.files(here("Data", "Wordbank")), ".csv")) %>%
  bind_rows(.id = "language") %>%
  as_tibble() %>%
  pivot_longer(6:ncol(.), names_to = "age", values_to = "proportion") %>%
  mutate(age = as.numeric(age),
         age_bin = factor(cut(age, breaks = breaks_wordbank, labels = bins_wordbank), levels = bins_interest),
         age_bin = as.numeric(age_bin)-2) %>%
  drop_na(age_bin)

# raw data
dat <- fread(here("Data", "04_prepared.csv")) %>%
  as_tibble() %>% 
  filter(type=="Comprehensive",
         lp=="Bilingual") %>%
  select(item, meaning, age_bin, item_dominance, cognate, proportion, frequency) %>%
  mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE)),
         item_dominance = as.factor(item_dominance),
         frequency = scale(frequency)[,1],
         cognate = as.factor(cognate)) %>%
  arrange(item, meaning, age_bin)

contrasts(dat$item_dominance) <- contr.sum(c("L2", "L1"))/2
contrasts(dat$cognate) <- contr.sum(c("Non-cognate", "Cognate"))/2

#### fit wordbank data ####################################
fit <- nls(proportion ~ SSlogis(age_bin, asym, mid, steep), dat_wordbank)
dat_fit <- expand_grid(age_bin = unique(dat_wordbank$age_bin)) %>% mutate(fit = predict(fit, newdata = .))
coefs <- tidy(fit, conf.int = 0.95) %>%
  clean_names() %>%
  mutate(estimate_scaled = estimate*2+16) %>%
  relocate(term, estimate, estimate_scaled)

# visualise model fit
x_mid <- coefs$estimate[coefs$term=="mid"]
y_mid <- dat_fit$fit[which.min(abs(dat_fit$age_bin-0.25-x_mid))]
steepness <- coefs$estimate[coefs$term=="steep"]
asymptote <- coefs$estimate[coefs$term=="asym"]

ggplot(dat_wordbank, aes(x = age_bin, y = proportion)) +
  geom_segment(x = -Inf, xend = Inf, y = asymptote, yend = asymptote, colour = "grey", size = 1) +
  annotate(geom = "text", label = paste0("Asymptote = ", round(asymptote, 2)),
           x = min(dat_wordbank$age_bin), y = asymptote+0.035, size = 4, hjust = 0) +
    geom_vline(xintercept = x_mid, colour = "grey", size = 1.25) +
  annotate(geom = "text", label = paste0("Mid-point = ", round(x_mid, 2)*2+16, " months"),
           x = x_mid+0.35, y = 0.15, size = 4, hjust = 0) +
  annotate(geom = "segment", x = x_mid-1.5, xend = x_mid+0.5, y = y_mid-0.05, yend = y_mid+0.17,
           size = 1, arrow = arrow(ends = "both", length = unit(0.2, units = "cm")), colour = "grey") +
  annotate(geom = "text", label = paste0("Steepness = ", round(steepness, 2)),
           x = x_mid-1.55, y = y_mid, size = 4, hjust = 0, angle = 50) +
  geom_line(data = dat_fit, aes(y = fit), size = 1, colour = "black") +
  stat_summary(aes(group = language),
               fun = "mean", geom = "line", na.rm = TRUE,
               size = 0.5, alpha = 0.40, colour = "orange") +
  stat_summary(fun = "mean", geom = "point", na.rm = TRUE,
               size = 3, colour = "black", shape = 21) +
  labs(x = "Age (months)", y = "Proportion of toddlers that\nacquired the item",
       title = "Computing priors from monolinguals in Wordbank",
       caption = "Data extracted from Wordbank (http://wordbank.stanford.edu/) [2].\nOrange lines represent the mean for each of the 16 languages.") +
  scale_colour_grey(start = 0, end = 0.45) +
  scale_fill_grey(start = 0, end = 0.45) +
  scale_x_continuous(breaks = seq(0, 9, by = 1), labels = bins_interest_wordbank) +
  theme_custom +
  theme(legend.position = c(0.8, 0.15),
        legend.title = element_blank(),
        plot.title = element_text(size = 14),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        panel.background = element_rect(fill = "transparent")) +
  ggsave(here("Figures", "05_priors-wordbank.png"), width = 4.5, height = 5)


#### sample prior ###########################################
fit_prior <- brm(formula = bf(proportion ~ inv_logit(asym) * inv(1 + exp((mid - age_bin) * exp(steep))),
                              asym ~ 1,
                              mid ~ 1 + item_dominance*cognate + frequency + (1 | meaning),
                              steep ~ 1, 
                              phi ~ 1,
                              nl = TRUE,
                              family = zero_one_inflated_beta),
                 prior = c(prior(normal(0.7857192, 0.1), nlpar = "asym", coef = "Intercept"),
                           prior(normal(5.369435, 1), nlpar = "mid", coef = "Intercept"),
                           prior(normal(1.7576520, 0.8), nlpar = "steep", coef = "Intercept"),
                           prior(normal(0, 1), class = "b", nlpar = "mid", coef = "item_dominance1"),
                           prior(normal(0, 1), class = "b", nlpar = "mid", coef = "cognate1"),
                           prior(normal(0, 1), class = "b", nlpar = "mid", coef = "item_dominance1:cognate1"),
                           prior(normal(0, 1), class = "b", nlpar = "mid", coef = "frequency"),
                           prior(normal(1.5, 1), dpar = "phi", class = "Intercept")),
                 data = dat,
                 silent = FALSE,
                 file = here("Results", "comp_fit-prior.rds"),
                 sample_prior = "only",
                 cores = 8,
                 save_all_pars = TRUE,
                 save_model = here("Code", "Stan", "comp_fit-prior.stan"))

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
  ggsave(here("Figures", "05_comp_prior-coefs.png"), height = 5)

#### prior predictive checks ###########################
posterior_check <- expand_grid(age_bin = unique(dat$age_bin),
                               item_dominance = c("L2", "L1"),
                               cognate = c("Non-cognate", "Cognate"),
                               frequency = seq_range(dat$frequency, 4)) %>%
  add_fitted_draws(model = fit_prior, n = 100, value = "proportion", scale = "linear", re_formula = NA) %>% 
  ungroup() %>%
  mutate(frequency = cut(frequency, breaks = seq_range(dat$frequency, 5), labels = paste("Frequency:", c("Q1", "Q2", "Q3", "Q4")), include.lowest = TRUE),
         .draw = as.character(.draw))

posterior_check %>%
  ggplot(aes(age_bin, proportion)) +
  stat_summary(fun = "median", geom = "line", na.rm = TRUE, size  =1) +
  stat_lineribbon(.width = seq(0.5, 0.99, by = 0.1), show.legend = FALSE) +
  labs(x = "Age (months)", y = "Proportion",
       group = "Cognate", linetype = "Dominance", colour = "Cognateness",
       subtitle = "Posterior predictive checks: What do our priors predict?",
       caption = "Lines represent the median of the marginal posterior\ndistribution of fitted values for each condition.") +
  scale_fill_brewer(palette =  "Oranges") +
  #scale_fill_manual(values = c("#44546A", "orange")) +
  scale_x_continuous(labels = bins_interest, breaks = seq(1, 11)) +
  theme_custom +
  theme(panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
        legend.position = "top",
        text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.margin = margin(t = 0.01, b = 0.01),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  ggsave(here("Figures", "05_comp_prior-checks.png"), height = 4, width = 5)


#### export data ###########################
fwrite(coefs, here("Data", "05_priors-comp.csv"), sep = ",", row.names = FALSE)
