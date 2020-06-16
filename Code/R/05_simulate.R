#### 05_simulations: Simnulate data

#### set up ##############################################

# load packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(wesanderson)
library(data.table)
library(truncnorm)
library(brms)
library(modelr)
library(tidybayes)
library(here)

# create/load functions
source(here("R", "functions.R"))
inv_logit <- function(x) 1 / (1 + exp(-x))

# set params
age_bins <- c("18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36")

#### generate data #######################################
mid           <- 7.5
mid_sd        <- 3
steep         <- 0.5
steep_sd      <- 0.3
asym          <- 1
b_cognateness <- 1
n <- 100

item <- factor(1:n)
age <- factor(0:length(age_bins)-1, ordered = TRUE)

dat <- expand_grid(item, age) %>%
  mutate(cognateness = rep(c(-0.5, 0.5), each = nrow(.)/2),
         cognateness_label = ifelse(cognateness==-0.5, "Non-cognate", "Cognate"),
         age = as.numeric(age)) %>%
  group_by(item) %>%
  mutate(mid = rtruncnorm(n = 1, mean = mid, sd = mid_sd, a = 0) + b_cognateness*cognateness,
         steep = rtruncnorm(n = 1, mean = steep, sd = steep_sd, a = 0),
         proportion = 1 / (1 + exp((mid - age) * steep))) %>%
  arrange(item, age, cognateness) %>%
  as_tibble()

ggplot(dat, aes(age, proportion, colour = cognateness_label, fill = cognateness_label)) +
  geom_hline(yintercept = 0.50, linetype = "dotted") +
  geom_density(aes(x = mid, y = stat(density)), alpha = 0.5, colour = NA, bw = 1) +
  geom_rug(aes(x = mid), sides = "b", alpha = 0.5) +
  geom_line(aes(group = item), alpha = 0.1) +
  stat_summary(fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line") +
  geom_segment(aes(x = mean(mid[cognateness==0.5]), xend = mean(mid[cognateness==0.5]), y = 0, yend = 0.50, group = cognateness_label), colour = "brown1") +
  geom_segment(aes(x = mean(mid[cognateness==-0.5]), xend = mean(mid[cognateness==-0.5]), y = 0, yend = 0.50, group = cognateness_label), colour = "#225ea8") +
  labs(x = "Age (bins)", y = "Prob. of positive response",
       colour = "Cognateness", fill = "Cognateness") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(0, 8), labels = age_bins) +
  theme_custom +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.9))

#### fit model ######################################

fit_formula <- bf(proportion ~ inv_logit(1) * inv(1 + exp((mid - age) * exp(steep))),
                  mid ~ 1 + cognateness,
                  steep ~ 1,
                  phi ~ 1,
                  nl = TRUE, 
                  family = Beta(link = identity))

priors <- c(prior(normal(7.5, 4), nlpar = "mid", coef = "Intercept"),
            prior(normal(-0.5, 1), nlpar = "steep", coef = "Intercept"),
            prior(normal(2, 1), dpar = "phi", class = "Intercept"),
            prior(normal(0, 0.1), nlpar = "mid", coef = "cognateness"))

fit_prior <- brm(formula = fit_formula,
                 data = dat,
                 prior = priors,
                 sample_prior = "only",
                 save_model = here("Stan", "fit_prior.stan"),
                 control = list(adapt_delta = 0.9, max_treedepth = 15))
fit <- brm(formula = fit_formula,
           data = dat,
           prior = priors,
           save_model = here("Stan", "fit.stan"),
           control = list(adapt_delta = 0.9, max_treedepth = 15))

#### predictions ####################################
dat_pred_prior <- dat %>%
  data_grid(proportion = seq_range(proportion, n = 200), age = seq(0, 10, length.out = 101), cognateness) %>%
  add_fitted_draws(fit, n = 100) %>%
  ungroup() %>%
  mutate(cognateness = ifelse(cognateness==-0.5, "Non-cognate", "Cognate")) 

ggplot(dat_pred_prior, aes(x = age, y = proportion, color = cognateness)) +
  geom_line(aes(y = .value, group = .draw), alpha = 1/20) +
  scale_colour_brewer(palette = "Set1")


dat_predicted_prior <- data_grid(age = seq(from = 0, to = 11,  length.out = 101),
                                 cognateness = seq(-0.5, 0.5, length.out = 101)
                              mid = seq(from = min(dat$mid), to = max(dat$mid),  length.out = 101),
                              steep = seq(from = min(dat$steep), to = max(dat$steep), length.out = 101)) %>%
  add_predicted_draws(newdata = ., model = fit_prior) %>% 
  bind_cols()

dat_fitted <- tibble(age = seq(from = 0, to = 11,  length.out = 101),
                           cognateness = seq(-0.5, 0.5, length.out = 101),
                           mid = seq(from = min(dat$mid), to = max(dat$mid),  length.out = 101),
                           steep = seq(from = min(dat$steep), to = max(dat$steep), length.out = 101)) %>%
  add_fitted_draws(newdata = ., model = fit) %>% 
  bind_cols()
dat_predicted <- tibble(age = seq(from = 0, to = 11,  length.out = 101),
                              cognateness = seq(-0.5, 0.5, length.out = 101),
                              mid = seq(from = min(dat$mid), to = max(dat$mid),  length.out = 101),
                              steep = seq(from = min(dat$steep), to = max(dat$steep), length.out = 101)) %>%
  add_predicted_draws(newdata = ., model = fit) %>% 
  bind_cols()

fit_dat_prior <- left_join(dat_fitted_prior, dat_predicted_prior) %>%
  mutate(element = "Prior") 
fit_dat <- left_join(dat_fitted, dat_predicted) %>%
  mutate(element = "Posterior")
fit_dat_all <- bind_rows(fit_dat_prior, fit_dat)

# preditions
ggplot(fit_dat_all, aes(age, .value, colour = cognateness)) +
  facet_wrap(~element) +
  stat_lineribbon(.width = c(0.89)) +
  labs(x = "Levenstein distance", y = "P(RT | Levenshtein)", fill = "Credible interval") +
  scale_fill_manual(values = wes_palette("Zissou1", 3)) +
  theme_custom +
  theme(legend.position = "top")


# coefficients
coefs_prior <- fit_prior %>% gather_draws(b_mid_Intercept, b_mid_cognateness, b_steep_Intercept, b_phi_Intercept) %>% mutate(element = "Prior")
coefs_posterior <- fit %>% gather_draws(b_mid_Intercept, b_mid_cognateness, b_steep_Intercept, b_phi_Intercept) %>% mutate(element = "Posterior")
coefs <- bind_rows(coefs_prior, coefs_posterior)

ggplot(coefs, aes(x = .value, y = element, fill = .variable)) +
  facet_wrap(~.variable, scales = "free") +
  geom_vline(xintercept = 0) +
  stat_dots() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Estimate", y = "Coefficient") +
  theme_custom +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")




fit <- nls(proportion ~ inv_logit(age, steep = steep_intercept, mid = mid_intercept), start = list(steep = 0.5, mid = 5), data = dat, algorithm = "plinear")

ggplot(dat, aes(age, proportion, colour = cognateness_label)) +
  stat_summary(aes(y = proportion), fun = "mean", geom = "line") +
  stat_summary(aes(y = predict(fit)), fun = "mean", geom = "line")

  stat_summary(aes(y = predict(fit)), fun = "mean", geom = "line")

  #### illustration ##############################
  
  # set params
  age_bins <- seq(12, 32, length.out = 100)
  mid           <- 23
  mid_sd        <- 0.5
  steep         <- 0.1
  steep_sd      <- 1
  asym          <- 1
  n             <- 1000
  item <- factor(1:n)
  
  dat_logcurve <- expand_grid(item, age_bins) %>%
    mutate(age = as.numeric(age_bins)) %>%
    group_by(item) %>%
    mutate(mid = rtruncnorm(n = 1, mean = mid, sd = mid_sd, a = 0),
           steep = rtruncnorm(n = 1, mean = steep, sd = steep_sd, a = 0),
           proportion = 1 / (1 + exp((mid - age) * steep))) %>%
    arrange(item, age) %>%
    as_tibble()
  
  mid_sim <- dat_logcurve %>% distinct(item, .keep_all = TRUE) %>% summarise(mid = mean(mid, na.rm = TRUE)) %>% pull(mid)
  prop_at_mid <- dat_logcurve %>%
    group_by(item) %>%
    filter(age==age[which.min(abs(age-mid_sim))]) %>%
    ungroup() %>%
    summarise(prop = mean(proportion, na.rm = TRUE)) %>%
    pull(prop)
  
  
  ggplot(dat_logcurve, aes(age, proportion)) +
    geom_hline(yintercept = asym+ 0.01, colour = "red", size = 1) +
    stat_summary(fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5) +
    stat_summary(fun = "mean", geom = "line") +
    annotate(geom = "text", label = "Asymptote = 1", x = min(dat_logcurve$age), y = asym-0.05, colour = "red", size = 4, hjust = 0) +
    annotate(geom = "segment", x = mid_sim, xend = mid_sim, y = 0, yend = prop_at_mid, colour = "blue", size = 1) +
    annotate(geom = "point", x = mid_sim, y = prop_at_mid, colour = "blue", size = 5) +
    annotate(geom = "text", label = "Mid point = 23 months", x = mid_sim+0.5, y = 0.25, colour = "blue", size = 4, hjust = 0) +
    annotate(geom = "segment", x = mid_sim-1.25, xend = mid_sim+1.25, y = prop_at_mid+0.00, yend = prop_at_mid+0.2, colour = "green", size = 1, arrow = arrow(ends = "both", length = unit(0.2, units = "cm"))) +
    annotate(geom = "text", label = "Steepness = 0.5", x = mid_sim-2, y = 0.55, colour = "green", size = 4, hjust = 0, angle = 45) +
    #geom_line(aes(group = item), alpha = 0.1) +
    labs(x = "Age (months)", y = "Prob. of positive response",
         colour = "Cognateness", fill = "Cognateness") +
    scale_colour_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(0, max(dat_logcurve$age, na.rm = TRUE))) +
    theme_custom +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.9)) +
    ggsave(here("Figures", "05_illustration.png"), height = 4)
  