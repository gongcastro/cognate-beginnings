#### analysis ------------------------------------------------------------------

#### set up --------------------------------------------------------------------

# load packages
library(tidyverse)
library(brms)
library(scales)
library(tidybayes)

# load helper functions
source("R/utils.R")
set.seed(888)

#### import data ---------------------------------------------------------------
proportions <- readRDS("Data/proportions.rds")

# set sum contrasts (Schad et al., 2018, https://arxiv.org/abs/1807.10451)
contrasts(proportions$cognate) <- contr.sum(2)/2

## model fitting -------------------------------------------------------------
formula_understands <- bf(
  understands ~ inv_logit(asymlogit) * inv(1 + exp((mid - age_bin) * scale)),
  mid ~ 1 + frequency + doe_bin*cognate + (1 | te),
  scale ~ 1,
  asymlogit ~ 1,
  phi ~ 1,
  nl = TRUE,
  family = Beta(link = "identity")
)

formula_understands_reduced <- bf(
  understands ~ inv_logit(asymlogit) * inv(1 + exp((mid - age_bin) * scale)),
  mid ~ 1 + frequency + doe_bin + (1 | te),
  scale ~ 1,
  asymlogit ~ 1,
  phi ~ 1,
  nl = TRUE,
  family = Beta(link = "identity")
)

formula_produces <- bf(
  produces ~ inv_logit(asymlogit) * inv(1 + exp((mid - age_bin) * scale)),
  mid ~ 1 + frequency + doe_bin*cognate + (1 | te),
  scale ~ 1,
  asymlogit ~ 1,
  phi ~ 1,
  nl = TRUE,
  family = Beta(link = "identity")
)

formula_produces_reduced <- bf(
  produces ~ inv_logit(asymlogit) * inv(1 + exp((mid - age_bin) * scale)),
  mid ~ 1 + frequency + doe_bin + (1 | te),
  scale ~ 1,
  asymlogit ~ 1,
  phi ~ 1,
  nl = TRUE,
  family = Beta(link = "identity")
)

prior <- c(
  # mid prior
  prior(normal(5, 1), nlpar = "mid", class = "b", coef = "Intercept"),
  prior(normal(0, 0.5), nlpar = "mid", class = "b", coef = "frequency"),
  prior(normal(0, 0.5), nlpar = "mid", class = "b", coef = "doe_bin"),
  prior(normal(0, 0.5), nlpar = "mid", class = "b", coef = "cognate1"),
  prior(normal(0, 0.5), nlpar = "mid", class = "b", coef = "doe_bin:cognate1"),
  prior(exponential(2), nlpar = "mid", class = "sd"),
  # scale
  prior(normal(3, 0.05), nlpar = "scale", class = "b", coef = "Intercept"),
  # asymptote
  prior(normal(0.75, 0.01), nlpar = "asymlogit", class = "b", coef = "Intercept"),
  # precision
  prior(normal(10, 2), dpar = "phi", class = "Intercept")
)


# comprehension
fit_understands <- brm(
  formula_understands,
  prior = prior,
  data = proportions,
  file = "Results/fit_understands.rds",
  save_model = "Stan/fit_understands.stan",
  chains = 6, cores = 6, iter = 500,
  save_pars = save_pars(all = TRUE)
)

# production
fit_produces <- brm(
  formula_produces,
  prior = prior,
  data = proportions,
  file = "Results/fit_produces.rds",
  save_model = "Stan/fit_produces.stan",
  save_pars = save_pars(all = TRUE),
  chains = 6, cores = 6, iter = 500
)

# reduced models
fit_understands_reduced <- update(
  fit_understands,
  formula. = formula_understands_reduced,
  file = "Results/fit_understands_reduced.rds",
  save_model = "Stan/fit_understands_reduced.stan"
)

fit_produces_reduced <- update(
  fit_produces,
  formula. = formula_produces_reduced,
  file = "Results/fit_produces_reduced.rds",
  save_model = "Stan/fit_produces_reduced.stan"
)

# compare models ----
bf_understands <- bayes_factor(fit_understands, fit_understands_reduced)
bf_produces <- bayes_factor(fit_produces, fit_produces_reduced)

# fixed effects ----
posterior <- list(
  understands = gather_draws(fit_understands, `b_.*`, regex = TRUE),
  produces = gather_draws(fit_produces, `b_.*`, regex = TRUE)
) %>% 
  bind_rows(.id = "type") %>% 
  mutate(.chain = as.character(.chain))

ggplot(posterior, aes(.iteration, .value)) +
  facet_wrap(.variable~type, scales = "free") +
  geom_line(colour = "grey30") +
  labs(x = "Iteration", y = "Value", fill = "Chain") +
  theme_ggdist() +
  theme(
    legend.title = element_blank(),
    panel.grid.major = element_line(linetype = "dotted"),
    legend.position = "top"
  ) +
  ggsave("Figures/chains.png")

ggplot(posterior, aes(.value, fill = type)) +
  facet_wrap(~.variable, scales = "free") +
  geom_histogram() +
  labs(x = "Estimate", y = "Posterior samples (N = 500)", fill = "Coef") +
  scale_fill_brewer(palette = "Dark2") +
  theme_ggdist() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_line(linetype = "dotted"),
    legend.position = "top"
  ) +
  ggsave("Figures-posterior.png")


# marginal means ---------
nd <- expand.grid(
  age_bin = seq(min(proportions$age_bin), max(proportions$age_bin), 0.1),
  doe_bin = unique(proportions$doe_bin),
  cognate = unique(proportions$cognate),
  frequency = 0
)

means <- list(
  understands = fit_understands,
  produces = fit_produces
) %>% 
  map(~add_fitted_draws(nd, ., n = 50, re_formula = NA, scale = "linear")) %>% 
  bind_rows(.id = "type")

ggplot(means, aes(
  age_bin, .value,
  colour = interaction(cognate, type, sep = " - "),
  fill = interaction(cognate, type, sep = " - "),
)) +
  facet_wrap(~doe_bin) +
  stat_lineribbon(.width = 0.95, alpha = 0.5, size = 0) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  geom_hline(yintercept = 0.5, colour = "black") +
  stat_summary(
    data = pivot_longer(
      proportions,
      c(understands, produces),
      names_to = "type",
      values_to = ".value"
    ),
    fun.data = mean_se,
    geom = "pointrange",
    size = 0.25,
    shape = 1,
    stroke = 1
  ) +
  labs(
    x = "Age (months)", y = "Understands",
    colour = "Cognateness", fill = "Cognateness"
  ) +
  # scale_x_continuous(
  #   breaks = seq(min(means$age_bin), max(means$age_bin), 1),
  #   labels = 1:length(unique(proportions$age_bin))
  # ) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.20), limits = c(0, 1)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_ggdist() +
  theme(
    legend.title = element_blank(),
    panel.grid.major = element_line(linetype = "dotted"),
    legend.position = "top"
  ) +
  ggsave("Figures/means.png", height = 8)

# group-level effects
nd_random <- expand.grid(
  age_bin = seq(min(proportions$age_bin), max(proportions$age_bin), 0.5),
  doe_bin = unique(proportions$doe_bin),
  cognate = unique(proportions$cognate),
  frequency = 0,
  te = unique(proportions$te)
)

cor <- gather_draws(
  fit_understands,
  cor_te__mid_Intercept__mid_doe_bin) %>% 
  mean_qi()

random_post <- list(
  understands = fit_understands,
  produces = fit_produces
) %>% 
  map(~gather_draws(., r_te__mid[te, .coef])) %>%
  bind_rows(.id = "type") %>% 
  group_by(te, type, .coef) %>% 
  summarise(
    .value = mean(.value),
    .groups = "drop") %>% 
  ungroup() %>% 
  pivot_wider(names_from = .coef, values_from = .value) %>% 
  left_join(distinct(multilex::pool, te, cognate)) %>% 
  mutate(
    cognate = ifelse(cognate, "Cognate", "Non-cognate"),
    cor.value = cor$.value,
    cor.lower = cor$.lower,
    cor.upper = cor$.upper
  )

ggplot(random_post, aes(.value_Intercept, .value_doe_bin, colour = cognate, fill = cognate)) +
  geom_point(shape = 1, stroke = 1, alpha = 0.5, size = 2) +
  labs(x = "Mid-point intercept", y = "Mid-point DOE slope",
       colour = "Cognateness", fill = "Cognateness") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_ggdist() +
  theme(
    legend.title = element_blank(),
    panel.grid.major = element_line(linetype = "dotted"),
    legend.position = "top"
  )

random <- list(
  understands = fit_understands,
  produces = fit_produces
) %>% 
  map(function(x) add_fitted_draws(nd_random, x, n = 20, scale = "linear")) %>%
  bind_rows(.id = "type") %>% 
  mutate(
    te = as.factor(te),
    doe_bin = factor(
      doe_bin,
      levels = unique(.$doe_bin),
      labels = paste0("DOE: ", c(
        "0-25%", "25-50%", "50%-75%", "75-100%"))
    )
  ) %>% 
  mean_qi()

ggplot(random, aes(age_bin, .value, colour = cognate, fill = cognate)) +
  facet_wrap(~doe_bin) +
  geom_line(aes(group = interaction(te, cognate)), size = 0.5, alpha = 0.5) +
  geom_hline(yintercept = 0.5, colour = "black") +
  labs(
    x = "Age (months)", y = "Understands",
    colour = "Cognateness", fill = "Cognateness"
  ) +
  scale_x_continuous(
    limits = c(min(means$age_bin), max(means$age_bin)),
    breaks = seq(min(means$age_bin), max(means$age_bin), 1),
    labels = 1:length(unique(proportions$age_bin))
  ) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.20), limits = c(0, 1)) +
  theme_ggdist() +
  theme(
    legend.title = element_blank(),
    panel.grid.major = element_line(linetype = "dotted")
  )

