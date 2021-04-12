#### analysis ------------------------------------------------------------------

#### set up --------------------------------------------------------------------

# load packages
library(tidyverse)
library(brms)
library(scales)
library(tidybayes)
library(emmeans)
library(here)

# load helper functions
source(here("R", "utils.R"))
options(mc.cores = 7, chains = 7, iter = 1000)
set.seed(888)

#### import data ---------------------------------------------------------------
responses <- readRDS(here("Data", "responses.rds")) %>% 
  filter(between(age, 12, 36)) %>% 
  mutate(
    doe = doe*10,
    age_scaled = age,
    frequency_scaled = frequency,
    doe_scaled = doe
  ) %>% 
  mutate_at(vars(frequency), function(x) scale(x)[,1]) %>% 
  mutate_at(vars(age, doe), function(x) scale(x, scale = FALSE)[,1])


# responses_subset <- filter(responses, te %in% sample(responses$te, 25)) %>% 
#   mutate_at(vars(age, frequency, doe), function(x) scale(x, scale = FALSE)[,1]) %>% 
#   mutate_at(vars(item_dominance, cognate), as.factor) %>% 
#   mutate(response = factor(response, ordered = TRUE))

# set sum contrasts (Schad et al., 2018, https://arxiv.org/abs/1807.10451)
contrasts(responses$cognate) <- contr.sum(2)/2

#### model fitting -------------------------------------------------------------

fit_0 <- brm(
  response ~ age + frequency,
  family = cratio(link = "logit"),
  data = responses,
  prior = prior(normal(0, 5), class = b),
  chains = 4, iter = 1000,
  file = here("Results", "fit_0.rds"),
  save_model = here("Stan", "fit_0.stan"),
  backend = "cmdstanr"
)

fit_1 <- update(
  fit_0, . ~ . + doe,
  newdata = responses, file = here("Results", "fit_1.rds"),
)

fit_2 <- update(
  fit_1, . ~ . + bilingualism + dominance:bilingualism,
  newdata = responses, file = here("Results", "fit_2.rds"),
)

fit_3 <- brm(
  response ~ age + frequency + dominance*bilingualism*cognate,
  family = cratio(link = "logit"),
  data = responses,
  prior = prior(normal(0, 5), class = b),
  chains = 4, iter = 1000,
  file = here("Results", "fit_3.rds"),
  backend = "cmdstanr"
)

#### examine posterior ---------------------------------------------------------
# extract posterior draws for estimated parameters
post <- gather_draws(fit_3, `b_.*`, regex = TRUE) %>% 
  mutate(.chain = as.factor(.chain))

# check convergence of MCMC chains
ggplot(post, aes(x = .iteration, y = .value, colour = .chain)) +
  facet_wrap(~.variable, scales = "free_y", nrow = 4) +
  geom_line() +
  labs(x = "Iteration", y = "Value", colour = "Chain") +
  scale_colour_brewer(palette = "Dark2", direction = -1) +
  theme_custom() +
  theme(legend.position = "top") +
  ggsave(here("Figures", "mcmc.png"))


post %>% 
  ggplot(aes(.value, reorder(.variable, desc(.variable)))) +
  stat_slab(aes(alpha = stat(cut_cdf_qi(cdf, .width = c(.5, .8, .95, 0.99), # quantiles
                                        labels = percent_format(accuracy = 1)))),
            fill = "steelblue") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  labs(x = "Value", y = "Probability density",
       fill = "CrI", alpha = "CrI") +
  scale_alpha_discrete(range = c(1, 0.25), na.translate = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  theme_custom() +
  theme(legend.position = "top") +
  ggsave(here("Figures", "coefs.png"), height = 4)


# compare models ---------------------------------------------------------------

# posterior predictions --------------------------------------------------------

# values to get posterior draws for (set frequency at mean = 0)
nd <- expand_grid(
  n = 1,
  age = seq(min(responses$age), max(responses$age), 0.1),
  dominance = c("L1", "L2"),
  bilingualism = c(min(responses$bilingualism), 0, max(responses$bilingualism)),
  frequency = 0,
  cognate = c("Cognate", "Non-Cognate")
)

nd_means <- expand_grid(
  n = 1,
  age = 0,
  dominance = c("L1", "L2"),
  bilingualism = c(min(responses$bilingualism), 0, max(responses$bilingualism)),
  frequency = 0,
  cognate = c("Cognate", "Non-Cognate")
)

# get 20 posterior draws
post_preds <- add_fitted_draws(nd, fit_3, n = 50, re_formula = NA) %>% 
  mutate(
    bilingualism = factor(
      bilingualism,
      levels = unique(.$bilingualism),
      labels = c("Bilingualism = 0%", "Bilingualism = Mean", "Bilingualism = 50%")
      ),
    .category = factor(.category, levels = c(1, 2, 3), labels = c("No", "Comprehension", "Production"))
  ) %>% 
  filter(.category %in% c("Comprehension", "Production"))

post_preds_means <- add_predicted_draws(nd_means, fit_3, n = 50, re_formula = NA) %>% 
  count(dominance, bilingualism, cognate, .prediction, name = ".value") %>% 
  rename(.category = .prediction) %>% 
  mutate(.value = .value/50) %>% 
  mutate(
    bilingualism = factor(
      bilingualism,
      levels = unique(.$bilingualism),
      labels = c("Bilingualism = 0%", "Bilingualism = Mean", "Bilingualism = 50%")
    ),
    .category = factor(.category, levels = c("No", "Understands", "Understands & Says"), labels = c("No", "Comprehension", "Production"))
  ) %>% 
  filter(.category %in% c("Comprehension", "Production"))

# visualise posterior predictions
post_preds %>% 
  ggplot(aes(
    age, .value,
    colour = interaction(dominance, cognate, sep = " - "),
    fill = interaction(dominance, cognate, sep = " - ")
  )) +
  facet_grid(.category~bilingualism) +
  geom_line(aes(group = interaction(dominance, cognate, bilingualism, .draw)), size = 0.4, alpha = 0.5) +
  stat_pointinterval(data = post_preds_means, aes(x = max(responses$age)+0.1)) +
  #stat_lineribbon(.width = 0.95, colour = NA, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line", size = 0.75) +
  #geom_point(data = proportion, alpha = 0.5) +
  labs(x = "Age (months)", y = "P(Y|X)") +
  guides(colour = guide_legend(ncol = 2)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_custom() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.direction = "horizontal"
  ) +
  ggsave(here("Figures", "post-preds.png"), width = 8)

#### test interactions ---------------------------------------------------------

# three-way interactions
deltas <- emmeans(fit_2, pairwise~dominance, by = "bilingualism",
           at = list(bilingualism = c(-1, 0, 1))$contrasts)  %>% 
  map(~as.mcmc(.) %>%
        as.matrix() %>% 
        as.data.frame() %>% 
        pivot_longer(everything(), names_to = "contrast", values_to = "delta")
  ) %>% 
  set_names(c("Comprehension", "Production")) %>% 
  bind_rows(.id = "type") %>% 
  separate(contrast, c("contrast", "bilingualism"), sep = ",") %>% 
  mutate(
    delta = delta/4,
    contrast = str_remove_all(contrast, "contrast"),
    bilingualism = str_remove_all(bilingualism, " bilingualism ") %>% 
      paste0(., " SD") %>% 
      str_replace(., "0 SD", "Mean") %>%
      paste0("Bilingualism: ", .) %>% 
      factor(., levels = c("Bilingualism: -1 SD", "Bilingualism: Mean", "Bilingualism: 1 SD"), ordered = TRUE)
  ) %>% 
  group_by(type, contrast, bilingualism) %>%
  mean_qi() %>% 
  select(type, bilingualism, contrast, delta, .lower, .upper) %>% 
  arrange(type, bilingualism, contrast)

ggplot(deltas,
       aes(delta, contrast, colour = type)) +
  facet_wrap(~bilingualism) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(size = 0.5) + 
  labs(
    x = "Marginal mean posterior difference",
    y = "Contrast",
    colour = "Type",
    fill = "Type",
    alpha = "CrI"
  ) +
  scale_colour_brewer(palette = "Dark2") +
  theme_custom() +
  theme(
    legend.position = "top",
    axis.title.y = element_blank()
  ) +
  ggsave(here("Figures", "contrasts.png"), height = 4)
