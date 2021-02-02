#### analysis ------------------------------------------------------------------

#### set up --------------------------------------------------------------------

# load packages
library(tidyverse)
library(brms)
library(scales)
library(tidybayes)
library(emmeans)
library(bayestestR)
library(here)

# load helper functions
source(here("R", "utils.R"))
options(mc.cores = 4)

#### import data ---------------------------------------------------------------
responses <- read_csv(here("Data", "responses.csv")) %>% 
  mutate_at(vars(age, frequency, bilingualism), function(x) scale(x)[,1]) %>% 
  mutate_at(vars(dominance, cognate), as.factor) %>% 
  filter(te %in% sample(.$te, 25))

# responses_subset <- filter(responses, te %in% sample(responses$te, 25)) %>% 
#   mutate_at(vars(age, frequency, doe), function(x) scale(x, scale = FALSE)[,1]) %>% 
#   mutate_at(vars(item_dominance, cognate), as.factor) %>% 
#   mutate(response = factor(response, ordered = TRUE))

# summarise responses (for visualisation purposes)
proportion <- responses %>% 
  pivot_longer(c(understands, produces), names_to = "type", values_to = ".value") %>% 
  mutate(type = str_to_sentence(type)) %>% 
  group_by(age, frequency, bilingualism, cognate, dominance, type) %>%
  summarise(
    .value = (.value+0.5)/(n()-.value+0.5),
    n = n(),
    .groups = "drop"
  ) %>% 
  rowwise() %>% 
  mutate(.value = prop_adj(.value, n)) %>% 
  ungroup() 

# set sum contrasts (Schad et al., 2018, https://arxiv.org/abs/1807.10451)
contrasts(responses$dominance) <- contr.sum(2)/2
contrasts(responses$cognate) <- contr.sum(2)/2

#### model fitting -------------------------------------------------------------

# comprehensive data
if (file.exists(here("Results", "comp_fits.RData"))) {
  load(here("Results", "comp_fits.RData"))
} else {
  comp_0 <- brm(
    understands | trials(n) ~ 1 + age + frequency + (1 + age | te),
    data = responses,
    family = binomial("logit"),
    # set weakly informative prior
    prior = c(prior(normal(0, 1.5), class = Intercept),
              prior(normal(0, 0.5), class = b),
              prior(normal(0, 1), class = sd),
              prior(lkj(3), class = cor)),
    save_all_pars = TRUE
  )
  comp_1 <- update(comp_0, . ~ . + dominance, newdata = responses)
  comp_2 <- update(comp_1, . ~ . + bilingualism, newdata = responses)
  comp_3 <- update(comp_2, . ~ . + dominance:bilingualism, newdata = responses)
  comp_4 <- update(comp_3, . ~ . + cognate, newdata = responses)
  comp_5 <- update(comp_4, . ~ . + dominance:cognate, newdata = responses)
  comp_6 <- update(comp_5, . ~ . + bilingualism:cognate, newdata = responses)
  comp_7 <- update(comp_6, . ~ . + dominance:bilingualism:cognate, newdata = responses, sample_prior = "yes")
  
  save(comp_0, comp_1, comp_2, comp_3, comp_4, comp_5, comp_6, comp_7,
       file = here("Results", "comp_fits.RData"))
}


# productive data
if (file.exists(here("Results", "prod_fits.RData"))) {
  load(here("Results", "prod_fits.RData"))
} else {
  prod_0 <- brm(
    production | trials(n) ~ 1 + age + frequency,
    data = responses,
    family = binomial("logit"),
    # set weakly uninformative prior
    prior = c(prior(normal(0, 1.5), class = Intercept),
              prior(normal(0, 0.5), class = b)),
    save_all_pars = TRUE,
    file = here("Results", "prod_0.rds")
  )
  prod_1 <- update(prod_0, . ~ . + dominance, newdata = responses)
  prod_2 <- update(prod_1, . ~ . + bilingualism, newdata = responses)
  prod_3 <- update(prod_2, . ~ . + dominance:bilingualism, newdata = responses)
  prod_4 <- update(prod_3, . ~ . + cognate, newdata = responses)
  prod_5 <- update(prod_4, . ~ . + dominance:cognate, newdata = responses)
  prod_6 <- update(prod_5, . ~ . + bilingualism:cognate, newdata = responses)
  prod_7 <- update(prod_6, . ~ . + dominance:bilingualism:cognate, newdata = responses, sample_prior = "yes")
  
  save(prod_0, prod_1, prod_2, prod_3, prod_4, prod_5, prod_6, prod_7,
       file = here("Results", "prod_fits.RData"))
}

#### model comparison ----------------------------------------------------------
if (file.exists(here("Results", "loos.RData"))) {
  load(here("Results", "loos.RData"))
} else {
  loos_comp <- loo_subsample(comp_0, comp_1, comp_2, comp_3, comp_4, comp_5, comp_6, comp_7, nsamples = 500)
  loos_prod <- loo_subsample(prod_0, prod_1, prod_2, prod_3, prod_4, prod_5, prod_6, prod_7, nsamples = 500)
  save(loos_comp, loos_prod, file = here("Results", "loos.RData"))
}


#### examine posterior ---------------------------------------------------------

# extract posterior draws for estimated parameters
post <- bind_rows(
  Understands = gather_draws(comp_7, `b.*`, regex = TRUE),
  Produces = gather_draws(prod_7, `b.*`, regex = TRUE),
  .id = "type") %>% 
  mutate(
    .chain = as.factor(.chain),
    .variable = factor(.variable, levels = c(
      "b_dominance1", "b_bilingualism", "b_cognate1", 
      "b_dominance1:bilingualism", "b_dominance1:cognate1", "b_bilingualism:cognate1",
      "b_dominance1:bilingualism:cognate1"
    ), ordered = TRUE),
    type = factor(type, levels = c("Understands", "Produces"), ordered = TRUE)
  )

# check convergence of MCMC chains
ggplot(post, aes(x = .iteration, y = .value, colour = .chain)) +
  facet_wrap(type~.variable, scales = "free_y", nrow = 4) +
  geom_line() +
  labs(x = "Iteration", y = "Value", colour = "Chain") +
  scale_colour_brewer(palette = "Dark2", direction = -1) +
  theme_custom() +
  theme(legend.position = "top") +
  ggsave(here("Figures", "mcmc.png"))


post %>% 
  filter(str_detect(.variable, "dominance|bilingualism|cognate")) %>% 
  ggplot(aes(inv_logit_scaled(.value), reorder(.variable, desc(.variable)), fill = type)) +
  #facet_wrap(~type) +
  stat_slab(aes(alpha = stat(cut_cdf_qi(cdf, .width = c(.5, .8, .95, 0.99), # quantiles
                                        labels = percent_format(accuracy = 1))))) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  labs(x = "Value", y = "Probability density",
       fill = "CrI", alpha = "CrI") +
  scale_alpha_discrete(range = c(1, 0.25), na.translate = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  theme_custom() +
  theme(legend.position = "top") +
  ggsave(here("Figures", "coefs.png"), height = 4)

#### posterior predictions -----------------------------------------------------

# values to get posterior draws for (set frequency at mean = 0)
nd <- expand_grid(
  n = 1,
  age = seq(min(responses$age), max(responses$age), 0.1),
  dominance = c("L1", "L2"),
  bilingualism = c(min(responses$bilingualism), 0, max(responses$bilingualism)),
  frequency = 0,
  cognate = c("Cognate", "Non-Cognate")
)

# get 20 posterior draws
post_preds <- bind_rows(
  Understands = add_fitted_draws(nd, comp_7, n = 20, re_formula = NA),
  Produces = add_fitted_draws(nd, prod_7, n = 20, re_formula = NA),
  .id = "type"
) %>% 
  mutate(
    bilingualism = factor(
      bilingualism,
      levels = unique(.$bilingualism),
      labels = c("Bilingualism = 0%", "Bilingualism = Mean", "Bilingualism = 50%")),
    type = factor(type, levels = c("Understands", "Produces"), ordered = TRUE)
  )

# visualise posterior predictions
post_preds %>% 
  ggplot(aes(
    age, .value,
    colour = interaction(dominance, cognate, sep = " - "),
    fill = interaction(dominance, cognate, sep = " - ")
  )) +
  facet_wrap(type~bilingualism) +
  geom_line(aes(group = interaction(dominance, cognate, bilingualism, .draw)), size = 0.4) +
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
  ggsave(here("Figures", "post-preds.png"), width = 7)

#### test interactions ---------------------------------------------------------

# three-way interactions
deltas <- map(
  list(comp_7, prod_7),
  ~emmeans(., pairwise~dominance*cognate, by = "bilingualism",
           at = list(bilingualism = c(-1, 0, 1)))$contrasts 
) %>% 
  map(~as.mcmc(.) %>% 
        as_tibble() %>% 
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
