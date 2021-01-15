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
options(mc.cores = 4)

#### import data ---------------------------------------------------------------
responses <- read_csv(here("Data", "responses.csv")) %>% 
  mutate_at(vars(age, frequency, bilingualism), function(x) scale(x)[,1]) %>% 
  mutate_at(vars(dominance, cognate), as.factor)

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
    .value = log((.value+0.5)/(n()-.value+0.5)),
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
comp_0 <- brm(
  understands | trials(n) ~ 1 + age + frequency,
  data = responses,
  family = binomial("logit"),
  # set weakly uninformative prior
  prior = c(prior(normal(0, 0.5), class = Intercept),
            prior(normal(0, 0.5), class = b)),
  save_model = here("Stan", "comp_0.stan"), # save Stan code
  save_all_pars = TRUE,
  file = here("Results", "comp_0.rds")
)

comp_1 <- update(
  comp_0, . ~ . + dominance,
  newdata = responses,
  save_model = here("Stan", "comp_1.stan"),
  file = here("Results", "comp_1.rds"),
)

comp_2 <- update(
  comp_1, . ~ . + bilingualism,
  newdata = responses,
  save_model = here("Stan", "comp_2.stan"),
  file = here("Results", "comp_2.rds"),
)

comp_3 <- update(
  comp_2, . ~ . + dominance:bilingualism,
  newdata = responses,
  save_model = here("Stan", "comp_3.stan"),
  file = here("Results", "comp_3.rds"),
)

comp_4 <- update(
  comp_3, . ~ . + cognate,
  newdata = responses,
  save_model = here("Stan", "comp_4.stan"),
  file = here("Results", "comp_4.rds"),
)

comp_5 <- update(
  comp_4, . ~ . + dominance:cognate,
  newdata = responses,
  save_model = here("Stan", "comp_5.stan"),
  file = here("Results", "comp_5.rds"),
)

comp_6 <- update(
  comp_5, . ~ . + bilingualism:cognate,
  newdata = responses,
  save_model = here("Stan", "comp_6.stan"),
  file = here("Results", "comp_6.rds")
)

comp_7 <- update(
  comp_6, . ~ . + dominance:bilingualism:cognate,
  newdata = responses,
  save_model = here("Stan", "comp_7.stan"),
  file = here("Results", "comp_7.rds")
)


# productive data
prod_0 <- brm(
  produces | trials(n) ~ 1 + age + frequency,
  data = responses,
  family = binomial("logit"),
  # set weakly uninformative prior
  prior = c(prior(normal(0, 0.5), class = Intercept),
            prior(normal(0, 0.5), class = b)),
  save_model = here("Stan", "prod_0.stan"), # save Stan code
  save_all_pars = TRUE,
  file = here("Results", "prod_0.rds")
)

prod_1 <- update(
  prod_0, . ~ . + dominance,
  newdata = responses,
  save_model = here("Stan", "prod_1.stan"),
  file = here("Results", "prod_1.rds"),
)

prod_2 <- update(
  prod_1, . ~ . + bilingualism,
  newdata = responses,
  save_model = here("Stan", "prod_2.stan"),
  file = here("Results", "prod_2.rds"),
)

prod_3 <- update(
  prod_2, . ~ . + dominance:bilingualism,
  newdata = responses,
  save_model = here("Stan", "prod_3.stan"),
  file = here("Results", "prod_3.rds"),
)

prod_4 <- update(
  prod_3, . ~ . + cognate,
  newdata = responses,
  save_model = here("Stan", "prod_4.stan"),
  file = here("Results", "prod_4.rds"),
)

prod_5 <- update(
  prod_4, . ~ . + dominance:cognate,
  newdata = responses,
  save_model = here("Stan", "prod_5.stan"),
  file = here("Results", "prod_5.rds"),
)

prod_6 <- update(
  prod_5, . ~ . + bilingualism:cognate,
  newdata = responses,
  save_model = here("Stan", "prod_6.stan"),
  file = here("Results", "prod_6.rds")
)

prod_7 <- update(
  prod_6, . ~ . + dominance:bilingualism:cognate,
  newdata = responses,
  save_model = here("Stan", "prod_7.stan"),
  file = here("Results", "prod_7.rds")
)


#### model comparison ----------------------------------------------------------
loos_comp <- loo_subsample(comp_0, comp_1, comp_2, comp_3, comp_4, comp_5, comp_6, comp_7, nsamples = 500)
loos_prod <- loo_subsample(prod_0, prod_1, prod_2, prod_3, prod_4, prod_5, prod_6, prod_7, nsamples = 500)

saveRDS(list(loos_comp, loos_prod), here("Results", "loos.rds"))

#### test interactions ---------------------------------------------------------
cognate_by_dominance <- emmeans(comp_7, "dominance", specs = pairwise ~ cognate, type =  "response")
dominance_by_cognate <- emmeans(comp_7, "cognate", specs = pairwise ~ dominance, type =  "response")
doe <- emmeans(comp_7, "cognate", specs = pairwise ~ dominance, type =  "response")


hypotheses <- hypothesis(
  comp_7, alpha = 0.99, 
  c(
    "exp(cognate1)*-0.5 = exp(cognate1)*0.5",
    "dominance1*-0.5 = dominance1*0.5",
    "exp(cognate1) + exp(dominance1:cognate1)*-0.5 = exp(cognate1) + exp(dominance1:cognate1)*0.5 ",
    "exp(dominance1) + exp(dominance1:cognate1)*-0.5 = exp(dominance1) + exp(dominance1:cognate1)*0.5 "
  )
)
#### examine posterior ---------------------------------------------------------

# extract posterior draws for estimated parameters
post <- gather_draws(comp_7, `b.*`, regex = TRUE) %>% 
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

# posterior density by parameter
ggplot(post, aes(exp(.value), .variable)) +
  stat_slab(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.5, .8, .95, 0.99), # quantiles
                                       labels = percent_format(accuracy = 1))))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Value", y = "Probability density",
       fill = "CrI") +
  scale_fill_brewer(palette = "Oranges", direction = -1, na.translate = FALSE) +
  theme_custom() +
  theme(legend.position = "top") +
  ggsave(here("Figures", "coefs.png"))

#### posterior predictions -----------------------------------------------------

# values to get posterior draws for (set frequency at mean = 0)
nd <- expand_grid(
  n = 1,
  age = seq(min(responses$age), max(responses$age), 1),
  dominance = c("L1", "L2"),
  bilingualism = c(min(responses$bilingualism), 0, max(responses$bilingualism)),
  frequency = 0,
  cognate = c("Cognate", "Non-Cognate")
)

# get 20 posterior draws
post_preds <- add_fitted_draws(nd, comp_7, n = 50) %>% 
  mutate(bilingualism = factor(
    bilingualism,
    levels = unique(.$bilingualism),
    labels = c("Bilingualism = 0%", "Bilingualism = Mean", "Bilingualism = 50%")))

# visualise posterior predictions
post_preds %>% 
  ggplot(aes(
    age, .value,
    colour = interaction(dominance, cognate, sep = " - "),
    fill = interaction(dominance, cognate, sep = " - ")
  )) +
  facet_wrap(~bilingualism) +
  #geom_line(aes(group = interaction(item_dominance, cognate, doe, .draw)), size = 0.4) +
  stat_lineribbon(.width = 0.95, colour = NA, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line", size = 0.75) +
  #geom_point(data = filter(proportion, type=="Understands"), alpha = 0.5) +
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
