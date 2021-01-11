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

#### import data ---------------------------------------------------------------
responses <- read_csv(here("Data", "responses.csv")) %>% 
  mutate_at(vars(age, frequency, doe), function(x) scale(x, scale = FALSE)[,1]) %>% 
  mutate_at(vars(item_dominance, cognate, response), as.factor) 

responses_subset <- filter(responses, te %in% sample(responses$te, 25)) %>% 
  mutate_at(vars(age, frequency, doe), function(x) scale(x, scale = FALSE)[,1]) %>% 
  mutate_at(vars(item_dominance, cognate), as.factor) %>% 
  mutate(response = factor(response, ordered = TRUE))

# summarise responses (for visualisation purposes)
proportion <- responses %>% 
  mutate(
    understands = response %in% 2,
    says = response %in% 3, 
    frequency = cut_quantiles(frequency),
    age = round(age)
  ) %>%
  select(-doe) %>% 
  pivot_longer(c(understands, says), names_to = ".category", values_to = ".value") %>% 
  mutate(
    .category = str_to_sentence(.category),
    .value = as.numeric(.value)
  ) %>% 
  group_by(age, cognate, item_dominance, .category) %>%
  summarise(
    .value = sum(.value),
    n = n(),
    .groups = "drop"
  ) %>% 
  rowwise() %>% 
  mutate(.value = prop_adj(.value, n)) %>% 
  ungroup() 


# set sum contrasts (Schad et al., 2018, https://arxiv.org/abs/1807.10451)
contrasts(responses_subset$item_dominance) <- contr.sum(2)/2
contrasts(responses_subset$cognate) <- contr.sum(2)/2

#### model fitting -------------------------------------------------------------

# set weakly uninformative prior
prior <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 0.5), class = b)
)

fit_0 <- brm(
  response ~ 1,
  data = responses_subset,
  family = cumulative("logit"),
  prior = prior[1,],
  save_model = here("Stan", "fit_0.stan"), # save Stan code
  file = here("Results", "fit_0.rds"),
  seed = 888, iter = 1000, chains = 4, cores = 4
)

fit_1 <- update(
  fit_0, . ~ . + age,
  prior = prior,
  newdata = responses_subset,
  save_model = here("Stan", "fit_1.stan"),
  file = here("Results", "fit_1.rds"),
  seed = 888, iter = 1000, chains = 4, cores = 4
)

fit_2 <- update(
  fit_1, . ~ . + frequency,
  newdata = responses_subset,
  save_model = here("Stan", "fit_2.stan"),
  file = here("Results", "fit_2.rds"),
  seed = 888, iter = 1000, chains = 4, cores = 4
>>>>>>> d2499895c5c393388941f69681aa05ed1cddafbd
)

fit_3 <- update(
  fit_2, . ~ . + item_dominance,
  newdata = responses_subset,
  save_model = here("Stan", "fit_3.stan"),
  file = here("Results", "fit_3.rds"),
  seed = 888, iter = 1000, chains = 4, cores = 4
)

fit_4 <- update(
  fit_3, . ~ . + doe,
  newdata = responses_subset,
  save_model = here("Stan", "fit_4.stan"),
  file = here("Results", "fit_4.rds"),
  seed = 888, iter = 1000, chains = 4, cores = 4
)

fit_5 <- update(
  fit_4, . ~ . + item_dominance:doe,
  newdata = responses_subset,
  save_model = here("Stan", "fit_5.stan"),
  file = here("Results", "fit_5.rds"),
  seed = 888, iter = 1000, chains = 4, cores = 4
)

fit_6 <- update(
  fit_5, . ~ . + cognate,
  newdata = responses_subset,
  save_model = here("Stan", "fit_6.stan"),
  file = "Results/fit_6.rds",
  seed = 888, iter = 1000, chains = 4, cores = 4
)

fit_7 <- update(
  fit_6, . ~ . + item_dominance:cognate,
  newdata = responses_subset,
  save_model = here("Stan", "fit_7.stan"),
  file = here("Results", "fit_7.rds"),
  seed = 888, iter = 1000, chains = 4, cores = 4
)

fit_8 <- update(
  fit_7, . ~ . + doe:cognate,
  newdata = responses_subset,
  save_model = here("Stan", "fit_8.stan"),
  file = here("Results", "fit_8.rds"),
  seed = 888, iter = 1000, chains = 4, cores = 4
)

fit_9 <- update(
  fit_8, . ~ . + item_dominance:doe:cognate,
  newdata = responses_subset,
  save_model = here("Stan", "fit_9.stan"),
  file = here("Results", "fit_9.rds"),
  seed = 888, iter = 1000, chains = 4, cores = 4
)


#### model comparison ----------------------------------------------------------
<<<<<<< HEAD
map2(list(fit0, fit1, fit2, fit2),
     list(here("Results", "responses0.rds"),
          here("Results", "responses1.rds"),
          here("Results", "responses2.rds")),
     ~add_criterion(
       .x,
       criterion = c("loo", "waic", "bayes_R2", "loo_R2", "marglik"),
       file = .y,
       overwrite = TRUE
     ))

loos <- loo(fit0, fit1, fit2)
waic <- WAIC(fit0, fit1, fit2)
bf <-
  
  #### test interactions ---------------------------------------------------------

h <- hypothesis(
  fit2,
  class = "bcs", seed = 888,
  c("cognate1[1] + item_dominance1[1] = 0",
    "cognate1[1] + age:item_dominance1[1] = 0",
    "item_dominance1[1] + cognate1[1] = 0",
    "item_dominance1[1] + age:cognate1[1] = 0",
    "cognate1[2] + item_dominance1[2] = 0",
    "cognate1[2] + age:item_dominance1[2] = 0",
    "item_dominance1[2] + cognate1[2] = 0",
    "item_dominance1[2] + age:cognate1[2] = 0",
    "age[1] = age[2]",
    "item_dominance1[1] = item_dominance1[2]",
    "cognate1[1] = cognate1[2]",
    "age:item_dominance1[1] = age:item_dominance1[2]",
    "age:cognate1[1] = age:cognate1[2]",
    "age:item_dominance1:cognate1[1] = age:item_dominance1:cognate1[2]"
  )
)
#### posterior -----------------------------------------------------------------
post <- list(posterior = gather_draws(fit0, `b.*`, regex = TRUE),
             prior = gather_draws(fit0, `prior.*`, regex = TRUE)) %>% 
  bind_rows(.id = "dist") %>% 
  mutate(.chain = as.factor(.chain),
         category = case_when(
           str_detect(.variable, "\\[1\\]") ~ "No-Understands",
           str_detect(.variable, "\\[2\\]") ~ "Understands-Produces",
           str_detect(.variable, "prior") ~ "Prior"
         ),
         .variable = str_remove(.variable, "prior_"),
         .variable = str_remove_all(.variable, "\\[.*?\\]")
  )

post %>% 
  ggplot(aes(x = .iteration, y = .value, colour = .chain)) +
  facet_wrap(category~.variable, scales = "free_y") +
  geom_line() +
  scale_colour_manual(values = c("#543005", "#BF812D", "#80CDC1", "#01665E")) +
  theme_minimal() +
  labs(x = "Iteration", y = "Value", colour = "Chain") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "top")

post %>% 
  filter(category != "Prior") %>% 
  ggplot(aes(.value, fill = category)) +
  facet_wrap(~.variable, scales = "free") +
  stat_halfeye() +
  labs(x = "Value", y = "Probability density",
       fill = "CrI") +
  #scale_fill_brewer(palette = "Oranges", direction = -1,) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey", colour = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "top") +
  ggsave(here("Figures", "responses-coefs.png"))

#### posterior predictions -----------------------------------------------------
post_preds <- expand_grid(
  age = seq(min(dat_responses$age, na.rm = TRUE), max(dat_responses$age, na.rm = TRUE), by = 0.5),
  #item_dominance = c("L1", "L2"),
  #cognate = c("Cognate", "Non-cognate")
) %>% 
  add_fitted_draws(., fit0, n = 50) %>% 
  mutate(
    #bilingualism = ifelse(bilingualism==0, "Monolingual", "Bilingual"),
    .category = case_when(
      .category==1 ~ "No",
      .category==2 ~ "Understands",
      .category==3 ~ "Understands & Produces"
    )
  )

ggplot(post_preds,
       aes(age, .value,
           #colour = interaction(item_dominance, cognate, lex.order = TRUE, sep = " - "),
           #fill = interaction(item_dominance, cognate, lex.order = TRUE, sep = " - ")
       )) +
  facet_wrap(~.category) +
  stat_lineribbon(.width = 0.95, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-14, 17, by = 5), labels = seq(10, 40, by = 5)) +
  labs(x = "Age (months)", y = "P(Y|X)") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey", colour = NA),
        legend.title =element_blank(),
        legend.position = "top",
        text = element_text(size = 15)) +
  ggsave(here("Figures", "responses-multinomial.png"), height = 4)
=======

# paretho-smooth leave-one-out cross-validation
fit_0 <- add_criterion(fit_0, c("loo", "waic"))
fit_1 <- add_criterion(fit_1, c("loo", "waic"))
fit_2 <- add_criterion(fit_2, c("loo", "waic"))
fit_3 <- add_criterion(fit_3, c("loo", "waic"))
fit_4 <- add_criterion(fit_4, c("loo", "waic"))
fit_5 <- add_criterion(fit_5, c("loo", "waic"))
fit_6 <- add_criterion(fit_6, c("loo", "waic"))
fit_7 <- add_criterion(fit_7, c("loo", "waic"))
fit_8 <- add_criterion(fit_8, c("loo", "waic"))
fit_9 <- add_criterion(fit_9, c("loo", "waic"))

loos <- loo_compare(fit_0, fit_1, fit_2, fit_3, fit_4, fit_5, fit_6, fit_7, fit_8, fit_9, criterion = c("loo", "waic"))
saveRDS(loos, here("Results", "loo.rds"))

#### test interactions ---------------------------------------------------------
cognate_by_dominance <- emmeans(fit_9, "item_dominance", specs = pairwise ~ cognate, type =  "response")
dominance_by_cognate <- emmeans(fit_9, "cognate", specs = pairwise ~ item_dominance, type =  "response")
dominance_by_cognate <- emmeans(fit_9, "cognate", specs = pairwise ~ item_dominance, type =  "response")

#### examine posterior ---------------------------------------------------------

# extract posterior draws for estimated parameters
post <- gather_draws(fit_9, `b.*`, regex = TRUE) %>% 
  mutate(.chain = as.factor(.chain)) 

# check convergence of MCMC chains
ggplot(post, aes(x = .iteration, y = .value, colour = .chain)) +
  facet_wrap(~.variable, scales = "free_y", nrow = 4) +
  geom_line() +
  labs(x = "Iteration", y = "Value", colour = "Chain") +
  scale_colour_brewer(palette = "Dark2", direction = -1) +
  theme_custom() +
  theme(legend.position = "top") +
  ggsave(here("Figures", "responses-mcmc.png"))

# posterior density by parameter
ggplot(post, aes(.value, .variable)) +
  stat_slab(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.5, .8, .95, 0.99), # quantiles
                                       labels = percent_format(accuracy = 1))))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Value", y = "Probability density",
       fill = "CrI") +
  scale_fill_brewer(palette = "Oranges", direction = -1, na.translate = FALSE) +
  theme_custom() +
  theme(legend.position = "top") +
  ggsave(here("Figures", "responses-coefs.png"))

#### posterior predictions -----------------------------------------------------
>>>>>>> d2499895c5c393388941f69681aa05ed1cddafbd

# values to get posterior draws for (set frequency at mean = 0)
nd <- expand_grid(
  age = seq(min(dat_responses$age), max(dat_responses$age), 1),
  item_dominance = c("L1", "L2"),
  doe = c(-1, 0, 1),
  frequency = 0,
  cognate = c("Cognate", "Non-Cognate")
)

# get 20 posterior draws
post_preds <- add_fitted_draws(nd, fit_9, n = 20) %>% 
  mutate(
    doe = doe %>%
      as.character() %>%  
      str_replace_all(c(
        "0" = "Mean",
        "1" = "1 SD"
      )) %>% 
      paste0("DOE = ", .) %>% 
      factor(
        levels = c(
          "DOE = -1 SD",
          "DOE = Mean",
          "DOE = 1 SD"
        ),
        ordered = TRUE
      )
  ) %>% 
  mutate(
    .category = case_when(
      .category==1 ~ "None",
      .category==2 ~ "Understands",
      .category==3 ~ "Says"
    ),
    .category = factor(.category, levels = c("None", "Understands", "Says"), ordered = TRUE)
  ) 

# visualise posterior predictions
post_preds %>% 
  filter(.category!="None") %>% 
  ggplot(aes(
    age, .value,
    colour = interaction(item_dominance, cognate, sep = " - "),
    fill = interaction(item_dominance, cognate, sep = " - ")
  )) +
  facet_grid(doe~.category) +
  #geom_line(aes(group = interaction(item_dominance, cognate, doe, .draw)), size = 0.4) +
  stat_lineribbon(.width = 0.95, colour = NA, alpha = 0.5) +
  stat_summary(fun = "median", geom = "line", size = 0.75) +
  geom_point(data = proportion, alpha = 0.5) +
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
  ggsave(here("Figures", "responses-post-preds.png"), width = 7)

