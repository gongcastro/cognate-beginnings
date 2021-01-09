#### set up --------------------------------------------------------------------

# load packages
library(tidyverse)
library(multilex)
library(brms)
library(janitor)
library(scales)
library(stringdist)
library(tidybayes)
library(here)

#### import data ---------------------------------------------------------------
ml_connect("gonzalo.garciadecastro@upf.edu")
participants <- ml_participants()
responses <- ml_responses(participants)  
logs <- ml_logs(participants, responses)
vocabulary <- ml_vocabulary(participants, responses)

#### items ---------------------------------------------------------------------
dat_items <- pool %>% 
  drop_na(cognate, ipa) %>% 
  filter(
    include,
    class %in% c("Noun")
  ) %>% 
  rename(frequency = frequency_zipf) %>% 
  select(te, language, item, ipa, frequency, cognate) %>% 
  pivot_wider(
    id_cols = c(te, cognate),
    names_from = language,
    values_from = c(item, ipa, frequency)
  ) %>% 
  clean_names() %>% 
  mutate(
    similarity = stringsim(ipa_catalan, ipa_spanish, method = "lv")
  ) %>% 
  pivot_longer(
    cols = matches("_catalan|_spanish"),
    names_to = c(".value", "language"),
    names_sep = "_",
  ) %>% 
  mutate(language = str_to_sentence(language),
         cognate = ifelse(cognate, "Cognate", "Non-Cognate")) %>% 
  relocate(te, language, item, ipa, cognate, frequency)

write.table(dat_items, "Data/items.csv", row.names = FALSE)

#### participants -----------------------------------------------------------------
dat_participants <- logs %>% 
  filter(
    completed,
    !(version %in% c("DevLex", "CBC")),
    lp %in% c("Monolingual", "Bilingual"),
    between(age, 10, 40),
  ) %>% 
  group_by(id) %>% 
  filter(time==max(time)) %>% 
  ungroup() %>% 
  select(id, time, age, dominance, doe_catalan, doe_spanish) %>% 
  mutate_at(vars(matches("doe")), function(x) x*0.01) 

write.table(dat_participants, "Data/participants.csv", row.names = FALSE)

#### vocabulary sizes ----------------------------------------------------------
dat_vocab <- dat_participants %>%
  left_join(vocabulary, by = c("id", "time")) %>% 
  mutate(
    id = as.numeric(as.factor(id)),
    vocab_type = str_to_sentence(vocab_type)
  )

write.table(dat_vocab, "Data/vocabulary.csv", row.names = FALSE)

#### process responses ---------------------------------------------------------
cut_quantiles <- function(x, quantiles = seq(0.0, 1, 0.2)) {
  cut(x,
      breaks = quantile(x, probs = quantiles), 
      labels = FALSE,
      include.lowest = TRUE
  )
}

dat_responses <- expand_grid(
  id = dat_participants$id,
  item = dat_items$item
) %>%
  left_join(dat_participants, by = "id") %>% 
  left_join(dat_items, by = "item") %>% 
  left_join(select(responses, id, item, response), by = c("id", "item")) %>% 
  select(id, age, dominance, doe_catalan, doe_spanish, te, language, item, ipa, cognate, frequency, response) %>% 
  drop_na() %>%
  distinct(id, te, age, item, .keep_all = TRUE) %>% 
  mutate(
    doe = case_when(
      dominance=="Catalan" ~ doe_spanish,
      dominance=="Spanish" ~ doe_catalan
    ),
    item_dominance = ifelse(dominance==language, "L1", "L2"),
    understands = response %in% c(2, 3),
    produces = response %in% 3,
    age = cut_width(age, width = 2, labels = FALSE),
    doe = round(doe, 1)*10,
    frequency = cut_quantiles(frequency)
  ) %>% 
  group_by(age, doe, te, frequency, cognate, item_dominance) %>% 
  summarise(
    understands = sum(understands),
    produces = sum(produces),
    n = n(),
    .groups = "drop"
  ) %>% 
  mutate_at(vars(age, frequency, doe), function(x) scale(x, scale = FALSE)[,1]) %>% 
  mutate_at(vars(item_dominance, cognate), as.factor) %>% 
  arrange(te) %>% 
  select(age, doe, te, frequency, item_dominance, cognate, understands, produces, n) %>% 
  drop_na() 

write.table(dat_responses, "Data/responses.csv", row.names = FALSE)

#### model fitting -------------------------------------------------------------
contrasts(dat_responses$item_dominance) <- contr.sum(2)/2
contrasts(dat_responses$cognate) <- contr.sum(2)/2

# fit comprehensive data
comp_0 <- brm(
  understands | trials(n) ~ 1,
  data = dat_responses,
  family = binomial("logit"),
  prior = prior(normal(0, 0.5), class = Intercept),
  save_model = "Stan/comp_0.stan",
  file = "Results/comp_0.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

comp_1 <- update(
  comp_0, . ~ . + age,
  prior = c(prior(normal(0, 0.5), class = Intercept),
            prior(normal(0, 0.5), class = b)),
  newdata = dat_responses,
  save_model = "Stan/comp_1.stan",
  file = "Results/comp_1.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

comp_2 <- update(
  comp_1, 
  . ~ . + frequency,
  newdata = dat_responses,
  save_model = "Stan/comp_2.stan",
  file = "Results/comp_2.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

comp_3 <- update(
  comp_2, 
  . ~ . + item_dominance,
  newdata = dat_responses,
  save_model = "Stan/comp_3.stan",
  file = "Results/comp_3.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

comp_4 <- update(
  comp_3, 
  . ~ .  + doe,
  newdata = dat_responses,
  save_model = "Stan/comp_4.stan",
  file = "Results/comp_4.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

comp_5 <- update(
  comp_4, 
  . ~ . + item_dominance:doe,
  newdata = dat_responses,
  save_model = "Stan/comp_5.stan",
  file = "Results/comp_5.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

comp_6 <- update(
  comp_5, 
  . ~ . + cognate,
  newdata = dat_responses,
  save_model = "Stan/comp_6.stan",
  file = "Results/comp_6.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

comp_7 <- update(
  comp_6, 
  . ~ . + item_dominance:cognate,
  newdata = dat_responses,
  save_model = "Stan/comp_7.stan",
  file = "Results/comp_7.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

comp_8 <- update(
  comp_7, 
  . ~ . + doe:cognate,
  newdata = dat_responses,
  save_model = "Stan/comp_8.stan",
  file = "Results/comp_8.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

comp_9 <- update(
  comp_8, 
  . ~ . + item_dominance:doe:cognate,
  newdata = dat_responses,
  save_model = "Stan/comp_9.stan",
  file = "Results/comp_9.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

# fit productive data
prod_0 <- brm(
  produces ~ 1,
  data = dat_responses,
  family = bernoulli("logit"),
  prior = prior(normal(0.5, 0.5), class = Intercept),
  save_model = "Stan/prod_0.stan",
  file = "Results/prod_0.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

prod_1 <- update(
  prod_0, . ~ . + age,
  prior = c(prior(normal(0.5, 0.5), class = Intercept),
            prior(normal(0, 0.5), class = b)),
  newdata = dat_responses,
  save_model = "Stan/prod_1.stan",
  file = "Results/prod_1.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

prod_2 <- update(
  prod_1, 
  . ~ . + frequency,
  newdata = dat_responses,
  save_model = "Stan/prod_2.stan",
  file = "Results/prod_2.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

prod_3 <- update(
  prod_2, 
  . ~ .  + item_dominance,
  newdata = dat_responses,
  save_model = "Stan/prod_3.stan",
  file = "Results/prod_3.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

prod_4 <- update(
  prod_3, 
  . ~ . + doe,
  newdata = dat_responses,
  save_model = "Stan/prod_4.stan",
  file = "Results/prod_4.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

prod_5 <- update(
  prod_4, 
  . ~ . + item_dominance:doe,
  newdata = dat_responses,
  save_model = "Stan/prod_5.stan",
  file = "Results/prod_5.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

prod_6 <- update(
  prod_5, 
  . ~ . + cognate,
  newdata = dat_responses,
  save_model = "Stan/prod_6.stan",
  file = "Results/prod_6.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

prod_7 <- update(
  prod_6, 
  . ~ . + item_dominance:cognate,
  newdata = dat_responses,
  save_model = "Stan/prod_7.stan",
  file = "Results/prod_7.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

prod_8 <- update(
  prod_7, 
  . ~ . + doe:cognate,
  newdata = dat_responses,
  save_model = "Stan/prod_8.stan",
  file = "Results/prod_8.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

prod_9 <- update(
  prod_8, 
  . ~ . + item_dominance:doe:cognate,
  newdata = dat_responses,
  save_model = "Stan/prod_9.stan",
  file = "Results/prod_9.rds",
  seed = 888, iter = 2000, chains = 4, cores = 4
)

#### model comparison ----------------------------------------------------------
comp_0_loo <- loo(comp_0)
comp_1_loo <- loo(comp_1)
comp_2_loo <- loo(comp_2)
comp_3_loo <- loo(comp_3)
comp_4_loo <- loo(comp_4)
comp_5_loo <- loo(comp_5)
comp_6_loo <- loo(comp_6)
comp_7_loo <- loo(comp_7)
comp_8_loo <- loo(comp_8)
comp_9_loo <- loo(comp_9)

kfold_comp <- kfold(comp_0, comp_1, comp_2, comp_3, comp_4, comp_5, comp_6, comp_7, comp_8, comp_9)
kfold_prod <- kfold(prod_0, prod_1, prod_2, prod_3, prod_4, prod_5, prod_6, prod_7, prod_8, prod_9)

#### test interactions ---------------------------------------------------------

hypothesis(comp_9, c(
  lowDoe_L1_cognate = "Intercept + 30*age - 1*doe - 0.5*item_dominance1 + 0.5*cognate1 - 0.5*item_dominance1:doe - 0.5*item_dominanceitem_dominance1 0.5*cognate1= 0"
  
))

#### examine posterior ---------------------------------------------------------
post <- list(
  comprehensive = gather_draws(comp_9, `b.*`, regex = TRUE),
  productive = gather_draws(prod_9, `b.*`, regex = TRUE)
)%>% 
  bind_rows(.id = "type") %>% 
  mutate(.chain = as.factor(.chain)) %>% 
  filter(!(.variable %in% c("b_Intercept", "b_age")))

ggplot(post, aes(x = .iteration, y = .value, colour = .chain)) +
  facet_wrap(type~.variable, scales = "free_y", nrow = 4) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2", direction = -1) +
  theme_minimal() +
  labs(x = "Iteration", y = "Value", colour = "Chain") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "top") +
  ggsave(here("Figures", "responses-mcmc.png"))


ggplot(post, aes(.value, .variable)) +
  facet_wrap(~type, scales = "free", nrow = 4) +
  stat_slab(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.5, .8, .95, 0.99), # quantiles
                                       labels = percent_format(accuracy = 1))))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Value", y = "Probability density",
       fill = "CrI") +
  scale_fill_brewer(palette = "Oranges", direction = -1, na.translate = FALSE) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey", colour = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "top") +
  ggsave(here("Figures", "responses-coefs.png"))

#### posterior predictions -----------------------------------------------------
nd <- expand_grid(
  age = seq(10, 40, 1),
  item_dominance = c("L1", "L2"),
  doe = c(-1, 0, 1),
  frequency = c(-1, 0, 1),
  cognate = c("Cognate", "Non-Cognate")
)
post_preds <- list(
  comprehensive = add_fitted_draws(nd, comp_8, n = 50),
  productive = add_fitted_draws(nd, prod_8, n = 50)
) %>% 
  bind_rows(.id = "type") %>% 
  mutate_at(vars(doe, frequency), as.character) %>% 
  mutate_at(vars(doe, frequency), function(x){
    str_replace_all(x, c("0" = "Mean", "1" = "1 SD")) %>% 
      factor(levels = c("-1 SD", "Mean", "1 SD"), ordered = TRUE)
  }) %>% 
  rename(
    DOE = doe,
    Frequency = frequency
  )

post_preds %>% 
  filter(type=="comprehensive") %>%
  ggplot(aes(age, .value, colour = interaction(item_dominance, cognate, sep = " - "))) +
  facet_grid(Frequency~DOE, labeller = label_both) +
  #geom_line(aes(group = interaction(item_dominance, cognate, .draw)), alpha = 0.25) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(x = "Age (months)", y = "P(Understands)") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey", colour = NA),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 15)) +
  ggsave(here("Figures", "responses-post-preds-comp.png"), height = 8)

post_preds %>% 
  filter(type=="productive") %>%
  ggplot(aes(age, .value, colour = interaction(item_dominance, cognate, sep = " - "))) +
  facet_grid(Frequency~DOE, labeller = label_both) +
  #geom_line(aes(group = interaction(item_dominance, cognate, .draw)), alpha = 0.25) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(x = "Age (months)", y = "P(Produces)") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey", colour = NA),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 15)) +
  ggsave(here("Figures", "responses-post-preds-prod.png"), height = 8)
