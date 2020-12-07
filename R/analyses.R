#### set up --------------------------------------------------------------------

# load packages
library(tidyverse)
library(multilex)
library(data.table)
library(brms)
library(tidybayes)
library(here)

#### import data ---------------------------------------------------------------
participants <- ml_participants("gonzalo.garciadecastro@upf.edu")
responses <- ml_responses(participants, "gonzalo.garciadecastro@upf.edu")  
logs <- ml_logs(participants, responses)
vocabulary <- ml_vocabulary(participants, responses)

#### vocabulary sizes ----------------------------------------------------------
dat_vocab <- logs %>% 
    filter(
        completed,
        !(version %in% c("DevLex", "CBC")),
        lp %in% c("Monolingual", "Bilingual"),
        between(age, 10, 40),
    ) %>% 
    select(id, time, age, doe_catalan, doe_spanish) %>% 
    left_join(
        vocabulary, by = c("id", "time")
    ) %>% 
    group_by(id) %>% 
    filter(time==min(.$time)) %>% 
    ungroup() %>% 
    mutate(
        bilingualism = case_when(
            doe_catalan > doe_spanish ~ doe_spanish/100,
            doe_spanish > doe_catalan ~ doe_catalan/100,
            TRUE ~ 0.5
        ),
        vocab_type = ifelse(vocab_type=="understands", 0, 1),
        id = as.numeric(as.factor(id))
    ) %>% 
    mutate_at(vars(vocab_type), as.factor) 

#### process responses ---------------------------------------------------------
dat_responses <- responses %>% 
    select(id, time, item, response, language) %>% 
    left_join(logs, by = c("id", "time")) %>% 
    mutate(
        item_dominance = ifelse(language==dominance, "L1", "L2"),
        bilingualism = ifelse(dominance=="Catalan", doe_spanish, doe_catalan)/100
    ) %>% 
    select(id, version, age, sex, dominance, lp, bilingualism, time, item, language, item_dominance, response, completed) %>% 
    left_join(select(pool, te, language, item, category, class, frequency, cognate, include),
              by = c("item", "language")) %>% 
    mutate(cognate = case_when(
        cognate ~ "Cognate",
        !cognate ~ "Non-cognate",
        TRUE ~ NA_character_)
    ) %>% 
    filter(
        completed,
        !(version %in% c("DevLex", "CBC")),
        lp %in% c("Monolingual", "Bilingual"),
        between(age, 10, 40),
        include,
        class %in% c("Verb", "Noun", "Adjective")
    ) %>% 
    group_by(id) %>% 
    filter(time==min(.$time)) %>% 
    ungroup() %>% 
    drop_na(response, te, cognate) %>% 
    select(id, age, sex, dominance, lp, te, item, response, language, item_dominance, cognate, frequency, bilingualism) %>% 
    #mutate(age_centred = scale(age, center = TRUE, scale = FALSE)[,1]) %>%
    mutate_at(vars(cognate, item_dominance), as.factor) %>%
    mutate(response = factor(
        response, ordered = TRUE,
        labels = c("No", "Understands", "Understands & Says"))
    ) %>% 
    arrange(item, te) %>% 
    select(id, age, bilingualism, item, te, item_dominance, cognate, response) 

contrasts(dat_responses$response) <- c(0, 1, 2)
contrasts(dat_responses$cognate) <- c(1, 0)
contrasts(dat_responses$item_dominance) <- c(0, 1)

#### model fitting -------------------------------------------------------------
priors <- c(
    prior(normal(0.5, 0.5), class = Intercept),
    prior(normal(0.1, 0.15), class = b, coef = "age"),
    prior(normal(0, 0.5), class = b, coef = "item_dominance1"),
    prior(normal(0, 0.5), class = b, coef = "age:item_dominance1"),
    prior(normal(0, 0.5), class = b, coef = "cognateness1"),
    prior(normal(0, 0.5), class = b, coef = "age:cognateness1"),
    prior(normal(0, 0.5), class = b, coef = "item_dominance1:cognateness1"),
    prior(normal(0, 0.5), class = b, coef = "age:item_dominance1:cognateness1")
)

fit0 <- brm(
    response ~ cs(age),
    data = dat_responses,
    family = acat(link = "logit", threshold = "flexible"),
    prior = priors[1:2,],
    save_all_pars = TRUE,
    sample_prior = "yes",
    save_model = here("Stan", "responses0.stan"),
    file = here("Results", "responses0.rds"),
    control = list(adapt_delta = 0.8, max_treedepth = 10),
    seed = 888, iter = 4000, chains = 4, init = 0, cores = 4
)

fit1 <- update(
    fit0, . ~ . -cs(age) + cs(age*item_dominance),
    newdata = dat_responses,
    file = here("Results", "responses1.rds"),
    save_model = here("Stan", "responses1.stan")
)

fit2 <- update(
    fit1, . ~ . + cs(age*item_dominance*cognate),
    newdata = dat_responses,
    file = here("Results", "responses2.rds"),
    save_model = here("Stan", "responses2.stan")
)

#### model comparison ----------------------------------------------------------
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


