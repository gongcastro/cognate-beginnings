# preprocess data

# set up ----
library(tidyverse)
library(multilex)
library(here)

# import data ----
ml_connect("gonzalo.garciadecastro@upf.edu")
p <- ml_participants()
r <- ml_responses(p, longitudinal = "first", update = TRUE)
l <- ml_logs(p, r)

# items ----
items <- multilex::pool %>%
    drop_na(cognate, ipa) %>% 
    filter(
        include,
        class %in% c("Noun", "Verb", "Adjective")
    ) %>% 
    rename(frequency = frequency_zipf) %>%  
    select(te, language, category, class, label, ipa, item, ipa, frequency_million, frequency, cognate) %>% 
    mutate(
        language = str_to_sentence(language),
        cognate = ifelse(cognate, "Cognate", "Non-Cognate")
    ) %>% 
    relocate(te, language, item, cognate, frequency)

saveRDS(items, here("Data", "items.rds"))

# participants ----
participants <- l %>%
    rename(dominant_language = dominance) %>% 
    filter(
        completed,
        lp %in% c("Monolingual", "Bilingual"),
        between(age, 10, 36),
        sum(doe_catalan+doe_spanish)>0.1
    ) %>% 
    mutate_at(vars(matches("doe")), function(x) x*0.01) %>% 
    filter(
        between(doe_spanish, 0, 1),
        between(doe_catalan, 0, 1),
        between(doe_others, 0, 1)
    ) %>% 
    mutate(doe = ifelse(dominant_language=="Catalan", doe_catalan, doe_spanish)) %>% 
    select(id, time_stamp, age, lp, dominant_language, doe_catalan, doe_spanish)

saveRDS(participants, here("Data", "participants.rds"))

# responses ----
responses <- expand_grid(
    id = participants$id,
    item = items$item
) %>%
    left_join(
        participants %>% 
            pivot_longer(
                c(doe_catalan, doe_spanish),
                names_to = "language",
                values_to = "doe"
            ) %>% 
            mutate(language = str_to_sentence(str_remove(language, "doe_"))) %>% 
            select(id, time_stamp, age, lp, dominant_language, doe)
    ) %>% 
    left_join(items) %>% 
    left_join(select(r, id, item, response))  %>% 
    drop_na() %>%
    distinct(id, te, age, item, .keep_all = TRUE) %>% 
    mutate(
        doe = scale(doe, scale = FALSE)[,1],
        age = scale(age, scale = FALSE)[,1],
        understands = response > 1,
        produces = response > 2,
        dominance = ifelse(dominant_language==language, "L1", "L2")
    ) %>% 
    mutate_at(vars(cognate, lp), as.factor) %>%
    mutate(frequency_center = scale(frequency)[,1]) %>% 
    select(id, age, lp, te, language, item, frequency, frequency_center, doe, cognate, understands, produces) %>% 
    arrange(id, te)

saveRDS(responses, here("Data", "responses.rds"))

