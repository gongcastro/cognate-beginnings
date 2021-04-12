#### preprocessing -------------------------------------------------------------

# set up -----------------------------------------------------------------------

# load packages
library(multilex)
library(janitor)
library(tidyverse)
library(here)

# load helper functions
source(here("R", "utils.R"))

# import data ------------------------------------------------------------------
ml_connect("gonzalo.garciadecastro@upf.edu")
p <- ml_participants()
r <- ml_responses(p, longitudinal = "first", update = FALSE)  
l <- ml_logs(p, r)
v <- ml_vocabulary(p, r, by = c("dominance", "language"), scale = "prop")

# items ------------------------------------------------------------------------
items <- multilex::pool %>%
    drop_na(cognate, ipa) %>% 
    filter(
        include,
        class %in% c("Noun", "Verb", "Adjective")
    ) %>% 
    rename(frequency = frequency_zipf) %>% 
    select(te, language, category, item, ipa, frequency, cognate) %>% 
    mutate(
        language = str_to_sentence(language),
        cognate = ifelse(cognate, "Cognate", "Non-Cognate")
    ) %>% 
    relocate(te, language, item, ipa, cognate, frequency)

# participants -----------------------------------------------------------------
participants <- l %>%
    rename(dominant_language = dominance) %>% 
    filter(
        completed,
        !(version %in% c("DevLex", "CBC")),
        lp %in% c("Monolingual", "Bilingual"),
        between(age, 12, 36),
    ) %>% 
    pivot_longer(c(doe_catalan, doe_spanish), names_to = "language", values_to = "doe") %>% 
    mutate(language = str_remove(language, "doe_") %>% str_to_sentence()) %>% 
    select(id, time_stamp, time, age, sex, dominant_language, language, doe) %>% 
    mutate_at(vars(matches("doe")), function(x) x*0.01)  

# responses --------------------------------------------------------------------
responses <- expand_grid(
    id = participants$id,
    item = items$item
) %>%
    left_join(participants) %>% 
    left_join(items) %>% 
    left_join(select(r, id, item, response)) %>% 
    select(id, age, dominant_language, doe, te, language, item, ipa, cognate, frequency, response) %>% 
    drop_na() %>%
    distinct(id, te, age, item, .keep_all = TRUE) %>% 
    mutate(
        age = age,
        response = response-1,
        response = factor(
            response,
            levels = c(0, 1, 2),
            labels = c("No", "Understands", "Understands & Says"),
            ordered = TRUE)
    ) %>% 
    mutate_at(vars(cognate), as.factor) %>% 
    select(id, age, doe, te, frequency, cognate, response) %>% 
    arrange(id, te, age, doe)

# export data
saveRDS(items, here("Data", "items.rds"))
saveRDS(participants, here("Data", "participants.rds"))
saveRDS(v, here("Data", "vocabulary.rds"))
saveRDS(responses, here("Data", "responses.rds"))
write_csv(responses, here("Data", "responses.csv"))

