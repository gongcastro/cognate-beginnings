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
r <- ml_responses(p)  
l <- ml_logs(p, r)
v <- ml_vocabulary(p, r)

# items ------------------------------------------------------------------------
items <- multilex::pool %>%
    drop_na(cognate, ipa) %>% 
    filter(
        include,
        class %in% c("Noun")
    ) %>% 
    rename(frequency = frequency_zipf) %>% 
    select(te, language, category, item, ipa, frequency, cognate) %>% 
    mutate(
        language = str_to_sentence(language),
        cognate = ifelse(cognate, "Cognate", "Non-Cognate")
    ) %>% 
    relocate(te, language, item, ipa, cognate, frequency)

write_csv(items, here("Data", "items.csv"))

# participants -----------------------------------------------------------------
participants <- l %>% 
    filter(
        completed,
        !(version %in% c("DevLex", "CBC")),
        lp %in% c("Monolingual", "Bilingual"),
        between(age, 10, 40),
    ) %>% 
    group_by(id) %>% 
    filter(time==max(time)) %>% 
    ungroup() %>% 
    mutate(doe = case_when(
        dominance=="Catalan" ~ doe_spanish,
        dominance=="Spanish" ~ doe_catalan
    )) %>% 
    mutate_at(vars(matches("doe")), function(x) x*0.01) %>% 
    select(id, time_stamp, time, age, sex, dominance, doe)

write_csv(participants, here("Data", "participants.csv"))

# vocabulary sizes -------------------------------------------------------------
vocabulary <- participants %>%
    left_join(v, by = c("id", "time")) %>% 
    mutate(
        id = as.numeric(as.factor(id)),
        vocab_type = str_to_sentence(vocab_type)
    )

write_csv(vocabulary, here("Data", "vocabulary.csv"))

# responses --------------------------------------------------------------------
responses <- expand_grid(
    id = participants$id,
    item = items$item
) %>%
    left_join(participants, by = "id") %>% 
    left_join(items, by = "item") %>% 
    left_join(select(r, id, item, response), by = c("id", "item")) %>% 
    select(id, age, dominance, doe, te, language, item, ipa, cognate, frequency, response) %>% 
    drop_na() %>%
    distinct(id, te, age, item, .keep_all = TRUE) %>% 
    mutate(
        item_dominance = ifelse(dominance==language, "L1", "L2"),
        doe = round(doe, 1)*10,
        understands = response %in% c(2, 3),
        produces = response %in% 3
    ) %>% 
    arrange(te) %>% 
    select(age, doe, te, frequency, item_dominance, cognate, response) %>% 
    drop_na() 

write_csv(responses, here("Data", "responses.csv"))

