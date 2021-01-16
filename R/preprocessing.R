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
r <- ml_responses(p, longitudinal = "first")  
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
    rename(dominant_language = dominance) %>% 
    filter(
        completed,
        !(version %in% c("DevLex", "CBC")),
        lp %in% c("Monolingual", "Bilingual"),
        between(age, 10, 40),
    ) %>% 
    mutate(bilingualism = case_when(
        dominant_language=="Catalan" ~ doe_spanish,
        dominant_language=="Spanish" ~ doe_catalan
    )) %>% 
    mutate_at(vars(matches("doe")), function(x) x*0.01) %>% 
    select(id, time_stamp, time, age, sex, dominant_language, bilingualism)

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
    select(id, age, dominant_language, bilingualism, te, language, item, ipa, cognate, frequency, response) %>% 
    drop_na() %>%
    distinct(id, te, age, item, .keep_all = TRUE) %>% 
    mutate(
        dominance = ifelse(dominant_language==language, "L1", "L2"),
        bilingualism = round(bilingualism/10),
        age = cut_width(age, 2, labels = FALSE),
        frequency = cut_quantiles(frequency),
        understands = response %in% c(2, 3),
        produces = response %in% 3
    ) %>% 
    select(age, bilingualism, te, frequency, dominance, cognate, understands, produces) %>% 
    group_by(te, age, bilingualism, frequency, dominance, cognate) %>% 
    summarise(
        understands = sum(understands, na.rm = TRUE),
        produces = sum(produces, na.rm = TRUE),
        n = n(),
        .groups = "drop"
    ) %>% 
    arrange(te, age, bilingualism)
    
write_csv(responses, here("Data", "responses.csv"))

