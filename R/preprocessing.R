#### Preprocessing: import, clean, and tidy data ###############################

#### set up --------------------------------------------------------------------

# load packages
library(tidyverse)
library(multilex)
library(data.table)
library(here)

#### import data ---------------------------------------------------------------
participants <- ml_participants("gonzalo.garciadecastro@upf.edu")
responses <- ml_responses(participants = participants,
                          formr_email = "gonzalo.garciadecastro@upf.edu")  
logs <- ml_logs(responses = responses,
                participants = participants)

#### process responses ---------------------------------------------------------
dat <- responses %>% 
    select(id, time, item, response, language) %>% 
    left_join(logs, by = c("id", "time")) %>% 
    mutate(item_dominance = ifelse(language==dominance, "L1", "L2"),
           bilingualism = ifelse(dominance=="Catalan", doe_spanish, doe_catalan)/100) %>% 
    select(id, version, age, sex, dominance, lp, bilingualism, time, item, language, item_dominance, response, completed) %>% 
    left_join(select(pool, te, language, item, category, class, frequency, cognate, include), by = c("item", "language")) %>% 
    mutate(cognate = case_when(cognate ~ "Cognate",
                               !cognate ~ "Non-cognate",
                               TRUE ~ NA_character_)) %>% 
    filter(
        completed,
        !(version %in% c("DevLex", "CBC")),
        lp %in% c("Monolingual", "Bilingual"),
        between(age, 10, 40),
        include,
        class %in% c("Verb", "Noun", "Adjective")) %>% 
    group_by(id) %>% 
    filter(time==min(.$time)) %>% 
    ungroup() %>% 
    drop_na(response, te, cognate) %>% 
    select(id, age, sex, dominance, lp, te, item, response, language, item_dominance, cognate, frequency, bilingualism, )

fwrite(dat, here("Data", "responses.csv"), sep = ",", dec = ".", row.names = FALSE)








