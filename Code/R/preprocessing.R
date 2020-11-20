#### Preprocessing: import, clean, and tidy data #########################

#### set up ##############################################################

# load packages
library(tidyverse)
library(multilex)
library(data.table)
library(janitor)
library(here)

# set params
set.seed(888)
bins <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- bins[-c(1, length(bins))]
breaks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 100)

#### import data ######################
participants_raw <- ml_participants("gonzalo.garciadecastro@upf.edu")
responses_raw <- ml_responses(participants = participants_raw,
                              formr_email = "gonzalo.garciadecastro@upf.edu")
logs_raw <- ml_logs(responses = responses_raw, participants = participants_raw) %>% 
    filter(age_bin %in% bins_interest,
           lp %in% c("Monolingual", "Bilingual")) 

#### process responses #####################################################
responses <- responses_raw %>% 
    select(id, time, item, response, language) %>% 
    left_join(logs_raw, by = c("id", "time")) %>% 
    drop_na(response) %>% 
    mutate(age_bin = factor(cut(age, breaks = breaks, labels = bins), levels = bins, ordered = TRUE),
           item_dominance = ifelse(language==dominance, "L1", "L2"),
           bilingualism = ifelse(dominance=="Catalan", doe_spanish, doe_catalan)) %>% 
    select(id, age_bin, age, sex, dominance, bilingualism, time, item, language, item_dominance, response) %>% 
    left_join(pool, by = c("item", "language")) %>% 
    select(id, time, age_bin, age, sex, dominance, bilingualism, time, te, item, cognate, language, item_dominance, response) %>% 
    mutate(cognate = case_when(cognate ~ "Cognate",
                               !cognate ~ "Non-cognate",
                               TRUE ~ NA_character_))

fwrite(responses, here("Data", "responses.csv"), sep = ",", dec = ".", row.names = FALSE)

#### vocabulary sizes ###########################################################################

vocabulary <- responses %>% 
    mutate(response = case_when(response == 1 ~ "no",
                                response == 2 ~ "understands",
                                response == 3 ~ "produces",
                                TRUE ~ NA_character_),
           understands = response %in% c("understands", "produces"),
           produces = response %in% "produces") %>% 
    filter(lp %in% c("Monolingual", "Bilingual"),
           age_bin %in% bins_interest) %>%
    select(-response) %>% 
    pivot_longer(c(understands, produces), names_to = "type", values_to = "response") %>%
    mutate(type = ifelse(type=="understands", "Comprehensive", "Productive")) %>%
    select(id, time, age, age_bin, sex, language, item_dominance, dominance, lp, type, response) %>% 
    filter(sex %in% c("Female", "Male")) %>% 
    drop_na(response) %>% 
    group_by(id, time, age, sex, type, language, lp, item_dominance) %>%
    summarise(yes = sum(response, na.rm = TRUE),
              n = sum(!is.na(response)),
              .groups = "drop") %>% 
    rowwise() %>% 
    mutate(vocab = yes/n) %>% 
    ungroup()

fwrite(vocabulary, here("Data", "vocabulary.csv"), sep = ",", dec = ".", row.names = FALSE)






