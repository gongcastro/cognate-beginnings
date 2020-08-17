#### preprocessing #########################

#### set up ################################

# load packages
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
library(data.table)
library(mice)
library(here)

# load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")
breaks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 100)

#### participant data ######################

participants <- list.files(here("Data", "Logs"), pattern = "logs", full.names = TRUE) %>%
    .[!str_detect(., "summary")] %>%
    last() %>%
    fread(na.strings = "") %>%
    as_tibble() %>%
    mutate_at(vars(date_sent, time_stamp), as_date) %>%
    mutate_at(vars(id_db), factor) %>%
    mutate_at(vars(age_bin), factor, levels = bins, ordered = TRUE) %>%
    mutate(dominance = case_when(id_db %in% "54469" ~ "Spanish",
                                 id_db %in% "57157" ~ "Catalan",
                                 TRUE ~ dominance),
           doe_catalan = case_when(id_db %in% "54469" ~ 0,
                                   id_db %in% "57157" ~ 80,
                                   TRUE ~ as.numeric(doe_catalan))) %>% 
    mutate(version = factor(version, levels = c("CBC", "DevLex", "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D", "BL-Long-1", "BL-Long-2"), ordered = TRUE),
           doe1 = ifelse(dominance %in% "Spanish", doe_spanish, doe_catalan),
           doe2 = ifelse(dominance %in% "Spanish", doe_catalan, doe_spanish),
           lp = case_when(data.table::between(doe1, 40, 100, incbounds = TRUE) ~ lp,
                          data.table::between(doe2, 0, 50, incbounds = TRUE) ~ lp,
                          between(doe1 + doe2, 90, 100, incbounds = TRUE) ~  lp,
                          TRUE ~ "Other")) %>% 
    rowwise() %>% 
    mutate(bilingualism = ifelse(dominance %in% "Spanish",
                                 1-(abs((doe_spanish-doe_catalan)/(doe_spanish+doe_catalan))),
                                 1-(abs((doe_catalan-doe_spanish)/(doe_spanish+doe_catalan))))) %>% 
    drop_na(lp, bilingualism) %>% 
    filter(completed, age_bin %in% bins_interest) 

# export data
fwrite(participants, here("Data", "01_participants.csv"), sep = ",", dec = ".", row.names = FALSE)

#### process responses #######################
pool <- read_xlsx(here("Data","01_pool.xlsx")) %>% 
    rename(te = meaning)

responses <- fread(here("Data", "02_merged.csv"), header = TRUE, stringsAsFactors = FALSE, na.strings = "") %>%
    as_tibble() %>%
    mutate_at(vars(time_stamp), as_date) %>%
    mutate_at(vars(id_db), as.character) %>%
    mutate(dominance = case_when(id_db %in% "54469" ~ "Spanish",
                                 id_db %in% "57157" ~ "Catalan",
                                 TRUE ~ dominance),
           doe_catalan = case_when(id_db %in% "54469" ~ 0,
                                   id_db %in% "57157" ~ 80,
                                   id_db %in% "57046" ~ 50,
                                   TRUE ~ as.numeric(doe_catalan)),
           doe_spanish = case_when(id_db %in% "57046" ~ 50,
                                   TRUE ~ as.numeric(doe_spanish))) %>% 
    mutate(version = factor(version, levels = c("CBC", "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D", "BL-Long-1", "BL-Long-2", "DevLex"), ordered = TRUE)) %>%
    drop_na(response) %>%
    mutate(response = case_when(response == 1 ~ "no", response == 2 ~ "understands", response == 3 ~ "produces", TRUE ~ NA_character_),
           understands = response %in% c("understands", "produces"),
           produces = response %in% "produces",
           doe1 = ifelse(dominance %in% "Spanish", doe_spanish, doe_catalan),
           doe2 = ifelse(dominance %in% "Spanish", doe_catalan, doe_spanish),
           age_bin = factor(cut(age, breaks = breaks, labels = bins), levels = bins, ordered = TRUE)) %>%
    rowwise() %>% 
    mutate(doe_total = doe1 + doe2) %>% 
    ungroup() %>% 
    mutate(lp = case_when(!data.table::between(doe1, 40, 100, incbounds = TRUE) ~ "Other",
                          !data.table::between(doe2, 0, 50, incbounds = TRUE) ~ "Other",
                          !between(doe1 + doe2, 90, 100, incbounds = TRUE) ~  "Other",
                          TRUE ~ lp)) %>% 
    filter(lp %in% c("Monolingual", "Bilingual"),
           age_bin %in% bins_interest) %>%
    select(-response) %>% 
    pivot_longer(c(understands, produces), names_to = "type", values_to = "response") %>%
    mutate(type = ifelse(type=="understands", "Comprehensive", "Productive")) %>%
    arrange(item, lp, item_dominance, age_bin) %>%
    # aggregate data
    group_by(item, version, sex, lp, age_bin, item_dominance, type) %>%
    summarise(n = sum(!is.na(response)),
              successes = sum(response, na.rm = TRUE),
              proportion = mean(response, na.rm = TRUE),
              .groups = "drop") %>%
    ungroup() %>%
    arrange(age_bin, lp, item_dominance, age_bin) %>%
    left_join(select(pool, -version), by = "item") %>%
    rename(cognate = cognate_rater1) %>%
    mutate_at(vars(include, cognate), ~as.logical(as.numeric(.))) %>%
    filter(include) %>%
    drop_na(cognate, age_bin) %>%
    select(-c(survey, ipa, source, label, A, B, C, D, include, cognate_expert, cognate_rater2, agreement, comments)) %>%
    mutate(cognate = case_when(cognate ~ "Cognate", !cognate ~ "Non-cognate", TRUE ~ NA_character_))


#### prepare data ##########################3

dat <- responses %>%
    rowwise() %>%
    mutate(prop = prod(successes, 1/n, na.rm = TRUE))  %>%
    filter(class %in% c("noun", "verb"),
           !(type=="Productive" & version=="DevLex")) %>%  
    ungroup() %>% 
    mice(data = ., method = "norm", m = 5, maxit = 5,  seed = 888) %>%
    complete() %>%
    as_tibble() %>%
    ungroup() %>%
    filter(lp=="Bilingual") %>%
    select(item, version, type, te, category, age_bin, item_dominance, cognate, n, successes, proportion) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1) %>%
    arrange(item, te, age_bin)

fwrite(dat, here("Data", "preprocessed.csv"), sep = ",", dec = ".", row.names = FALSE)







