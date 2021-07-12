# set up -----

# load packages
library(multilex)
library(janitor)
library(tidyverse)

# load helper functions
source("R/utils.R")

# import data ----
ml_connect("gonzalo.garciadecastro@upf.edu")
p <- ml_participants()
r <- ml_responses(p, longitudinal = "first", update = FALSE) %>% 
    mutate(postcode = as.integer(postcode)) %>% 
    bind_rows(multilex:::devlex, multilex:::cbc, multilex:::formr1, multilex:::formr_short)
l <- ml_logs(p, r)
v <- ml_vocabulary(p, r, by = c("dominance", "language"), scale = "prop")

age_bin_labels <- c("10-12", "12-14", "14-16", "16-18", "18-20", "20-22",
                    "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36")
doe_bin_labels <- paste0("DOE: ", c("0-25%", "25-50%", "50%-75%", "75-100%"))

# items ----
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
    relocate(te, language, item, cognate, frequency)

# participants ----
participants <- l %>%
    rename(dominant_language = dominance) %>% 
    filter(
        completed,
        lp %in% c("Monolingual", "Bilingual"),
        between(age, 10, 36)
    ) %>% 
    pivot_longer(c(doe_catalan, doe_spanish), names_to = "language", values_to = "doe") %>% 
    mutate_at(vars(matches("doe")), function(x) x*0.01) %>% 
    filter(between(doe, 0, 1)) %>% 
    mutate(
        language = str_to_sentence(str_remove(language, "doe_")),
        doe_bin = cut_interval(doe, length = 0.25, labels = doe_bin_labels),
        age_bin = cut_interval(age, length = 2, labels = age_bin_labels)
    ) %>% 
    select(id, time_stamp, age, age_bin, lp, doe, doe_bin)

# responses ----
processed <- expand_grid(
    id = participants$id,
    item = items$item
) %>%
    left_join(participants) %>% 
    left_join(items) %>% 
    left_join(select(r, id, item, response))  %>% 
    drop_na() %>%
    distinct(id, te, age, age_bin, item, .keep_all = TRUE) %>% 
    mutate(
        understands = response %in% c(2, 3),
        produces = response %in% 3
    ) %>% 
    mutate_at(vars(cognate), as.factor) %>% 
    select(id, age_bin, age, language, doe, doe_bin, te, frequency, cognate, understands, produces) %>% 
    arrange(id, te, age_bin)

# aggregate data  ----
proportions <- processed %>% 
    mutate(
        age_bin = as.numeric(age_bin),
        doe_bin = as.numeric(doe_bin),
        frequency = scale(frequency)[,1]
    ) %>% 
    group_by(te, frequency, age_bin, doe_bin, cognate) %>% 
    summarise(
        n = n(),
        understands = mean(understands, na.rm = TRUE),
        produces = mean(produces,na.rm = TRUE),
        .groups = "drop"
    ) %>% 
    mutate_at(
        vars(understands, produces),
        function(x) {
            case_when(
                x==0 ~ 0.001,
                x==1 ~ 0.999,
                TRUE ~ x
            )
        }
    ) 

# export data
saveRDS(items, "Data/items.rds")
saveRDS(participants, "Data/participants.rds")
saveRDS(v, "Data/vocabulary.rds")
saveRDS(processed, "Data/processed.rds")
saveRDS(proportions, "Data/proportions.rds")
write_csv(proportions, "Data/proportions.csv")

