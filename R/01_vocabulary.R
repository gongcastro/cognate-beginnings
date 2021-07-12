# vocabulary sizes

# preprocess data

# set up ----
library(tidyverse)
library(multilex)
library(here)

# import data ----
ml_connect("gonzalo.garciadecastro@upf.edu")
p <- ml_participants()
r <- ml_responses(p, longitudinal = "first", update = FALSE)
l <- ml_logs(p, r)
v <- ml_vocabulary(p, r, scale = c("prop", "count"))
pool <- select(multilex::pool, item, te, version_pool = version)

# process data ----
d <- left_join(l, v) %>%
    filter(completed, lp != "Other", between(age, 10, 40)) %>% 
    distinct(id, time, type, .keep_all = TRUE) %>% 
    select(id, time, time_stamp, version, age, lp, type, starts_with("vocab_")) %>% 
    mutate(
        version = str_remove(version, "BL-"),
        version = ifelse(str_detect(version, "Long"), "Long", str_replace(version, "Lockdown", "Short"))
    ) %>% 
    rename_at(vars(starts_with("vocab_")), str_remove_all, "vocab_|dominance_")  %>% 
    pivot_longer(
        starts_with("prop_") | starts_with("count_"),
        names_to = c("scale", "modality"), 
        names_sep = "_*?(_)"
    ) %>% 
    pivot_wider(names_from = scale, values_from = value) %>% 
    mutate_at(vars(type, modality), str_to_sentence)

# validate long version
long <- r %>% 
    select(id, age, time, item, language, dominance, response) %>% 
    left_join(select(l, id, lp, time, version, completed)) %>%
    filter(completed, lp != "Other", between(age, 10, 40)) %>%
    left_join(pool) %>% 
    filter(str_detect(version, "Long")) %>% 
    mutate( 
        understands = response > 1,
        produces = response > 2,
        item_dominance = ifelse(language==dominance, "L1", "L2")
    ) %>% 
    select(-version) %>% 
    rename(version = version_pool) %>% 
    unnest(cols = version) %>% 
    group_by(id, time, age, version) %>% 
    summarise(
        understands_total = mean(understands, na.rm = TRUE),
        understands_l1 = mean(understands[item_dominance=="L1"], na.rm = TRUE),
        understands_l2 = mean(understands[item_dominance=="L2"], na.rm = TRUE),
        produces_total = mean(produces, na.rm = TRUE),
        produces_l1 = mean(produces[item_dominance=="L1"], na.rm = TRUE),
        produces_l2 = mean(produces[item_dominance=="L2"], na.rm = TRUE),
        .groups = "drop"
    )  %>% 
    pivot_longer(
        starts_with("understands_") | starts_with("produces_"),
        names_to = c("type", "scale"), 
        names_sep = "_"
    ) %>% 
    mutate_at(vars(version, scale), str_to_sentence)

# export ----
saveRDS(long, here("Data", "vocabulary_long.rds"))
saveRDS(d, here("Data", "vocabulary.rds"))

