# vocabulary

get_vocabulary <- function(
    participants,
    multilex_data,
    type = c("understands", "produces")
){
    
    pool <- select(multilex_data$pool, item, te, version_pool = version)
    
    # wide dataset
    wide <- left_join(participants, multilex_data$vocabulary) %>%
        distinct(id, time, type, .keep_all = TRUE) %>% 
        select(id, time, time_stamp, version, age, lp, type, starts_with("vocab_")) %>% 
        mutate(
            version = str_remove(version, "BL-"),
            version = ifelse(
                str_detect(version, "Long"),
                "Long",
                str_replace(version, "Lockdown", "Short")
            )
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
    long <- multilex_data$responses %>% 
        select(id, age, time, item, language, dominance, response) %>% 
        left_join(select(participants, id, lp, time, version, completed)) %>%
        filter(completed, lp != "Other", between(age, 10, 40)) %>%
        left_join(pool) %>% 
        filter(str_detect(version, "Long")) %>% 
        mutate( 
            understands = response>1,
            produces = response>2,
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
    
    vocabulary <- list(wide = wide, long = long)
    
    return(vocabulary)
    
}
