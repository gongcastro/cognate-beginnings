# items

get_items <- function(
    multilex_data,
    cognate_codings,
    impute = TRUE
){
    
    cognate_codings <- cognate_codings %>% 
        select(te, levenshtein, n_characters, levenshtein_norm, cognate_gonzalo)
    
    items <- multilex_data$pool %>%
        select(-cognate) %>% 
        left_join(cognate_codings) %>% 
        filter(
            include,
            class %in% c("Noun", "Verb", "Adjective")
        ) %>% 
        rename(
            frequency = frequency_zipf,
            cognate = cognate_gonzalo
        ) %>%  
        select(
            te, language, category, class, label, ipa, item, ipa, frequency_million,
            frequency, cognate, levenshtein, levenshtein_norm
        ) %>% 
        mutate(
            language = str_to_sentence(language),
            cognate = case_when(
                cognate %in% c(0, 1) ~ "Non-cognate",
                cognate %in% c(2) ~ "Non-identical cognate",
                cognate %in% c(3, 4) ~ "Identical cognate"
            )
        ) %>% 
        relocate(te, language, item, cognate, frequency) %>% 
        drop_na(cognate, ipa)
    
    if (impute){
        items <- items %>% 
            mice() %>% 
            complete() %>% 
            as_tibble()
    }
    
    return(items)
    
}