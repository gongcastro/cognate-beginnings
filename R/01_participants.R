# participants

get_participants <- function(
    multilex_data,
    age_range = c(10, 36),
    lps = c("Monolingual", "Bilingual", "Other"),
    other_threshold = 0.1
){
    
    participants <- multilex_data$logs %>%
        rename(dominant_language = dominance) %>% 
        filter(
            completed,
            lp %in% lps,
            between(age, age_range[1], age_range[2]),
            sum(doe_catalan + doe_spanish) > other_threshold
        ) %>% 
        filter(
            between(doe_spanish, 0, 1),
            between(doe_catalan, 0, 1),
            between(doe_others, 0, 1)
        ) %>% 
        mutate(doe = ifelse(dominant_language=="Catalan", doe_catalan, doe_spanish)) %>% 
        select(id, time, time_stamp, version, age, lp, dominant_language, doe_catalan, doe_spanish, edu_parent, completed)
    
    return(participants)
}