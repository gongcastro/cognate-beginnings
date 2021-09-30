# responses

get_responses <- function(
    participants,
    items,
    multilex_data
){
    responses <- expand_grid(
        id = participants$id,
        item = items$item
    ) %>% 
        left_join(select(items, te, item, language, frequency, cognate)) %>% 
        left_join(
            participants %>% 
                pivot_longer(c(doe_catalan, doe_spanish), names_to = "language", values_to = "doe") %>% 
                mutate(language = str_to_sentence(str_remove(language, "doe_"))) %>% 
                select(id, time_stamp, age, language, doe)
        ) %>% 
        left_join(select(multilex_data$responses, id, time, item, response)) %>% 
        drop_na() %>% 
        distinct(id, te, age, item, .keep_all = TRUE) %>% 
        mutate(
            age_center = scale(age, scale = FALSE)[,1],
            frequency_center = scale(frequency)[,1],
            doe = doe*10,
            doe_center = scale(doe)[,1],
            understands = response>1,
            produces = response>2
        ) %>% 
        mutate_at(vars(cognate), as.factor) %>%
        select(id, time, age, age_center, te, language, item, frequency, frequency_center, doe, doe_center, cognate, understands, produces) %>% 
        arrange(id, time, te)
    
    # contrasts
    contrasts(responses$cognate) <- cbind(c(0.25, -0.5, 0.25), c(0.5, 0, -0.5))

    return(responses)
    
}
