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
        left_join(
            participants %>% 
                pivot_longer(
                    c(doe_catalan, doe_spanish),
                    names_to = "language",
                    values_to = "doe"
                ) %>% 
                mutate(language = str_to_sentence(str_remove(language, "doe_"))) %>% 
                select(id, time_stamp, age, lp, dominant_language, doe)
        ) %>% 
        left_join(items) %>% 
        left_join(select(multilex_data$responses, id, item, response))  %>% 
        drop_na() %>%
        distinct(id, te, age, item, .keep_all = TRUE) %>% 
        mutate(
            doe = scale(doe, scale = FALSE)[,1],
            age = scale(age, scale = FALSE)[,1],
            understands = response>1,
            produces = response>2,
            dominance = ifelse(dominant_language==language, "L1", "L2")
        ) %>% 
        mutate_at(vars(cognate, lp), as.factor) %>%
        mutate(frequency_center = scale(frequency)[,1]) %>% 
        select(id, age, lp, te, language, item, frequency, frequency_center, doe, cognate, understands, produces) %>% 
        arrange(id, te)
    
    # contrasts
    contrasts(responses$cognate) <- cbind(c(0.25, -0.5, 0.25), c(0.5, 0, -0.5))
    contrasts(responses$lp) <- c(0.5, -0.5)
    
    return(responses)
    
}
