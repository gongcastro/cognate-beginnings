
# log into multilex database ----
get_credentials <- function(){
    ml_connect()
}

# get multilex data ----
get_multilex <- function(
        update = FALSE,
        type = c("understands", "produces")
){
    get_credentials()
    
    p <- ml_participants()
    r <- ml_responses(p, update = update)
    l <- ml_logs(p, r) %>% 
        mutate_at(
            vars(starts_with("edu_")),
            function(x){
                factor(
                    x,
                    levels = c(
                        "noeducation",
                        "primary", 
                        "secondary",
                        "complementary", 
                        "vocational",
                        "university"
                    ),
                    ordered = TRUE
                ) %>% 
                    as.numeric()
            }
        ) %>% 
        mutate(
            # get maximum educational attainment of parents
            edu_parent = apply(
                cbind(edu_parent1, edu_parent2),
                1,
                max, 
                na.rm = FALSE
            ),
            # recode it as factor
            edu_parent = factor(
                edu_parent,
                levels = 1:6,
                labels = c(
                    "No education",
                    "Primary", 
                    "Secondary",
                    "Complementary",
                    "Vocational", 
                    "University"
                )
            )
        ) %>% 
        select(id, time, time_stamp, age, lp, dominance, version,
               completed, doe_catalan, doe_spanish, doe_others, edu_parent)
    
    v <- ml_vocabulary(
        p, 
        r, 
        scale = c("prop", "count"),
        by = "id_exp"
    ) %>% 
        filter(type %in% type)
    
    m <- list(
        participants = p,
        responses = r, 
        logs = l,
        vocabulary = v,
        pool = multilex::pool
    )
    
    return(m)
}
