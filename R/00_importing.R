
#' Get Multilex credentials
#' @examples ml_connect()
get_credentials <- function(){
    ml_connect()
}

#' Get multilex data
#' @param update A logical value. Should Multilex data be updated and new questionnaire responses be fetched?
#' @param type A character vector indicating for what type of measure should vocabulary and prevalence data be computed? It takes "understands" and/or "produces"
#' @returns A named list of data frames containing questionnaire responses, participant data, item data, prevalence data and vocabualary data
get_multilex <- function(
        update = FALSE,
        type = c("understands", "produces")
){
    get_credentials() # see ?get_credentials
    
    # get multilex participant data
    p <- ml_participants() 
    # get multilex questionnaire responses
    r <- ml_responses(p, update = update)
    # merge participant data with questionnaire responses
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
    
    # get multilex vocabulary data
    v <- ml_vocabulary(
        p, 
        r, 
        scale = c("prop", "count"),
        by = "id_exp"
    ) %>% 
        filter(type %in% type)
    
    # get list of all relevant datasets
    m <- list(
        participants = p,
        responses = r, 
        logs = l,
        vocabulary = v,
        pool = multilex::pool
    )
    
    return(m)
}
