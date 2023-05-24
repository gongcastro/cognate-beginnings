#' Get BVQ data
#' @param ... Arguments to be passed to [bvq::bvq_responses()]
#' @returns A named list of data frames containing questionnaire responses, participant data, and item data from BVQ
get_bvq <- function(...) {
    # get bvq participant data
    p <- bvq_participants()
    # get bvq questionnaire responses
    r <- bvq_responses(p, ...)
    
    # merge participant data with questionnaire responses
    edu_levels <- c(
        "noeducation",
        "primary",
        "secondary",
        "complementary",
        "vocational",
        "university"
    )
    edu_labels <- c(
        "No education",
        "Primary",
        "Secondary",
        "Complementary",
        "Vocational",
        "University"
    )
    
    l <- bvq_logs(p, r) |>
        mutate(id_bvq = id) |> 
        mutate(
            across(starts_with("edu_"), ~ as.numeric(
                factor(.x, levels = edu_levels, ordered = TRUE)
            )),
            # get maximum educational attainment of parents
            edu_parent = apply(cbind(edu_parent1, edu_parent2), 1, max, na.rm = FALSE),
            # recode it as factor
            edu_parent = factor(edu_parent, levels = 1:6, labels = edu_labels)
        ) |>
        select(
            id,
            id_bvq,
            time,
            time_stamp,
            age,
            lp,
            dominance,
            version,
            completed,
            doe_catalan,
            doe_spanish,
            doe_others,
            edu_parent
        )
    
    pool <- bvqdev::pool |>
        mutate(sampa_flat = flatten_sampa(sampa))
    
    # get list of all relevant datasets
    bvq_data <- list(
        participants = p,
        responses = r,
        logs = l,
        pool = pool
    )
    
    return(bvq_data)
}
