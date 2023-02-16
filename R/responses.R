
#' Prepare data for analyses
#' 
#' @param bvq_data A named list resulting from calling \code{get_bvq}
#' @param items A data frame resulting from calling \code{get_items}
#' @param participants A data frame resulting from calling \code{get_participants}
get_responses <- function(bvq_data, items, participants) {
    # merge all datasets
    responses_tmp <- bvq_data$responses |>
        mutate(time = as.integer(time),
               item = str_remove(item, "cat_|spa_")) |>
        # drop missing responses
        # by default datasets are expanded so that every participant has rows for all items,
        # even for those that were not included in their version of the questionnaire
        drop_na(response) |>
        rename(id_bvq = id) |>
        inner_join(distinct(participants, id, id_bvq)) |>
        select(id_bvq, time, code, language, item, response)
    
    responses <- lst(select(items, -list),
                     responses_tmp,
                     participants) |>
        reduce(inner_join) |>
        mutate(
            # code responses as factor
            response = factor(
                response,
                levels = c(1, 2, 3),
                labels = c("No", "Understands", "Understands and Says"),
                ordered = TRUE
            ),
            # does should have the value of the corresponding language
            doe = ifelse(language == "Catalan", doe_catalan, doe_spanish),
            # standardise numeric predictors
            n_phon_std = scale(n_phon)[, 1],
            freq_std = scale(freq)[, 1],
            lv_std = scale(lv)[, 1],
            age_std = scale(age)[, 1],
            doe_std = scale(doe)[, 1],
            exposure = freq * doe,
            exposure_std = scale(exposure)[, 1],
        ) |>
        # get only relevant variables
        select(
            id,
            time,
            age,
            age_std,
            te,
            language,
            meaning,
            item,
            response,
            lv,
            lv_std,
            freq,
            freq_std,
            n_phon,
            n_phon_std,
            doe,
            doe_std,
            exposure,
            exposure_std
        ) |>
        # reorder rows
        arrange(id, te, language)
    
    # export data
    save_files(responses, folder = "data")
    
    return(responses)
    
}