#' Generate dataset with posterior predictions from a brmsfit model
#'
#' @param model A brmsfit object
#' 
#' @param data Data with which the model was fist, containing the original variables
#' 
#' @param group Group for which posterior predictions will be generated:
#' * `NULL` (default): posterior predictions will be generated for fixed effect.
#' * `"te"`: posterior predictions will be generated for translation equivalents.
#' * `"id"`: posterior predictions will be generated for participants.
#' @param levels If group is `"te"` or `"id"`, specific levels (translation equivalents or participants) can be specified. If NULL (default), posterior predictions are generated for all levels of the grouping variable.
#' @inheritParams marginaleffects::datagrid
#' 
generate_newdata <- function(model, group = NULL, levels = NULL, ...) {
    
    # validate data
    is.data.provided <- length(list(...)) > 0
    if (!is.data.provided) {
        cli::cli_abort("Predictor values must be provided")
    }
    
    # validate group variable names
    grouping.vars <- names(brms::ranef(model))
    if (!is.null(group)) {
        if (!(group %in% grouping.vars) | length(group) > 1) {
            cli::cli_abort("group must be one of {grouping.vars}")
        }
    }
    
    # make sure that all levels exist in the dataset
    if (!is.null(levels)) {
        levels_in_data <- levels %in% model[["data"]][[group]]
        if (!all(levels_in_data)) {
            missing_levels <- paste0(levels[!levels_in_data], collapse = ", ")
            cli::cli_abort("Level {missing_levels} in {.field {group}} is missing")
        }
    }
    
    # data frame with prediction combinations
    if (is.null(group)) {
        nd <- expand_grid(te = NA, id = NA, ...)
    } else {
        # expand predictor levels in data frame to generate predictions
        if (group == "te") {
            # if group is "te", generate predictions for each level of `te`
            nd <- expand_grid(te = levels,  id = NA, ...) |>
                select(-lv_std) |>
                left_join(distinct(data, te, lv_std))
        }
        if (group == "id") {
            # expand predictor levels in data frame to generate predictions
            nd <- expand_grid(id = levels, te = NA, ...) |>
                select(-age_std) |>
                left_join(distinct(data, id, age_std))
        }
    }
    
    return(nd)
}



#' Generate expected posterior predictions for fixed effects brmsfit model via [tidybayes::add_epred_draws()]
#'
#' @param model A brmsfit object
#' @param ndraws Number of posterior draws to use
#' @inheritParams tidybayes::add_epred_draws
posterior_epreds <- function(model, ndraws = NULL, ...) {
    # generate data for predictions
    newdata <- generate_newdata(model, ...)
    
    # use marginaleffects to get posterior means
    predictions <- tidybayes::add_epred_draws(model,
                                              newdata = newdata,
                                              re_formula = NA,
                                              ndraws = ndraws,
                                              value = ".value") |> 
        filter(.category!="No") |> 
        pivot_wider(id_cols = any_of(c(".draw", colnames(newdata))),
                    names_from = .category,
                    values_from = .value) |> 
        mutate(`Understands` = `Understands and Says` + `Understands`) |>
        pivot_longer(c(`Understands`, `Understands and Says`),
                     names_to = ".category",
                     values_to = ".value")
    
    # save predictions as Parquet file
    save_files(predictions, 
               formats = "csv",
               folder = "results/predictions")
    
    return(predictions)
}
