#' Save an R object as data or results as CSV, or RDS files
#'
#' @param x A tabular R object
#' @param folder Folder in which to write the file. If the resulting path does not exist, a new directory will be generated.
#' @param formats Formats in which to write the files. Must be at least one of 'csv', or 'rds'
#' @param .sep Path separator, takes "/" by default
save_files <- function(x,
                       folder,
                       file_name = deparse(substitute(x)),
                       formats = c("csv", "rds"),
                       .sep = "/") 
{
    # check arguments
    if (!all(formats %in% c("csv", "rds"))) {
        cli_abort("formats must be 'csv' or 'rds'")
    }
    
    # create directories if missing
    if (length(formats) > 1) {
        dirs <- glue("{folder}{.sep}{formats}")
        dirs_exist <- dir.exists(dirs)
        if (any(!dirs_exist)) {
            missing_dir <-
                glue("{folder}{.sep}{formats[which(!dirs_exist)]}{.sep}")
            invisible(map(missing_dir, dir.create))
            cli_alert_warning("Created {.path {missing_dir}}")
            file_paths <-
                glue("{folder}{.sep}{formats}{.sep}{file_name}.{formats}")
        }
    } else {
        file_paths <- glue("{folder}{.sep}{file_name}.{formats}")
    }
    
    # save files
    write_parquet(flatten_columns(x), file_paths[grepl(".csv", file_paths)])
    saveRDS(x, file_paths[grepl(".rds", file_paths)])
    if (interactive()) cli_alert_success("Saved to {.path {folder}}")
}

#' Transform any list column in a data frame to collapsed character vector
#'
#' @param x A data frame
flatten_columns <- function(x) {
    mutate(x, 
           across(
               where(is.list),
               function(x) {
                   out <- map(x, function(x) {
                       paste0(x, collapse = ", ")
                   })
                   unlist(out)
               }))
}


