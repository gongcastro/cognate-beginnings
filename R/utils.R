#' Resolve NAMESPACE conflicts
#'
resolve_conflicts <- function() {
    suppressMessages({
        conflicted::conflict_prefer("last_warnings", "rlang")
        conflicted::conflict_prefer("filter", "dplyr")
        conflicted::conflict_prefer("lag", "dplyr")
        conflicted::conflict_prefer("between", "dplyr")
        conflicted::conflict_prefer("timestamp", "utils")
        conflicted::conflict_prefer("ar", "brms")
        conflicted::conflict_prefer("chisq.test", "stats")
        conflicted::conflict_prefer("discard", "scales")
        conflicted::conflict_prefer("duration", "lubridate")
        conflicted::conflict_prefer("fisher.test", "stats")
        conflicted::conflict_prefer("lag", "dplyr")
    })
    
    if (interactive()) cli_alert_success("Resolved conflicts")
}

#' Save an R object as data or results as Arrow Parquet, CSV, or RDS files
#'
#' @param x A tabular R object
#' @param folder Folder in which to write the file. If the resulting path does not exist, a new directory will be generated.
#' @param formats Formats in which to write the files. Must be at least one of 'parquet', 'csv', or 'rds'
#' @param .sep Path separator, takes '/' by default
save_files <- function(x,
                       folder,
                       file_name = deparse(substitute(x)),
                       formats = c("parquet", "csv", "rds"),
                       .sep = "/") {
    # check arguments
    if (!all(formats %in% c("parquet", "csv", "rds"))) {
        cli_abort("formats must be 'parquet', 'csv' or 'rds'")
    }
    
    # create directories if missing
    dirs <- glue("{folder}{.sep}{formats}")
    dirs_exist <- dir.exists(dirs)
    if (any(!dirs_exist)) {
        missing_dir <-
            glue("{folder}{.sep}{formats[which(!dirs_exist)]}{.sep}")
        invisible(map(missing_dir, dir.create))
        cli_alert_warning("Created {.path {missing_dir}}")
    }
    
    # save files
    file_paths <-
        glue("{folder}{.sep}{formats}{.sep}{file_name}.{formats}")
    write_csv_arrow(flatten_columns(x), file_paths[grepl(".parquet", file_paths)])
    write_parquet(flatten_columns(x), file_paths[grepl(".csv", file_paths)])
    saveRDS(x, file_paths[grepl(".rds", file_paths)])
    if (interactive()) cli_alert_success("Saved to {.path {folder}}")
}

#' Transform any list column in a data frame to collapsed character vector
#'
#' @param x A dataframe
flatten_columns <- function(x) {
    mutate_if(x,
              .predicate = is.list,
              ~ unlist(map(., ~ paste0(., collapse = ", "))))
}


