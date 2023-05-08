#' Transform months to years and months
#'
#' @param x Age in months
#' @param .sep Separator between years and months, ';' by default
months_to_years <- function(x, .sep = ";") {
    glue(floor(x %/% 12),
         floor(x %% 12),
         .sep = .sep)
}

#' Transform months to years and months
#'
#' @param x Time in days
#' @param .sep Separator between months and days, ';' by default
days_to_months <- function(x, .sep = ";") {
    glue(floor(x %/% 30),
         floor(x %% 30),
         .sep = .sep)
}

#' Rescale standardised variable
#'
#' @param x Numeric vector to be rescaled
#' @param mean Numeric value indicating the mean of the original vector
#' @param sd Numeric value indicating the standard deviation of the original vector
rescale_variable <- function(x, mean, sd) {
    (x * sd) + mean
}

#' Cut age variable into age bins
#'
#' @param x Numeric vector with ages in months
cut_age <- function(x) {
    y <- cut(x, breaks = seq(9, 35, 2), labels = seq(10, 34, 2))
    y <- as.integer(as.character(y))
    return(y)
}

#' Adjusted estimated proportion of successes from Gelman, Hill & Vehtari (2020)
#'
#' @param y Number of successes
#' @param n Number of trials
prop_adj <- function(y, n) {
    (y + 2) / (n + 4)
}

#' Standard error of the adjusted proportion of successes from Gelman, Hill & Vehtari (2020)
#'
#' @param y Number of successes
#' @param n Number of trials
prop_adj_se <- function(y, n) {
    prop <- prop_adj(y, n)
    sqrt(prop * (1 - prop) / (n + 4))
}

#' Confidence interval of the adjusted proportion of successes from Gelman, Hill & Vehtari (2020)
#'
#' @param y Number of successes
#' @param n Number of trials
#' @param .width Width of the confidence interval (0.95 by default)
prop_adj_ci <- function(y, n, .width = 0.95, limit) {
    prop <- (y + 2) / (n + 4)
    se <- sqrt(prop * (1 - prop) / (n + 4))
    ci <- prop + qnorm(c((1 - .width) / 2, (1 - (1 - .width) / 2))) * se
    ci[1] <- ifelse(ci[1] < 0, 0, ci[1]) # truncate at 0
    ci[2] <- ifelse(ci[2] > 1, 1, ci[2]) # truncate at 1
    
    if (limit == ".lower")
        return(ci[1])
    if (limit == ".upper")
        return(ci[2])
}

#' Empirical age of acquisition
#'
#' @param preds Dataframe with the posteiror predictions of the model
#' @param ... Additional variables to group by
get_aoa <- function(preds, ...) {
    aoas <- preds |>
        group_by(..., .draw) |>
        summarise(aoa = age[which.min(abs(.epred - 0.5))],
                  .epred = .epred[which.min(abs(.epred - 0.5))],
                  .groups = "drop") |>
        filter(.category != "No") |>
        select(-.epred) |>
        distinct(..., aoa)
    
    return(aoas)
    
}


#' Get CHILDES lexical frequencies
#'
#' @param collection CHILDES corpora from where to fetch transcriptions. Takes "Eng-NA" (North American English by default). See \href{https://childes.talkbank.org/access/}{CHILDES Index to corpora} to see options
#' @param age_range Numeric vector of lenght two indicating the minimum and maximum age range of interest for which to comoute lexical frequencies in the CHILDES corpora. Frequencies will be summarised across this age range using the mean
#' @paran ... Additional arguments passed to \code{childesr::get_types}
get_childes_frequencies <- function(collection = "Eng-NA",
                                    age_range = c(10, 36),
                                    ...) {
    
    suppressMessages({
        
        roles <- c(
            "Mother",
            "Father",
            "Investigator",
            "Sibling",
            "Sister",
            "Grandmother",
            "Adult",
            "Friend",
            "Brother",
            "Visitor",
            "Relative",
            "Grandfather",
            "Teacher",
            "Student"
        )
        
        counts <- get_types(collection = collection, role = roles, ...)
        
        speaker_ids <- distinct(counts,
                                collection_id,
                                corpus_id,
                                transcript_id,
                                speaker_id)
        
        speakers <- speaker_ids |>
            left_join(
                get_speaker_statistics(collection = collection),
                by = c("collection_id",
                       "corpus_id", 
                       "speaker_id", 
                       "transcript_id")) |>
            select(collection_id,
                   corpus_id,
                   transcript_id,
                   speaker_id,
                   num_tokens)
        
        childes <- counts |>
            left_join(speakers,
                      by = c("collection_id", 
                             "corpus_id",
                             "speaker_id", 
                             "transcript_id")) |>
            mutate(
                id = as.character(id),
                age_months = target_child_age,
                age_bin = as.integer(floor(age_months / 6) * 6),
                token = tolower(gloss)
            ) |>
            group_by(age_bin, token, target_child_id, transcript_id) |>
            summarise(
                transcript_count = sum(count),
                transcript_num_tokens = sum(num_tokens),
                .groups = "drop"
            ) |>
            filter(between(age_bin,
                           age_range[1],
                           age_range[2])) |>
            group_by(token) |>
            summarise(
                freq_count = mean(transcript_count),
                total_count = mean(transcript_num_tokens),
                n_children = length(unique(target_child_id)),
                .groups = "drop"
            ) |>
            mutate(
                freq_million = freq_count / total_count * 1e6,
                freq_zipf = log10(freq_million) + 3
            ) |>
            relocate(token,
                     n_children,
                     freq_count,
                     freq_million,
                     freq_zipf)
        
    })
    
    return(childes)
}

#' Generate a regular sequence of \code{n} elements in the range of a numeric vector \code{x}
#'
#' @param x Numeric vector
#' @param n Length of the vector to be generated
seq_range <- function(x, n) {
    seq(min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        length.out = n)
}

#' Transform any list column in a dataframe to collapsed character vector
#'
#' @param x A dataframe
flatten_columns <- function(x) {
    mutate_if(x,
              .predicate = is.list,
              ~ unlist(map(., ~ paste0(., collapse = ", "))))
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
    cli_alert_success("Saved to {.path {folder}}")
}

remove_nul <- function() {
    paths <- c("manuscript", "docs")
    cur_path <- gsub("/", "\\\\", getwd())
    nul_path <- glue("{cur_path}\\{paths}\\NUL.")
    file.exists(nul_path)
    cmd1 <- glue("rename \\\\.\\{nul_path} delete.txt")
    cmd2 <- glue("del \\\\.\\{nul_path}\\delete.txt")
    lapply(cmd1, shell)
    lapply(cmd2, shell)
    shell(cmd2)
}
