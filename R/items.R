#' Get item data
#'
#' @param bvq_data A named list resulting from calling \code{get_bvq}
#' @param childes A dataframe with lexical frequencies extracted from CHILDES, as returned by the \code{get_childes_frequencies}
#' @param class A character vector indicating the word classes to be included in the resulting dataset. Takes "Adjective", "Noun" and/or "Verb" as values.
get_items <- function(bvq_data, childes, class = "Noun") {
    classes_available <- c("Noun", "Verb", "Adjective")
    if (!(class %in% classes_available)) {
        cli_abort("class must be one of {classes_available}")
    }
    classes <- class
    
    # find TEs that have one word-form in each language
    duplicated_te <- bvq_data$pool$te[duplicated(bvq_data$pool$te)]
    
    pool_tmp <- bvq_data$pool |>
        # drop items with missing observations in these variables
        drop_na(sampa, wordbank_lemma) |>
        filter(n_lemmas == 1,
               # exclude items with more than two lemmas
               !is_multiword,
               # exclude multi-word items
               include,
               # exclude problematic items (e.g., multi-word items)
               te %in% duplicated_te,
               # get only translation equivalents with at least one item in each language
               class %in% classes) |>
        add_count(te, name = "n_te") |> # get only items with one translation in each language
        filter(n_te == 2) |>
        distinct(language, te, .keep_all = TRUE)
    
    # compute normalised Levenshtein distance (see ?stringdist::`stringdist-package`)
    # number of edit operations needed to make two strings identical
    # when applied to phonological transcriptions, it provides an approximation of phonological similarity between two word-forms
    lv_similarities <- pool_tmp |>
        mutate(sampa = str_remove_all(sampa, "\\.")) |>
        pivot_wider(
            id_cols = te,
            names_from = language,
            values_from = sampa,
            names_repair = make_clean_names
        ) |>
        # make sure strings are coded as UTF-8 before computing LVs
        mutate(lv = stringsim(catalan, spanish)) |>
        distinct(te, lv)
    
    # merge datasets
    items <- pool_tmp |>
        rename(list = version) |>
        left_join(lv_similarities) |> # add LVs
        left_join(childes, by = c("childes_lemma" = "token")) |> 
        drop_na(lv, list, wordbank_lemma, freq_zipf) |>
        mutate(n_phon = nchar(sampa_flat),
               item = str_remove(item, "cat_|spa_")) |>
        select(
            te,
            meaning = wordbank_lemma,
            language,
            item,
            ipa,
            sampa = sampa_flat,
            lv,
            n_phon,
            freq = freq_zipf,
            list
        ) |>
        arrange(te)
    
    # export to data folder
    save_files(items, folder = "data")
    
    return(items)
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
