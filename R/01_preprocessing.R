#' Get item data
#' 
#' @param bvq_data A named list resulting from calling \code{get_bvq}
#' @param class A character vector indicating the word classes to be included in the resulting dataset. Takes "Adjective", "Noun" and/or "Verb" as values.
get_items <- function(bvq_data, childes, class = "Noun") {
    
    # compute normalised Levenshtein distance (see ?stringdist::`stringdist-package`)
    # number of edit operations needed to make two strings identical
    # when applied to phonological transcriptions, it provides an approximation of phonological similarity between two word-forms
    lv_similarities <- bvq_data$pool %>% 
        mutate(language = ifelse(grepl("cat_", item), "Catalan", "Spanish")) %>% 
        pivot_wider(id_cols = te, names_from = language, values_from = sampa) %>% 
        mutate(
            # make sure strings are coded as UTF-8 before computing LVs
            lv = stringsim(Catalan, Spanish),
            lv_dist = as.integer(stringdist(Catalan, Spanish))
        ) %>% 
        select(te, lv, lv_dist)
    
    # merge datasets
    items <- bvq_data$pool %>% 
        rename(list = version) %>% 
        left_join(lv_similarities) %>% # add LVs
        # drop items with missing observations in these variables
        mutate(n_phon = nchar(sampa_flat)) %>% # number of phoneme
        # get only translation equivalents with at least one item in each language
        dplyr::filter(
            te %in% .$te[duplicated(.$te)],
            class %in% .env$class,
            include # exclude problematic items (e.g., multi-word items)
        ) %>% 
        left_join(childes, by = c("childes_lemma" = "token")) %>% 
        drop_na(lv, list, wordbank_lemma, freq_zipf) %>% 
        select(te, item, wordbank_lemma, sampa_flat, lv, lv_dist, list, n_phon, freq_zipf) %>% 
        mutate(freq_zipf)
        
    
    # export as Parquet file
    write_ipc_stream(items, here("data", "items.parquet"))
    
    return(items)
}

#' Get item responses
#' 
#' @param bvq_data A named list resulting from calling \code{get_bvq}
#' @param update A logical value. Should BVQ data be updated and new questionnaire responses be fetched?
#' @param longitudinal Should longitudinal data be included? If "all" (default), all responses (including repeated measures) are included. If "no", participants with more than one responses to the questionnaire (regardless of the version) are excluded. If "first", only the first response of each participant is included. If "last", only the last response of each participant is included. If "only", only responses with repeated measures are included.
get_responses <- function(bvq_data, update = TRUE, longitudinal = "all"){
    
    # see ?ml_responses
    responses <- bvq_responses(
        participants = bvq_data$participants,
        update = update, 
        longitudinal = longitudinal
    ) %>% 
        # get only short versions of the questionnaire
        filter(str_detect(version, "Lockdown|Short")) %>% 
        mutate(time = as.integer(time)) %>% 
        # drop missing responses
        # by default datasets are expanded so that every participant has rows for all items,
        # even for those that were not included in their version of the questionnaire
        drop_na(response) %>% 
        select(id, time, code, language, item, response)
    
    write_ipc_stream(responses, here("data", "responses.parquet"))
    
    return(responses)
    
}

#' Get participant-level data
#' 
#' @param multilex_data A named list resulting from calling \code{get_multilex}
#' @param longitudinal Should longitudinal data be included? If "all" (default), all responses (including repeated measures) are included. If "no", participants with more than one responses to the questionnaire (regardless of the version) are excluded. If "first", only the first response of each participant is included. If "last", only the last response of each participant is included. If "only", only responses with repeated measures are included.
#' @param age Numeric vector of length two indicating the minimum and maximum age of participants that will be included in the resulting dataset.
#' @param lp Character vector indicating the language profile (LP) of the participants that will be included in the resulting dataset. In takes "Monolingual", "Bilingual", and/or "Other" as values.
#' @param other_threshold Numeric value between 0 and 1 indicating the minimum exposure to a language other than Catalan or Spanish that a participant need to be exposed to to be excluded.
get_participants <- function(
        bvq_data,
        longitudinal = "all",
        age = c(12, 32),
        lp = c("Monolingual", "Bilingual"),
        other_threshold = 0.1
){
    
    participants <- bvq_data$logs %>%
        filter(
            completed, # get only data from complete questionnaire responses
            # rlang::.env makes sure we use the objects provided in the arguments
            # of the function, and not variables in the piped data frame
            lp %in% .env$lp,
            between(age, .env$age[1], .env$age[2]),
            sum(doe_catalan + doe_spanish) > .env$other_threshold,
            id != "bilexicon_1699", # exclude participants (duplicated entry)
            # make sure that degrees of exposure are between 0 and 1
            between(doe_spanish, 0, 1),
            between(doe_catalan, 0, 1),
            between(doe_others, 0, 1)
        ) %>% 
        # see ?multilex::get_longitudinal
        get_longitudinal(longitudinal = longitudinal) %>% 
        select(id, time, time_stamp, version, age, lp, doe_catalan, doe_spanish) 
    
    # export data as Parquet file
    write_ipc_stream(participants, here("data", "participants.parquet"))
    
    return(participants)
}


#' Prepare data for analyses
#' 
#' @param items A data frame resulting from calling \code{get_items}
#' @param responses A data frame resulting from calling \code{get_responses}
#' @param participants A data frame resulting from calling \code{get_participants}
get_data <- function(items, responses, participants) {
    
    df <- lst(items, responses, participants) %>% # merge all datasets
        reduce(inner_join) %>% 
        rename(freq = freq_zipf) %>% 
        # recode variables
        mutate(
            # code resposnes as factor
            response = factor(
                response,
                levels = c(1, 2, 3),
                labels = c("No", "Understands", "Understands and Says"),      
                ordered = TRUE
            ),
            # does should have the value of the corresponding language
            doe = ifelse(language=="Catalan", doe_catalan, doe_spanish),
            # standardise numeric predictors
            n_phon_std = scale(n_phon)[, 1],
            freq_std = scale(freq)[, 1],
            lv_std = scale(lv)[, 1],
            age_std = scale(age)[, 1],
            doe_std = scale(doe)[, 1],
            exposure = freq*doe,
            exposure_std = scale(exposure)[, 1],
        ) %>% 
        # get only relevant variables
        select(te, item, list, lv, lv_std, freq, freq_std, n_phon, n_phon_std,
               doe, doe_std, exposure, exposure_std, id, age, age_std, response) %>% 
        # reorder rows
        arrange(te, item, age, id)
    
    
    # save dataset
    write_ipc_stream(df, here("data", "main.parquet"))
    
    return(df)
    
}

