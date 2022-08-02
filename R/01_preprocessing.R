
#' Get item data
#' @param multilex_data A named list resulting from calling \code{get_multilex}
#' @param class A character vector indicating the word classes to be included in the resulting dataset. Takes "Adjective", "Noun" and/or "Verb" as values.
get_items <- function(
        multilex_data, 
        class = c("Adjective", "Noun", "Verb")
){
    
    # compute normalised Levenshtein distance (see ?stringdist::`stringdist-package`)
    # number of edit operations needed to make two strings identical
    # when applied to phonological transcriptions, it provides an approximation of phonological similarity between two words
    lv_similarities <- multilex_data$pool %>% 
        pivot_wider(
            id_cols = te, 
            names_from = language,
            values_from = ipa_flat
        ) %>% 
        mutate(
            # make sure strings are coded as UTF-8 before computing LVs
            lv = stringsim(enc2utf8(Catalan), enc2utf8(Spanish)),
            lv_dist = as.integer(stringdist(enc2utf8(Catalan), enc2utf8(Spanish)))
        ) %>% 
        select(te, lv, lv_dist)
    
    # see ?multilex::pool
    single_te <- multilex_data$pool 
    
    # merge datasets
    items <- multilex_data$pool %>% 
        rename(list = version) %>% 
        left_join(lv_similarities) %>% # add LVs
        # drop items with missing observations in these variables
        drop_na(cognate, lv, frequency_zipf, list) %>% 
        mutate(n_phon = nchar(enc2utf8(ipa_flat))) %>% # number of phonemes
        # only content words
        filter(
            class %in% .env$class, 
            include # exclude problematic items (e.g., multi-word items)
        ) %>% 
        select(
            te,
            item,
            ipa_flat,
            lv,
            lv_dist,
            list,
            freq = frequency_zipf,
            n_phon
        ) %>% 
        # get only translation equivalents with at least one item in each language
        filter(te %in% .$te[duplicated(.$te)])
    
    # export as Parquet file
    write_ipc_stream(items, here("data", "items.parquet"))
    
    return(items)
}

#' Get item responses
#' @param multilex_data A named list resulting from calling \code{get_multilex}
#' @param update A logical value. Should Multilex data be updated and new questionnaire responses be fetched?
#' @param longitudinal Should longitudinal data be included? If "all" (default), all responses (including repeated measures) are included. If "no", participants with more than one responses to the questionnaire (regardless of the version) are excluded. If "first", only the first response of each participant is included. If "last", only the last response of each participant is included. If "only", only responses with repeated measures are included.
get_responses <- function(
        multilex_data,
        update = FALSE,
        longitudinal = "all"
){
    
    # see ?ml_responses
    responses <- ml_responses(
        participants = multilex_data$participants,
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
#' @param multilex_data A named list resulting from calling \code{get_multilex}
#' @param longitudinal Should longitudinal data be included? If "all" (default), all responses (including repeated measures) are included. If "no", participants with more than one responses to the questionnaire (regardless of the version) are excluded. If "first", only the first response of each participant is included. If "last", only the last response of each participant is included. If "only", only responses with repeated measures are included.
#' @param age Numeric vector of length two indicating the minimum and maximum age of participants that will be included in the resulting dataset.
#' @param lp Character vector indicating the language profile (LP) of the participants that will be included in the resulting dataset. In takes "Monolingual", "Bilingual", and/or "Other" as values.
#' @param other_threshold Numeric value between 0 and 1 indicating the minimum exposure to a language other than Catalan or Spanish that a participant need to be exposed to to be excluded.
get_participants <- function(
        multilex_data,
        longitudinal = "all",
        age = c(12, 32),
        lp = c("Monolingual", "Bilingual"),
        other_threshold = 0.1
){
    
    participants <- multilex_data$logs %>%
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

#' Get participant vocabulary data
#' @param multilex_data A named list resulting from calling \code{get_multilex}
#' @param age Numeric vector of length two indicating the minimum and maximum age of participants that will be included in the resulting dataset.
#' @param type A character vector indicating for what type of measure should vocabulary and prevalence data be computed? It takes "understands" and/or "produces".
get_vocabulary <- function(
        multilex_data,
        age = c(12, 32),
        type = c("understands", "produces")
){
    
    suppressWarnings({
        
        suppressMessages({
            
            # see ?multilex::pool
            pool <- select(multilex_data$pool, item, te, version_pool = version)
            
            # wide dataset
            wide <- left_join(multilex_data$logs, multilex_data$vocabulary) %>%
                distinct(id, time, type, .keep_all = TRUE) %>% 
                select(id, time, time_stamp, version, age, lp, type, starts_with("vocab_")) %>%
                # homogenize questionnaire version names
                mutate(
                    version = str_remove(version, "BL-"),
                    version = ifelse(
                        str_detect(version, "Long"),
                        "Long",
                        str_replace(version, "Lockdown", "Short")
                    )
                ) %>% 
                rename_at(
                    vars(starts_with("vocab_")),
                    str_remove_all, 
                    "vocab_|dominance_"
                )  %>% 
                pivot_longer(
                    starts_with("prop_") | starts_with("count_"),
                    names_to = c("scale", "modality"), 
                    names_sep = "_*?(_)"
                ) %>% 
                pivot_wider(
                    names_from = scale,
                    values_from = value
                ) %>% 
                mutate_at(
                    vars(type, modality),
                    str_to_sentence
                )
            
            # validate long version
            long <- multilex_data$responses %>% 
                select(id, age, time, item, language, dominance, response) %>% 
                left_join(select(multilex_data$logs, id, lp, time, version, completed)) %>%
                filter(
                    completed,
                    lp != "Other",
                    between(age, .env$age[1], .env$age[2])
                ) %>%
                left_join(pool) %>% 
                filter(str_detect(version, "Long")) %>% 
                mutate( 
                    understands = response > 1,
                    produces = response > 2,
                    item_dominance = ifelse(language==dominance, "L1", "L2")
                ) %>% 
                select(-version) %>% 
                rename(version = version_pool) %>% 
                unnest(cols = version) %>% 
                group_by(id, time, age, version) %>% 
                summarise(
                    understands_total = mean(understands, na.rm = TRUE),
                    understands_l1 = mean(understands[item_dominance=="L1"], na.rm = TRUE),
                    understands_l2 = mean(understands[item_dominance=="L2"], na.rm = TRUE),
                    produces_total = mean(produces, na.rm = TRUE),
                    produces_l1 = mean(produces[item_dominance=="L1"], na.rm = TRUE),
                    produces_l2 = mean(produces[item_dominance=="L2"], na.rm = TRUE),
                    .groups = "drop"
                )  %>% 
                pivot_longer(
                    starts_with("understands_") | starts_with("produces_"),
                    names_to = c("type", "scale"), 
                    names_sep = "_"
                ) %>% 
                mutate_at(
                    vars(version, scale), 
                    str_to_sentence
                )
            
            vocabulary <- list(wide = wide, long = long)
            
            # export data as Parquet files
            write_ipc_stream(vocabulary$wide, here("data", "vocabulary_wide.parquet"))
            write_ipc_stream(vocabulary$long, here("data", "vocabulary_long.parquet"))
            
        })
    })
    
    return(vocabulary)
    
}


#' Prepare data for analyses
#' @param items A data frame resulting from calling \code{get_items}
#' @param responses A data frame resulting from calling \code{get_responses}
#' @param participants A data frame resulting from calling \code{get_participants}
get_data <- function(
        items,
        responses,
        participants
){
    
    df <- lst(items, responses, participants) %>% # merge all datasets
        reduce(inner_join) %>% 
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
            doe = ifelse(
                language=="Catalan",
                doe_catalan,
                doe_spanish
            ),
            # standardise numeric predictors
            n_phon_std = scale(n_phon)[, 1],
            freq_std = scale(freq)[, 1],
            lv_std = scale(lv)[, 1],
            age_std = scale(age)[, 1],
            doe_std = scale(doe)[, 1],
            
        ) %>% 
        # get only relevant variables
        select(
            te,
            item,
            list,
            lv,
            lv_std,
            freq,
            freq_std,
            n_phon,
            n_phon_std,
            doe,
            doe_std,
            id,
            age,
            age_std,
            response
        ) %>% 
        # reorder rows
        arrange(te, item, age, id)
    
    
    # save dataset
    write_ipc_stream(df, here("data", "main.parquet"))
    
    return(df)
    
}

