library(targets)
library(tarchetypes)

source("R/utils.R")
source("R/00_items.R")
source("R/01_participants.R")
source("R/02_vocabulary.R")
source("R/03_responses.R")
source("R/04_models.R")

# set parameters ----
tar_option_set(
    packages = c(
        "dplyr", "tidyr", "stringr", "multilex", "keyring",
        "readxl", "janitor", "mice", "here", "lubridate", "purrr",
        "brms", "tidybayes"
    )
)
options(tidyverse.quiet = TRUE)

# End this file with a list of target objects.
list(
    
    # multilex data
    tar_target(credentials, get_credentials()),
    tar_target(
        multilex_data,
        get_multilex(
            update = FALSE,
            type = c("understands", "produces")
        )
    ),
    
    # items
    tar_target(
        cognate_codings_path,
        here("Data", "cognate_codings.xlsx")
    ),
    tar_target(
        cognate_codings,
        read_xlsx(cognate_codings_path)
    ),
    tar_target(
        items, 
        get_items(
            multilex_data = multilex_data,
            cognate_codings = cognate_codings,
            impute = TRUE
        )
    ),
    
    # participants
    tar_target(
        participants, 
        get_participants(
            multilex_data,
            age_range = c(10, 36),
            lps = c("Monolingual", "Bilingual"),
            other_threshold = 0.1
        )
    ),
    
    # vocabulary
    tar_target(
        vocabulary,
        get_vocabulary(
            participants = participants,
            multilex_data = multilex_data,
            type = c("understands", "produces")
        )
    ),
    
    # responses
    tar_target(
        responses,
        get_responses(
            participants = participants,
            items = items,
            multilex_data = multilex_data
        )
    ),
    
    # fit models
    tar_target(
        fits,
        fit_models(
            data = responses,
            iter = 500,
            cores = 4,
            chains = 4,
            inits = 0,
            save_pars = save_pars(all = TRUE),
            backend = "cmdstanr",
            sample_prior = "only"
        )
    )
    

)





