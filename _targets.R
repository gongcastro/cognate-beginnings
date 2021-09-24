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
        "dplyr", "tidyr", "stringr", "ggplot2", "tibble", "forcats", "multilex", "keyring",
        "readxl", "janitor", "mice", "here", "lubridate", "purrr", "scales",
        "brms", "tidybayes", "gt", "patchwork", "wesanderson", "papaja"
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
    
    # fit models (prior, comprehension)
    tar_target(
        fits_comp_prior,
        fit_models_comp(
            data = responses,
            iter = 500,
            cores = 4,
            chains = 4,
            inits = 0,
            save_pars = save_pars(all = TRUE),
            backend = "cmdstanr",
            sample_prior = "only"
        )
    ),
    
    # fit models (prior, production)
    tar_target(
        fits_prod_prior,
        fit_models_prod(
            data = responses,
            iter = 500,
            cores = 4,
            chains = 4,
            inits = 0,
            save_pars = save_pars(all = TRUE),
            backend = "cmdstanr",
            sample_prior = "only"
        )
    ),
    
    # fit models (comprehension)
    tar_target(
        fits_comp,
        fit_models_comp(
            data = responses,
            iter = 500,
            cores = 4,
            chains = 4,
            inits = 0,
            save_pars = save_pars(all = TRUE),
            backend = "cmdstanr"
        )
    ),
    
    # fit models (production)
    tar_target(
        fits_prod,
        fit_models_prod(
            data = responses,
            iter = 500,
            cores = 4,
            chains = 4,
            inits = 0,
            save_pars = save_pars(all = TRUE),
            backend = "cmdstanr"
        )
    ),

    tar_target(
        loo_comp,
        compare_models(fits_comp)
    ),

    tar_target(
        loo_prod,
        compare_models(fits_prod)
    ),

    # render report.Rmd
    tar_render(report, "Rmd/report.Rmd"),
    
    # render manuscript
    tar_render(manuscript, "Rmd/manuscript.Rmd")
    
)





