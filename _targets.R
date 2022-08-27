library(targets)
library(tarchetypes)

#### load R functions ----------------------------------------------------------

source("R/utils.R")
source("R/00_importing.R")
source("R/01_preprocessing.R")
source("R/02_models.R")

#### list package dependencies -------------------------------------------------

tar_option_set(
    packages = c(
        "arrow",
        "brms", 
        "conflicted",
        "dplyr", 
        "forcats",
        "ggplot2", 
        "ggsci",
        "gt", 
        "here", 
        "janitor",
        "keyring",
        "knitr",
        "lubridate",
        "mice", 
        "multilex",
        "patchwork", 
        "papaja", 
        "purrr", 
        "readxl", 
        "rmarkdown",
        "rlang",
        "scales", 
        "stringdist",
        "stringr",
        "testthat",
        "tibble",
        "tidybayes", 
        "tidyr", 
        "usethis",
        "wesanderson"
    )
)

#### define global options -----------------------------------------------------
options(
    mc.cores = 2,
    brms.backend = "cmdstanr",
    tidyverse.quiet = TRUE,
    knitr.duplicate.label = "allow"
)

list(
    
    # resolve namespace conflicts ----------------------------------------------
    tar_target(
        resolve_conficts,
        {
            conflict_prefer("last_warnings", "rlang")
            conflict_prefer("filter", "dplyr")
            conflict_prefer("between", "dplyr")
            conflict_prefer("timestamp", "utils")
            conflict_prefer("ar", "brms")
            conflict_prefer("chisq.test", "stats")
            conflict_prefer("discard", "scales")
            conflict_prefer("duration", "lubridate")
            conflict_prefer("fisher.test", "stats")
            conflict_prefer("lag", "dplyr")
        }
    ),
    
    # import data --------------------------------------------------------------
    tar_target(
        credentials,
        get_credentials() # see R/utils.R
    ),
    
    tar_target(
        multilex_data,
        # see R/00_importing.R
        get_multilex(
            update = TRUE, 
            type = c("understands", "produces")
        )
    ),
    
    # items
    tar_target(
        items,
        # see R/01_preprocessing.R
        get_items(
            multilex_data = multilex_data,
        )
    ),
    
    # participants
    tar_target(
        participants,
        # see R/01_preprocessing.R
        get_participants(
            multilex_data,
            age = c(10, 36), 
            lp = c("Monolingual", "Bilingual"), 
            other_threshold = 0.1
        )
    ),
    
    # vocabulary
    tar_target(
        vocabulary,
        # see R/01_preprocessing.R
        get_vocabulary(
            multilex_data = multilex_data,
            type = c("understands", "produces")
        )
    ),
    
    # responses
    tar_target(
        responses,
        # see R/01_preprocessing.R
        get_responses(
            multilex_data = multilex_data
        )
    ),
    
    # process data -------------------------------------------------------------
    tar_target(
        df,
        # see R/01_preprocessing.R
        get_data(
            participants = participants,
            items = items,
            responses = responses
        )
    ),
    
    # model priors -------------------------------------------------------------
    # these priors were set so that they generate data similar to what we expect
    # based on Wordbank data (see manuscript and lab notes)
    tar_target(
        model_prior,
        c(
            prior(normal(-0.25, 0.1), class = "Intercept"),
            prior(normal(1, 0.1), class = "sd", group = "te"),
            prior(normal(1, 0.1), class = "sd", group = "id"),
            prior(lkj(2), class = "cor"),
            prior(normal(1, 0.1), class = "b", coef = "age_std"),
            prior(normal(0, 0.1), class = "b", coef = "freq_std"),
            prior(normal(0, 0.1), class = "b", coef = "n_phon_std"),
            prior(normal(0, 0.1), class = "b", coef = "doe_std"),
            prior(normal(0, 0.1), class = "b", coef = "lv_std"),
            prior(normal(0, 0.1), class = "b", coef = "doe_std:lv_std"),
            prior(normal(0, 0.1), class = "b", coef = "age_std:doe_std"),
            prior(normal(0, 0.1), class = "b", coef = "age_std:lv_std"),
            prior(normal(0, 0.1), class = "b", coef = "age_std:doe_std:lv_std")
        )
    ),
    
    # fit models ---------------------------------------------------------------
    # multilevel model with crossed random effects (participants an items)
    # responses are generated from a categorical distribution:
    #   - https://journals.sagepub.com/doi/full/10.1177/2515245918823199
    #   - https://cran.r-project.org/web/packages/brms/vignettes/brms_families.html
    #   - https://bookdown.org/content/3686/ordinal-predicted-variable.html
    # the probability of each response category is adjusted by age (population-level effect)
    # and adjusted for each individual participant and item (group-level effects)
    
    # only intercepts (category boundaries)
    tar_target(
        model_fit_0,
        fit_model(
            name = "fit_0",
            formula =  bf(
                response ~ 1 +
                    (1 | id) +
                    (1 | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior[1:3, ]
        )
    ),
    
    # add age main effect
    tar_target(
        model_fit_1,
        fit_model(
            name = "fit_1",
            formula = bf(
                response ~ age_std +
                    (1 + age_std | id) +
                    (1 + age_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior[1:5, ]
        )
    ),
    
    # add age main effect
    tar_target(
        model_fit_2,
        fit_model(
            name = "fit_2",
            formula = bf(
                response ~ age_std + freq_std + 
                    (1 + age_std + freq_std | id) +
                    (1 + age_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior[1:6, ]
        )
    ),
    
    # add dominance main effect
    tar_target(
        model_fit_3,
        fit_model(
            name = "fit_3",
            formula = bf(
                response ~ age_std + freq_std + n_phon_std +
                    (1 + age_std + freq_std + n_phon_std | id) +
                    (1 + age_std + freq_std + n_phon_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior[1:7, ]
        )
    ),
    tar_target(
        model_fit_4,
        fit_model(
            name = "fit_4",
            formula = bf(
                response ~ age_std + freq_std + n_phon_std + doe_std +
                    (1 + age_std + freq_std + n_phon_std + doe_std | id) +
                    (1 + age_std + freq_std + n_phon_std + doe_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior[1:8,]
        )
    ),
    
    # add age:dominance interaction
    tar_target(
        model_fit_5,
        fit_model(
            name = "fit_5",
            formula = bf(
                response ~ age_std + freq_std + n_phon_std + doe_std + lv_std + 
                    (1 + age_std + freq_std + n_phon_std + doe_std + lv_std | id) +
                    (1 + age_std + freq_std + n_phon_std + doe_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior[1:9,]
        )
    ),
    # add age:dominance interaction
    tar_target(
        model_fit_6,
        fit_model(
            name = "fit_6",
            formula = bf(
                response ~ age_std + freq_std + n_phon_std + doe_std*lv_std + 
                    (1 + age_std + freq_std + n_phon_std + doe_std*lv_std | id) +
                    (1 + age_std + freq_std + n_phon_std + doe_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior[1:10,]
        )
    ),
    tar_target(
        model_fit_7,
        fit_model(
            name = "fit_7",
            formula = bf(
                response ~ age_std + freq_std + n_phon_std + doe_std*lv_std + age_std:(doe_std*lv_std) + 
                    (1 + age_std + freq_std + n_phon_std + age_std:(doe_std*lv_std) | id) +
                    (1 + age_std + freq_std + n_phon_std + age_std:doe_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior
        )
    ),
    
    # compare models -----------------------------------------------------------
    
    tar_target(
        model_loos,
        {
            models <- lst(
                model_fit_0,
                model_fit_1,
                model_fit_2,
                model_fit_3,
                model_fit_4,
                model_fit_5,
                model_fit_6,
                model_fit_7
            )
            loos <- lapply(models, loo_subsample)
            saveRDS(
                loos,
                here("results", "model_loos.rds")
            )
        }
    ),
    
    # diagnose models ----------------------------------------------------------
    
    # R-hat (aka. Gelman-Rubin statistic)
    tar_target(
        model_rhats,
        map(
            lst(
                model_fit_0,
                model_fit_1,
                model_fit_2,
                model_fit_3,
                model_fit_4,
                model_fit_5,
                model_fit_6,
                model_fit_7
            ),
            rhat
        )
    ),
    # effective sample size
    tar_target(
        model_neffs,
        map(
            lst(
                model_fit_0,
                model_fit_1,
                model_fit_2,
                model_fit_3,
                model_fit_4,
                model_fit_5,
                model_fit_6,
                model_fit_7
            ),
            neff_ratio
        )
    )
    
    
    # # render report.Rmd
    # tar_render(report, "docs/report.Rmd"),
    # 
    # # render manuscript
    # tar_render(manuscript, "manuscript/manuscript.Rmd")
    
)





