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
        "bayesplot",
        "bayestestR",
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
        "quarto",
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
    mc.cores = 4,
    brms.backend = "cmdstanr",
    tidyverse.quiet = TRUE,
    knitr.duplicate.label = "allow",
    loo.cores = 1
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
        model_fit_1,
        fit_model(
            name = "fit_1",
            formula = bf(
                response ~ age_std + freq_std + n_phon_std + doe_std + lv_std + 
                    (1 + age_std + freq_std + n_phon_std + doe_std + lv_std | id) +
                    (1 + age_std + freq_std + n_phon_std + doe_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior[1:9,],
            sample_prior = "yes"
        )
    ),
    # add age:dominance interaction
    tar_target(
        model_fit_2,
        fit_model(
            name = "fit_2",
            formula = bf(
                response ~ age_std + freq_std + n_phon_std + doe_std*lv_std + 
                    (1 + age_std + freq_std + n_phon_std + doe_std*lv_std | id) +
                    (1 + age_std + freq_std + n_phon_std + doe_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior[1:10,],
            sample_prior = "yes"
        )
    ),
    tar_target(
        model_fit_3,
        fit_model(
            name = "fit_3",
            formula = bf(
                response ~ age_std + freq_std + n_phon_std + doe_std*lv_std + age_std:(doe_std*lv_std) + 
                    (1 + age_std + freq_std + n_phon_std + age_std:(doe_std*lv_std) | id) +
                    (1 + age_std + freq_std + n_phon_std + age_std:doe_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior,
            sample_prior = "yes"
            
        )
    ),
    tar_target(
        model_fit_3_prior,
        fit_model(
            name = "fit_3_prior",
            formula = bf(
                response ~ age_std + freq_std + n_phon_std + doe_std*lv_std + age_std:(doe_std*lv_std) + 
                    (1 + age_std + freq_std + n_phon_std + age_std:(doe_std*lv_std) | id) +
                    (1 + age_std + freq_std + n_phon_std + age_std:doe_std | te),
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior,
            sample_prior = "only"
        )
    ),
    
    # compare models -----------------------------------------------------------
    tar_target(
        model_log_liks,
        {
            models <- lst(
                model_fit_0,
                model_fit_1,
                model_fit_2,
                model_fit_3
            )
            get_log_lik(models, re_formula = NA, ndraws = 700L)
        }
    ),
    tar_target(
        model_loos,
        get_loo(model_log_liks)
    ),
    
    # diagnose models ----------------------------------------------------------
    tar_target(
        rope_coefs,
        {
            read.csv("data/rope-calculation.csv", na.strings = "") %>% 
                as_tibble() %>%
                group_by(study, year, predictor, estimate) %>% 
                summarise(n_participants = sum(n_participants), .groups = "drop") %>% 
                filter(!str_detect(study, "Mitchell")) %>% 
                mutate(
                    predictor = str_replace_all(predictor, "\\*", "\u00d7"),
                    study = paste0(study, " (", year, ")\nN = ", n_participants)
                )
        }
    ),
    tar_target(
        rope_interval,
        median(rope_coefs$estimate)*c(lower = -1, upper = +1)
    ),
    # describe posterior
    tar_target(
        posterior_description,
        describe_posterior(
            model_fit_3,
            test = "rope",
            rope_range = rope_interval, 
            ci_method = "HDI"
        ) %>% 
            as_tibble() %>% 
            clean_names()
    ),
    tar_target(
        rope_test,
        as_tibble(rope(model_fit_3, rope_range = rope_interval))
    ),
    # R-hat (aka. Gelman-Rubin statistic)
    tar_target(
        model_rhats,
        map(
            lst(
                model_fit_0,
                model_fit_1,
                model_fit_2,
                model_fit_3
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
                model_fit_3
            ),
            neff_ratio
        )
    )
    
    
    # render report.Rmd
    # tar_quarto(report, "index.qmd")
    # 
    # # render manuscript
    # tar_render(manuscript, "manuscript/manuscript.Rmd")
    
)





