library(targets)
library(tarchetypes)
library(conflicted)
library(cli)

# load R functions -------------------------------------------------------------
function_paths <- list.files("R",
                             pattern = ".R",
                             full.names = TRUE)
invisible(lapply(function_paths, source))

test_paths <- list.files("tests/testthat",
                         pattern = ".R",
                         full.names = TRUE)
invisible(lapply(test_paths, source))

# list package dependencies ----------------------------------------------------
tar_option_set(
    packages = c(
        "arrow",
        "bayestestR",
        "brms",
        "bvqdev",
        "childesr",
        "cli",
        "conflicted",
        "dplyr",
        "ggplot2",
        "glue",
        "gt",
        "janitor",
        "keyring",
        "knitr",
        "lubridate",
        "marginaleffects",
        "mice",
        "patchwork",
        "purrr",
        "quarto",
        "readxl",
        "rlang",
        "scales",
        "stringdist",
        "stringr",
        "testthat",
        "tibble",
        "tidybayes",
        "tidyr",
        "usethis"
    )
)

resolve_conflicts()

# define global options --------------------------------------------------------
options(
    mc.cores = 4,
    brms.backend = "cmdstanr",
    tidyverse.quiet = TRUE,
    knitr.duplicate.label = "allow",
    loo.cores = 1
)

list(
    ## resolve namespace conflicts ---------------------------------------------
    tar_target(namespace_conficts, resolve_conflicts()),
    
    ## import data -------------------------------------------------------------
    tar_target(bvq_data, get_bvq(
        update = TRUE, longitudinal = "all"
    )),
    
    # get CHILDES frequencies
    tar_target(childes, get_childes_frequencies(age_range = c(12, 32))),
    
    # items
    tar_target(items, get_items(bvq_data = bvq_data, childes = childes)),
    tar_target(items_test, test_items(items)),
    
    # participants
    tar_target(
        participants,
        get_participants(
            bvq_data,
            age = c(12, 32),
            lp = c("Monolingual", "Bilingual"),
            other_threshold = 0.1
        )
    ),
    tar_target(participants_test, test_participants(participants)),
    
    # responses
    tar_target(responses, get_responses(bvq_data, items, participants)),
    tar_target(responses_test, test_responses(responses)),
    
    # fit models ---------------------------------------------------------------
    
    # model priors: these priors were set so that they generate data similar to
    # what we expect based on Wordbank data (see manuscript and lab notes)
    tar_target(
        model_prior,
        c(
            prior(normal(-0.25, 0.1), class = "Intercept"),
            prior(normal(1, 0.1), class = "sd", group = "te"),
            prior(normal(1, 0.1), class = "sd", group = "id"),
            prior(lkj(2), class = "cor"),
            prior(normal(1, 0.25), class = "b", coef = "age_std"),
            prior(normal(0, 0.25), class = "b", coef = "n_phon_std"),
            prior(normal(0, 0.25), class = "b", coef = "exposure_std"),
            prior(normal(0, 0.25), class = "b", coef = "lv_std"),
            prior(normal(0, 0.25), class = "b", coef = "exposure_std:lv_std"),
            prior(normal(0, 0.25), class = "b", coef = "age_std:exposure_std"),
            prior(normal(0, 0.25), class = "b", coef = "age_std:lv_std"),
            prior(normal(0, 0.25), class = "b", coef = "age_std:exposure_std:lv_std")
        )
    ),
    
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
                response ~ age_std + n_phon_std + exposure_std +
                    (1 + age_std + n_phon_std + exposure_std | id) +
                    (1 + age_std + n_phon_std + exposure_std | te),
                family = cumulative(link = "logit") # cumulative, continuation ratio
            ),
            data = responses,
            prior = model_prior[1:7, ]
        )
    ),
    
    # add age:dominance interaction
    tar_target(
        model_fit_1,
        fit_model(
            name = "fit_1",
            formula = bf(
                response ~ age_std * exposure_std + n_phon_std +
                    (1 + age_std * exposure_std + n_phon_std | id) +
                    (1 + age_std * exposure_std + n_phon_std | te),
                family = cumulative(link = "logit") # cumulative, continuation ratio
            ),
            data = responses,
            prior = model_prior[c(1:7, 10), ],
            sample_prior = "yes"
        )
    ),
    # add age:exposure interaction
    tar_target(
        model_fit_2,
        fit_model(
            name = "fit_2",
            formula = bf(
                response ~ age_std * exposure_std + n_phon_std + lv_std +
                    (1 + age_std * exposure_std + n_phon_std + lv_std |
                         id) +
                    (1 + age_std * exposure_std + n_phon_std | te),
                family = cumulative(link = "logit") # cumulative, continuation ratio
            ),
            data = responses,
            prior = model_prior[c(1:8, 10),],
            sample_prior = "yes"
        )
    ),
    tar_target(
        model_fit_3,
        fit_model(
            name = "fit_3",
            formula = bf(
                response ~ age_std * exposure_std + n_phon_std + lv_std + exposure_std:lv_std +
                    (
                        1 + age_std * exposure_std + n_phon_std + lv_std + exposure_std:lv_std |
                            id
                    ) +
                    (1 + age_std * exposure_std + n_phon_std | te),
                family = cumulative(link = "logit") # cumulative, continuation ratio
            ),
            data = responses,
            prior = model_prior[1:10, ],
            sample_prior = "yes"
            
        )
    ),
    tar_target(
        model_fit_4,
        fit_model(
            name = "fit_4",
            formula = bf(
                response ~ age_std * exposure_std * lv_std + n_phon_std +
                    (1 + age_std * exposure_std * lv_std + n_phon_std |
                         id) +
                    (1 + age_std * exposure_std + n_phon_std | te),
                family = cumulative(link = "logit") # cumulative, continuation ratio
            ),
            data = responses,
            prior = model_prior,
            sample_prior = "yes"
            
        )
    ),
    # model with only prior samples
    tar_target(
        model_fit_4_prior,
        fit_model(
            name = "fit_4_prior",
            formula = model_fit_4$formula,
            data = responses,
            prior = model_prior,
            sample_prior = "only"
        )
    ),
    
    ## compare models ----------------------------------------------------------
    tar_target(model_log_liks,
               # 700 samples, amount decided based on computational constraints
               # basically, RStudio crashes with more samples in the current machine
               get_log_lik(lst(model_fit_0,
                               model_fit_1,
                               model_fit_2,
                               model_fit_3,
                               model_fit_4), 
                           re_formula = NA, 
                           ndraws = 700L)),
    tar_target(model_loos,
               get_loo(model_log_liks)),
    
    ## describe models ---------------------------------------------------------
    tar_target(
        posterior_draws,
        get_posterior_draws(model_fit_4, data = responses)
    ),
    
    tar_target(posterior_draws_re,
               get_posterior_draws_re(model_fit_4)),
    
    ## marginal effects --------------------------------------------------------
    tar_target(
        marginal_effects_epreds,
        posterior_predictions(model = model_fit_4, responses)
    ),
    
    tar_target(
        marginal_effects_epreds_te,
        posterior_predictions_re(model = model_fit_4, responses, group = "te")
    ),
    
    tar_target(
        marginal_effects_epreds_id,
        posterior_predictions_re(model = model_fit_4, responses, group = "id")
    ),
    
    # R-hat (aka. Gelman-Rubin statistic)
    tar_target(model_rhats,
               map(
                   lst(
                       model_fit_0,
                       model_fit_1,
                       model_fit_2,
                       model_fit_3,
                       model_fit_4
                   ),
                   rhat
               )),
    # effective sample size
    tar_target(model_neffs,
               map(
                   lst(
                       model_fit_0,
                       model_fit_1,
                       model_fit_2,
                       model_fit_3,
                       model_fit_4
                   ),
                   neff_ratio
               )),
    
    # appendix -----------------------------------------------------------------
    
    # fit model with frequency and DoE as separate predictors instead of exposure
    tar_target(
        model_prior_doe,
        c(
            prior(normal(-0.25, 0.1), class = "Intercept"),
            prior(normal(1, 0.1), class = "sd", group = "te"),
            prior(normal(1, 0.1), class = "sd", group = "id"),
            prior(lkj(2), class = "cor"),
            prior(normal(1, 0.25), class = "b", coef = "age_std"),
            prior(normal(0, 0.25), class = "b", coef = "n_phon_std"),
            prior(normal(0, 0.25), class = "b", coef = "freq_std"),
            prior(normal(0, 0.25), class = "b", coef = "doe_std"),
            prior(normal(0, 0.25), class = "b", coef = "lv_std"),
            prior(normal(0, 0.25), class = "b", coef = "doe_std:lv_std"),
            prior(normal(0, 0.25), class = "b", coef = "age_std:doe_std"),
            prior(normal(0, 0.25), class = "b", coef = "age_std:lv_std"),
            prior(normal(0, 0.25), class = "b", coef = "age_std:doe_std:lv_std")
        )
    ),
    
    tar_target(
        model_fit_4_doe,
        fit_model(
            name = "fit_4_doe",
            formula = bf(
                response ~ age_std * doe_std * lv_std + n_phon_std + freq_std +
                    (1 + age_std * doe_std * lv_std + n_phon_std + freq_std |
                         id) +
                    (1 + age_std * doe_std + n_phon_std + freq_std |
                         te),
                family = cumulative(link = "logit") # cumulative, continuation ratio
            ),
            data = responses,
            prior = model_prior_doe,
            sample_prior = "yes"
        )
    ),
    
    # render report ------------------------------------------------------------
    tar_quarto(
        report,
        "docs/index.qmd",
        execute = TRUE,
        quiet = FALSE
    ),
    
    # render manuscript
    tar_quarto(
        manuscript,
        "manuscript/manuscript.qmd",
        execute = TRUE,
        quiet = FALSE
    )
    
)
