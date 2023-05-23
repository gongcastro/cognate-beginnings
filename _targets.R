suppressPackageStartupMessages({
    # workflows and project
    library(targets)
    library(tarchetypes)
    library(cli)
    # data handling, cleaning, and testing
    library(tidyverse)
    library(janitor)
    library(glue)
    library(testthat)
    # data collection
    library(childesr)
    library(bvq)
    library(stringdist)
    library(ipa)
    # modelling
    library(brms)
    library(tidybayes)
    library(marginaleffects)
    library(bayestestR)
    library(bayesplot)
    # communication and visualisation
    library(tidytext)
    library(gt)
    library(ggtext)
    library(patchwork)
    library(magick)
    library(scales)
})

# load R functions -------------------------------------------------------------

invisible({
    lapply(
        list.files(
            c("R", "tests/testthat"),
            pattern = ".R",
            full.names = TRUE
        ),
        source
    )
})

# define global options --------------------------------------------------------

options(repos = c("https://mc-stan.org/r-packages/",
                  "https://gongcastro.r-universe.dev", 
                  "https://cloud.r-project.org"),
        mc.cores = parallel::detectCores(),
        brms.backend = "cmdstanr",
        tidyverse.quiet = TRUE,
        knitr.duplicate.label = "allow",
        loo.cores = 1,
        knitr.graphics.error = FALSE)

list(
    ## import data -------------------------------------------------------------
    
    tar_target(bvq_data_file, "data-raw/bvq.rds", format = "rds"),
    tar_target(bvq_data, readRDS(bvq_data_file)),
    
    # get CHILDES frequencies
    tar_target(childes, get_childes_frequencies(age_range = c(12, 32))),
    
    # items
    tar_target(items, get_items(bvq_data = bvq_data, 
                                childes = childes)),
    tar_target(items_test, test_items(items)),
    
    # participants
    tar_target(participants,
               get_participants(bvq_data,
                                age = c(12, 32),
                                lp = c("Monolingual", "Bilingual"),
                                other_threshold = 0.1)),
    
    tar_target(participants_test, test_participants(participants)),
    
    # responses
    tar_target(responses, get_responses(bvq_data, items, participants)),
    
    tar_target(responses_test, test_responses(responses)),
    
    # fit models ---------------------------------------------------------------
    
    # model prior
    tar_target(
        model_prior,
        c(prior(normal(-0.25, 0.5), class = "Intercept"),
          prior(normal(1, 0.25), class = "sd", group = "te"),
          prior(normal(1, 0.25), class = "sd", group = "id"),
          prior(normal(0, 1), class = "b"),
          prior(lkj(2), class = "cor")
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
    tar_target(model_formula,
               bf(response ~ age_std * exposure_std * lv_std + n_phon_std + 
                      (1 + age_std * exposure_std * lv_std + n_phon_std | id) +
                      (1 + age_std * exposure_std + n_phon_std | te),
                  family = cumulative("logit"))),
    
    # add age:exposure interaction
    tar_target(model_fit,
               fit_model(name = "fit",
                         formula = model_formula,
                         data = responses,
                         prior = model_prior,
                         sample_prior = "yes")),
    
    # model with only prior samples
    tar_target(model_fit_prior,
               fit_model(name = "fit_prior",
                         formula = model_formula,
                         data = responses,
                         prior = model_prior,
                         sample_prior = "only")),
    
    ## describe models ---------------------------------------------------------
    
    tar_target(model_vars_dict, get_vars_dict(responses)),
    
    # get posterior draws for population-level effects
    tar_target(model_draws,
               get_posterior_draws(model_fit,
                                   data = responses,
                                   model_vars_dict)),
    
    # get summary of posterior draws for population-level effects
    tar_target(model_summary,
               get_posterior_summary(model_fit, 
                                     data = responses,
                                     model_vars_dict)),
    
    # get posterior draws for group-level effects
    tar_target(model_draws_re,
               get_posterior_draws_re(model_fit)),
    
    ## marginal effects --------------------------------------------------------
    tar_target(model_epreds,
               posterior_predictions(model = model_fit,
                                     responses, 
                                     age_std = scale(seq(7, 40),
                                                     mean(responses$age),
                                                     sd(responses$age)),
                                     exposure_std = c(-1, 0, 1),
                                     lv_std = scale(seq(0, 1, 0.5), 
                                                    mean(responses$lv),
                                                    sd(responses$lv)),
                                     n_phon_std = 0)),
    
    # convergence diagnostics (rhat and n_eff)
    tar_target(model_convergence, get_model_convergence(model_fit)),
    
    # posterior predictive checks
    tar_target(model_ppcs, get_model_ppc(model_fit, responses)),
    
    # get age-of-acquisition ---------------------------------------------------
    # 
    # tar_target(aoa_data, get_aoa_data(responses)),
    # 
    # tar_target(aoa_model_prior,
    #            model_prior <- c(
    #                prior(normal(0, 5), nlpar = "mid", coef = "Intercept"),
    #                prior(normal(0, 5), nlpar = "mid", class = "b"),
    #                
    #                prior(normal(0, 1), nlpar = "scale", coef = "Intercept"),
    #                prior(exponential(6), nlpar = "scale", class = "sd"),
    #                
    #                prior(lkj(8), class = "cor")
    #            )),
    # 
    # tar_target(aoa_model_fit_comprehension,
    #            get_aoa_model_fit(aoa_data, type = "comprehension")),
    # 
    # tar_target(aoa_model_fit_production,
    #            get_aoa_model_fit(aoa_data, type = "production")),
    # 
    # tar_target(aoa_model_posterior_comprehension,
    #            get_aoa_model_posterior(aoa_model_fit_comprehension, aoa_data)),
    # 
    # tar_target(aoa_model_posterior_production,
    #            get_aoa_model_posterior(aoa_model_fit_production, aoa_data)),
    # 
    # appendix -----------------------------------------------------------------
    
    # fit model with frequency and DoE as separate predictors instead of exposure
    tar_target(model_doe,
               fit_model(name = "fit_doe",
                         formula = bf(
                             response ~ age_std + doe_std * lv_std + n_phon_std + freq_std +
                                 (1 + age_std + doe_std * lv_std + n_phon_std + freq_std | id) +
                                 (1 + age_std + doe_std + n_phon_std + freq_std | te),
                             family = cumulative(link = "logit")),
                         data = responses,
                         prior = model_prior,
                         sample_prior = "yes")),
    
    tar_target(
        posterior_draws_doe,
        {
            # tidy predictor names
            str_repl <- c(
                "b_Intercept[1]" = "Comprehension and Production",
                "b_Intercept[2]" = "Comprehension",
                "b_age_std" = glue("Age (+1 SD, {round(sd(responses$age), 2)}, months)"),
                "b_n_phon_std" = glue("Phonemes (+1 SD, {round(sd(responses$n_phon), 2)} phonemes)"),
                "b_doe_std" = glue("DoE (+1 SD, {round(sd(responses$doe_std), 2)})"),
                "b_freq_std" = glue("Frequency (+1 SD, {round(sd(responses$freq_std), 2)})"),
                "b_lv_std" = glue("Cognateness (+1 SD, {percent(sd(responses$lv))})"),
                "b_doe_std:lv_std" = "Exposure \u00d7 Cognateness"
            )
            
            get_posterior_summary(model_doe,
                                  data = responses) |>
                mutate(.variable_name = factor(.variable,
                                               levels = names(str_repl),
                                               labels = str_repl,
                                               ordered = TRUE) |>
                           as.character())
        }),
    
    tar_target(syllables_data,
               get_syllable_data(items)),
    
    tar_target(model_fit_syllables,
               {
                   fit_model(name = "fit_syllables",
                             formula = freq_syll ~ n_syll_std + lv_std + 
                                 (1 + n_syll_std | te),
                             prior = c(prior(normal(0, 10), class = "Intercept"),
                                       prior(normal(0, 10), class = "b"),
                                       prior(exponential(3), class = "sigma"),
                                       prior(exponential(3), class = "sd"),
                                       prior(lkj(3), class = "cor")),
                             data = syllables_data,
                             sample_prior = "yes" )
               }
    ),
    
    # render report ------------------------------------------------------------
    # tar_quarto(
    #     report,
    #     "docs/index.qmd",
    #     execute = TRUE,
    #     quiet = FALSE
    # ),
    # 
    #     # render manuscript
    tar_quarto(manuscript,
               "manuscript/manuscript.qmd",
               execute = TRUE,
               cache = FALSE,
               quiet = FALSE
    ),
    
    tar_quarto(appendix,
               "manuscript/appendix.qmd",
               execute = TRUE,
               cache = FALSE,
               quiet = FALSE)
)
