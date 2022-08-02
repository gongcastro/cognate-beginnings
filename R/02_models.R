
#' Specify and sample a brms model
#' @param name A character string indicating the name to be assigned to the model
#' @param ... Arguments to be passed to \code{brms::brm()}
fit_model <- function(name, ...){
    # we run the model in the background as an RStudio job to keep the console free
    # see R/utils.R
    fit <- brm(
        ...,
        sample_prior = "yes", # samples prior for faster computation of Bayes Factors and LOO
        iter = 4000,
        chains = 2,
        init = 0, # where to initialise MCMCs
        seed = 888, # for reproducibility
        backend = "cmdstanr", # for faster, less problematic compilation in C++
        file = here("results", paste0(name, ".rds")), # save model as file
        # file_refit = "always", # should model be refitted or loaded from file?
        control = list(
            adapt_delta = 0.9, # for better convergence of MCMCs
            max_treedepth = 15
        ),
        save_model = here("stan", paste0(name, ".stan")) # save Stan code
    )
    
    return(fit)
}


#' Compare brms models
#' @param x A list of \code{brmsfit} objects that will be compared
#' @param criterion A character string indicating the criterion to use to compare models. For now, it can only take "loo" as value, which invokes Leave-One-Out Cross-Validation (LOO).
compare_models <- function(x, criterion = "loo", ...) {
    
    if (criterion=="loo") {
        # models are too large, compute LOO from subsamples (500 by default)
        loos <- lapply(x, loo_subsample, ...)
        saveRDS(
            loos,
            here("results", paste0("model_", criterion, ".rds"))
        )
        
    } else {
        # future versions of this function might allow other criteria like WAIC
        stop("Criterion must be one of: 'loo'")
        
    }
    return(loos)
}




