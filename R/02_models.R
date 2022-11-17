
#' Specify and sample a brms model
#' @param name A character string indicating the name to be assigned to the model
#' @param ... Arguments to be passed to \code{brms::brm()}
fit_model <- function(name, ...){
    # we run the model in the background as an RStudio job to keep the console free
    # see R/utils.R
    fit <- brm(
        ...,
        iter = 1000,
        chains = 4,
        init = 0.5, # where to initialise MCMCs
        seed = 888, # for reproducibility
        backend = "cmdstanr", # for faster, less problematic compilation in C++
        file = here("results", name), # save model as file
        # file_refit = "always", # should model be refitted or loaded from file?
        control = list(
            adapt_delta = 0.9, # for better convergence of MCMCs
            max_treedepth = 15
        ),
        save_model = here("stan", name) # save Stan code
    )
    
    return(fit)
}

#' Extract log-likelihood of brmsfit object or list of brmsfit objects via \code{loo::log_lik}
#' @param x A list of \code{brmsfit} objects that will be compared
#' @param ... Arguments to be passed to \code{log_lik}
get_log_lik <- function(x, ...){
    # make sure x is a brms fit object or a list of thereof
    stopifnot("x must be a brmsfit object or a list of brmsfit objects" = all(sapply(x, is.brmsfit)))
    # if x is a single brmsfit object, make it a list (of one element)
    if (is.brmsfit(x)) x <- list(x)
    n_models <- length(x) 
    log_liks <- vector(mode = "list", length = n_models) # pre-allocate vectors
    for (i in 1:n_models){
        log_liks[[i]] <- log_lik(x[[i]], ...)  # compute log-likelihood
        message(paste0(i, "/", n_models, " log-likelihoods computed"))
    }
    if (!is.null(names(x))) names(log_liks) <- names(x) # keep names if any
    return(log_liks)
}


#' Compute leave-one-out cross-validation of brmsfit object or list of brmsfit objects via \code{loo::loo}
#' @param x A list of log-likelihoods extracted from brmsfit objects using \code{loo:log_lik}
#' @param ... Arguments to be passed to \code{loo}
get_loo <- function(x, ...){
    n_models <- length(x)
    loos <- vector(mode = "list", length = n_models) # pre-allocate vector
    for (i in 1:n_models){
        loos[[i]] <- loo(x[[i]], ...) # compute log-likelihood
        message(paste0(i, "/", n_models, " LOOs computed"))
    }
    if (!is.null(names(x))) names(loos) <- names(x) # keep names if any
    return(loos)
}

#' 

