if (!targets::tar_active() && interactive()) {
    source("renv/activate.R")
    invisible({
        lapply(list.files("src", pattern = ".R$", full.names = TRUE), source)
    })
    options(crayon.enabled = TRUE)
    
    suppressWarnings({
        library(targets)
        library(cli)
    })
    
    welcome_message()
    
    resolve_conflicts()
}

