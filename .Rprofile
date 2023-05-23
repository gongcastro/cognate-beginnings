if (interactive()) {
    
    source("renv/activate.R")
    
    options(crayon.enabled = TRUE)
    
    suppressWarnings({
        library(targets)
        library(cli)
    })
    
    invisible({
        src_files <- list.files("src", pattern = ".R$", full.names = TRUE)
        lapply(src_files, source)
    })
    
    welcome_message()
}

