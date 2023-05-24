source("renv/activate.R")

options(crayon.enabled = TRUE,
        repos = c("https://cloud.r-project.org",
                  "https://gongcastro.r-universe.dev"))

if (interactive()) {
    
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

