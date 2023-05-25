#' Run the targets workflow as a RStudio job using [jobs::job()]
make <- function() {
    job::job({
        targets::tar_make()
        job::export("none")  # return nothing
    },
    import = NULL,
    title = "cognate-beginnings")
    
    to_delete <- c("manuscript/QTDublinIrish.otf")
    for(i in to_delete) {
        if (file.exists(i)) {
            file.remove(i)
        }
    }
}

#' Remove targets products using
#' 
unmake <- function(keep_fits = TRUE) {
    path <- "results/fit.rds"
    tar_destroy(ask = FALSE)
    
    if (!keep_fits) {
        filenames <-
            list.files("results", pattern = "fit", full.names = TRUE)
        if (length(filenames > 0)) {
            lapply(filenames, file.remove)
        }
    }
    
    if (interactive()) cli_alert_success("Removed project outputs!")
}

#' Print welcome message in console
#' 
welcome_message <- function() {
    id <- cli_status("")
    cli({
        cli_h1("cognate-beginnings")
        cli_text("This project works with the {.pkg renv} and 
                     {.pkg targets} R packages.")
        cli_text()
        cli_li("{.pkg renv} manages package dependencies so that you can
                   install the packages needed to run the code in this repository
                   in their correct versions, without touching the packages that
                   you already have installed in your machine. This ensures that
                   the code and R packages that you use in your other projects is
                   not affected by running the code in this project.")
        cli_text()
        cli_li("{.pkg targets} manages reproducible workflows. The file 
                   {.path _targets.R} contains the commands to be run in order to get the 
                   outcomes and byproducts of the code in this repository. These
                   functions are defined in the scripts in {.path R/}. See the
                   instructions below to run the workflow.")
        
        cli_h2("Instructions")
        cli_ol(c("Run {.code renv::restore()}", 
                 "Restart the R session",  
                 "Run {.code tar_make()}"))
        cli_h2("Repository info")
        cli_ul(c("Lab notes: {.url gongcastro.github.io/cognate-beginnings}",
                 "URL: {.url https://github.com/gongcastro/cognate-beginnings}",
                 "OSF: {.url https://osf.io/hy984/}"
        ))
    })
    cli_status_clear(id)
}
