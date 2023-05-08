source("renv/activate.R")
source("src/helpers.R")

options(crayon.enabled = TRUE)

library(targets)
library(cli)

welcome_message <- function() {
    if (interactive()) {
        id <- cli_status("")
        cli({
            cli_h1("trajectories")
            cli_text("This project works with the {{renv} and {{targets}} R packages.")
            cli_text()
            cli_li("{{renv}} manages package dependencies so that you can install the packages needed to run the code in this repository in their correct versions, without touching the packages that you already have installed in your machine. This ensures that the code and R packages that you use in your other projects is not affected by running the code in this project. ")
            cli_text()
            cli_li("{{targets}} manages reproducible workflows. The file {.path _targets.R} contains the commands to be run in order to get the outcomes and byproducts of the code in this repository. These functions are defined in the scripts in {.path R/}. See the instructions below to run the workflow.")
            
            cli_h2("Instructions")
            cli_ol(c("Run {.code renv::restore()}", 
                     "Restart the R session",  
                     "Run {.code make()}"))
            cli_h2("Repository info")
            cli_ul(c("Lab notes: {.url gongcastro.github.io/trajectories}",
                     "URL: {.url https://github.com/gongcastro/trajectories}",
                     "OSF: {.url https://osf.io/hy984/}"
            ))
        })
        cli_status_clear(id)
    }
}
welcome_message()
