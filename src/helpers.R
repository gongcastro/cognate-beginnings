# run the targets workflow
make <- function() {
    job::job({
        targets::tar_make()
        job::export("none")  # return nothing
    },
    import = NULL,
    title = "Trajectories")
    
    to_delete <- c("manuscript/QTDublinIrish.otf")
    for(i in to_delete) {
        if (file.exists(i)) {
            file.remove(i)
        }
    }
}

# remove targets products
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
    
    usethis::ui_done("Removed project outputs!")
}

#' Resolve NAMESPACE conflicts
#'
resolve_conflicts <- function() {
    suppressMessages({
        conflict_prefer("last_warnings", "rlang")
        conflict_prefer("filter", "dplyr")
        conflict_prefer("lag", "dplyr")
        conflict_prefer("between", "dplyr")
        conflict_prefer("timestamp", "utils")
        conflict_prefer("ar", "brms")
        conflict_prefer("chisq.test", "stats")
        conflict_prefer("discard", "scales")
        conflict_prefer("duration", "lubridate")
        conflict_prefer("fisher.test", "stats")
        conflict_prefer("lag", "dplyr")
    })
    cli_alert_success("Resolved conflicts")
}
