# utils

# run the targets workflow
make <- function() {
    job::job(
        { 
            targets::tar_make()
            job::export("none")  # return nothing
        }, 
        import = NULL,
        title = "Trajectories"
        
    )
}

# load all built targets (and packages)
tar_load_all <- function(){
    invisible({
        suppressMessages({
            tar_load_globals()
        })
        tars <- tar_objects()
        message("Loading targets: ", paste0(tars, collapse = ", "))
        lapply(tars, tar_load_raw, envir = .GlobalEnv)
    })
    
    usethis::ui_done("Targets loaded")
    
}

# remove targets products
unmake <- function(keep_fits = FALSE) {
    path <- "Results/fit.rds"
    tar_destroy(ask = FALSE)
    
    if (!keep_fits){
        
        filenames <- list.files("results", pattern = "fit")
        
        if (length(filenames > 0)) {
            
            lapply(filenames, file.remove)
            
        }
    }
    
    usethis::ui_done("Removed project outputs!")
}

# custom ggplot theme
theme_custom <- function(){
    theme_minimal() +
        theme(
            panel.grid = element_line(colour = "grey", linetype = "dotted"),
            axis.line = element_line(colour = "black"),
            text = element_text(size = 12, colour = "black"),
            axis.text = element_text(colour = "black")
        )
}

# dark GitHub theme
theme_github <- function(){
    theme_dark() +
        theme(
            text = element_text(colour = "white", size = 12),
            axis.text = element_text(colour = "white", size = 8),
            legend.title = element_text(colour = "white", size = 10),
            legend.text = element_text(colour = "white", size = 10),
            legend.background = element_rect(fill = "#0D1117"),
            strip.text = element_text(size = 10),
            legend.key = element_rect(fill = "#0D1117", colour = "#0D1117"),
            strip.background = element_rect(fill = "#161B22"),
            plot.background = element_rect(fill = "#0D1117"),
            panel.background = element_rect(fill = "#0D1117"),
            panel.border = element_rect(fill = "transparent", colour = "#0D1117")
        )
}


# not in function
`%!in%` <- function(x, y){
    !(x %in% y)
}

# adjusted proportion, SE, and CI, from Gelman, Hill & Vehtari (2020)
prop_adj <- function(y, n) (y+2)/(n+4)

prop_se_adj <- function(y, n){
    
    prop <- prop_adj(y, n)
    sqrt(prop*(1-prop)/(n+4))
    
}

prop_ci_adj <- function(y, n, conf = 0.95){
    
    prop <- prop_adj(y, n)
    
    se <- sqrt(prop*(1-prop)/(n+4))
    
    ci <- list(prop + qnorm((1-conf)/2)*se, prop + qnorm(1-(1-conf)/2)*se)
    ci[[1]] <- ifelse(ci[[1]]<0, 0, ci[[1]])
    ci[[2]] <- ifelse(ci[[2]]>1, 1, ci[[2]])
    
    return(ci)
    
}

# transform logit scale to probability
logit_to_prob <- function(x) {
    exp(x) / (1 + exp(x))
}


