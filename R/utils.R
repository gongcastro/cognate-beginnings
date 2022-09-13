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

# remove targets products
unmake <- function(keep_fits = TRUE) {
    path <- "Results/fit.rds"
    tar_destroy(ask = FALSE)
    
    if (!keep_fits){
        filenames <- list.files("results", pattern = "fit", full.names = TRUE)
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

# transform months to years and months ----
months_to_years <- function(x, sep = ";") {
    paste(
        floor(x %/% 12),
        floor(x %% 12),
        sep = sep
    )
}

# rescale standardised variable ---
rescale_variable <- function(x, mean, sd){
    (x * sd) + mean
}

# cut age variable into chunks ----
cut_age <- function(x) {
    y <- cut(x, breaks = seq(9, 35, 2), labels = seq(10, 34, 2))
    y <- as.integer(as.character(y))
    return(y)
}

# adjusted proportion, SE, and CI ----
# from Gelman, Hill & Vehtari (2020)
prop_adj <- function(y, n) (y+2)/(n+4)

prop_adj_se <- function(y, n) {
    prop <- prop_adj(y, n)
    sqrt(prop*(1-prop)/(n+4))
}

prop_adj_ci <- function(y, n, .width = 0.95, limit) {
    prop <- (y+2)/(n+4)
    se <- sqrt(prop*(1-prop)/(n+4))
    ci <-  prop + qnorm(c((1-.width)/2, (1-(1-.width)/2)))*se
    ci[1] <- ifelse(ci[1]<0, 0, ci[1]) # truncate at 0
    ci[2] <- ifelse(ci[2]>1, 1, ci[2]) # truncate at 1
    
    if (limit==".lower") return(ci[1])
    if (limit==".upper") return(ci[2])
}

# get age of acquisition ----
get_aoa <- function(preds, ...){
    
    aoas <- preds %>% 
        group_by(..., .draw) %>% 
        summarise(
            aoa = age[which.min(abs(.epred-0.5))],
            .epred = .epred[which.min(abs(.epred-0.5))],
            .groups = "drop"
        ) %>% 
        filter(.category != "No") %>% 
        select(-.epred) %>% 
        distinct(..., aoa)
    
    return(aoas)
    
}


# get posterior predictions for single predictor
get_posterior_predictions <- function(object, x, step = 0.1, ...){
    
    # unquote variable name
    x <- as.character(ensyms(x))
    
    # get ellipsis "..." arguments and check if re_formula has been provided
    ellipsis_arguments <- as.list(substitute(list(...)))[-1L]
    is_arg_re_formula <- isTRUE("re_formula" %in% names(ellipsis_arguments))
    
    # get random effects and exclude them if re_formula is NA
    groups <- unique(object$ranef$group)
    vars <- colnames(object[["data"]])
    vars <- vars[!(vars %in% object$formula$resp)]
    if (!(x %in% vars)) stop("x is not a variable in object data")
    if (is_arg_re_formula){
        if (is.na(ellipsis_arguments[["re_formula"]])){
            vars <- vars[!(vars %in% groups)]
        }
    }
    
    # build data frame with vector of interest and zeroes in all other variables
    x_range <- range(object[["data"]][[x]])
    x_seq <- seq(x_range[1], x_range[2], step)
    x_df <- as.data.frame(matrix(0, length(x_seq), length(vars)))
    colnames(x_df) <- vars
    x_df[[x]] <- x_seq
    
    # get posterior predictions from data frame and model
    preds <- add_epred_draws(
        newdata = x_df, 
        object = object,
        ...
    ) %>% 
        # get cumulative probabilities for production
        pivot_wider(
            names_from = .category,
            values_from = .epred
        ) %>% 
        mutate(Understands = Understands + `Understands and Says`) %>% 
        pivot_longer(
            c(No, Understands, `Understands and Says`),
            names_to = ".category",
            values_to = ".epred"
        ) %>% 
        # get curves of interest
        filter(.category %in% c("Understands", "Understands and Says")) %>% 
        # more informative labels
        mutate(
            .category = case_when(
                .category=="Understands" ~ "Comprehension",
                .category=="Understands and Says" ~ "Production"
            )
        )
    
    return(preds)
}


