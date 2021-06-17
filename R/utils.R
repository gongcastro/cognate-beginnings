#### helper functions ----------------------------------------------------------

# inverse logit
inv_logit <- function(x) 1 / (1 + exp(-x))

# discretize a continuous variable into arbitrary quantiles
cut_quantiles <- function(x, quantiles = seq(0.0, 1, 0.2)) {
    cut(x,
        breaks = quantile(x, probs = quantiles), 
        labels = FALSE,
        include.lowest = TRUE
    )
}

# proportion adjusted from boundary values (Gelman, Hill & Vehtari, 2020)
prop_adj <- function(x, n){
    e <- (x+2)/(n+4)
    return(e)
}

# adjusted standard error of proportion (Gelman, Hill & Vehtari, 2020)
prop_adj_se <- function(x, n) {
    e <- (x+2)/(n+4)
    se <- sqrt(e*(1-e)/(n+4))
    return(se)
}


theme_custom <- function(){
    theme(
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey", linetype = "dotted"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        strip.background = element_rect(fill = "grey", colour = NA),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 17.5)
    )
}
