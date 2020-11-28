#### functions: Constant functions and objects ###########
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu 
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### functions ###########################################

# load packages
library(dplyr)
library(rlang)
library(ggplot2)

# evaluate if x is included in y
"%!in%" <- function(x, y) !(x %in% y)

# logistic curve
inv_logit <- function(x) 1 / (1 + exp(-x))

# logit to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# get posterior draws
get_posterior_draws <- function(model, min_iter = 1000) {
  require(brms)
  require(dplyr)
  require(tibble)
  require(tidyr)
  as.mcmc({{ model }}) %>%
    map(., as_tibble) %>%
    bind_rows(.id = "chain")  %>%
    pivot_longer(cols = -chain, names_to = "coefficient", values_to = "value") %>%
    rownames_to_column("iteration") %>%
    filter(iteration >= {{ min_iter }})
}

# set custom theme for plots
theme_custom <- theme(panel.background = element_rect(fill = "transparent"),
                      panel.grid = element_blank(),
                      panel.border = element_rect(fill = "transparent", colour = "grey"),
                      plot.background = element_rect(fill = "transparent"),
                      text = element_text(size = 12, colour = "black"),
                      axis.text = element_text(colour = "black"),
                      axis.line = element_line(colour = "grey"),
                      legend.background = element_rect(fill = "transparent"),
                      legend.key = element_rect(colour = "transparent", fill = "transparent")
)

pred_labels <- function(x){
  require(dplyr)
  case_when({{ x }} %in% "b_asym_Intercept" ~ "Asymptote (Intercept)",
            {{ x }} %in% "b_steep_Intercept" ~ "Steepness (Intercept)",
            {{ x }} %in% "b_mid_Intercept" ~ "Mid-point (Intercept)",
            {{ x }} %in% "b_mid_bilingualism" ~ "Bilingualism (Slope)",
            {{ x }} %in% "b_mid_cognate1" ~ "Cognateness (Slope)",
            {{ x }} %in% "b_mid_bilingualism:cognate1" ~ "Bilingualism \U000D7 Cognateness",
            {{ x }} %in% "sd_meaning__mid_Intercept" ~ "SD TE (Intercept)",
            {{ x }} %in% "sd_meaning__mid_cognate1" ~ "SD TE-Cognateness (Slope)",
            
            {{ x }} %in% "phi_Intercept" ~ "\U03C6 (Intercept)",
            {{ x }} %in% "b_phi_Intercept" ~ "\U03C6 (Slope)",
            {{ x }} %in% "zoi" ~ "ZOI",
            {{ x }} %in% "coi" ~ "COI")
}
