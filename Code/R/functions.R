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

# logarithmic curve
log_curve <- function(x, u, k, mid){ {{ u }}/(1+exp(-{{ k }}*({{ x }} - {{ mid }} )))}

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