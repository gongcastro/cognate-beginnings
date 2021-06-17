#### simulations ---------------------------------------------------------------
library(tidyverse)
library(cmdstanr)
library(bayesplot)

options(mc.cores = 4)
color_scheme_set("brightblue")
bayesplot_theme_set(theme_minimal())
inv_logit <- function(x) 1 / (1 + exp(-x))

# create data ------------------------------------------------------------------
responses  