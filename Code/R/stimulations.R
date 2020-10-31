#### simulate: simulate data to check priors and degub Stan code ###############

#### set up ####################################################################

# load packages
library(tidyverse)
library(rstan)
library(here)

# set params

#### create data ###############################################################
log_curve <- function(x, Asym = 1, xmid = 24, scal = 1){
    y <- Asym / (1 + exp((xmid - x) / scal))
    return(y)
}

dat <- expand_grid(x = seq(12, 34, by = 0.1), item = as.factor(1:10)) %>%
    group_by(item) %>% 
    mutate(xmid = rnorm(1, 21, 3)) %>% 
    ungroup() %>% 
    mutate(y = log_curve(x, xmid = xmid))

#### fit model #################################################################
dat_stan <- list(N = nrow(dat),
                 J = length(unique(dat$item)),
                 i = dat$item,
                 age = dat$x,
                 X = model.matrix(y~1, dat))
fit <- stan(here("Code", "Stan", "fit.stan"), data = dat_stan, chains = 1)


#### visualise data ############################################################
ggplot(dat, aes(x, y, group = item)) +
    geom_line() +
    labs(x = "Age (months)", y = "P(understands|age)") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"))
