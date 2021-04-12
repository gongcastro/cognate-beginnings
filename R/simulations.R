#### simulations ---------------------------------------------------------------
library(tidyverse)
library(cmdstanr)
library(bayesplot)

options(mc.cores = 4)
color_scheme_set("brightblue")
bayesplot_theme_set(theme_minimal())
inv_logit <- function(x) 1 / (1 + exp(-x))

# create data ------------------------------------------------------------------
N_te <- 100
N_participant <- 50
age <- c(10, 36)
dominance <- c("L1", "L2")
cognate <- c("Cognate", "Non-cognate")

d <- full_join(
    full_join(
        data.frame(te = 1:N_te, cognate = sample(cognate, size = N_te, prob = c(0.25, 0.75), replace = TRUE)),
        expand.grid(te = 1:N_te, dominance = dominance
        )
    ),
    full_join(
        expand.grid(participant = 1:N_participant, te = 1:N_te),
        data.frame(participant = 1:N_participant, age = runif(N_participant, age[1], age[2]))
    )
) %>% 
    mutate_at(vars(dominance, cognate), as.factor) %>% 
    relocate(participant, age, te, dominance, cognate) %>% 
    arrange(participant, te) 

contrasts(d$dominance) <- c(0.5, -0.5)
contrasts(d$cognate) <- c(0.5, -0.5)

x <- model.matrix(~age + dominance*cognate, d)

b <- c(
    alpha = -5,
    beta_age = 0.2,
    beta_dominance = 2,
    beta_cognate = 1,
    beta_dominance_cognate = 1
)

mu <- (x %*% b)[,1]
y <- rbinom(n = length(mu), size = 1, prob = inv_logit(mu))
d$y <- y

# plot data --------------------------------------------------------------------
ggplot(d, aes(age, y, colour = interaction(dominance, cognate, sep = " - "))) +
    facet_wrap(~dominance) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    geom_jitter(height = 0.05, shape = 1, alpha = 0.5) +
    labs(x = "Age (months)", y = "Pr(Y|X)", colour = "Dominance") +
    theme_bw() +
    theme(legend.title = element_blank())

# fit model --------------------------------------------------------------------
X <- model.matrix(~age + dominance*cognate, d)
d_stan <- list(
    N = nrow(d),
    J = length(unique(d$te)),
    te = as.numeric(as.factor(d$te)),
    X = X,
    K = ncol(X),
    y = d$understands
)

model <- cmdstan_model(stan_file = "Stan/simulations.stan")
fit <- model$sample(data = d_stan)
diagnose <- fit$cmdstan_diagnose()
summary <- fit$cmdstan_summary()
draws <- fit$draws()
mcmc_areas(draws, regex_pars =  c("beta", "sigma"))
