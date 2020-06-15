#### 05_wordbank: compute priors from Worbank data
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(data.table)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(stringr)
library(janitor)
library(ggplot2)
library(ggridges)
library(broom)
library(modelr)
library(wesanderson)
library(patchwork)
library(here)

# load functions
source(here("R", "functions.R"))
inv_logit <- function(x) 1 / (1 + exp(-x))

# set params
set.seed(888)

#### load data ############################################

dat <- list.files(here("Data", "Wordbank"), full.names = TRUE) %>%
  map(., fread) %>%
  set_names(str_remove(list.files(here("Data", "Wordbank")), ".csv")) %>%
  bind_rows(.id = "language") %>%
  as_tibble() %>%
  pivot_longer(6:ncol(.), names_to = "age", values_to = "proportion") %>%
  mutate(age = as.numeric(age)) %>%
  filter(between(age, 16, 30))

#### linear least squares
fit_l     <- lm(proportion ~ age, dat)
s_l       <- summary(fit_l)
dat_fit_l <- expand_grid(age = unique(dat$age)) %>% mutate(fit = predict(fit_l, newdata = .))
coefs_l   <- tidy(fit_l, conf.int = 0.95) %>% clean_names()

#### non-linear least squares ##############################
fit_nl <- nls(proportion ~ SSlogis(age, asym, mid, steep), dat)
s_nl <- summary(fit_nl)
dat_fit_nl <- expand_grid(age = unique(dat$age)) %>% mutate(fit = predict(fit_nl, newdata = .))
coefs_nl <- tidy(fit_nl, conf.int = 0.95) %>% clean_names()

#### compare models
dat_fit <- bind_rows(dat_fit_l, dat_fit_nl, .id = "type") %>%
  mutate(type = ifelse(type==1, "Linear", "Non-linear"))
residuals <- dat %>% 
  drop_na() %>%
  mutate(residual_l = (s_nl$residuals-mean(s_l$residuals))/sd(s_l$residuals),
         residual_nl = (s_nl$residuals-mean(s_nl$residuals))/sd(s_nl$residuals)) %>%
  pivot_longer(contains("residual"), names_to = "model", values_to = "residual") %>%
  mutate(model = ifelse(model=="residual_l", "Linear", "Non-linear"))

#### visualise data #######################################

# visualise model fit
x_mid <- coefs_nl$estimate[coefs_nl$term=="mid"]
y_mid <- dat_fit_nl$fit[which.min(abs(dat_fit$age-x_mid))]
steepness <- coefs_nl$estimate[coefs_nl$term=="steep"]
asymptote <- coefs_nl$estimate[coefs_nl$term=="asym"]

ggplot(dat, aes(x = age, y = proportion)) +
  geom_segment(x = x_mid, xend = x_mid, y = 0, yend = y_mid, colour = "blue", size = 1) +
  annotate(geom = "point", x = x_mid, y = y_mid, colour = "blue", size = 5) +
  annotate(geom = "text", label = paste0("Mid point = ", round(x_mid, 2), "months"), x = mid_sim+1.5, y = 0.25, colour = "blue", size = 4, hjust = 0) +
  geom_segment(x = -Inf, xend = Inf, y = asymptote, yend = asymptote, colour = "red") +
  annotate(geom = "text", label = paste0("Asymptote = ", round(asymptote, 2)),
           x = min(dat$age), y = asym-0.3, colour = "red", size = 4, hjust = 0) +
  annotate(geom = "segment", x = x_mid-1.25, xend = x_mid+1.2, y = y_mid+0.005, yend = y_mid+0.15,
           colour = "green", size = 1, arrow = arrow(ends = "both", length = unit(0.2, units = "cm"))) +
  annotate(geom = "text", label = paste0("Steepness = ", round(steepness, 2)), x = mid_sim-0.6, y = y_mid+0.02, colour = "green", size = 4, hjust = 0, angle = 22.5) +
  geom_line(data = dat_fit, aes(y = fit, colour = type), size = 1) +
  stat_summary(aes(group = language),
               fun = "mean", geom = "line", na.rm = TRUE,
               size = 0.5, alpha = 0.5, colour = "grey") +
  stat_summary(fun = "mean", geom = "point", na.rm = TRUE,
               size = 3, colour = "black", shape = 21) +
  labs(x = "Age (months)", y = "Predicted proportion",
       title = "Non-linear (Logarithmic) vs. linear model",
       subtitle = "Each grey line is one language",
       caption = "Data extracted from Wordbank (http://wordbank.stanford.edu/)") +
  scale_x_continuous(limits = c(min(dat$age, na.rm = TRUE), max(dat$age, na.rm = TRUE)), breaks = seq(min(dat$age), max(dat$age))) +
  scale_colour_grey(start = 0, end = 0.45) +
  scale_fill_grey(start = 0, end = 0.45) +
  theme_custom +
  theme(legend.position = c(0.8, 0.15),
        legend.title = element_blank()) +
  ggsave(here("Figures", "05_wordbank.png"), height = 5)

#### export data ###########################
fwrite(dat, here("Data", "05_wordbank.csv"), sep = ",", row.names = FALSE)
fwrite(coefs_nl, here("Data", "05_wordbank-priors.csv"), sep = ",", row.names = FALSE)
