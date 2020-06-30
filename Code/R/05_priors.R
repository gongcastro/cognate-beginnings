#### 05_wordbank: compute priors from Worbank data
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(data.table)
library(tidyverse)
library(janitor)
library(ggridges)
library(broom)
library(modelr)
library(patchwork)
library(here)

# load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins <- c("< 16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "> 40")
breaks <- c(0, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 100)
bins_interest <- c("16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36")


#### load data ############################################
dat <- list.files(here("Data", "Wordbank"), full.names = TRUE) %>%
  map(., fread) %>%
  set_names(str_remove(list.files(here("Data", "Wordbank")), ".csv")) %>%
  bind_rows(.id = "language") %>%
  as_tibble() %>%
  pivot_longer(6:ncol(.), names_to = "age", values_to = "proportion") %>%
  mutate(age = as.numeric(age),
         age_bin = factor(cut(age, breaks = breaks, labels = bins), levels = bins_interest),
         age_bin = as.numeric(age_bin)-1) %>%
  drop_na(age_bin)

#### non-linear least squares ##############################
fit <- nls(proportion ~ SSlogis(age_bin, asym, mid, steep), dat)
s <- summary(fit)
dat_fit <- expand_grid(age_bin = unique(dat$age_bin)) %>% mutate(fit = predict(fit, newdata = .))
coefs <- tidy(fit, conf.int = 0.95) %>%
  clean_names() %>%
  mutate(estimate_scaled = estimate*2+16) %>%
  relocate(term, estimate, estimate_scaled)

#### visualise data #######################################

# visualise model fit
x_mid <- coefs$estimate[coefs$term=="mid"]
y_mid <- dat_fit$fit[which.min(abs(dat_fit$age_bin-x_mid))]
steepness <- coefs$estimate[coefs$term=="steep"]
asymptote <- coefs$estimate[coefs$term=="asym"]

ggplot(dat, aes(x = age_bin, y = proportion)) +
  
  geom_line(data = dat_fit, aes(y = fit), size = 1, colour = "black") +
  stat_summary(aes(group = language),
               fun = "mean", geom = "line", na.rm = TRUE,
               size = 0.5, alpha = 0.40, colour = "orange") +
  stat_summary(fun = "mean", geom = "point", na.rm = TRUE,
               size = 3, colour = "black", shape = 21) +
  
  # asymptote
  geom_segment(x = -Inf, xend = Inf, y = asymptote, yend = asymptote, linetype = "dotted") +
  annotate(geom = "text", label = paste0("Asymptote = ", round(asymptote, 2)),
           x = min(dat$age_bin), y = asymptote+0.025, size = 4, hjust = 0) +
  
  # steepness
  annotate(geom = "segment", x = x_mid-1.25, xend = x_mid+1.2, y = y_mid+0.005, yend = y_mid+0.275,
           size = 1, arrow = arrow(ends = "both", length = unit(0.2, units = "cm"))) +
  annotate(geom = "text", label = paste0("Steepness = ", round(steepness, 2)),
           x = x_mid-1.15, y = y_mid+0.1, size = 4, hjust = 0, angle = 40) +
  
  # mid-point
  geom_segment(x = x_mid, xend = x_mid, y = 0, yend = y_mid , linetype = "dashed", size = 1) +
  annotate(geom = "point", x = x_mid, y = y_mid, size = 5) +
  annotate(geom = "text", label = paste0("Mid-point = ", round(x_mid, 2), " months"),
           x = x_mid+0.15, y = 0.15, size = 4, hjust = 0) +
  labs(x = "Age bin", y = "Predicted proportion",
       title = "Computing priors from monolinguals in Wordbank",
       caption = "Data extracted from Wordbank (http://wordbank.stanford.edu/) [1].\nOrange lines represent the mean for each of the 16 languages.") +
  scale_colour_grey(start = 0, end = 0.45) +
  scale_fill_grey(start = 0, end = 0.45) +
  scale_x_continuous(limits = range(dat$age_bin, na.rm = TRUE)) +
  theme_custom +
  theme(legend.position = c(0.8, 0.15),
        legend.title = element_blank(),
        plot.title = element_text(size = 14),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major.y = element_line(colour = "grey"),
        axis.title.y = element_blank()) +
  ggsave(here("Figures", "05_priors-wordbank.png"), width = 5, height = 4)

#### export data ###########################
fwrite(coefs, here("Data", "05_priors-wordbank.csv"), sep = ",", row.names = FALSE)
