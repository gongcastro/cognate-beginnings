#### priors: Compute priors from Wordbank data #################################

#### set up ####################################################################

# load libraries
library(tidyverse)
library(data.table)
library(brms)
library(tidybayes)
library(here)

# set params
set.seed(888)
bins <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- bins[-c(1, length(bins))]
breaks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 60)

#### import data ###############################################################
dat <- list.files(here("Data", "Wordbank"), full.names = TRUE) %>% 
    map(fread, encoding = "UTF-8") %>% 
    set_names(str_remove_all(list.files(here("Data", "Wordbank")), "wb_|.csv")) %>% 
    bind_rows(.id = "language") %>% 
    as_tibble() %>% 
    pivot_longer(6:26, names_to = "age", values_to = "proportion") %>% 
    mutate(age = as.numeric(age),
           age_bin = cut(age, breaks = breaks, labels = bins, ordered_result = TRUE) %>%
               factor(levels = bins, ordered = TRUE),
           age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1,
           num_item_id = as.factor(num_item_id),
           successes = as.integer(proportion*100),
           n = as.integer(100)) %>% 
    drop_na(age_bin, proportion) %>% 
    group_by(language) %>% 
    filter(num_item_id %in% sample(unique(.$num_item_id), size = 10))

#### fit model #################################################################
fit <- brm(successes | trials(n)  ~ age_bin + (1 + age_bin | language),
           family = binomial("logit"),
           chains = 1,
           file = here("Results", "fitp_wordbank.rds"),
           data = dat)

#### examine posterior #########################################################
post <- gather_draws(fit, `b_.*`, regex = TRUE) 
ggplot(post, aes(.value)) +
    facet_wrap(~.variable, scales = "free") +
    stat_slab()

post_preds_fixed <- expand.grid(age_bin = seq(min(dat$age_bin), max(dat$age_bin), by = 0.1),
                          n = 1) %>% 
    add_fitted_draws(fit, n = 10, re_formula = NA) 
post_preds_group <- expand.grid(age_bin = seq(min(dat$age_bin), max(dat$age_bin), by = 0.1),
                                language = unique(dat$language),
                          n = 1) %>% 
    left_join(distinct(dat, language, num_item_id)) %>% 
    add_fitted_draws(fit, n = 10) %>% 
    mutate(num_item_id = as.factor(num_item_id))
ggplot(post_preds_group, aes(age_bin, .value, colour = language)) +
    stat_summary(data = post_preds_group, fun = "median", geom = "line", size = 0.5, alpha = 0.75) +
    stat_lineribbon(data = post_preds_fixed, .width = 0.95, alpha = 0.5, size = 0,
                    colour = "black", fill = "black", show.legend = FALSE) +
    stat_summary(data = post_preds_fixed, fun = "median", geom = "line", size = 1, colour = "black") +
    labs(x = "Age (months)", y = "p(y|age)", colour = "Language") +
    scale_color_viridis_d() +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))

#### get AoAs ##################################################################
aoa <- spread_draws(fit, b_age_bin, `r_language:num_item_id`[language_item, param]) %>%
    pivot_wider(names_from = "param", values_from =  `r_language:num_item_id`) %>% 
    rowwise() %>% 
    mutate(b_age_bin_adj = b_age_bin + age_bin,
           aoa = -Intercept/b_age_bin_adj) %>% 
    ungroup() %>% 
    separate(language_item, c("language", "num_item_id"), sep = "_")

ggplot(aoa, aes(num_item_id, aoa, fill = language)) +
    facet_wrap(~language, scales = "free") +
    stat_slab() +
    coord_flip() +
    labs(x = "Age (months)", y = "p(y|age)", colour = "Language") +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
