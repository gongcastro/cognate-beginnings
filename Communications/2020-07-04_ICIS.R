#### 2020-07-06_ICIS: Figures and results for ICIS poster #####
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ####################################################

# load packages
library(tidyverse)
library(data.table)
library(modelr)
library(lubridate)
library(janitor)
library(tidybayes)
library(ggmcmc)
library(here)

# load functions
source(here("Code", "R", "functions.R"))

# set params
bins <- c("14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")
params_interest <- c("b_mid_frequency", "b_mid_item_dominance1", "b_mid_cognate1", "b_mid_item_dominance1:cognate1")
#### load data ##################################################

# processed data
dat <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    arrange(meaning, age_bin, lp) %>%
    filter(type=="Comprehensive") %>%
    select(meaning, lp, age_bin, cognate, item_dominance, proportion, frequency) 

# participant data
samples <- list.files(here("Data", "Logs"), pattern = "logs", full.names = TRUE) %>%
    .[!str_detect(., "summary")] %>%
    last() %>%
    fread(na.strings = "") %>%
    mutate_at(vars(date_sent, time_stamp), as_date) %>%
    mutate_at(vars(id_db), factor) %>%
    mutate_at(vars(age_bin), factor, levels = bins, ordered = TRUE) %>%
    mutate(version = factor(version, levels = c("CBC", "DevLex", "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D", "BL-Long-1", "BL-Long-2"), ordered = TRUE)) %>%     filter(completed, lp %!in% "Other", age_bin %in% bins) %>% 
    filter(completed) %>%
    group_by(lp, dominance, age_bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(age_bin, lp) %>%
    mutate(y = ifelse(dominance=="Catalan", sum(n), n)) %>%
    filter(lp %in% "Bilingual", age_bin %in% bins_interest)

# model fit
fit0 <- readRDS(here("Results", "comp_fit0.rds"))
fit1 <- readRDS(here("Results", "comp_fit1.rds"))

#### participants ################################################
samples %>%
    ggplot(aes(x = age_bin, y = n, fill = dominance)) +
    geom_col() +
    geom_text(data = samples, aes(x = age_bin, y = y, label = n, colour = dominance),
              position = position_nudge(y = 3), size = 3) +
    labs(x = "Age (months)", y = "Number of partcipants",
         colour = "Language of highest exposure",
         fill = "Language of highest exposure",
         title = "Distribution of participants across ages and profiles",
         subtitle = "Only participants that completed the questionnaire are included") +
    scale_y_continuous(labels = as.integer) +
    scale_fill_manual(values = c("#44546A", "#F7C033")) +
    scale_colour_manual(values = c("black", "white")) +
    theme_custom +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "#FDE699"),
          panel.background = element_rect(fill = "white"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
          axis.text.x = element_text(size = 7)) +
    ggsave(here("Communications", "2020-07-04_ICIS_logs.png"))

#### posterior ####################################
posterior <- ggs(fit1) %>%
    clean_names() %>%
    filter(iteration >= 1000) %>%
    mutate(chain = as.factor(chain)) %>%
    mutate(parameter = as.character(parameter)) %>%
    mutate(parameter_lab = case_when(parameter=="b_asym_Intercept" ~ "Asymptote (Intercept)",
                                     parameter=="b_steep_Intercept" ~ "Steepness (Intercept)",
                                     parameter=="b_mid_Intercept" ~ "Mid-point (Intercept)",
                                     parameter=="b_phi_Intercept" ~ "\U03C6",
                                     parameter=="zoi" ~ "ZOI",
                                     parameter=="coi" ~ "COI",
                                     parameter=="b_mid_frequency" ~ "Frequency\n(Centered)",
                                     parameter=="b_mid_item_dominance1" ~ "Item dominance\n(L1/L2, centered)",
                                     parameter=="b_mid_cognate1" ~ "Cognateness \n(NC/C, centered)",
                                     parameter=="b_mid_item_dominance1:cognate1" ~ "Item dominance \U000D7 Cognateness",
                                     parameter=="sd_meaning__mid_Intercept" ~ "Meaning: \U03F9",
                                     TRUE ~ "Meaning: Intercept"),
           parameter_lab = factor(parameter_lab, ordered = TRUE))

#### convergence ###################################
ggplot(posterior, aes(iteration, value, colour = chain)) +
    facet_wrap(~parameter_lab, scales = "free", nrow = 6) +
    geom_line(alpha = 0.5) +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    scale_color_brewer(palette = "Blues") +
    theme(legend.position = "right",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          legend.box.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "#FDE699"),
          panel.background = element_rect(fill = "white"),
          axis.title.y = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 5),
          axis.text.x = element_blank()) +
    ggsave(here("Communications", "2020-07-04_ICIS_convergence.png"), height = 6, width = 6)

#### coefficients ################################################
intervals <- posterior %>% 
    filter(parameter %in% params_interest) %>%
    mutate(parameter = as.character(parameter)) %>%
    group_by(parameter_lab) %>%
    median_qi(value, .width = c(0.89, 0.66, 0.50)) 

posterior %>%
    filter(parameter %in% params_interest) %>%
    ggplot(aes(x = value, y = parameter_lab)) +
    stat_slabh(fill = "#44546A") + 
    geom_intervalh(data = intervals, position = position_nudge(y = -0.20), size = 1.5, show_point = TRUE, point_size = 1, point_colour = "black") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Value", y = "Parameter",
         fill = "Dominance", colour = "Credible Interval",
         title = "Sampling the posterior of the predictors' coefficients",
         subtitle = "Negative coefficients decrease age of acquisition\nPositive coefficients increase age of acquisition") +
    scale_colour_brewer(palette = "YlOrBr") +
    scale_fill_manual(values = c("#F7C033", "#44546A")) +
    theme_custom +
    theme(legend.position = "right",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          panel.grid.major.y = element_line(colour = "grey"),
          plot.background = element_rect(fill = "#FDE699"),
          panel.background = element_rect(fill = "white"),
          axis.title.y = element_blank()) +
    ggsave(here("Communications", "07_analysis_comp-coefs.png"), height = 3)

#### predictive posterior checks ########################################
posterior_check_L1 <- expand_grid(age_bin = as.numeric(factor(unique(dat$age_bin), levels = bins, ordered = TRUE)),
                                  cognate = c("Non-cognate", "Cognate"),
                                  item_dominance = c("L2", "L1"),
                                  frequency = seq_range(dat$frequency, 4),
                                  meaning = unique(dat$meaning)) %>%
    add_fitted_draws(model = fit1, n = 10, value = "proportion", scale = "linear") %>%
    ungroup() %>%
    mutate(frequency = cut(frequency, breaks = seq_range(dat$frequency, 5), labels = paste("Frequency:", c("Q1", "Q2", "Q3", "Q4")), include.lowest = TRUE),
           .draw = as.character(.draw))

posterior_check_L2 <- expand_grid(age_bin = unique(dat$age_bin),
                                  cognate = c(-0.5, 0.5),
                                  frequency = seq_range(dat$frequency, 4)) %>%
    add_fitted_draws(model = fit1_L2, n = 100, value = "proportion", scale = "linear") %>%
    ungroup() %>%
    mutate(cognate = ifelse(cognate==-0.5, "Non-cognate", "Cognate"),
           frequency = cut(frequency, breaks = seq_range(dat$frequency, 5), labels = paste("Frequency:", c("Q1", "Q2", "Q3", "Q4")), include.lowest = TRUE),
           .draw = as.character(.draw))

posterior_check <- list("L1" = posterior_check_L1, "L2" = posterior_check_L2) %>%
    bind_rows(.id = "dominance") %>%
    mutate(lp = ifelse(is.na(lp), "Bilingual", lp),
           lp = factor(lp, levels = c("Monolingual", "Bilingual")))

ggplot(posterior_check, aes(age_bin, proportion, colour = cognate, fill = cognate, group = cognate)) +
    facet_wrap(lp~dominance) +
    stat_lineribbon(.width = c(0.89), alpha = 0.25, colour = NA) +
    stat_summary(fun = "mean", geom = "line") +
    labs(x = "Age (months)", y = "Proportion", fill = "Cognate", colour = "Cognate") +
    scale_x_continuous(breaks = seq(0, 11, by = 1), labels = bins) +
    scale_colour_manual(values = c("#F7C033", "#44546A")) +
    scale_fill_manual(values = c("#F7C033", "#44546A")) +
    theme_custom +
    theme(panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
          legend.position = "top",
          legend.text = element_text(size = 7),
          plot.background = element_rect(fill = "#FDE699"),
          panel.background = element_rect(fill = "white"),
          legend.title = element_text(size = 8),
          axis.text.x = element_text(size = 7, angle = 300)) +
    ggsave(here("Communications", "07_analysis_posterior-checks-poster.png"))


