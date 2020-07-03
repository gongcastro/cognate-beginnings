#### 07_anova-prod: Survival analysis on productive data ##########
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up #######################################################

# load packages
library(tidyverse)
library(data.table)
library(tidybayes)
library(BayesFactor)
library(ggridges)
library(patchwork)
library(see)
library(here)

# load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

#### import data ###################################################
fit1 <- readRDS(here("Results", "prod_fit1.rds"))
dat <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    filter(type=="Productive",
           lp=="Bilingual") %>%
    select(item, meaning, age_bin, item_dominance, cognate, proportion, frequency) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins, ordered = TRUE)),
           item_dominance = factor(item_dominance, levels = c("L2", "L1")),
           frequency = scale(frequency)[,1],
           cognate = factor(cognate, levels = c("Non-cognate", "Cognate"))) %>%
    arrange(item, meaning, age_bin) 

#### extract midpoints ##############################################
midpoints <- fit1 %>%
    spread_draws(b_mid_Intercept, b_mid_item_dominance1, b_mid_frequency, r_meaning__mid[meaning, term]) %>% 
    mutate(mid_L1 = b_mid_Intercept + r_meaning__mid + 0.5*b_mid_item_dominance1 + b_mid_frequency,
           mid_L2 = b_mid_Intercept + r_meaning__mid + -0.5*b_mid_item_dominance1 + b_mid_frequency) %>% 
    median_qi(mid_L1, mid_L2) %>%
    select(-c(term, .interval, .point, .width)) %>%
    ungroup() %>% 
    right_join(dat, by = "meaning") %>%
    rowwise() %>% 
    mutate(mid_diff = mid_L2 - mid_L1) %>% 
    ungroup() %>% 
    distinct(meaning, cognate, mid_L1, mid_L2, mid_diff) %>%
    mutate(cognate = factor(cognate, levels = c("Non-cognate", "Cognate"), ordered = TRUE))
anova <- anovaBF(mid_diff ~ cognate, data = midpoints)

ggplot(midpoints, aes(cognate, mid_diff, fill = cognate)) +
    geom_violin(colour = NA) +
    geom_boxplot(width = 0.1, fill = "white", colour = "black") +
    labs(x = "Cognateness", y = "AoA Difference (months)") +
    scale_fill_manual(values = c("#44546A", "orange")) +
    scale_colour_manual(values = c("#44546A", "orange")) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 14),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot",
          legend.position = "none",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.y = element_line(colour = "grey"),
          axis.title.y = element_blank()) +
    
    midpoints %>%
    pivot_longer(c(mid_L1, mid_L2), names_to = "item_dominance", values_to = "mid") %>%
    mutate(item_dominance = str_remove(item_dominance, "mid_"),
           item_dominance = ifelse(item_dominance == "L1", "Dominant\nlanguage", "Non-dominant\nlanguage")) %>%
    ggplot(aes(x = item_dominance, y = mid, colour = cognate, fill = cognate)) +
    facet_wrap(~cognate, ncol = 2) +
    geom_violin(colour = NA) +
    geom_boxplot(width = 0.1, fill = "white", colour = "black") +
    labs(x = "Item dominance", y = "AoA (months)") +
    scale_fill_manual(values = c("#44546A", "orange")) +
    scale_colour_manual(values = c("#44546A", "orange")) +
    coord_flip() +
    theme_custom +
    theme_custom +
    theme(plot.title = element_text(size = 7),
          plot.title.position = "plot",
          legend.position = "none",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.y = element_line(colour = "grey"),
          axis.title.y = element_blank()) +
    
    plot_layout(nrow = 1) &
    plot_annotation(title = "Difference in Age of Acquisition (AoA) across Translation Equivalents (TEs)") &
    theme(plot.title = element_text(size = 14),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot",
          legend.position = "none",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.y = element_line(colour = "grey"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.title.y = element_blank()) &
    
    ggsave(here("Figures", "08_anova-prod.png"), width = 7, height = 4)


poster #### export results ############################
saveRDS(anova, here("Results", "prod_anova.rds"))
