#### 01_logs: Explore logs #################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ################################

# load packages
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(wesanderson)
library(patchwork)
library(data.table)
library(here)

# load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

#### import data ###########################
dat <- list.files(here("Data", "Logs"), pattern = "logs", full.names = TRUE) %>%
    .[!str_detect(., "summary")] %>%
    last() %>%
    fread(na.strings = "") %>%
    as_tibble() %>%
    mutate_at(vars(date_sent, time_stamp), as_date) %>%
    mutate_at(vars(id_db), factor) %>%
    mutate_at(vars(age_bin), factor, levels = bins, ordered = TRUE) %>%
    mutate(version = factor(version, levels = c("CBC", "DevLex", "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D", "BL-Long-1", "BL-Long-2"), ordered = TRUE),
           doe = ifelse(dominance=="Spanish", doe_spanish, doe_catalan),
           lp = ifelse(!between(doe, 50, 100, incbounds = TRUE), "Other", lp),
           lp = factor(lp, levels = c("Monolingual", "Bilingual"))) %>%
    drop_na(lp)
    
#### visualise dates #######################
samples <- dat %>% 
    filter(completed, lp %in% "Bilingual", age_bin %in% bins_interest) %>% 
    group_by(lp, dominance, age_bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(age_bin, lp) %>%
    mutate(y = ifelse(dominance=="Catalan", sum(n), n))

dat %>%
    filter(completed, lp %!in% "Other", age_bin %in% bins_interest) %>%
    ggplot(aes(x = age_bin, fill = dominance)) +
    geom_bar() +
    labs(x = "Age (months)", y = "Number of partcipants",
         fill = "Language of highest exposure") +
    scale_y_continuous(labels = as.integer) +
    scale_fill_manual(values = c("#44546A", "orange")) +
    theme_custom +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          axis.text = element_text(colour = "black"),
          panel.grid.major.y = element_line(colour = "grey", linetype = "dotted")) +
    ggsave(here("Figures", "01_logs-distribution.png"), height = 4, width = 5)
