#### 01_logs: Explore logs #################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ################################

# load packages
library(dplyr)
library(tidyr)
library(lubridate)
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
           lp = ifelse(!data.table::between(doe, 50, 100, incbounds = TRUE), "Other", lp),
           lp = factor(lp, levels = c("Monolingual", "Bilingual"))) %>%
    rowwise() %>% 
    mutate(bilingualism = ifelse(dominance %in% "Spanish",
                                 1-(abs((doe_spanish-doe_catalan)/(doe_spanish+doe_catalan))),
                                 1-(abs((doe_catalan-doe_spanish)/(doe_spanish+doe_catalan))))) %>% 
    drop_na(lp) %>% 
    filter(completed, age_bin %in% bins_interest) 

#### export data ##########################
fwrite(dat, here("Data", "01_participants.csv"), sep = ",", dec = ".", row.names = FALSE)

#### visualise data #######################
# distribution across ages
samples <- dat %>% 
    group_by(lp, dominance, age_bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(age_bin, lp) %>%
    mutate(y = ifelse(dominance=="Catalan", sum(n), n))

dat %>%
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

# distribution across profiles
dat %>%
    ggplot(aes(x = age_bin, y = bilingualism)) +
    stat_bin_2d(binwidth = 0.10, na.rm = TRUE) +
    labs(x = "Age (months)", y = "Bilingualism score\n(DoE1-DoE2)/(DoE1+DoE2)",
         fill = "Participants") +
    theme_custom +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          axis.text = element_text(colour = "black"),
          panel.grid = element_blank()) +
    ggsave(here("Figures", "01_logs-distribution.png"), height = 4, width = 5)
    
