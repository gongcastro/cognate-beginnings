# 00_items: Import statistics of words in pool ##########
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################

# load packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(data.table)    # for importing and exporting data
library(readxl)        # for importing Excel files
library(here)          # for locating files

# load/create functions
source(here("Code", "R", "functions.R"))

#### import data ########################################

# import pool
dat <- read_xlsx(here("Data", "01_pool.xlsx")) %>%
  mutate(frequency = as.numeric(frequency),
         meaning = as.character(meaning),
         cognate_rater1 = ifelse(cognate_rater1==1, "Cognate", "Non-cognate")) %>%
  drop_na(language, cognate_rater1)

#### visualise data #####################################

dat_freqdiff <- dat %>%
  select(item, meaning, language, frequency, cognate_rater1) %>%
  pivot_wider(id_cols = c(cognate_rater1, meaning), names_from = language,
              values_from = frequency,
              names_sep = "_") %>%
  unnest(cols = c(Catalan, Spanish)) %>%
  rowwise() %>%
  mutate(frequency_diff = abs(Catalan-Spanish)) %>%
  pivot_longer(cols = c(Spanish, Catalan), names_to = "language", values_to = "frequency")

ggplot(dat_freqdiff, aes(x = language, y = frequency, colour = frequency_diff, alpha = frequency_diff)) +
  facet_wrap(~cognate_rater1) +
  geom_violin(fill = "black", colour = NA, alpha = 0.4, na.rm = TRUE) +
  geom_line(aes(group = meaning), na.rm = TRUE, size = 0.5, alpha = 0.5) +
  geom_boxplot(width = 0.05, na.rm = TRUE, colour = "black") +
  labs(x = "Language", y = "Frequency (Zipf score)", colour = "Absolute difference") +
  scale_alpha(guide = FALSE) +
  theme_custom +
  theme(legend.position = "top") +
  ggsave(here("Figures", "00_pool_freq_cognateness.png"))
