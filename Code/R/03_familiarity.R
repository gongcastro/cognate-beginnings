#### 03_vocabulary: vocabulary sizes #####################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(tibble)        # for nice data frames
library(googlesheets4) # for import Google spreadsheets
library(magrittr)      # for using pipes
library(data.table)    # for importing data
library(dplyr)         # for manipulating data
library(stringr)       # for working with character strings
library(lubridate)     # for working with dates
library(tidyr)         # for reshaping datasets
library(readxl)        # for importing Excel spreadsheets
library(janitor)       # for cleaning vairable names
library(ggplot2)       # for visualising data
library(patchwork)     # for arranging plots
library(here)          # for locating files

# load/create functions
source(here("R", "functions.R"))

# set params
bins          <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- c("18-20", "20-22", "22-24", "24-26", "26-28", "28-30")
breaks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 100)

#### import data ############################################
pool <- read_xlsx(here("Data","01_pool.xlsx"))

dat <- fread(here("Data", "02_merged.csv"), header = TRUE, stringsAsFactors = FALSE, na.strings = "") %>%
  as_tibble() %>%
  mutate_at(vars(time_stamp), as_date) %>%
  mutate_at(vars(id_db), as.character) %>%
  mutate(version = factor(version, levels = c("CBC", "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D", "BL-Long-1", "BL-Long-2", "DevLex"), ordered = TRUE)) %>%
  drop_na(response) %>%
  mutate(response = case_when(response == 1 ~ "no", response == 2 ~ "understands", response == 3 ~ "produces", TRUE ~ NA_character_),
         understands = response %in% c("understands", "produces"),
         produces = response %in% "produces",
         age_bin = factor(cut(age, breaks = breaks, labels = bins), levels = bins, ordered = TRUE)) %>%
  filter(completed,
         lp %in% c("Monolingual", "Bilingual"),
         age_bin %in% bins_interest) %>%
  group_by(item, lp, item_dominance, age_bin) %>%
  summarise(understands = mean(understands, na.rm = TRUE),
            produces = mean(produces, na.rm = TRUE),
            n_understands = sum(!is.na(understands)),
            n_produces = sum(!is.na(produces)),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(age_bin, lp, item_dominance, age_bin)

#### export data #############################################
fwrite(dat, here("Data", "03_familiarity.csv"), sep = ",", dec = ".", row.names = FALSE)

#### visualise data ##########################################

dat %>%
  pivot_longer(c(understands, produces), names_to = "type", values_to = "response") %>%
  ggplot(aes(age_bin, response)) +
  
