#### 04_prepare: Prepare data for analysis ################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ###############################################

# load packages
library(data.table) # for importing data
library(dplyr)      # for manipulating data
library(tidyr)      # for rehsaping datasets
library(readxl)     # for importing Excel spreadsheets
library(mice)       # for imputing data
library(purrr)      # for working with lists
library(here)       # for locating files

# create functions
source(here("Code", "R", "functions.R"))

# set params
age_bins <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "30-32")

#### import data #########################################
pool <- read_xlsx(here("Data", "01_pool.xlsx")) %>%
    mutate(include = as.logical(include)) %>%
    filter(include) %>%
    drop_na(cognate_rater1)

dat_familiarity <- fread(here("Data", "03_familiarity.csv"), quote = '"', na.strings = c("", "NA"))

#### aggregate data ######################################
dat_aggregated <- dat_familiarity %>%
    group_by(meaning, item, type, age_bin, language, item_dominance, frequency, lp, cognate_rater1) %>%
    summarise(n = n(),
              successes = sum(response, na.rm = TRUE),
              failures = sum(n, -successes, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(proportion = successes/n) %>%
    arrange(meaning, item_dominance, lp, age_bin)


#### filter data ########################################
dat_filtered <- dat_aggregated %>%
    filter(item %in% pool$item,
           lp != "Other",
           age_bin %in% age_bins,
           item_dominance == "L1") %>%
    right_join(expand(., item, age_bin, item_dominance, lp),
               by = c("item", "age_bin", "item_dominance", "lp")) %>%
    select(-item_dominance)

#### impute data ########################################
dat_imputed <- dat_filtered 
#mice(data = .,
#    method = "norm",
#     m = 5,
#     maxit = 5, 
#     seed = 888) %>%
#complete() %>%
#as_tibble() %>%
#mutate_at(vars(n, successes, failures), round, 1)

#### export data ########################################
fwrite(dat_imputed, here("Data", "04_prepared.csv"), sep = ",", row.names = FALSE)
