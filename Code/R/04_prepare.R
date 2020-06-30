#### 04_prepare: Prepare data for analysis ################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ###############################################

# load packages
library(data.table) # for importing data
library(tibble)     # more more informative data frames
library(dplyr)      # for manipulating data
library(mice)       # for imputing data
library(purrr)      # for working with lists
library(here)       # for locating files

# create functions
source(here("Code", "R", "functions.R"))

# set params
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

#### import data #########################################
dat_familiarity <- fread(here("Data", "03_familiarity.csv"), quote = '"', na.strings = c("", "NA")) %>% 
    rowwise() %>%
    mutate(prop = prod(successes, 1/n, na.rm = TRUE))  %>%
    filter(n >= 4) %>%
    ungroup()

#### impute data ########################################
dat_imputed <- dat_familiarity %>%
    mice(data = .,
         method = "norm",
         m = 5,
         maxit = 5, 
         seed = 888) %>%
    complete() %>%
    as_tibble() %>%
    ungroup()

#### export data ########################################
fwrite(dat_imputed, here("Data", "04_prepared.csv"), sep = ",", row.names = FALSE)

