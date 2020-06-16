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
bins <- c("18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32")

#### import data #########################################
dat_familiarity <- fread(here("Data", "03_familiarity.csv"), quote = '"', na.strings = c("", "NA")) %>% 
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins, ordered = TRUE)),
           lp = ifelse(lp=="Monolingual", -0.5, 0.5),
           cognate = ifelse(cognate=="Cognate", 0.5, -0.5),
           frequency = scale(frequency)[,1])

#### impute data ########################################
dat_imputed <- dat_familiarity %>%
    mice(data = .,
         method = "norm",
         m = 5,
         maxit = 5, 
         seed = 888) %>%
    complete() %>%
    as_tibble() 

#### export data ########################################
fwrite(dat_imputed, here("Data", "04_prepared.csv"), sep = ",", row.names = FALSE)
