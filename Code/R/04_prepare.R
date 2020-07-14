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
    filter(n >= 4,
           class %in% c("noun", "verb")) %>%
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

#### visualise data #######################################
ggplot(dat_imputed, aes(bilingualism, proportion, group = cognate, colour = cognate, fill = cognate)) +
    facet_grid(item_dominance~type) +
    stat_summary(fun = "mean", geom = "point", size = 0.5, na.rm = TRUE, alpha = 0.5) +
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, 2)) +
    labs(x = "Bilingualism", y = "Proportion", colour = "Cognateness", fill = "Cognateness") +
    scale_colour_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme_custom +
    theme(legend.title = element_blank(),
          legend.position = "top") +
    ggsave(here("Figures", "03_familiarity.png"), height = 8, width = 10)
