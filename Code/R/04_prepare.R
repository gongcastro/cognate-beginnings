#### 04_prepare: Prepare data for analysis ################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ###############################################

# load packages
library(data.table) # for importing data
library(tibble)     # more more informative data frames
library(dplyr)      # for manipulating data
library(mice)       # for imputing data
library(here)       # for locating files

# create functions
source(here("Code", "R", "functions.R"))

# set params
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

#### import data #########################################
dat_familiarity <- fread(here("Data", "03_familiarity.csv"), quote = '"', na.strings = c("", "NA")) %>% 
    rowwise() %>%
    mutate(prop = prod(successes, 1/n, na.rm = TRUE))  %>%
    filter(class %in% c("noun", "verb"),
           lp %in% "Bilingual") %>% 
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
ggplot(dat_imputed, aes(age_bin, proportion, colour = cognate, group = cognate)) +
    facet_grid(item_dominance~type) +
    geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, size = 0.75) +
    stat_summary(fun = "mean", geom = "point", shape = 1,  na.rm = TRUE) +
    labs(x = "Age (months)", y = "Proportion", colour = "Cognateness", fill = "Cognateness") +
    scale_colour_manual(values = c("red", "blue")) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 7),
          axis.text.x = element_text(size = 7),
          legend.position = "top") +
    ggsave(here("Figures", "03_familiarity.png"), height = 5)
