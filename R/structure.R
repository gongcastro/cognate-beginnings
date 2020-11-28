#### structure: Vocabulary structure ###########################################

#### set up #####################################################################

# load packages
library(tidyverse)
library(data.table)
library(here)

# create/load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

##### import data and set params ###############################################
dat <- fread(here("Data", "preprocessed.csv"), na.string = c("", "NA")) %>% 
    as_tibble() %>% 
    filter(type %in% "Comprehensive") %>% 
    mutate(cognate = as.factor(cognate),
           item_dominance = as.factor(item_dominance),
           response = as.numeric(response),
           age_group = case_when(age_bin %in% c("18-20", "20-22") ~ "20 months",
                                 age_bin %in% c("22-24", "24-26") ~ "24 months",
                                 age_bin %in% c("28-30", "30-32") ~ "30 months",
                                 TRUE ~ NA_character_),
           age_group = as.factor(age_group)) %>%
    drop_na(age_group) %>% 
    select(item, te, id, category, item_dominance, cognate, age_group, response) %>% 

contrasts(dat$cognate) <- c(0.5, -0.5) 
contrasts(dat$item_dominance) <- c(0.5, -0.5)
contrasts(dat$age_group) <- c(-0.5, 0, 0.5)

#### matrix ####################################################################
probs <- dat %>% 
    filter(item_dominance %in% "L1") %>% 
    group_by(te, age_group, category) %>% 
    summarise(n = n(),
              y = sum(response, na.rm = TRUE),
              .groups = "drop") %>% 
    rowwise() %>% 
    mutate(lik = list(dbinom(y, prob = seq(0.01, 0.99, by = 0.01), size = n, log = TRUE))) %>% 
    mutate(max_lik = seq(0.01, 0.99, by = 0.01)[which.max(lik)]) %>% 
    ungroup() %>% 
    group_by(age_group) %>% 
    arrange(category, te) %>% 
    mutate(te_index = as.character(row_number())) %>% 
    ungroup() %>% 
    select(age_group, category, te_index, max_lik) 
    

mat <- probs %>% 
    rename(te = te_index) %>% 
    mutate(te2 = te) %>% 
    expand(te, te2) %>% 
    left_join(probs, by = c("te" = "te_index")) %>% 
    left_join(probs, by = c("te2" = "te_index", "age_group", "category")) %>% 
    drop_na() %>% 
    rename(p = max_lik.x,
           p2 = max_lik.y) %>% 
    rowwise() %>% 
    mutate(p_joint = p*p2) %>% 
    ungroup() %>% 
    mutate_at(vars(te, te2), as.numeric) %>% 
    pivot_wider(names_from = "te", values_from = "p_joint") %>% 
    

#### visualise data ############################################################
ggplot(mat, aes(te, te2, fill = p_joint)) +
    facet_grid(age_group~category, scales = "free") +
    geom_tile() +
    scale_fill_distiller(palette = "Greys", direction = 1) +
    theme(axis.text = element_blank())
    

y <- probs$y
n <- probs$n

opt <- optim(par = 0.5, fn = NLL, method = "Brent", lower = 0.00001, upper = 0.99999, y = y)
ml <- exp(-opt$value)

hist(y, xlim = c(0, 10), freq = F)
curve(dbinom(x, prob = opt$par, size = 10), add = TRUE, from = 0, to = 10, n = 11)
