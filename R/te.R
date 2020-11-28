#### te: Analyse acquisition of TEs ############################################

#### set up ####################################################################

# load packages
library(tidyverse)
library(multilex)
library(data.table)
library(readxl)
library(brms)
library(tidybayes)
library(here)

# set params
set.seed(888)
options(loo.cores = 4, mc.cores = 4)

#### process data ##############################################################
dat <- fread(here("Data", "responses.csv"), na.strings = "") %>% 
    as_tibble() %>% 
    filter(time==1) %>% 
    left_join(mutate(pool, cognate = ifelse(cognate, "Cognate", "Non-cognate")),
              c("te", "item", "cognate", "language")) %>% 
    mutate(understands = response %in% c(2, 3),
           produces = response %in% 3) %>% 
    drop_na(item_dominance, te, produces, understands) %>% 
    select(id, lp, age, te, item_dominance, understands, produces) %>% 
    pivot_wider(id_cols = c(id, te, lp, age), names_from = item_dominance,
                values_from = c(understands, produces)) 
    mutate_at(vars(understands_L1, understands_L2, produces_L1, produces_L2), unlist)
    summarise(understands = all(understands),
              produces = all(produces),
              .groups = "drop") %>% 
    pivot_longer(c(understands, produces), names_to = "type", values_to = "response") %>% 
    group_by(id, lp, age, type) %>% 
    summarise(te = sum(response, na.rm = TRUE),
              n = n(),
              .groups = "drop") %>% 
    rowwise() %>% 
    mutate(te_prop = te/n) %>% 
    ungroup() %>% 
    mutate(type = str_to_sentence(type),
           type = factor(type, levels = c("Understands", "Produces"), ordered = TRUE)) 

ggplot(dat, aes(age, te_prop, colour = lp)) +
    facet_wrap(~type) +
    geom_point(shape = 1, stroke = 1, size = 2, alpha = 0.5) +
    labs(x = "Age (months)", y = "TEs", colour = "Group") +
    scale_colour_manual(values = c("#BF812D", "#01665E")) +
    theme_minimal() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.title = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA))

