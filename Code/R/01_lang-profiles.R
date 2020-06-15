#### 00_profiles: language profiles ######################
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
source(here("Code", "R", "functions.R"))

# set params
bins          <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- c("18-20", "20-22", "22-24", "24-26", "26-28", "28-30")
breaks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 100)

### import data #########################################
pool <- read_xlsx(here("Data","01_pool.xlsx"))
studies <- range_read("1ApKe-vwnNvHehKXCKJ1RMGx6KlGpEH7xs0ogM9Loh3s") %>% distinct(q_version, language, q_items) 
logs <- list.files(here("Data", "Logs"), pattern = "logs", full.names = TRUE) %>%
    .[!str_detect(., "summary")] %>%
    last() %>%
    fread(na.strings = "", stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate_at(vars(date_sent, time_stamp), as_date) %>%
    mutate_at(vars(age_bin), factor, levels = bins, ordered = TRUE) %>%
    mutate_at(vars(id_db), as.character) 

#### process data #######################################
dat <- fread(here("Data", "02_merged.csv"), header = TRUE, stringsAsFactors = FALSE, na.strings = "") %>%
    as_tibble() %>%
    mutate_at(vars(time_stamp), as_date) %>%
    mutate_at(vars(id_db), as.character) %>%
    left_join(logs, by = c("id_db", "study", "time_stamp", "age", "sex", "dominance", "code", "version", "lp", "age_bin")) %>%
    mutate(version = factor(version, levels = c("CBC", "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D", "BL-Long-1", "BL-Long-2", "DevLex"), ordered = TRUE)) %>%
    left_join(studies, by = c("version" = "q_version", "language")) %>%
    select(-c(id_exp, code)) %>%
    rename(dominance_doe = dominance) %>%
    drop_na(response) %>%
    mutate(response = case_when(response == 1 ~ "no",
                                response == 2 ~ "understands",
                                response == 3 ~ "produces",
                                TRUE          ~ NA_character_)) %>%
    group_by(id_db, study, version, language, lp, sex, dominance_doe, doe_catalan, doe_spanish, response, q_items) %>%
    summarise(count = n(), age = sample(age, 1), .groups = "drop") %>%
    ungroup() %>%
    pivot_wider(names_from = "response", values_from = "count") %>%
    rowwise() %>%
    mutate(no = ifelse(is.na(no), 0, no),
           vocab_total = sum(produces, understands, no, na.rm = TRUE),
           understands = sum(understands, produces, na.rm = TRUE),
           vocab_comp  = prod(understands, 1/vocab_total, na.rm = FALSE),
           vocab_prod  = prod(produces, 1/vocab_total, na.rm = FALSE),
           age_bin     =  factor(cut(age, breaks = breaks, labels = bins),
                                 levels = bins,
                                 ordered = TRUE),
           completed   = (abs(q_items-vocab_total)/q_items) < 0.05) %>%
    ungroup() %>%
    select(id_db, study, version, language, no, produces, understands, vocab_comp, vocab_prod, vocab_total, q_items, completed, age_bin, lp, dominance_doe, doe_catalan, doe_spanish, sex, age) %>%
    arrange(version, age_bin, id_db, language) %>%
    filter(completed) %>%
    distinct(id_db, language, version, .keep_all = TRUE) %>%
    group_by(id_db, version) %>%
    mutate(n_lang = length(language)) %>%
    filter(n_lang > 1) %>%
    select(-n_lang) %>%
    arrange(id_db, version, language) %>%
    mutate(vocab_comp_cat = vocab_comp[language=="Catalan"],
           vocab_comp_spa = vocab_comp[language=="Spanish"],
           vocab_prod_cat = vocab_prod[language=="Catalan"],
           vocab_prod_spa = vocab_prod[language=="Spanish"]) %>% 
    rowwise() %>%
    mutate(dominance_vocab_comp = ifelse(vocab_comp_cat >= vocab_comp_spa, "Catalan", "Spanish"),
           dominance_vocab_prod = ifelse(vocab_prod_cat >= vocab_prod_spa, "Catalan", "Spanish"),
           lp_num_doe = case_when(dominance_doe=="Catalan" ~ doe_catalan,
                                  dominance_doe=="Spanish" ~ doe_spanish),
           lp_num_vocab_comp = abs(vocab_comp_cat-vocab_comp_spa),
           lp_num_vocab_prod = abs(vocab_prod_cat-vocab_prod_spa)) %>%
    ungroup() %>%
    mutate(lp_num_doe_norm = scale(lp_num_doe),
           lp_num_vocab_comp_norm = scale(lp_num_vocab_comp),
           lp_num_vocab_prod_norm = scale(lp_num_vocab_prod),
           lp_vocab_comp = ifelse(lp_num_vocab_comp > 0.2, "Monolingual", "Bilingual"),
           lp_vocab_prod = ifelse(lp_num_vocab_prod > 0.2, "Monolingual", "Bilingual")) %>%
    rename(lp_doe = lp) %>%
    select(-c(vocab_comp_cat, vocab_comp_spa, vocab_prod_cat, vocab_prod_spa)) %>%
    ungroup() %>%
    pivot_longer(cols = c(vocab_comp, vocab_prod), names_to = "type", values_to = "vocab") %>%
    mutate(type = ifelse(type=="vocab_comp", "Comprehensive", "Productive"),
           lp_num_doe = lp_num_doe/100,
           lp_doe_90 = ifelse(lp_num_doe >= 0.9, "Monolingual", "Bilingual"))

#### visualise data #######################################

# category
dat %>%
    filter(lp_doe %in% c("Monolingual", "Bilingual")) %>%
    ggplot(aes(doe_spanish, doe_catalan, colour = lp_num_doe)) +
    annotate(geom = "rect", xmin = 0, xmax = 25, ymin = 0, ymax = 100, alpha = 0.5, fill = "grey") +
    annotate(geom = "rect", xmin = 25, xmax = 75, ymin = 0, ymax = 100, alpha = 0.5, fill = "transparent") +
    annotate(geom = "rect", xmin = 75, xmax = 100, ymin = 0, ymax = 100, alpha = 0.5, fill = "grey") +
    geom_hline(yintercept = 50) +
    geom_vline(xintercept = 50) +
    geom_point(alpha = 0.5, na.rm = TRUE) +
    labs(x = "Degree of Exposure to Spanish (%)", y = "Degree of Exposure to Catalan (%)",
         colour = "LP") +
    scale_colour_viridis_c() +
    theme_custom


plot_doe_comp <- dat %>%
    distinct(id_db, language, .keep_all = TRUE) %>%
    filter(lp_doe %in% c("Monolingual", "Bilingual"), age_bin %in% bins_interest) %>%
    rename(LP = lp_doe) %>%
    ggplot(aes(lp_num_doe, lp_num_vocab_comp, colour = LP)) +
    geom_point(alpha = 0.5, na.rm = TRUE) +
    geom_vline(xintercept = 0.75, colour = "black", linetype = "dashed") +
    geom_hline(yintercept = median(dat$lp_num_vocab_comp, na.rm = TRUE), colour = "black", linetype = "dashed") +
    labs(x = "Standardized degree of exposure to L1", y = "Standardized absolute difference in\nproductive vocabulary",
         fill = "Density",
         title = "Degree of exposure vs. comprehensive vocabulary") +
    scale_colour_brewer(palette = "Set1") +
    theme_custom +
    theme(title = element_text(size = 10),
          legend.position = "top")


plot_doe_prod <- dat %>%
    distinct(id_db, language, .keep_all = TRUE) %>%
    filter(lp_doe %in% c("Monolingual", "Bilingual"), age_bin %in% bins_interest) %>%
    rename(LP = lp_doe) %>%
    ggplot(aes(lp_num_doe, lp_num_vocab_prod, colour = LP)) +
    geom_point(alpha = 0.5, na.rm = TRUE) +
    geom_vline(xintercept = 0.75, colour = "black", linetype = "dashed") +
    geom_hline(yintercept = median(dat$lp_num_vocab_prod, na.rm = TRUE), colour = "black", linetype = "dashed") +
    labs(x = "Standardized degree of exposure to L1", y = "Standardized absolute difference in\nproductive vocabulary",
         fill = "Density",
         title = "Degree of exposure vs. productive vocabulary") +
    scale_colour_brewer(palette = "Set1") +
    theme_custom +
    theme(title = element_text(size = 10),
          legend.position = "top")


plot_vocab_both <- dat %>%
    distinct(id_db, language, .keep_all = TRUE) %>%
    filter(lp_doe %in% c("Monolingual", "Bilingual"), age_bin %in% bins_interest) %>%
    rename(LP = lp_doe) %>%
    ggplot(aes(lp_num_vocab_comp, lp_num_vocab_prod, colour = LP)) +
    geom_point(alpha = 0.5, na.rm = TRUE) +
    geom_vline(xintercept = median(dat$lp_num_vocab_comp, na.rm = TRUE), colour = "black", linetype = "dashed") +
    geom_hline(yintercept = median(dat$lp_num_vocab_prod, na.rm = TRUE), colour = "black", linetype = "dashed") +
    labs(x = "Standardized absolute difference in\ncomprehensive vocabulary", y = "Standardized absolute difference in\nproductive vocabulary",
         fill = "Density",
         title = "Comprehensive vs. productive vocabulary") +
    scale_colour_brewer(palette = "Set1") +
    theme_custom +
    theme(title = element_text(size = 10),
          legend.position = "top")

(plot_doe_comp / plot_doe_prod / plot_vocab_both) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A",
                    title = "Using different criteria to\nclassify monolinguals and bilinguals",
                    subtitle = "Each plot is a pairwise comparison of each criterion",
                    caption = "Inclusion criteria:\n   - Age: 18-30 months \n   - Completed both the Catalan and Spanish questionnaires \n   - Are not exposed to a 3rd language > 10% of the time") &
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) &
    ggsave(here("Figures", "01_lp.png"), height = 10, width = 5) 






