#### 03_vocabulary: vocabulary sizes #####################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(tidyverse)       
library(googlesheets4) # for import Google spreadsheets
library(magrittr)      # for using pipes
library(data.table)    # for importing data
library(lubridate)     # for working with dates
library(readxl)        # for importing Excel spreadsheets
library(janitor)       # for cleaning vairable names
library(patchwork)     # for arranging plots
library(here)          # for locating files

# load/create functions
source(here("Code", "R", "functions.R"))

# set params
bins          <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")
breaks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 100)

#### import data #########################################
pool <- read_xlsx(here("Data","01_pool.xlsx"))
studies <- fread(here("Data", "00_studies.csv")) %>% distinct(q_version, language, q_items) 
logs <- fread(here("Data", "01_participants.csv"))

dat <- fread(here("Data", "02_merged.csv"), header = TRUE, stringsAsFactors = FALSE, na.strings = "") %>%
  as_tibble() %>%
  mutate_at(vars(time_stamp), as_date) %>%
  mutate_at(vars(id_db), as.character) %>%
  left_join(logs) %>% 
  mutate(version = factor(version, levels = c("CBC", "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D", "BL-Long-1", "BL-Long-2", "DevLex"), ordered = TRUE)) %>% 
  left_join(studies, by = c("version" = "q_version", "language")) %>%
  select(-c(id_exp, code)) %>% 
  rename(dominance_doe = dominance) %>%
  drop_na(response) %>%
  mutate(response = case_when(response == 1 ~ "no",
                              response == 2 ~ "understands",
                              response == 3 ~ "produces",
                              TRUE          ~ NA_character_)) %>%
  group_by(id_db, study, version, language, lp, sex, dominance_doe, doe_catalan, doe_spanish, response, q_items, completed) %>%
  summarise(count = n(), age = sample(age, 1), .groups = "drop") %>% 
  ungroup() %>%
  pivot_wider(names_from = "response", values_from = "count") %>%
  rowwise() %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         vocab_total = sum(produces, understands, no, na.rm = TRUE),
         understands = sum(understands, produces, na.rm = TRUE),
         vocab_comp  = prod(understands, 1/vocab_total, na.rm = FALSE),
         vocab_prod  = prod(produces, 1/vocab_total, na.rm = FALSE),
         age_bin     =  factor(cut(age, breaks = breaks, labels = bins), levels = bins, ordered = TRUE),
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
         lp_doe_90 = ifelse(lp_num_doe >= 0.90, "Monolingual", "Bilingual"))


#### export data ##########################################
fwrite(dat, here("Data", "03_vocabulary.csv"), sep = ",", row.names = FALSE)

#### visualise data #######################################

# vocabulary across ages
dat %>%
  filter(lp_doe %in% c("Monolingual", "Bilingual"),
         age_bin %in% bins_interest) %>%
  drop_na() %>%
  ggplot(aes(x = age_bin, y = vocab, colour = lp_doe, fill = lp_doe)) +
  facet_wrap(~type) +
  geom_jitter(size = 0.25, alpha = 0.25, width = 0.1, na.rm = TRUE, show.legend = FALSE) +
  geom_smooth(aes(group = lp_doe), method = "loess", formula = "y ~ x", na.rm = TRUE) +
  labs(x = "Age (months)", y = "Vocabulary size (%)",
       colour = "Language profile", fill = "Language profile") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_custom +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 8, angle = -40),
        legend.title = element_blank()) +
  ggsave(here("Figures", "02_vocabulary.png"), width = 10)






