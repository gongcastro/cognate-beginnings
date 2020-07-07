#### 03_vocabulary: vocabulary sizes #####################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(tidyverse)        # for nice data frames
library(magrittr)      # for using pipes
library(data.table)    # for importing data
library(lubridate)     # for working with dates
library(readxl)        # for importing Excel spreadsheets
library(here)          # for locating files

# load/create functions
source(here("R", "functions.R"))

# set params
bins          <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")
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
         doe = ifelse(dominance=="Spanish", doe_spanish, doe_catalan),
         lp = ifelse(!between(doe, 50, 100, incbounds = TRUE), "Other", lp),
         lp = factor(lp, levels = c("Monolingual", "Bilingual")),
         age_bin = factor(cut(age, breaks = breaks, labels = bins), levels = bins, ordered = TRUE)) %>%
  rowwise() %>% 
  mutate(bilingualism = ifelse(dominance %in% "Spanish",
                               1-abs((doe_spanish-doe_catalan)/(doe_spanish+doe_catalan)),
                               1-abs((doe_catalan-doe_spanish)/(doe_spanish+doe_catalan)))) %>% 
  ungroup() %>% 
  filter(lp %in% c("Monolingual", "Bilingual"),
         age_bin %in% bins_interest) %>%
  select(-response) %>%
  pivot_longer(c(understands, produces), names_to = "type", values_to = "response") %>%
  mutate(type = ifelse(type=="understands", "Comprehensive", "Productive")) %>%
  arrange(item, lp, item_dominance, age_bin) %>%
  # aggregate data
  group_by(item, lp, bilingualism, age_bin, item_dominance, type) %>%
  summarise(n = sum(!is.na(response)),
            successes = sum(response, na.rm = TRUE),
            proportion = mean(response, na.rm = TRUE),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(age_bin, lp, item_dominance, age_bin) %>%
  left_join(pool, by = "item") %>%
  rename(cognate = cognate_rater1) %>%
  mutate_at(vars(include, cognate), ~as.logical(as.numeric(.))) %>%
  filter(include) %>%
  drop_na(cognate, age_bin) %>%
  select(-c(survey, version, ipa, source, label, A, B, C, D, include, cognate_expert, cognate_rater2, agreement, comments)) %>%
  mutate(cognate = case_when(cognate ~ "Cognate", !cognate ~ "Non-cognate", TRUE ~ NA_character_))

#### export data #############################################
fwrite(dat, here("Data", "03_familiarity.csv"), sep = ",", dec = ".", row.names = FALSE)

#### visualise data ##########################################
ggplot(dat, aes(age_bin, proportion, group = cognate, colour = cognate, fill = cognate)) +
  facet_grid(item_dominance~type~lp) +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3)) +
  #stat_summary(fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5, na.rm = TRUE) +
  #stat_summary(fun = "mean", geom = "line", na.rm = TRUE) +
  labs(x = "Age (months)", y = "Proportion", colour = "Cognateness", fill = "Cognateness") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_custom +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  ggsave(here("Figures", "03_familiarity.png"), height = 8, width = 10)
  
# by lang profile
ggplot(dat, aes(bilingualism, proportion, group = cognate, colour = cognate, fill = cognate)) +
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
