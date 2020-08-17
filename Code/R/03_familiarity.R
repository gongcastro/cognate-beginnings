#### 03_vocabulary: vocabulary sizes #####################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(dplyr)        # for nice data frames
library(tidyr)
library(magrittr)      # for using pipes
library(data.table)    # for importing data
library(lubridate)     # for working with dates
library(readxl)        # for importing Excel spreadsheets
library(here)          # for locating files

# load/create functions
source(here("Code", "R", "functions.R"))


# set params
bins          <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")
breaks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 100)

#### import data ############################################
pool <- read_xlsx(here("Data","01_pool.xlsx")) %>%
  rename(te = meaning) %>% 
  select(-c(survey, version, ipa, source, label, A, B, C, D, cognate_expert, cognate_rater2, agreement, comments)) %>% 
  rename(cognate = cognate_rater1) %>%
  mutate_at(vars(include, cognate), ~as.logical(as.numeric(.)))

  

dat <- fread(here("Data", "02_merged.csv"), header = TRUE, stringsAsFactors = FALSE, na.strings = "") %>%
  as_tibble() %>%
  mutate_at(vars(time_stamp), as_date) %>%
  mutate_at(vars(id_db), as.character) %>%
  mutate(dominance = case_when(id_db %in% "54469" ~ "Spanish",
                               id_db %in% "57157" ~ "Catalan",
                               TRUE ~ dominance),
         doe_catalan = case_when(id_db %in% "54469" ~ 0,
                                 id_db %in% "57157" ~ 80,
                                 id_db %in% "57046" ~ 50,
                                 TRUE ~ as.numeric(doe_catalan)),
         doe_spanish = case_when(id_db %in% "57046" ~ 50,
                                 TRUE ~ as.numeric(doe_spanish))) %>% 
  mutate(version = factor(version, levels = c("CBC", "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D", "BL-Long-1", "BL-Long-2", "DevLex"), ordered = TRUE)) %>%
  drop_na(response) %>%
  mutate(response = case_when(response == 1 ~ "no", response == 2 ~ "understands", response == 3 ~ "produces", TRUE ~ NA_character_),
         understands = response %in% c("understands", "produces"),
         produces = response %in% "produces",
         doe1 = ifelse(dominance=="Spanish", doe_spanish, doe_catalan),
         doe2 = ifelse(dominance=="Spanish", doe_catalan, doe_spanish),
         lp = case_when(!data.table::between(doe1, 40, 100, incbounds = TRUE) ~ "Other",
                        !data.table::between(doe2, 0, 50, incbounds = TRUE) ~ "Other",
                        !between(doe1 + doe2, 90, 100, incbounds = TRUE) ~  "Other",
                        TRUE ~ lp),
         age_bin = factor(cut(age, breaks = breaks, labels = bins), levels = bins, ordered = TRUE)) %>%
  filter(lp %in% c("Monolingual", "Bilingual"),
         age_bin %in% bins_interest) %>%
  select(-response) %>% 
  pivot_longer(c(understands, produces), names_to = "type", values_to = "response") %>%
  mutate(type = ifelse(type=="understands", "Comprehensive", "Productive")) %>%
  arrange(item, lp, item_dominance, age_bin) %>%
  filter(!(type=="Productive" & version=="DevLex")) %>% 
  # aggregate data
  group_by(item, lp, age_bin, item_dominance, type) %>%
  summarise(n = sum(!is.na(response)),
            successes = sum(response, na.rm = TRUE),
            proportion = mean(response, na.rm = TRUE),
            logodds = log10((successes+0.5)/(n-successes+0.5)),
            probability = 1/(1+exp(-logodds)),
            weights = (1/(successes+0.5))+(1/(n-successes+0.5)),
            .groups = "drop") %>%
  arrange(age_bin, item_dominance, age_bin) %>%
  left_join(pool, by = "item") %>%
  filter(include) %>%
  select(-include) %>% 
  mutate(cognate = case_when(cognate ~ "Cognate", !cognate ~ "Non-cognate", TRUE ~ NA_character_)) %>% 
  drop_na(cognate, age_bin) 

#### export data #############################################
fwrite(dat, here("Data", "03_familiarity.csv"), sep = ",", dec = ".", row.names = FALSE)

#### visualise data ##########################################
dat %>%
  mutate(lp_cog = case_when(cognate %in% "Cognate" & lp %in% "Bilingual" ~ "BIL - Cognate",
                            cognate %in% "Non-cognate" & lp %in% "Bilingual" ~ "BIL - Non-cognate",
                            cognate %in% "Cognate" & lp %in% "Monolingual" ~ "MON - Cognate",
                            cognate %in% "Non-cognate" & lp %in% "Monolingual" ~ "MON - Non-cognate")) %>% 
  ggplot(aes(age_bin, proportion, colour = lp_cog, group = lp_cog)) +
  facet_grid(item_dominance~type) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, size = 0.75) +
  stat_summary(fun = "mean", geom = "point", shape = 1,  na.rm = TRUE) +
  labs(x = "Age (months)", y = "Proportion", colour = "Cognateness", fill = "Cognateness") +
  scale_colour_manual(values = c("red", "orange", "navy", "blue")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        legend.position = "top") +
  ggsave(here("Figures", "03_familiarity.png"), height = 5)
  