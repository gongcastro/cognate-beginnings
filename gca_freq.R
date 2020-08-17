#### Analysis: Analyse vocabulary data ##########################################

#### set up #####################################################################

# load packages
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(patchwork)
library(ggdist)
library(broom.mixed)
library(data.table)
library(nlme)
library(lme4)
library(bayestestR)
library(BayesFactor)
library(janitor)
library(here)

# create/load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

##### import data and set params #######################################################################################3
dat_comp <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    filter(type=="Comprehensive") %>% 
    select(item, type, te, category, age_bin, item_dominance, cognate, frequency, n, successes, proportion, probability, logodds, weights) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1) %>% 
    mutate_at(vars(cognate, item_dominance), as.factor) %>% 
    arrange(item, te, age_bin) %>%
    groupedData(proportion ~ 1|te, .)

dat_prod <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    filter(type=="Productive") %>% 
    select(item, type, te, category, age_bin, item_dominance, cognate, frequency, n, successes, proportion, probability, logodds, weights) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1) %>% 
    mutate_at(vars(cognate, item_dominance), as.factor) %>% 
    arrange(item, te, age_bin) %>% 
    groupedData(proportion ~ 1|te, .)

contrasts(dat_comp$cognate) <- contr.sum(c("Non-cognate", "Cognate"))/2
contrasts(dat_comp$item_dominance) <- contr.sum(c("L2", "L1"))/2
contrasts(dat_prod$cognate) <- contr.sum(c("Non-cognate", "Cognate"))/2
contrasts(dat_prod$item_dominance) <- contr.sum(c("L2", "L1"))/2

dat_priors <- fread(here("Data", "05_priors-wordbank.csv")) %>%
    mutate(estimate_scaled = (estimate_scaled-14)/2)
inits <- c(fixed = c(Asym = 0.7631182, xmid = 4.3694351, scal = 1.6859966))

#### comprehensive data ##################################################################################################

# fit models
fit0_comp <- nlme(proportion ~ SSlogis(age_bin, Asym, xmid, scal),
                  data = dat_comp,
                  fixed = list(Asym ~ 1, xmid ~ 1, scal ~ 1),
                  random = xmid~1|te,
                  start = inits)

fit1_comp <- update(fit0_comp, fixed = list(Asym ~ 1, xmid ~ 1 + frequency, scal ~ 1),
                   start = c(inits, frequency = 0.1))

fit2_comp <- update(fit0_comp, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance, scal ~ 1),
                    random = pdDiag(form = xmid~item_dominance),
                    start = c(inits, frequency = 0.1, item_dominance1 = 0.1))

fit3_comp <- update(fit0_comp, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance + cognate, scal ~ 1),
                    random = pdDiag(form = xmid~item_dominance),
                    start = c(inits, frequency = 0.1, item_dominance1 = 0.1, cognate1 = 0.1))

fit4_comp <- update(fit0_comp, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance*cognate, scal ~ 1),
                    random = pdDiag(form = xmid~item_dominance),
                    start = c(inits, frequency = 0.1, item_dominance1 = 1, cognate1 = 1, `item_dominance:cognate` = 1))

# compare models
selection_comp <- anova(fit0_comp, fit1_comp, fit2_comp, fit3_comp, fit4_comp)
anova_comp <- anova(fit4_comp)
summary_comp <- summary(fit4_comp)

# age of acquisition
aoa_comp <- summary_comp$coefficients$random$te %>% 
    as_tibble() %>% 
    mutate(te = unique(as.character(dat_comp$te))) %>% 
    left_join(mutate_all(distinct(dat_comp, item_dominance, cognate, te, .keep_all = TRUE), as.character)) %>% 
    clean_names() %>% 
    mutate(asym_intercept = summary_comp$coefficients$fixed["Asym"],
           scal_intercept = summary_comp$coefficients$fixed["scal"],
           xmid_intercept = summary_comp$coefficients$fixed["xmid.(Intercept)"] + xmid_intercept,
           xmid_item_dominance = summary_comp$coefficients$fixed["xmid.item_dominance1"] + xmid_item_dominance1, 
           xmid_cognate = summary_comp$coefficients$fixed["xmid.cognate1"],
           xmid_item_dominance_cognate = summary_comp$coefficients$fixed["xmid.item_dominance1:cognate1"],
           item_dominance_coded = ifelse(item_dominance=="L1", 0.5, -0.5),
           cognate_coded = ifelse(cognate=="Cognate", 0.5, -0.5)) %>% 
    rowwise() %>% 
    mutate(item_dominance_cognate_coded = item_dominance_coded*cognate_coded,
           asym = asym_intercept,
           scal = scal_intercept,
           xmid = xmid_intercept+(item_dominance_coded*xmid_item_dominance)+(cognate_coded*xmid_cognate)+(item_dominance_cognate_coded*xmid_item_dominance_cognate)) %>% 
    ungroup() %>% 
    select(te, item_dominance, cognate, xmid) %>% 
    pivot_wider(names_from = item_dominance, values_from = xmid) %>% 
    rowwise() %>% 
    mutate(xmid_diff = abs(L2-L1)) %>% 
    ungroup() %>% 
    as.data.frame() %>% 
    mutate(cognate = as.factor(cognate))


#### productive data ##################################################################################################

# fit models
fit0_prod <- nlme(proportion ~ SSlogis(age_bin, Asym, xmid, scal), data = dat_prod,
                  fixed = list(Asym ~ 1, xmid ~ 1, scal ~ 1),
                  random = xmid ~ 1|te,
                  start = inits)

fit1_prod <- update(fit0_prod, fixed = list(Asym ~ 1, xmid ~ 1 + frequency, scal ~ 1),
                    random = pdDiag(form = xmid~item_dominance),
                    start = c(inits, frequency = 0.1))

fit2_prod <- update(fit0_prod, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance, scal ~ 1),
                    random = pdDiag(form = xmid~item_dominance),
                    start = c(inits, frequency = 0.1, item_dominance = 0.1))

fit3_prod <- update(fit1_prod, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance+cognate, scal ~ 1),
                    random = pdDiag(form = xmid~item_dominance),
                    start = c(inits, frequency = 0.1, item_dominance1 = 0.1, cognate1 = 0.1))

fit4_prod <- update(fit1_prod, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance*cognate, scal ~ 1),
                    random = pdDiag(form = xmid~item_dominance),
                    start = c(inits, frequency = 0.1, item_dominance1 = 0.1, cognate1 = 0.1, `item_dominance:cognate` = 0.1))

# compare models
selection_prod <- anova(fit0_prod, fit1_prod, fit2_prod, fit3_prod, fit4_prod)
anova_prod <- anova(fit4_prod)
summary_prod <- summary(fit4_prod)

# age of acquisition
aoa_prod <- summary_prod$coefficients$random$te %>% 
    as_tibble() %>% 
    mutate(te = unique(as.character(dat_comp$te))) %>% 
    left_join(mutate_all(distinct(dat_comp, item_dominance, cognate, te, .keep_all = TRUE), as.character)) %>% 
    clean_names() %>% 
    mutate(asym_intercept = summary_prod$coefficients$fixed["Asym"],
           scal_intercept = summary_prod$coefficients$fixed["scal"],
           xmid_intercept = summary_prod$coefficients$fixed["xmid.(Intercept)"] + xmid_intercept,
           xmid_item_dominance = summary_prod$coefficients$fixed["xmid.item_dominance1"] + xmid_item_dominance1, 
           xmid_cognate = summary_prod$coefficients$fixed["xmid.cognate1"],
           xmid_item_dominance_cognate = summary_prod$coefficients$fixed["xmid.item_dominance1:cognate1"],
           item_dominance_coded = ifelse(item_dominance=="L1", 0.5, -0.5),
           cognate_coded = ifelse(cognate=="Cognate", 0.5, -0.5)) %>% 
    rowwise() %>% 
    mutate(item_dominance_cognate_coded = item_dominance_coded*cognate_coded,
           asym = asym_intercept,
           scal = scal_intercept,
           xmid = xmid_intercept+(item_dominance_coded*xmid_item_dominance)+(cognate_coded*xmid_cognate)+(item_dominance_cognate_coded*xmid_item_dominance_cognate)) %>% 
    ungroup() %>% 
    select(te, item_dominance, cognate, xmid) %>% 
    pivot_wider(names_from = item_dominance, values_from = xmid) %>% 
    rowwise() %>% 
    mutate(xmid_diff = abs(L2-L1)) %>% 
    ungroup() %>% 
    as.data.frame() %>% 
    mutate(cognate = as.factor(cognate))

#### visualise data ################################
bind_rows(aoa_comp, aoa_prod, .id = "type") %>% 
    mutate(type = ifelse(type==1, "Comprehensive", "Productive")) %>% 
    pivot_longer(c(L1, L2), names_to = "item_dominance", values_to = "xmid") %>% 
    ggplot(aes(xmid, fill = item_dominance, colour = item_dominance)) +
    facet_grid(type~cognate) +
    geom_density(alpha = 0.5) +
    geom_rug(alpha = 0.5) +
    labs(x = "Mid-point", y = "Counts", fill = "Item dominance", colour = "Item dominance") +
    theme_bw() +
    scale_fill_brewer(palette = "Set1") + 
    scale_colour_brewer(palette = "Set1") + 
    theme(legend.position = "top") +
    ggsave("aoa.png")



ggplot(coef_rand$te, aes(x = `xmid.(Intercept)`+coef_fix$estimate[2], y = `scal.(Intercept)`+coef_fix$estimate[6], colour = category)) +
    geom_vline(xintercept = coef_fix$estimate[2], linetype = "dashed") +
    geom_hline(yintercept = coef_fix$estimate[6], linetype = "dashed") +
    geom_point(shape = 1) +
    geom_smooth(aes(group = 0), method = "lm", formula = y ~ x, colour = "black") +
    plot_layout(guides = "collect") +
    labs(x = "Mid-point (intercept)", y = "Scale (intercept)", colour = "Category") +
    guides(colour = guide_legend(ncols = 2)) +
    theme_bw() +
    ggsave(here("Figures", "random.png"))



d <- fread(here("Data", "04_prepared.csv")) %>%
    as_tibble() %>% 
    select(item, type, te, category, age_bin, item_dominance, cognate, frequency, proportion) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1) %>% 
    mutate_at(vars(cognate, item_dominance), as.factor) %>% 
    arrange(type, item, te, age_bin) %>%
    mutate(fitted = c(fitted(fit4_comp), fitted(fit4_prod))) %>% 
    ungroup()

ggplot(d, aes(age_bin, proportion, colour = interaction(item_dominance, cognate, sep = " - "), fill = interaction(item_dominance, cognate, sep = " - "))) +
    facet_wrap(~type) +
    stat_summary(aes(y = fitted), fun.data = "mean_cl_boot", geom = "ribbon", colour = NA, alpha = 0.25) +
    stat_summary(aes(y = fitted), fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5) +
    stat_summary(aes(y = fitted), fun = "mean", geom = "line") +
    stat_summary(fun = "mean", geom = "point", shape = 1, size = 3) +
    labs(x = "Age (months)", y = "Proportion", colour = "Cognate-Item dominance", fill =  "Cognate-Item dominance") +
    guides(colour = guide_legend(ncol = 2)) +
    scale_x_continuous(breaks = unique(dat_prod$age_bin), labels = bins_interest, guide = guide_axis(n.dodge = 2)) +
    scale_colour_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme_bw() +
    theme(legend.position = c(0.75, 0.9),
          legend.background = element_rect(fill = NA),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          text = element_text(size = 12),
          axis.text = element_text(colour = "black")) +
    ggsave(here("Figures", "predictions.png"))

                 
                 