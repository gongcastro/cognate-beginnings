#### Analysis: Analyse vocabulary data ##########################################

#### set up #####################################################################

# load packages
library(dplyr)
library(tibble)
library(tidyr)
library(ggdist)
library(broom.mixed)
library(data.table)
library(forcats)
library(nlme)
library(modelr)
library(janitor)
library(here)

# create/load functions
source(here("Code", "R", "functions.R"))

# set params
set.seed(888)
bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34")

##### import data and set params #######################################################################################3
dat <- fread(here("Data", "preprocessed.csv"), na.string = c("", "NA")) %>% 
    as_tibble() %>% 
    select(item, type, te, category, age_bin, item_dominance, cognate, frequency, n, successes, proportion) %>%
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1) %>% 
    mutate_at(vars(cognate, item_dominance), as.factor) %>% 
    arrange(item, te, age_bin) 

contrasts(dat$cognate) <- contr.sum(c("Non-cognate", "Cognate"))/2
contrasts(dat$item_dominance) <- contr.sum(c("L2", "L1"))/2

dat_comp <- dat %>% filter(type=="Comprehensive") %>% groupedData(proportion ~ 1|te, .)
dat_prod <- dat %>% filter(type=="Productive") %>% groupedData(proportion ~ 1|te, .)

dat_priors <- fread(here("Data", "05_priors-wordbank.csv")) %>%
    mutate(estimate_scaled = (estimate_scaled-14)/2)
inits <- c(fixed = c(Asym = 0.7631182, xmid = 4.3694351, scal = 1.6859966))

#### comprehensive data ##################################################################################################

# fit models
fit0_comp <- nlme(proportion ~ SSlogis(age_bin, Asym, xmid, scal),
                  data = dat_comp,
                  fixed = list(Asym ~ 1, xmid ~ 1, scal ~ 1),
                  random = xmid~1|te,
                  start = inits,
                  control = nlmeControl(msMaxIter = 100))

fit1_comp <- update(fit0_comp, fixed = list(Asym ~ 1, xmid ~ 1 + frequency, scal ~ 1),
                    start = c(inits, frequency = 0.1))

fit2_comp <- update(fit0_comp, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance, scal ~ 1),
                    random = xmid~item_dominance|te,
                    start = c(inits, frequency = 0.1, item_dominance1 = 0.1))

fit3_comp <- update(fit0_comp, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance + cognate, scal ~ 1),
                    random = xmid~item_dominance|te,
                    start = c(inits, frequency = 0.1, item_dominance1 = 0.1, cognate1 = 0.1))

fit4_comp <- update(fit0_comp, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance*cognate, scal ~ 1),
                    random = xmid~item_dominance|te,
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
                  start = inits,
                  control = nlmeControl(msMaxIter = 100))

fit1_prod <- update(fit0_prod, fixed = list(Asym ~ 1, xmid ~ 1 + frequency, scal ~ 1),
                    start = c(inits, frequency = 0.1))

fit2_prod <- update(fit0_prod, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance, scal ~ 1),
                    random = xmid~item_dominance|te,
                    start = c(inits, frequency = 0.1, item_dominance = 0.1))

# this model does not converge successfully, so let's try a Zero-correlation parameter model
fit3_prod <- update(fit1_prod, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance+cognate, scal ~ 1),
                    random = xmid~item_dominance|te,
                    start = c(inits, frequency = 0.1, item_dominance1 = 0.1, cognate1 = 0.1))

fit3_zero_prod <- update(fit1_prod, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance+cognate, scal ~ 1),
                         random = pdDiag(form = xmid~item_dominance),
                         start = c(inits, frequency = 0.1, item_dominance1 = 0.1, cognate1 = 0.1))

fit4_prod <- update(fit1_prod, fixed = list(Asym ~ 1, xmid ~ 1 + frequency + item_dominance*cognate, scal ~ 1),
                    random = xmid~item_dominance|te,
                    start = c(inits, frequency = 0.1, item_dominance1 = 0.1, cognate1 = 0.1, `item_dominance:cognate` = 0.1))

# compare models
selection_prod <- anova(fit0_prod, fit1_prod, fit2_prod, fit3_zero_prod, fit4_prod)
anova_prod <- anova(fit4_prod)
summary_prod <- summary(fit4_prod)

#### predictions ##################################

# comprehension predictions
comp_asym <- summary_comp$coefficients$fixed["Asym"]
comp_scal <- summary_comp$coefficients$fixed["scal"]
comp_xmid_intercept <- summary_comp$coefficients$fixed["xmid.(Intercept)"]
comp_xmid_frequency <- summary_comp$coefficients$fixed["xmid.frequency"]
comp_xmid_item_dominance <- summary_comp$coefficients$fixed["xmid.item_dominance1"]
comp_xmid_cognate <- summary_comp$coefficients$fixed["xmid.cognate1"]
comp_xmid_item_dominance_cognate <- summary_comp$coefficients$fixed["xmid.item_dominance1:cognate1"]

comp_preds <- expand_grid(cognate = c(-0.5, 0.5),
                          item_dominance = c(-0.5, 0.5),
                          frequency = mean(dat$frequency, na.rm = TRUE),
                          age_bin = seq_range(dat_prod$age_bin, n = 100)) %>% 
    rowwise() %>% 
    mutate(xmid = comp_xmid_intercept + frequency*comp_xmid_frequency + item_dominance*comp_xmid_item_dominance + cognate*comp_xmid_cognate + (item_dominance*cognate)*comp_xmid_item_dominance_cognate,
           proportion = comp_asym/(1+exp((xmid-age_bin)/comp_scal))) %>% 
    ungroup() %>% 
    mutate(cognate = ifelse(cognate==-0.5, "Non-cognate", "Cognate"),
           item_dominance = ifelse(item_dominance==-0.5, "L2", "L1"))


# production predictions
prod_asym <- summary_prod$coefficients$fixed["Asym"]
prod_scal <- summary_prod$coefficients$fixed["scal"]
prod_xmid_intercept <- summary_prod$coefficients$fixed["xmid.(Intercept)"]
prod_xmid_frequency <- summary_prod$coefficients$fixed["xmid.frequency"]
prod_xmid_item_dominance <- summary_prod$coefficients$fixed["xmid.item_dominance1"]
prod_xmid_cognate <- summary_prod$coefficients$fixed["xmid.cognate1"]
prod_xmid_item_dominance_cognate <- summary_prod$coefficients$fixed["xmid.item_dominance1:cognate1"]

prod_preds <- expand_grid(cognate = c(-0.5, 0.5),
                          item_dominance = c(-0.5, 0.5),
                          frequency = mean(dat$frequency, na.rm = TRUE),
                          age_bin = seq_range(dat_prod$age_bin, n = 100)) %>% 
    rowwise() %>% 
    mutate(xmid = prod_xmid_intercept + frequency*prod_xmid_frequency + item_dominance*prod_xmid_item_dominance + cognate*prod_xmid_cognate + (item_dominance*cognate)*prod_xmid_item_dominance_cognate,
           proportion = prod_asym/(1+exp((xmid-age_bin)/prod_scal))) %>% 
    ungroup() %>% 
    mutate(cognate = ifelse(cognate==-0.5, "Non-cognate", "Cognate"),
           item_dominance = ifelse(item_dominance==-0.5, "L2", "L1"))

#### random effects ##############################################

# comprehension random effects
rancoefs_comp <- summary(fit4_comp)$coefficients$random$te %>%
    as_tibble() %>% 
    mutate(te = as.character(unique(dat_comp$te))) %>% 
    relocate(te) %>% 
    clean_names() %>% 
    rowwise() %>% 
    mutate(xmid_intercept = comp_xmid_intercept + xmid_intercept,
           xmid_item_dominance1 = comp_xmid_item_dominance + xmid_item_dominance1)

# production random effects
rancoefs_prod <- summary(fit4_prod)$coefficients$random$te %>% 
    as_tibble() %>% 
    mutate(te = as.character(unique(dat_prod$te))) %>% 
    relocate(te) %>% 
    clean_names() %>% 
    mutate(xmid_intercept = xmid_intercept,
           xmid_item_dominance1 = xmid_item_dominance1)

# joint random effects
rancoefs <- bind_rows(rancoefs_comp, rancoefs_prod, .id = "type") %>%
    mutate(type = ifelse(type==1, "Comprehensive", "Productive")) %>% 
    left_join(distinct(mutate(dat, te = as.character(te)), te, type, cognate))


#### ages of acquisition ###################################

# comprehensive AoAs
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

# productive AoAs
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

# joint AoAs
aoas <- bind_rows(aoa_comp, aoa_prod, .id = "type") %>% 
    mutate(type = ifelse(type==1, "Comprehensive", "Productive")) %>% 
    pivot_longer(c(L1, L2), names_to = "item_dominance", values_to = "xmid")