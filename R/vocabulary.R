#### vocabulary: Analyse vocabulary sizes ######################################

#### set up ####################################################################

# load packages
library(tidyverse)
library(multilex)
library(data.table)
library(brms)
library(tidybayes)
library(here)

# set params
set.seed(888)

#### import data ###############################################################
participants <- ml_participants("gonzalo.garciadecastro@upf.edu")
responses <- ml_responses(participants)
logs <- ml_logs(participants, responses)
vocabulary <- ml_vocabulary(participants, responses)

dat <- logs %>% 
    filter(completed,
           str_detect(version, "BL-")) %>% 
    select(id, time, sex, age, doe_catalan, doe_spanish) %>% 
    left_join(vocabulary) %>% 
    mutate(bilingualism = case_when(doe_catalan > doe_spanish ~ doe_spanish/100,
                                    doe_spanish > doe_catalan ~ doe_catalan/100,
                                    TRUE ~ 0.5),
           sex = ifelse(sex=="Male", -0.5, 0.5),
           vocab_type = ifelse(vocab_type=="understands", 0, 1),
           id = as.numeric(as.factor(id))) %>% 
    mutate_at(vars(sex, vocab_type), as.factor) %>% 
    mutate_at(vars(age, bilingualism), function(x) scale(x, center = TRUE, scale = FALSE)[,1])

fwrite(dat, here("Data", "vocabulary.csv"), sep = ",", dec = ".", row.names = FALSE)

#### fit models ################################################################


priors <- c(prior(normal(0, 1), class = "Intercept"),
            prior(normal(0, 0.5), class = "b"))

fit0 <- make_stancode(
    vocab_count | trials(vocab_n) ~ vocab_type,
    data = dat,
    family = binomial("logit"),
    prior = priors,
    save_model = here("Stan", "vocabulary0.stan")
)
fit1 <- make_stancode(
    vocab_count | trials(vocab_n) ~ vocab_type*age,
    data = dat,
    family = binomial("logit"),
    prior = priors,
    save_model = here("Stan", "vocabulary1.stan")
)
fit2 <- make_stancode(
    vocab_count | trials(vocab_n) ~ vocab_type*age*bilingualism,
    data = dat,
    family = binomial("logit"),
    prior = priors,
    save_model = here("Stan", "vocabulary2.stan")
)


#### model comparison ##########################################################
loo0 <- loo(fit0, moment_match = TRUE)
loo1 <- loo(fit1, moment_match = TRUE)
loo2 <- loo(fit2, moment_match = TRUE)
loo_comp <- loo_compare(loo0, loo1, loo2)

#### examine posterior #########################################################
posterior_dist <- gather_draws(fit2, `b_.*`, regex = TRUE) %>% 
    mutate(.chain = as.factor(.chain))

# traceplots
ggplot(posterior_dist, aes(.iteration, .value, colour = .chain)) +
    facet_wrap(~.variable, scales = "free_y") +
    geom_line() +
    scale_colour_manual(values = c("#543005", "#BF812D", "#80CDC1", "#01665E")) +
    theme_minimal() +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA),
          legend.title = element_text(face = "bold"),
          legend.position = "top") 

# distributions
ggplot(posterior_dist, aes(.value)) +
    facet_wrap(~.variable, scales = "free_x") +
    stat_halfeye(show.legend = FALSE) +
    #scale_fill_brewer(palette = "BrBG") +
    theme_minimal() +
    labs(x = "Iteration", y = "Value", fill = "Chain") +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA),
          legend.title = element_text(face = "bold"),
          legend.position = "top") 

# prior predictive distribution
posterior_preds <- expand.grid(type = c("Comprehensive", "Productive"),
                               age_center = seq(min(dat$age_center),
                                                max(dat$age_center),
                                                by = 0.1),
                               item_dominance = c("L1", "L2"),
                               bilingualism_center = seq(min(dat$bilingualism_center),
                                                         max(dat$bilingualism_center),
                                                         by = 0.05),
                               n = 1) %>% 
    add_fitted_draws(fit3, n = 50) %>% 
    mean_qi()

ggplot(posterior_preds, aes(age_center, bilingualism_center, fill = .value)) +
    facet_wrap(~type) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c(option = "plasma") +
    
    #scale_fill_distiller(palette = "YlOrRd") +
    scale_x_continuous(breaks = seq(-13, 17, by = 5), labels = seq(10, 40, by = 5)) +
    scale_y_continuous(breaks = seq(-0.23, 0.27, by = 0.1), labels = seq(0, 0.5, by = 0.1)) +
    theme_minimal() +
    labs(x = "Age (months)", y = "DoE to L2 (%)", fill = "Vocabulary\nsize (%)") +
    theme(legend.position = "right", 
          panel.grid = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA)) +
    ggsave(here("Figures", "vocabulary-heatmap.png"))



