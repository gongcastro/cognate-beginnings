#### responses: analyse responses data #########################################

#### set up ####################################################################

# load packages
library(tidyverse)
library(data.table)
library(brms)
library(tidybayes)
library(here)

# set params
bins <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
bins_interest <- bins[-c(1, length(bins))]
options(loo.cores = 4, mc.cores = 4)

##### import data and set params ###############################################
dat <- fread(here("Data", "responses.csv"), na.string = c("", "NA")) %>% 
    as_tibble() %>% 
    mutate(age_bin = as.numeric(factor(age_bin, levels = bins_interest, ordered = TRUE))-1,
           age = scale(age, center = TRUE, scale = FALSE)[,1],
           bilingualism = bilingualism/100) %>%
    mutate_at(vars(cognate, item_dominance), as.factor) %>%
    #mutate(response = factor(response, ordered = TRUE)) %>% 
    arrange(item, te, age_bin) %>% 
    select(id, age, bilingualism, item, te, item_dominance, cognate, response) %>% 
    filter(te %in% sample(unique(.$te), 15))

contrasts(dat$cognate) <- c(0.5, -0.5) 
contrasts(dat$item_dominance) <- c(0.5, -0.5) 

#### fit model #################################################################
priors <- c(prior(normal(0.5, 0.5), class = Intercept),
            prior(normal(0.5, 0.1), class = b))
            
fit0 <- brm(
    response ~ age,
    data = dat,
    family = sratio(link = "logit"),
    prior = priors,
    save_model = here("Code", "Stan", "responses_0.stan"),
    file = here("Results", "fit0.rds"),
    seed = 888, cores = 4
)

fit1 <- update(
    fit0, . ~ . -age + age*item_dominance,
    newdata = dat,
    prior = priors,
    file = here("Results", "fit1.rds"),
    save_model = here("Code", "Stan", "responses_1.stan"),
    seed = 888, cores = 4
)

fit2 <- update(
    fit1, . ~ . -age*item_dominance + age*item_dominance*bilingualism,
    newdata = dat,
    prior = priors,
    file = here("Results", "fit2.rds"),
    save_model = here("Code", "Stan", "responses_2.stan"),
    seed = 888, cores = 4
)

fit3 <- update(
    fit1, . ~ . -age*item_dominance*bilingualism + age*item_dominance*bilingualism*cognate,
    newdata = dat,
    prior = priors,
    file = here("Results", "fit3.rds"),
    save_model = here("Code", "Stan", "responses_3.stan"),
    seed = 888, cores = 4
)

# model comparison
loos <- loo(fit0, fit1, fit2, fit3)

# rescale coeffients to get probabilities
fitted(fit3)[1, , ] %>% 
    round(digits = 2) %>% 
    t()

#### examine posterior #########################################################
post <- gather_draws(fit3, `b_.*`, regex = TRUE) %>% 
    mutate(.chain = as.factor(.chain))

ggplot(post, aes(x = .iteration, y = .value, colour = .chain)) +
    facet_wrap(~.variable, scales = "free_y") +
    geom_line() +
    scale_colour_manual(values = c("#543005", "#BF812D", "#80CDC1", "#01665E")) +
    theme_minimal() +
    labs(x = "Iteration", y = "Value", colour = "Chain") +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "top")

ggplot(post, aes(.value, .variable)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slab(show.legend = FALSE) +
    labs(x = "Value", y = "Probability density", fill = "Parameter") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA),
          legend.title = element_text(face = "bold"),
          legend.position = "top")    

post_preds <- expand_grid(age = seq(min(dat$age, na.rm = TRUE), max(dat$age, na.rm = TRUE), by = 0.5),
                          item_dominance = c("L1", "L2"),
                          bilingualism =  c(0, 0.5),
                          cognate = c("Cognate", "Non-cognate")) %>% 
    add_fitted_draws(., fit3, n = 10, re_formula = NA, scale = "response") %>% 
    mutate(bilingualism = ifelse(bilingualism==0, "Monolingual", "Bilingual"),
           .category = case_when(.category==1 ~ "No",
                                 .category==2 ~ "Understands",
                                 .category==3 ~ "Understands & Produces"))
ggplot(post_preds, aes(age, .value, colour = interaction(bilingualism, cognate, sep = "-"))) +
    facet_grid(item_dominance~.category) +
    #stat_lineribbon(.width = 0.95, alpha = 0.5)
    #geom_line(aes(group = interaction(.draw, bilingualism, cognate)),  alpha = 0.5) +
    stat_summary(aes(fill = interaction(bilingualism, cognate, sep = "-")),
                 fun.data = mean_qi, geom = "ribbon", colour = NA, alpha = 0.5) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    scale_fill_brewer(palette = "Dark2") +
    scale_colour_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(-14, 17, by = 5), labels = seq(10, 40, by = 5)) +
    labs(x = "Age (months)", y = "P(N,U,US|Age)") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA),
          legend.title =element_blank(),
          legend.position = "top",
          text = element_text(size = 15)) +
    ggsave(here("Figures", "responses-multinomial.png"))

#### estimated AOAs ############################################################
aoa <- gather_draws(fit1, r_te[te, param]) %>% 
    pivot_wider(names_from = param, values_from = .value) %>%
    mutate(Intercept = Intercept + fixef(fit1)[1,1],
           age_bin = age_bin+fixef(fit1)[2,1]) %>% 
    rowwise() %>% 
    mutate(.value = -Intercept/age_bin) %>% 
    ungroup() %>% 
    select(te, .value) %>% 
    group_by(te) %>% 
    #mean_qi() %>% 
    left_join(distinct(dat, te, cognate)) 

ggplot(aoa, aes(as.factor(te), .value, colour = cognate)) +
    stat_pointinterval() +
    coord_flip()
