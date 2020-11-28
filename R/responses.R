#### responses: analyse responses data #########################################

#### set up ####################################################################

# load packages
library(tidyverse)
library(data.table)
library(brms)
library(tidybayes)
library(here)

# set params
options(loo.cores = 4, mc.cores = 4)

##### import data and set params ###############################################
dat <- fread(here("Data", "responses.csv"), na.string = c("", "NA")) %>% 
    as_tibble() %>% 
    mutate(age = scale(age, center = TRUE, scale = FALSE)[,1]) %>%
    mutate_at(vars(cognate, item_dominance), as.factor) %>%
    #mutate(response = factor(response, ordered = TRUE)) %>% 
    arrange(item, te) %>% 
    select(id, age, bilingualism, item, te, item_dominance, cognate, response) 

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
    save_model = here("Stan", "responses0.stan"),
    file = here("Results", "responses0.rds"),
    seed = 888, cores = 4
)

fit1 <- update(
    fit0, . ~ . -age + age*item_dominance,
    newdata = dat,
    prior = priors,
    file = here("Results", "responses1.rds"),
    save_model = here("Stan", "responses_1.stan"),
    seed = 888, cores = 4
)

fit2 <- brm(
    response ~ age*item_dominance*cognate,
    data = dat,
    family = sratio(link = "logit"),
    prior = priors,
    save_model = here("Stan", "responses2.stan"),
    file = here("Results", "responses2.rds"),
    seed = 888, cores = 4
)

# model comparison
loos <- loo(fit0, fit1, fit2, fit3)

#### examine posterior #########################################################
post <- gather_draws(fit2, `b_.*`, regex = TRUE) %>% 
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

ggplot(post, aes(.value)) +
    facet_wrap(~.variable, scales = "free") +
    stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.80, .95, .99), 
                                            labels = scales::percent_format(accuracy = 1))))) +
    labs(x = "Value", y = "Probability density",
         fill = "CrI") +
    scale_fill_brewer(palette = "Oranges", direction = -1,) +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey", colour = NA),
          legend.title = element_text(face = "bold"),
          legend.position = "top") +
    ggsave(here("Figures", "responses-coefs.png"))

post_preds <- expand_grid(
    age = seq(min(dat$age, na.rm = TRUE), max(dat$age, na.rm = TRUE), by = 0.5),
    item_dominance = c("L1", "L2"),
    cognate = c("Cognate", "Non-cognate")
) %>% 
    add_fitted_draws(., fit2, n = 50, re_formula = NA, scale = "response") %>% 
    mutate(
        #bilingualism = ifelse(bilingualism==0, "Monolingual", "Bilingual"),
        .category = case_when(.category==1 ~ "No",
                              .category==2 ~ "Understands",
                              .category==3 ~ "Understands & Produces")
    )
ggplot(post_preds, aes(age, .value,
                       colour = interaction(item_dominance, cognate, lex.order = TRUE, sep = " - "),
                       fill = interaction(item_dominance, cognate, lex.order = TRUE, sep = " - "))) +
    facet_grid(~.category) +
    stat_lineribbon(.width = 0.95, alpha = 0.5) +
    #geom_line(aes(group = interaction(.draw, bilingualism, cognate)),  alpha = 0.5) +
    #stat_summary(fun.data = mean_qi, geom = "ribbon", colour = NA, alpha = 0.5) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    scale_fill_brewer(palette = "Dark2") +
    scale_colour_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(0, 1)) +
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

