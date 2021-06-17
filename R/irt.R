# IRT

# set up ----
library(tidyverse)
library(multilex)
library(brms)
library(tidybayes)

# import data ----
ml_connect("gonzalo.garciadecastro@upf.edu")
p <- ml_participants()
r <- ml_responses(p, longitudinal = "first", update = FALSE)
l <- ml_logs(p, r)

# items ----
items <- multilex::pool %>%
    drop_na(cognate, ipa) %>% 
    filter(
        include,
        class %in% c("Noun")
    ) %>% 
    rename(frequency = frequency_zipf) %>% 
    select(te, language, category, item, ipa, frequency, cognate) %>% 
    mutate(
        language = str_to_sentence(language),
        cognate = ifelse(cognate, "Cognate", "Non-Cognate")
    ) %>% 
    relocate(te, language, item, cognate, frequency)

# participants ----
participants <- l %>%
    rename(dominant_language = dominance) %>% 
    filter(
        completed,
        lp %in% c("Monolingual", "Bilingual"),
        between(age, 10, 36),
        sum(doe_catalan+doe_spanish)>0.1
    ) %>% 
    pivot_longer(c(doe_catalan, doe_spanish), names_to = "language", values_to = "doe") %>% 
    mutate_at(vars(matches("doe")), function(x) x*0.01) %>% 
    filter(between(doe, 0, 1)) %>% 
    mutate(
        language = str_to_sentence(str_remove(language, "doe_"))
    ) %>% 
    select(id, time_stamp, age, lp, dominant_language, doe)

# responses ----
d <- expand_grid(
    id = participants$id,
    item = items$item
) %>%
    left_join(participants) %>% 
    left_join(items) %>% 
    left_join(select(r, id, item, response))  %>% 
    drop_na() %>%
    distinct(id, te, age, item, .keep_all = TRUE) %>% 
    mutate(
        doe = scale(doe, scale = FALSE)[,1],
        age = scale(age, scale = FALSE)[,1],
        understands = response %in% c(2, 3),
        produces = response %in% 3,
        dominance = ifelse(dominant_language==language, "L1", "L2")
    ) %>% 
    mutate_at(vars(cognate), as.factor) %>%
    select(id, age, lp, te, frequency, doe, cognate, understands, produces) %>% 
    arrange(id, te)

# model ----

# set contrasts
contrasts(d$cognate) <- c(0.5, -0.5)

# formula
f <- bf(
    understands ~ 1 + age + frequency + doe*cognate + (1 + age + doe | te) + (1 + frequency + cognate | id),
    family =  bernoulli("logit")
)

# priors
p <- c(
    prior(normal(0, 3), class = "Intercept"),
    prior(normal(0, 3), class = "b"),
    prior(normal(0, 0.5), class = "sd", group = "te"),
    prior(constant(1), class = "sd", group = "id"),
    prior(lkj(5), class = "cor")
)

# fit model
fit <- brm(
    formula = f, data = d, prior = p,
    chains = 4, cores = 4, iter = 2000,
    save_model = "Stan/irt.stan",
    backend = "cmdstanr"
)

# random effects
re_id <- ranef(fit)$id[,,1] %>% 
    as_tibble() %>% 
    mutate_if(is.numeric, function(x) inv_logit_scaled(x + fixef(fit)[1])) %>% 
    bind_cols(distinct(d, id, lp)) 
    right_join(distinct(d, id, lp)) %>% 
    drop_na()

ggplot(re_id, aes(Estimate, reorder(id, Estimate), colour = lp)) +
    geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5),
                   size = 0.25) +
    geom_point(shape = 4) +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    labs(x = "Participant average probability of comprehension",
         y = "Participant") +
    scale_color_brewer(palette = "Set1") +
    theme_ggdist() +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.minor.x = element_line(linetype = "dotted"),
        axis.ticks = element_blank()
    )

re_te <- ranef(fit)$te[,,1] %>% 
    as_tibble() %>% 
    rownames_to_column("te") %>% 
    mutate_if(is.numeric, function(x) inv_logit_scaled(x + fixef(fit)[1])) %>% 
    right_join(mutate(distinct(d, te, cognate), te = as.character(te))) %>% 
    drop_na()

ggplot(re_te, aes( + Estimate, reorder(te, Estimate), colour = cognate)) +
    geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5),
                   size = 0.25) +
    geom_point(shape = 4) +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    labs(x = "TE average probability of comprehension",
         y = "TE") +
    scale_color_brewer(palette = "Set1") +
    theme_ggdist() +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.minor.x = element_line(linetype = "dotted"),
        axis.ticks = element_blank()
    )
