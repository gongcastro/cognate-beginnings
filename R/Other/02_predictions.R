library(multilex)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(tidybayes)
library(ggplot2)
library(patchwork)
library(ggsci)
library(scales)
library(arrow)
library(here)


# import data ----
model_fit <- readRDS(here("results", "fit.rds"))
df <- read_ipc_stream(here("data", "main.parquet"))
theme_set(theme_ggdist())
source(here("R", "utils.R"))

# expand dataset to get predictions with larger resolution ----
df_preds <- expand_grid(
    age_std = seq(-4, 4, 0.1),
    dominance = c("L1", "L2")
) 

# expected posterior predictions ----
preds <- add_epred_draws(
    newdata = df_preds, 
    object = model_fit,
    ndraws = 50,
    re_formula = NA
) %>% 
    pivot_wider(
        names_from = .category,
        values_from = .epred
    ) %>% 
    mutate(
        Understands = Understands + `Understands and Says`
    ) %>% 
    pivot_longer(
        c(No, Understands, `Understands and Says`),
        names_to = ".category",
        values_to = ".epred"
    ) %>% 
    # get curves of interest
    filter(.category %in% c("Understands", "Understands and Says")) %>% 
    # more informative labels
    mutate(
        .category = case_when(
            .category=="Understands" ~ "Comprehension",
            .category=="Understands and Says" ~ "Production"
        ),
        # see R/utils.R
        age = rescale_age(age_std, mean = mean(df$age), sd = sd(df$age))
    ) 

write_ipc_stream(preds, here("results", "posterior_predictions-population.parquet"))

# visualise predictions ----

# empirical response cumulative probabilities
aoas <- get_aoa(preds, .category, dominance, .draw)

write_ipc_stream(aoas, here("results", "posterior_aoas-population.parquet"))

plot_curves <- preds %>%  
    ggplot() +
    aes(
        x = age,
        y = .epred,
        colour = interaction(dominance, .category, sep = " / "),
        fill = interaction(dominance, .category, sep = " / "),
        shape = interaction(dominance, .category, sep = " / ")
    ) +
    # linea de referencia (50% suele ser considerado el punto de adquisición)
    geom_hline(
        yintercept = 0.5,
        colour = "grey"
    ) +
    # posterior predictions for each individual posterior draw
    geom_line(
        aes(group = interaction(.draw, dominance, .category , sep = " / ")),
        alpha = 0.25,
        size = 1,
        linetype = "solid"
    ) +
    labs(
        x = "Age (months)",
        y = "P(acquisition | age, dominance)",
        colour = "Response",
        fill = "Response",
        shape = "Response",
        linetype = "Response",
        title = "Acquisition curves",
        subtitle = "Each line corresponds to a posterior prediction"
    ) +
    scale_color_d3() +
    scale_fill_d3() +
    scale_y_continuous(
        limits = c(0, 1),
        labels = scales::percent
    ) +
    scale_x_continuous(
        limits = c(0, 45),
        breaks = seq(0, 45, 5)
    ) +
    theme(
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
        axis.text.x.top = element_text(),
        axis.title.x.top = element_text(colour = "top"),
        axis.ticks.x.top = element_line(),
        axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.line.x = element_blank()
    ) 

plot_aoas <- aoas %>% 
    filter(.category != "No") %>% 
    ggplot() +
    aes(
        x = aoa,
        y = 0,
        colour = interaction(dominance, .category, sep = "/")
    ) +
    stat_pointinterval(
        position = position_dodge(width = 0.5)
    ) +    labs(
        x = "Age (months)",
        y = "Response",
        colour = "Response",
        title = "Age of acquisition"
    ) +
    scale_x_continuous(
        limits = c(0, 45),
        breaks = seq(0, 45, 5)
    ) +
    scale_color_d3() +
    scale_fill_d3() +
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

plot_curves + 
    plot_aoas +
    plot_layout(
        ncol = 1,
        heights = c(0.8, 0.2)
    ) +
    plot_annotation(
        title = "Posterior predictions",
        tag_levels = "A"
    )

# save figure
ggsave(here("img", "posterior_predictions-population.png"), width = 9, height = 8)


# item marginal curves ----

df_items <- multilex::pool %>% 
    select(te, label, language) %>% 
    pivot_wider(
        id_cols = te,
        names_from = language,
        values_from = label
    ) %>%
    mutate(
        label = paste0(Catalan, " / ", Spanish),
        te = as.factor(te)
    ) %>% 
    filter(te %in% unique(model_fit$data$te))

# (aka. curvas características del ítem)
df_preds_marg <- expand_grid(
    te = unique(model_fit$data$te),
    age_std = seq(-4, 4, 0.1),
    dominance = c("L1", "L2")
) 

preds_marg <- add_epred_draws(
    newdata = df_preds_marg, 
    object = model_fit,
    ndraws = 50,
    re_formula = ~ (1 | te)
) %>% 
    pivot_wider(
        names_from = .category,
        values_from = .epred
    ) %>% 
    mutate(Understands = Understands + `Understands and Says`) %>% 
    pivot_longer(
        c(No, Understands, `Understands and Says`),
        names_to = ".category",
        values_to = ".epred"
    ) %>% 
    # get curves of interest
    filter(.category %in% c("Understands", "Understands and Says")) %>% 
    # more informative labels
    mutate(
        .category = case_when(
            .category=="Understands" ~ "Comprehension",
            .category=="Understands and Says" ~ "Production"
        ),
        # see R/utils.R
        age = rescale_age(age_std, mean = mean(df$age), sd = sd(df$age))
    ) %>% 
    left_join(df_items) 

write_ipc_stream(preds_marg, here("results", "posterior_predictions-group.parquet"))

aoas_marg <- get_aoa(preds_marg, te, .category, dominance, .draw) %>% 
    left_join(df_items)

write_ipc_stream(aoas_marg, here("results", "posterior_aoas-group.parquet"))


plot_curves <- preds_marg %>% 
    filter(te == 155) %>% 
    ggplot() +
    aes(
        x = age,
        y = .epred,
        colour = interaction(dominance, .category, sep = " / "),
        fill = interaction(dominance, .category, sep = " / "),
        shape = interaction(dominance, .category, sep = " / ")
    ) +
    # linea de referencia (50% suele ser considerado el punto de adquisición)
    geom_hline(
        yintercept = 0.5,
        colour = "grey"
    ) +
    # posterior predictions for each individual posterior draw
    geom_line(
        aes(group = interaction(.draw, dominance, .category , sep = " / ")),
        alpha = 0.25,
        size = 1,
        linetype = "solid"
    ) +
    labs(
        x = "Age (months)",
        y = "P(acquisition | age, dominance)",
        colour = "Response",
        fill = "Response",
        shape = "Response",
        linetype = "Response",
        title = "Acquisition curves",
        subtitle = "Each line corresponds to a posterior prediction"
    ) +
    scale_color_d3() +
    scale_fill_d3() +
    scale_y_continuous(
        limits = c(0, 1),
        labels = scales::percent
    ) +
    scale_x_continuous(
        limits = c(0, 45),
        breaks = seq(0, 45, 5)
    ) +
    theme(
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
        axis.text.x.top = element_text(),
        axis.title.x.top = element_text(colour = "top"),
        axis.ticks.x.top = element_line(),
        axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.line.x = element_blank()
    ) 

plot_aoas <- aoas_marg %>% 
    filter(te == 155) %>% 
    ggplot() +
    aes(
        x = aoa,
        y = 0,
        colour = interaction(dominance, .category, sep = "/")
    ) +
    stat_pointinterval(
        position = position_dodge(width = 0.5)
    ) +
    labs(
        x = "Age (months)",
        y = "Response",
        colour = "Response",
        title = "Age of acquisition"
    ) +
    scale_x_continuous(
        limits = c(0, 45),
        breaks = seq(0, 45, 5)
    ) +
    scale_color_d3() +
    scale_fill_d3() +
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

plot_curves + 
    plot_aoas +
    plot_layout(
        ncol = 1,
        heights = c(0.8, 0.2)
    ) +
    plot_annotation(
        title = unique(plot_aoas$data$label),
        subtitle = "Posterior predictions",
        tag_levels = "A"
    )

# save image
ggsave(here("img", "posterior_predictions-marginal.png"), width = 11, height = 7)

# diagnose model ----
# see https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html#general-mcmc-diagnostics-1
post <- gather_draws(
    model_fit_0,
    `b_.*`,
    `sd_.*`,
    `cor_.*`,
    regex = TRUE
) %>% 
    mutate(.chain = as.factor(.chain))

post %>% 
    ggplot() +
    aes(
        x = .iteration,
        y = .value,
        colour = .chain
    ) +
    facet_wrap(
        ~.variable,
        scales = "free_y"
    ) + 
    geom_line() +
    labs(
        title = "MCMC traceplot",
        subtitle = "Chains should be intermixed and look like a hairy caterpillar",
        x = "Iteration",
        y = "Value",
        colour = "Chain"
    ) +
    scale_color_d3()  +
    theme(
        legend.position = "top"
    )

ggsave(here("img", "diagnostics_traceplot.png"))


# Gelman-Rubin (R-hat statistic) (https://www.jstor.org/stable/2246093?seq=1)
# values between 1 and 1.1 indicate that MCMCs have converged decently
    data.frame(.rhat = .) %>% 
    rownames_to_column(".variable") %>% 
    as_tibble() %>% 
    ggplot() +
    aes(
        x = .rhat
    ) +
    # geom_vline(xintercept = 1.1, colour = "grey") +
    geom_histogram(
        fill = "dodgerblue",
        colour = "white"
    ) +
    labs(
        title = "Chain convergence",
        subtitle = "Values should not be larger than 1.1",
        x = "Gelman-Rubin statistic (R-hat)",
        y = "Number of samples"
    ) 

ggsave(here("img", "diagnostics_rhat.png"), width = 8, height = 4)


# effective sample size: estimate of the number of independent draws from the
# posterior distribution of the estimand of interest
# the larger the ratio of neff to N the better
neff_ratio(model_fit_0) %>% 
    data.frame(.neff = .) %>% 
    rownames_to_column(".variable") %>% 
    as_tibble() %>% 
    ggplot() +
    aes(
        x = .neff
    ) +
    geom_histogram(
        fill = "dodgerblue",
        colour = "white"
    ) +
    labs(
        title = "Effective sample size",
        subtitle = "Overall, values should be larger than 1",
        x = "Effective sample size ratio (N eff.)",
        y = "Number of samples"
    ) +
    theme(
        axis.title.y = element_blank()
    ) 

ggsave(here("img", "diagnostics_neff.png"), width = 8, height = 4)


# posterior distribution ----

# population-level coefficients
post %>% 
    filter(str_detect(.variable, "b_")) %>% 
    mutate(
        .value = case_when(
            str_detect(.variable, "Intercept") ~ inv_logit_scaled(.value),
            TRUE ~ .value/4
        ),
        .variable = .variable %>% 
            factor(
                levels = c(
                    "b_Intercept[2]",
                    "b_Intercept[1]",
                    "b_age_std",
                    "b_dominance1",
                    "b_age_std:dominance1"
                ),
                labels = c(
                    "Intercept 1 (Comprehension)",
                    "Intercept 2 (Production)",
                    "Age (+1 SD)",
                    "Dominance (L1 vs. L2)",
                    "Age × Dominance"
                )
            )
    ) %>% 
    ggplot() +
    aes(
        x = .value,
        y = fct_rev(.variable)
    ) +
    geom_vline(
        xintercept = 0,
        linetype = "dotted"
    ) +
    stat_slab(
        aes(
            fill = stat(cut_cdf_qi(cdf, labels = percent))
        ),
        colour = "white"
    ) +
    stat_pointinterval(
        colour = "black"
    ) +
    scale_fill_brewer(
        palette = "Oranges",
        direction = -1, 
        na.value = "gray90"
    ) +
    labs(
        x = "Sampling space",
        y = "Posterior likelihood density",
        fill = "Credible Interval"
    ) +
    scale_x_continuous(
        limits = c(-0.2, 1),
        labels = percent
    ) +
    theme(
        legend.position = "top",
        panel.grid.major.y = element_line(colour = "grey")
    )

ggsave(here("img", "posterior_distribution-population.png"), width = 8, height = 4)


# group-level coefficients (SD)
post %>% 
    filter(str_detect(.variable, "sd_")) %>% 
    mutate(
        .value = inv_logit_scaled(.value),
        group = ifelse(str_detect(.variable, "_te__"), "TE", "ID"),
        .variable = .variable %>% 
            factor(
                levels = c(
                    "sd_id__Intercept",
                    "sd_id__age_std",
                    "sd_id__dominance1",
                    "sd_id__age_std:dominance1",
                    "sd_te__Intercept",
                    "sd_te__age_std",
                    "sd_te__dominance1",
                    "sd_te__age_std:dominance1"
                ),
                labels = c(
                    "Intercept",
                    "Age (+1 SD)",
                    "Dominance (L1 vs. L2)",
                    "Age × Dominance",
                    "Intercept",
                    "Age (+1 SD)",
                    "Dominance (L1 vs. L2)",
                    "Age × Dominance"
                )
            )
    ) %>% 
    ggplot() +
    aes(
        x = .value,
        y = fct_rev(.variable)
    ) +
    facet_wrap(~group, ncol = 1) +
    stat_slab(
        aes(
            fill = stat(cut_cdf_qi(cdf, labels = percent))
        ),
        colour = "white"
    ) +
    stat_pointinterval(
        colour = "black"
    ) +
    scale_fill_brewer(
        palette = "Reds",
        direction = -1, 
        na.value = "gray90"
    ) +
    labs(
        x = "Sampling space",
        y = "Posterior likelihood density",
        fill = "Credible Interval"
    ) +
    scale_x_continuous(
        limits = c(0, 1),
        labels = percent
    ) +
    theme(
        legend.position = "top",
        panel.grid.major.y = element_line(colour = "grey")
    )

ggsave(here("img", "posterior_distribution-group.png"), width = 8, height = 4)