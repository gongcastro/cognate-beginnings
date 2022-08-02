# set params
tar_load_globals()
tar_load_all()
theme_set(theme_custom())

words_per_month <- 10000
acquisition_threshold <- 1000

sim_p <- tibble(
    id = 1:5,
    doe_l1 = seq(0, 1, 0.25),
    doe_l2 = seq(1, 0, -0.25)
) %>% 
    expand_grid(age = seq(0, 36)) %>% 
    pivot_longer(
        c(doe_l1, doe_l2),
        names_to = "language",
        values_to = "doe",
        names_transform = list(language = ~str_remove(., "doe_"))
    )

sim_i <- tibble(
    item = paste("item", 1:5),
    word_rate = seq(0, 100, 25)
) 


sim_d <- expand_grid(sim_p, sim_i) %>% 
    mutate(
        eli = word_rate*age*doe,
        p_acquisition = 1/(1+(exp(1)^(-0.005*(eli-acquisition_threshold)))),
        is_acquired = eli >= acquisition_threshold
    ) %>% 
    arrange(id, item, age)

sim_d %>% 
    ggplot() +
    aes(
        x = age,
        y = eli,
        colour = language
    ) +
    facet_grid(
        id~item,
        labeller = label_both
    ) +
    geom_hline(
        yintercept = acquisition_threshold,
        colour = "grey",
        linetype = "dotted"
    ) +
    geom_line(size = 1) +
    labs(
        x = "Age (arbitrary units)",
        y = "Effective learning instances",
        colour = "Language exposure"
    ) +
    
    sim_d %>% 
    mutate(
        language = str_to_sentence(language),
        language_label = paste0(language, ": ", percent(doe))) %>% 
    ggplot() +
    aes(
        x = age,
        y = p_acquisition,
        colour = as.factor(language)
    ) +
    facet_grid(
        id~item,
        labeller = label_both
    ) +
    geom_hline(
        yintercept = 0.5,
        colour = "grey",
        linetype = "dotted"
    ) +
    geom_line(
        size = 1
    ) +
    geom_text(
        aes(x = 20, y = 0.75, label = percent(doe)),
        size = 3,
        position = position_dodge(width = 30),
        show.legend = FALSE
    ) +
    labs(
        x = "Age (arbitrary units)",
        y = "P(acquisition)",
        colour = "Language"
    ) +
    scale_y_continuous(
        labels = percent
    ) +
    
    plot_layout(
        guides = "collect"
    ) &
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey", colour = NA),
        panel.border = element_rect(colour = "grey", fill = NA)
    )





