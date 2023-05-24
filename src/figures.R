generate_eli <- function(conditions = c("**Cognate**", "**Non-cognate**"),
                         items = c("gat /gat/",
                                   "gato /gato/",
                                   "gos /gos/",
                                   "perro /ˈpe.ro/"),
                         age = seq(10, 34, length.out = 100),
                         freq_month = 1,
                         threshold = 500,
                         l1_doe = 0.65)
{
    
    item_df <- tibble(te = conditions,
                      l1.item = items[c(1, 3)],
                      l2.item = items[c(2, 4)]) |>
        mutate(lv = c(stringsim("gat", "gato"),
                      stringsim("gos", "pero"))) |>
        mutate(l1.doe = l1_doe, l2.doe = 1-l1_doe)
    
    eli <- expand_grid(item_df,
                       age = age) |>
        mutate(n_month = rpois(n(), freq_month) * age,
               l1.h0_eli = n_month * l1.doe,
               l2.h0_eli = n_month * l2.doe,
               l1.h1_eli = l1.h0_eli + (lv * l2.h0_eli),
               l2.h1_eli = l2.h0_eli + (lv * l1.h0_eli),
               across(matches("h0|h1"),
                      \(x) ifelse(x < 0 , 0, x))) |>
        mutate(across(matches("h0|h1"), cumsum),
               .by = te) |>
        select(te, lv, age, matches("l1|l2")) |>
        pivot_longer(-c(te, lv, age),
                     names_to = c("language", ".value"),
                     names_pattern = "(.+)\\.(.+)") |>
        mutate(language = toupper(language)) |>
        pivot_longer(matches("h0|h1"),
                     names_to = c("hypothesis", ".value"),
                     names_pattern = "(.+)_(.+)",
                     names_transform = toupper) |>
        rename_with(tolower) |>
        arrange(te, language, hypothesis, age)
    
    return(eli)
}

generate_aoa <- function(eli, threshold = 50) {
    aoa <- eli |>
        mutate(aoa = age[which.min(abs(threshold-eli))],
               aoa = ifelse(aoa > max(age), NA, aoa),
               .by = c(te, hypothesis, language)) |>
        rename_with(\(x) gsub("_eli_aoa", "_aoa", x)) |>
        distinct(pick(-c(age, eli))) |>
        arrange(te, language) |>
        mutate(threshold = .env$threshold)
    
    return(aoa)
}

# cumulative learning instances plot -------------------------------------------

generate_figure <- function(...) {
    
    # simulate data ---------------
    eli_df <- generate_eli(threshold = 500, ...) |>
        filter(!(hypothesis=="H1" & grepl("Non-cognate", te)))
    
    aoa_df <- generate_aoa(eli_df, threshold = 500) |>
        mutate(across(aoa, lst(min, max)), .by = c(te, language)) |>
        filter(!(hypothesis=="H1" & grepl("Non-cognate", te)))
    
    img <- c(cat = "assets/img/diagram_cat.png",
             dog = "assets/img/diagram_dog.png") |>
        map(magick::image_read) |>
        map(\(x) magick::image_ggplot(x, interpolate = FALSE))
    
    plot <- eli_df |>
        ggplot(aes(age, eli,
                   colour = language,
                   linetype = hypothesis,
                   shape = hypothesis)) +
        facet_grid(~ te) +
        geom_segment(data = aoa_df,
                     aes(x = aoa,
                         xend = aoa,
                         y = 0,
                         yend = threshold),
                     linewidth = 3/4) +
        geom_hline(yintercept = aoa_df$threshold) +
        geom_line(linewidth = 1) +
        geom_point(data = aoa_df,
                   stroke = 0.75,
                   aes(x = aoa, y = threshold),
                   size = 2.25) +
        labs(x = "Age (months)",
             y = "Cumulative\nlearning instances",
             colour = "Language (exposure)",
             shape = "Hypothesis",
             linetype = "Hypothesis") +
        scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
        theme_ggdist() +
        theme(panel.background = element_rect(fill = NA),
              legend.key.width = unit(1.5, "cm"),
              legend.position = "bottom",
              legend.margin = margin(c(0, 0, 0, 0)),
              legend.justification = c(0, 1),
              legend.title = element_text(size = 9),
              legend.text= element_text(size = 8),
              strip.background = element_rect(fill = "grey90", colour = "grey90"),
              strip.text = ggtext::element_markdown(),
              panel.grid = element_blank(),
              panel.grid.major.y = element_line(colour = "grey",
                                                linetype = "dotted"),
              panel.grid.minor.y = element_line(colour = "grey",
                                                linetype = "dotted")) +
        inset_element(img$cat, on_top = FALSE, ignore_tag = TRUE,
                      left = -0.25, bottom = 0.50, right = 0.5, top = 1) +
        inset_element(img$dog, on_top = FALSE, ignore_tag = TRUE,
                      left = 0.25, bottom = 0.50, right = 1, top = 1) +
        scale_x_continuous(breaks = seq(min(eli_df$age),
                                        max(eli_df$age), 4)) +
        theme(panel.grid = element_blank(),
              plot.background = element_rect(fill = "white",
                                             colour = NA))
    
    
    return(plot)
    
}
