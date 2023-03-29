library(ggplot2)
library(patchwork)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(bvqdev)
library(glue)
library(ggtext)
library(magick)

resolve_conflicts()

# set options ------------------------------------------------------------------
clrs <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")

options(ggplot2.ordinal.fill = clrs,
        ggplot2.ordinal.colour = clrs,
        ggplot2.discrete.fill = clrs,
        ggplot2.discrete.colour = clrs,
        ggplot2.continuous.fill = scale_color_gradient,
        ggplot2.continuous.colour = scale_color_gradient)

set.seed(888)

# set parameters ---------------------------------------------------------------

generate_aoa <- function(conditions = c("**Cognate**", "**Non-cognate**"),
                         items = c("gat /gat/",
                                   "gato /gato/",
                                   "gos /gos/", 
                                   "perro /ˈpe.ro/"),
                         age_step = 0.5,
                         age_range = c(10, 34),
                         freq_month = 50,
                         freq_month_sd = 10,
                         thres = 500,
                         doe_l1 = 0.65,
                         thres_diff = 100) 
{
    
    item_df <- tibble(te = rep(conditions, each = 2),
                      item = items,
                      doe = c(L1 = doe_l1, 
                              Spanish = 1-doe_l1, 
                              L1 = doe_l1, 
                              Spanish = 1-doe_l1))
    
    eli_df <- expand_grid(item_df,
                          age = seq(age_range[1], age_range[2], age_step),
                          n_month = freq_month*age_step,
                          threshold = thres,
                          hypothesis = c("No cognateness effect",
                                         "Cognateness effect")) |> 
        mutate(n_month_wgt = ceiling(n_month*doe),
               eli = n_month_wgt + rnorm(n(), 0, freq_month_sd),
               eli = round(ifelse(eli < 0 , 0, eli), 2)) |> 
        group_by(item, doe) |> 
        mutate(eli_cum = cumsum(eli),
               aoa = age[which.min(abs(threshold-eli_cum))],
               doe_hypothesis = case_when(doe==doe_l1 & item==items[1] & hypothesis=="No cognateness effect" ~ glue("Catalan ({percent(doe_l1)}) (no effect)"),
                                          doe==1-doe_l1 & item==items[2] & hypothesis=="No cognateness effect" ~ glue("Spanish ({percent(1-doe_l1)}) (no effect)"),
                                          doe==doe_l1 & item==items[1] & hypothesis=="Cognateness effect" ~ glue("Catalan ({percent(doe_l1)}) (effect)"),
                                          doe==1-doe_l1 & item==items[2] & hypothesis=="Cognateness effect" ~ glue("Spanish ({percent(1-doe_l1)}) (effect)"),
                                          doe==doe_l1 & item==items[3] ~ glue("Catalan ({percent(doe_l1)}) (no effect)"),
                                          doe==1-doe_l1 & item==items[4] ~ glue("Spanish ({percent(1-doe_l1)}) (no effect)")),
               doe = ifelse(doe==doe_l1, paste0("Catalan (", percent(doe), ")"),  paste0("Spanish (", percent(doe), ")"))) |> 
        ungroup() |> 
        filter(!(hypothesis=="Cognateness effect" & item %in% c(items[3], items[4]))) |> 
        select(te, item, doe_hypothesis, doe, age, threshold, eli_cum, aoa) |> 
        group_by(te) |> 
        mutate(threshold = ifelse(
            te==conditions[1] & (age+age_step > min(aoa)) & grepl("\\(effect\\)", doe_hypothesis),
            threshold - thres_diff,
            threshold)) |> 
        group_by(item, doe_hypothesis) |> 
        mutate(aoa = age[which.min(abs(threshold-eli_cum))]) |> 
        ungroup() |> 
        filter(doe_hypothesis!=glue("Catalan ({percent(doe_l1)}) (effect)"))
    
    aoa_df <- eli_df |> 
        group_by(te, item, doe, doe_hypothesis, aoa) |>
        summarise(threshold = ifelse(grepl("Spanish", doe), min(threshold), max(threshold)),
                  .groups = "drop") |> 
        distinct(te, item, doe_hypothesis, doe, aoa, threshold) |> 
        mutate(aoa_min = ifelse(te==conditions[1] & doe_hypothesis==glue("Spanish ({percent(1-doe_l1)}) (effect)"), 
                                aoa[te==conditions[1] & doe_hypothesis==glue("Spanish ({percent(1-doe_l1)}) (effect)")],
                                NA),
               aoa_max = ifelse(te==conditions[1] & doe_hypothesis==glue("Spanish ({percent(1-doe_l1)}) (effect)"), 
                                aoa[te==conditions[1] & doe_hypothesis==glue("Spanish ({percent(1-doe_l1)}) (no effect)")],
                                NA),
               aoa = ifelse(te==conditions[1] & doe_hypothesis==glue("Spanish ({percent(1-doe_l1)}) (no effect)"), 
                            aoa[te==conditions[1] & doe_hypothesis==glue("Spanish ({percent(1-doe_l1)}) (no effect)")],
                            aoa)) |> 
        mutate(hypothesis = ifelse(grepl("\\(effect\\)", doe_hypothesis),
                                   "Cognate effect",
                                   "No cognate effect")) 
    
    thres_df <- eli_df |> 
        distinct(te, item, age, doe_hypothesis, threshold) |> 
        mutate(hypothesis = ifelse(grepl("\\(effect\\)", doe_hypothesis), 
                                   "Cognate effect", 
                                   "No cognate effect")) |> 
        filter(!(te==conditions[1] & hypothesis=="No cognate effect" & age < min(aoa_df$aoa)))
    
    eli_df <- eli_df |> 
        distinct(te, item, doe_hypothesis, doe, age, .keep_all = TRUE) |> 
        mutate(hypothesis = ifelse(grepl("\\(effect\\)", doe_hypothesis), 
                                   "Cognate effect",
                                   "No cognate effect")) 
    
    dfs <- lst(eli_df, aoa_df, thres_df) |> 
        map(function(x){
            mutate(x, te = ifelse(te==conditions[1], 
                                  paste0(conditions[1], ": ", items[1], " - ", items[2]),
                                  paste0(conditions[2], ": ", items[3], " - ", items[4])))
        })
    
    return(dfs)
}

# simulate data ----------------------------------------------------------------

dataset <- generate_aoa()

# word-referent association plot -----------------------------------------------

img <- c(cat = "manuscript/img/diagram_cat.png",
         dog = "manuscript/img/diagram_dog.png") |> 
    map(image_read) |>
    map(\(x) image_ggplot(x, interpolate = FALSE))

# cumulative learning instances plot -------------------------------------------
elis_plot <- dataset$eli_df |> 
    ggplot(aes(age, eli_cum, colour = doe)) +
    facet_grid(~ te) +
    geom_rect(data = dataset$aoa_df, 
              aes(xmin = aoa_min, xmax = aoa_max,
                  ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              na.rm = TRUE,
              linejoin = "round",
              fill = clrs[4],
              alpha = 1/4) +
    geom_text(data = dataset$aoa_df,
              aes(label = ifelse(is.na(aoa_max), NA, "Cognateness effect"),
                  x = 23.1, y = 1600),
              na.rm = TRUE,
              hjust = 0,
              colour = clrs[4],
              size = 2.5) +
    geom_segment(data = dataset$aoa_df,
                 aes(x = aoa_min, 
                     xend = aoa_max,
                     y = 1600, yend = 1600),
                 colour = clrs[4],
                 na.rm = TRUE,
                 arrow = arrow(length = unit(0.1, "cm"), ends = "first"),
                 linewidth = 0.5) +
    geom_step(data = dataset$thres_df,
              aes(x = age, y = threshold, 
                  colour = hypothesis,
                  linetype = hypothesis),
              colour = "grey60",
              linewidth = 0.5,
              inherit.aes = FALSE) +
    geom_segment(data = dataset$aoa_df,
                 aes(x = aoa, xend = aoa,
                     y = -Inf, yend = threshold,
                     colour = doe),
                 linewidth = 0.5,
                 na.rm = TRUE,
                 linetype = "dotted") +
    geom_step(linewidth = 1) +
    geom_point(data = dataset$aoa_df,
               stroke = 0.75,
               aes(x = aoa, y = threshold,
                   shape = hypothesis, colour = doe),
               inherit.aes = FALSE,
               size = 2.25) +
    labs(x = "Age (months)",
         y = "Cumulative learning instances",
         colour = "Language (exposure)",
         shape = "Hypothesis",
         linetype = "Hypothesis") +
    guides(linetype = guide_none()) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                       breaks = seq(0, 5000, 250)) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_custom() +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.position = "right",
          legend.margin = margin(c(0, 0, 0, 0)),
          legend.justification = c(0, 1),
          legend.title = element_text(size = 9),
          legend.text= element_text(size = 8),
          strip.background = element_rect(fill = "grey90", colour = "grey90"),
          strip.text = element_markdown(),
          panel.grid = element_blank()) +
    inset_element(img$cat, on_top = FALSE, ignore_tag = TRUE,
                  left = -0.25, bottom = 0.5, right = 0.5, top = 1) +
    inset_element(img$dog, on_top = FALSE, ignore_tag = TRUE,
                  left = 0.25, bottom = 0.5, right = 1, top = 1) 


# logistic function ------------------------------------------------------------

probs_plot <- dataset$eli_df |> 
    select(te, item, doe, doe_hypothesis, age, aoa) |> 
    mutate(prob = plogis(age, location = aoa, scale = 2)) |> 
    mutate(hypothesis = ifelse(grepl("\\(effect\\)", doe_hypothesis), "Cognate effect", "No cognate effect")) |> 
    ggplot(aes(age, prob, colour = doe, linetype = hypothesis)) +
    facet_grid(~ te) +
    geom_rect(data = dataset$aoa_df,
              aes(xmin = aoa_min, xmax = aoa_max,
                  ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              na.rm = TRUE,
              linejoin = "round",
              fill = clrs[4],
              alpha = 1/4) +
    geom_hline(yintercept = 0.5,
               colour = "grey",
               linewidth = 0.5) +
    geom_segment(data = dataset$aoa_df,
                 aes(x = aoa, xend = aoa,
                     y = -Inf, yend = 0.5,
                     colour = doe),
                 linewidth = 0.5,
                 na.rm = TRUE,
                 linetype = "dotted") +
    geom_line(linewidth = 1) +
    geom_point(data = dataset$aoa_df,
               stroke = 0.75,
               aes(x = aoa, y = 0.5,
                   shape = hypothesis, colour = doe),
               inherit.aes = FALSE,
               size = 2.25) +
    labs(x = "Age (months)",
         y = "Probability of acquisition",
         colour = "Language (exposure)",
         shape = "Hypothesis",
         linetype = "Hypothesis") +
    guides(linetype = guide_none()) +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       labels = percent) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_custom() +
    theme(legend.position = "none",
          strip.background = element_rect(fill = "grey90", colour = "grey90"),
          strip.text = element_markdown())

# join plots -------------------------------------------------------------------

elis_plot + probs_plot +
    plot_layout(ncol = 1,
                guides = "collect") &
    scale_x_continuous(breaks = seq(min(dataset$eli_df$age), 
                                    max(dataset$eli_df$age), 4)) &
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA))

ggsave("manuscript/img/diagram.png",
       width = 10,
       height = 6,
       dpi = 800)
