---
title: "Cognate beginnings to bilingual lexical acquisition"
subtitle: "Lab notes"
short-title: "Cognate beginnings"
author:
  - name: Gonzalo Garcia-Castro
    corresponding: true
    orcid: 0000-0002-8553-4209
    email: gonzalo.garciadecastro@upf.edu
    affiliations:
      - id: cbc
        name: Universitat Pompeu Fabra
        department: Center for Brain and Cognition
        address: Ramon Trias Fargas 25-27
        city: Barcelona
        region: Spain
        postal-code: "08005"
  - name: Daniela S. Avila-Varela
    orcid: 0000-0002-3518-8117
    affiliations: 
      - ref: cbc
      - id: ul
        name: Universidade de Lisboa
        department: Facultade de Letras
        address: Alameda da Universidade
        city: Lisboa
        region: Portugal
        postal-code: "1600-214"
  - name: Ignacio Castillejo
    orcid: 0000-0001-7445-0416
    affiliations: 
      - id: uam 
        name: Universidad Autónoma de Madrid
        department: Departamento de Psicología
        address: Iván Pavlov, 6
        city: Madrid
        region: Spain
        postal-code: "28049"
  - name: Núria Sebastian-Galles
    orcid: 0000-0001-6938-2498
    affiliations: 
      - ref: cbc
date: "May 10th, 2023"
code-repo: "Access the code, data, and analysis at <https://github.com/gongcastro/trajectories>"
keywords:
  - lexical acquisition
  - vocabulary
  - bilingualism
  - item response theory
  - bayesian
csl: "../assets/apa7.csl"
bibliography: "../assets/references.bib"
link-citations: true
editor: source

code-fold: true
warning: false

format:
  html: default

abstract: |
    Language non-selectivity is prominent feature of bilingual lexical processing. An instance of such non-selectivity is embodied by cognateness, which recent studies have suggested to facilitate vocabulary acquisition in bilingual toddlers, who show larger vocabulary sizes when their languages share many cognates, and to be acquired at earlier ages than non-cognates. The specific mechanisms behind such facilitation are unclear. We present an account of early bilingual vocabulary growth in which cognateness interacts with lexical frequency and language exposure to facilitate the word acquisition. We evaluated this model against vocabulary data from 436 Catalan-Spanish bilinguals toddlers. We used Bayesian Explanatory Item Response Models to estimate participants’ probability of acquisition of 604 words, conditional to the cognate status and lexical frequency of the word-form, and the age and degree of exposure to each language of the toddler. We found cognateness facilitation effect only in low-frequency words. Overall, our findings support an interactive account of bilingual vocabulary acquisition in which the lexical representations in one language interact with the acquisition of words in the other language.

fig-dpi: 1000

---

::: {.cell}

```{.r .cell-code}
# load objects -----------------------------------------------------------------

library(patchwork)
library(gt)
library(ggplot2)

targets::tar_config_set(store = here::here("_targets"),
                        script = here::here("_targets.R"))

targets::tar_load_globals()

targets::tar_load(c(bvq_data, childes, items, 
                    participants, responses))

targets::tar_load(c(model_fit, model_summary, model_epreds,
                    model_convergence, model_fit_prior, 
                    model_ppcs, posterior_doe_summary,
                    model_draws, model_summary,
                    posterior_doe_draws))

targets::tar_load(c(syllables_data, model_fit_syllables, post_syllables))

# process objects
items <- bvq_data$pool  |>  
    mutate(item = str_remove(item, "cat_|spa_")) |> 
    select(item, language, semantic_category, class) |> 
    right_join(items, by = c("item", "language"))

rope_interval <- c(lower = -0.1, upper = 0.1)

# set ggplot theme and colour palette ------------------------------------------

my_theme <- theme_minimal() +
    theme(panel.grid = element_line(colour = "grey",
                                    linetype = "dotted"),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 12, colour = "black"),
          axis.text = element_text(colour = "black"))

theme_set(my_theme)

bayesplot::bayesplot_theme_set(my_theme + theme(panel.grid = element_blank()))

clrs <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")

set.seed(888)

options(ggplot2.ordinal.fill = clrs,
        ggplot2.ordinal.colour = clrs,
        ggplot2.discrete.fill = clrs,
        ggplot2.discrete.colour = clrs,
        ggplot2.continuous.fill = scale_color_gradient,
        ggplot2.continuous.colour = scale_color_gradient,
        knitr.graphics.error = FALSE) 
```
:::



# Methods

::: {.callout-tip}
Data and materials are available at [OSF](https://osf.io/hy984/), and code is available at [GitHub](https://github.com/gongcastro/trajectories).
:::

## Questionnaires


::: {.cell}

```{.r .cell-code}
data("pool", package = "bvq")
n_categories <- distinct(pool, semantic_category) %>% nrow()
n_items <- length(unique(pool$item))
n_item_language <- count(pool, language) %>% 
    pull(n) %>%
    set_names(c("catalan", "spanish"))
```
:::


The questionnaire was implemented on-line using the formR platform [@arslan2020formr], and was structured in three blocks: a (1) language questionnaire, a (2) demographic survey, and a (3) Catalan and a Spanish vocabulary checklists. Vocabulary checklists followed a similar structure as the Oxford Communicative Developmental Inventory [@hamilton2000infant] and consisted of two lists of words: one in Catalan and one in Spanish. The Catalan inventory contained 793 items and the Spanish inventory contained 797 items. Items in one language were translation equivalents of the items in the other language (e.g., whenever *gos* \[dog\] was included in the Catalan inventory, the word *perro* was included in the Spanish inventory), roughly following a one-to-one mapping. When there were two acceptable translation equivalents for a given word, we included both in separate items (e.g., Catalan *acabar* \[*to finish*\] and Spanish *acabar* and *terminar*), or merged them into a single items (e.g., Spanish *mono* \[*monkey*\] and Catalan *mono/mico*. We included items from a diverse sample of 26 semantic/functional categories (see Appendix 1). For the analyses included in this study, we excluded items from the adverbs, auxiliary words, connectives, interjections and games and routines categories, so that only data from content words (nouns, adjectives, and verbs) were used.

For each word in the vocabulary checklists, we asked parents to report whether their child was able to understand it, understand *and* say it, or did not understand or say it (checked out by default). Some families filled a long version of the vocabulary checklists (800 translation equivalents; 800 items in Catalan, 800 items in Spanish), while others filled a shorter version (\~400 translation equivalents, \~400 items in Catalan, \~400 items in Spanish). These last families were randomly assigned to one of four different subsets of the complete list of items. These lists were designed so that each contained a representative subsample of the items from the complete list. Semantic/functional categories with less than 16 items were not divided in the short version of the questionnaire to preserve a minimum of four items per list for each category: all of their items were included in the four lists. Another subset of items that were part of the trial lists of some experiments in the lab were also included in all versions. Table 2 in Appendix 1 shows the distribution of items across questionnaire versions. We excluded from the analysis multi-word items (e.g., *barrita de cereales* \[cereal bar\]) and items that included more than one word-form (e.g., *mono / mico*). Table 3 shows the classification of items in cognates and non-cognates and their lexical frequency scores across the four lists of the inventories.

## Participants


::: {.cell}

```{.r .cell-code}
participants %>% 
    group_by(time_stamp) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    mutate(n = cumsum(n)) %>% 
    ggplot(aes(time_stamp, n)) +
    geom_vline(xintercept = ymd("2020-03-15"), size = 0.5) +
    annotate(geom = "text", y = 350, x = ymd("2020-03-15"), label = "COVID-19 lockdown in Spain",
             angle = 90, vjust = 1.5, hjust = 1) +
    geom_line(size = 1, colour = clrs[1]) +
    labs(x = "Date", y = "Number of responses", colour = "Dominant language") +
    scale_y_continuous(breaks = seq(0, 500, 100), limits = c(0, 500)) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 20, hjust = 1),
          axis.title.x = element_blank()) +
    
    participants %>% 
    mutate(age = cut(floor(age), breaks = seq(7, 35, 5))) %>% 
    group_by(time_stamp, age) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    group_by(age) %>% 
    mutate(n = cumsum(n)) %>% 
    ggplot(aes(time_stamp, n, colour = age)) +
    geom_vline(xintercept = ymd("2020-03-15"), size = 0.5) +
    geom_line(size = 1) +
    labs(x = "Date", y = "Number of responses", colour = "Age (months(") +
    scale_y_continuous(breaks = seq(0, 500, 50)) +
    scale_colour_manual(values = clrs) +
    
    plot_layout(ncol = 1) &
    
    scale_x_date(date_breaks = "3 month", date_labels = "%B %Y") &
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1),
        axis.title.x = element_blank(),
    )
```

::: {.cell-output-display}
![Date at which responses were collected](index_files/figure-html/fig-participants-time-1.png){#fig-participants-time width=7000}
:::
:::


### Age

Number of months elapsed between participants' birth date and questionnaire completion.


::: {.cell}

```{.r .cell-code}
participants %>% 
    mutate(age = floor(age)) %>% 
    count(age) %>% 
    ggplot() +
    aes(x = age, y = n) +
    geom_col(fill = "grey60") +
    geom_text(aes(label = n), size = 3.5, vjust = -1) +
    labs(x = "Age (months)",
         y = "# participants") +
    scale_x_continuous(breaks = seq(0, 40, 2)) +
    scale_y_continuous(limits = c(0, 55),
                       breaks = seq(0, 55, 10)) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )
```

::: {.cell-output-display}
![Distribution of participants' ages](index_files/figure-html/fig-participants-age-1.png){#fig-participants-age width=7000}
:::
:::


### Language exposure (Degree of Exposure)

Participant degree of exposure to the words' language ($DoE$): percentage of exposure to the language the word belonged to. For example, for a participant with 90% exposure to Catalan, and 10% to Spanish, the DoE of the Catalan word *taula* would be 90%, and the DoE for the Spanish word *mesa* would be 10%.


::: {.cell tbl-cap='Participant sample size by age and degree of exposure to Catalan. Information about language degree of exposure (DoE) was provided by families before filling the questionnaire. In this table, a 100\% indicates that the participant was exclusively exposed to Catalan, and never to Spanish. A 0\% indicates that the participant was not exposed to Catalan ever, and most of the time to Spanish. A 50\% indicates that the participant was exposed to Catalan and Spanish approximately half of the time each. The 100\%, 0\%, and 50\% would be traditionaly classified as Catalan monolingual, Spanish monolingual, and Catalan-Spanish bilingual, respectively. For illustration purposes this table, DoEs were binned into 25\%-wide bins, and ages (in months) binned into 4 month-wide bins. Hyphens indicate that no participants from that specific combination of age and DoE filled the questionnaire.'}

```{.r .cell-code}
participants |>
    mutate_at(vars(starts_with("doe_")), 
              (\(x) floor(x * 10) / 10)) |> 
    mutate(
        age = cut(floor(age),
                  c(10, 14, 18, 22, 26, 30, 34, 36),
                  include.lowest = TRUE),
        doe_catalan = cut(
            doe_catalan,
            c(0, 0.25, 0.5, 0.75, 1),
            labels = c("0-25%",
                       "25-50%", 
                       "50-75%",
                       "75-100%"),
            include.lowest = TRUE
        ) |>
            as.character()
    ) |>
    count(age, doe_catalan) |>
    arrange(desc(doe_catalan)) |>
    pivot_wider(names_from = age, values_from = n) |>
    gt() |>
    sub_missing(everything(), everything(), missing_text = "--") |>
    cols_label(doe_catalan = "Catalan exposure") |>
    tab_spanner("Age (months)", 2:7) |>
    tab_style(cell_text(align = "left"),
              list(cells_body(doe_catalan),
                   cells_column_labels(doe_catalan)))
```

::: {.cell-output-display}
```{=html}
<div id="haokkgcraj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#haokkgcraj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#haokkgcraj thead, #haokkgcraj tbody, #haokkgcraj tfoot, #haokkgcraj tr, #haokkgcraj td, #haokkgcraj th {
  border-style: none;
}

#haokkgcraj p {
  margin: 0;
  padding: 0;
}

#haokkgcraj .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#haokkgcraj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#haokkgcraj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#haokkgcraj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#haokkgcraj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#haokkgcraj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#haokkgcraj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#haokkgcraj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#haokkgcraj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#haokkgcraj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#haokkgcraj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#haokkgcraj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#haokkgcraj .gt_spanner_row {
  border-bottom-style: hidden;
}

#haokkgcraj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#haokkgcraj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#haokkgcraj .gt_from_md > :first-child {
  margin-top: 0;
}

#haokkgcraj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#haokkgcraj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#haokkgcraj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#haokkgcraj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#haokkgcraj .gt_row_group_first td {
  border-top-width: 2px;
}

#haokkgcraj .gt_row_group_first th {
  border-top-width: 2px;
}

#haokkgcraj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#haokkgcraj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#haokkgcraj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#haokkgcraj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#haokkgcraj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#haokkgcraj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#haokkgcraj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#haokkgcraj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#haokkgcraj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#haokkgcraj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#haokkgcraj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#haokkgcraj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#haokkgcraj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#haokkgcraj .gt_left {
  text-align: left;
}

#haokkgcraj .gt_center {
  text-align: center;
}

#haokkgcraj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#haokkgcraj .gt_font_normal {
  font-weight: normal;
}

#haokkgcraj .gt_font_bold {
  font-weight: bold;
}

#haokkgcraj .gt_font_italic {
  font-style: italic;
}

#haokkgcraj .gt_super {
  font-size: 65%;
}

#haokkgcraj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#haokkgcraj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#haokkgcraj .gt_indent_1 {
  text-indent: 5px;
}

#haokkgcraj .gt_indent_2 {
  text-indent: 10px;
}

#haokkgcraj .gt_indent_3 {
  text-indent: 15px;
}

#haokkgcraj .gt_indent_4 {
  text-indent: 20px;
}

#haokkgcraj .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="text-align: left;" scope="col" id="Catalan exposure">Catalan exposure</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="6" scope="colgroup" id="Age (months)">
        <span class="gt_column_spanner">Age (months)</span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="[10,14]">[10,14]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="(14,18]">(14,18]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="(18,22]">(18,22]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="(22,26]">(22,26]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="(26,30]">(26,30]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="(30,34]">(30,34]</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="doe_catalan" class="gt_row gt_right" style="text-align: left;">75-100%</td>
<td headers="[10,14]" class="gt_row gt_right">18</td>
<td headers="(14,18]" class="gt_row gt_right">23</td>
<td headers="(18,22]" class="gt_row gt_right">36</td>
<td headers="(22,26]" class="gt_row gt_right">38</td>
<td headers="(26,30]" class="gt_row gt_right">20</td>
<td headers="(30,34]" class="gt_row gt_right">7</td></tr>
    <tr><td headers="doe_catalan" class="gt_row gt_right" style="text-align: left;">50-75%</td>
<td headers="[10,14]" class="gt_row gt_right">8</td>
<td headers="(14,18]" class="gt_row gt_right">13</td>
<td headers="(18,22]" class="gt_row gt_right">30</td>
<td headers="(22,26]" class="gt_row gt_right">41</td>
<td headers="(26,30]" class="gt_row gt_right">18</td>
<td headers="(30,34]" class="gt_row gt_right">1</td></tr>
    <tr><td headers="doe_catalan" class="gt_row gt_right" style="text-align: left;">25-50%</td>
<td headers="[10,14]" class="gt_row gt_right">10</td>
<td headers="(14,18]" class="gt_row gt_right">17</td>
<td headers="(18,22]" class="gt_row gt_right">45</td>
<td headers="(22,26]" class="gt_row gt_right">29</td>
<td headers="(26,30]" class="gt_row gt_right">17</td>
<td headers="(30,34]" class="gt_row gt_right">–</td></tr>
    <tr><td headers="doe_catalan" class="gt_row gt_right" style="text-align: left;">0-25%</td>
<td headers="[10,14]" class="gt_row gt_right">7</td>
<td headers="(14,18]" class="gt_row gt_right">11</td>
<td headers="(18,22]" class="gt_row gt_right">21</td>
<td headers="(22,26]" class="gt_row gt_right">17</td>
<td headers="(26,30]" class="gt_row gt_right">8</td>
<td headers="(30,34]" class="gt_row gt_right">1</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::


### SES/parental education


::: {#tbl-participants-edu .cell tbl-cap='Participant exposure to the language (DoE) and socio-economic status (SES).'}

```{.r .cell-code}
participants %>%
    mutate(id = as.character(id)) |> 
    left_join(bvq_data$logs,
              by = join_by(id, time, age, lp, doe_catalan, doe_spanish, edu_parent)) %>%
    mutate(
        edu_parent = edu_parent |> 
            as.factor() |> 
            fct_na_value_to_level(),
        doe_catalan = cut(doe_catalan,
                          breaks = seq(0, 1, 0.2),
                          include.lowest = TRUE)
    ) %>%
    count(doe_catalan, edu_parent) %>%
    right_join(
        expand_grid(
            doe_catalan = levels(.$doe_catalan),
            edu_parent = unique(bvq_data$logs$edu_parent)
        ),
        by = join_by(doe_catalan, edu_parent)
    ) %>%
    rename(total = n) %>%
    pivot_wider(names_from = edu_parent,
                values_from = total,
                values_fill = 0) %>%
    mutate(total = rowSums(cbind(.[,3:8]), na.rm = TRUE)) %>%
    rename(Total = total) %>%
    select(-"NA") %>%
    gt() |> 
    cols_label(doe_catalan = "DoE Catalan") %>%
    sub_missing(columns = everything(),
                missing_text = "--") %>%
    tab_spanner("Educational attainment", 
                Secondary:Primary) %>%
    tab_style(
        style = list(cell_borders(weight = px(2),
                                  sides = "left", 
                                  color = "grey50")),
        locations = cells_body(columns = 8,
                               rows = 1:5)
    ) %>%
    tab_style(
        cell_text(style = "italic"),
        cells_column_labels(columns = 2:7)
    ) %>%
    tab_style(
        cell_text(weight = "bold"),
        cells_column_labels(columns = c(1, 8))
    ) %>%
    tab_style(
        cell_text(weight = "bold"),
        cells_column_spanners(spanners = "Educational attainment")
    )
```

::: {.cell-output-display}
```{=html}
<div id="bfagkgavys" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bfagkgavys table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#bfagkgavys thead, #bfagkgavys tbody, #bfagkgavys tfoot, #bfagkgavys tr, #bfagkgavys td, #bfagkgavys th {
  border-style: none;
}

#bfagkgavys p {
  margin: 0;
  padding: 0;
}

#bfagkgavys .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#bfagkgavys .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#bfagkgavys .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#bfagkgavys .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#bfagkgavys .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bfagkgavys .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bfagkgavys .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bfagkgavys .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#bfagkgavys .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#bfagkgavys .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bfagkgavys .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bfagkgavys .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#bfagkgavys .gt_spanner_row {
  border-bottom-style: hidden;
}

#bfagkgavys .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#bfagkgavys .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#bfagkgavys .gt_from_md > :first-child {
  margin-top: 0;
}

#bfagkgavys .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bfagkgavys .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#bfagkgavys .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#bfagkgavys .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#bfagkgavys .gt_row_group_first td {
  border-top-width: 2px;
}

#bfagkgavys .gt_row_group_first th {
  border-top-width: 2px;
}

#bfagkgavys .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bfagkgavys .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bfagkgavys .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bfagkgavys .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bfagkgavys .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bfagkgavys .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bfagkgavys .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#bfagkgavys .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bfagkgavys .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bfagkgavys .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bfagkgavys .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bfagkgavys .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bfagkgavys .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bfagkgavys .gt_left {
  text-align: left;
}

#bfagkgavys .gt_center {
  text-align: center;
}

#bfagkgavys .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bfagkgavys .gt_font_normal {
  font-weight: normal;
}

#bfagkgavys .gt_font_bold {
  font-weight: bold;
}

#bfagkgavys .gt_font_italic {
  font-style: italic;
}

#bfagkgavys .gt_super {
  font-size: 65%;
}

#bfagkgavys .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#bfagkgavys .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bfagkgavys .gt_indent_1 {
  text-indent: 5px;
}

#bfagkgavys .gt_indent_2 {
  text-indent: 10px;
}

#bfagkgavys .gt_indent_3 {
  text-indent: 15px;
}

#bfagkgavys .gt_indent_4 {
  text-indent: 20px;
}

#bfagkgavys .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" style="font-weight: bold;" scope="col" id="DoE Catalan">DoE Catalan</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="6" style="font-weight: bold;" scope="colgroup" id="Educational attainment">
        <span class="gt_column_spanner">Educational attainment</span>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;" scope="col" id="Total">Total</th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Secondary">Secondary</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Complementary">Complementary</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Vocational">Vocational</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="University">University</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="No education">No education</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Primary">Primary</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="doe_catalan" class="gt_row gt_left">[0,0.2]</td>
<td headers="Secondary" class="gt_row gt_right">2</td>
<td headers="Complementary" class="gt_row gt_right">1</td>
<td headers="Vocational" class="gt_row gt_right">14</td>
<td headers="University" class="gt_row gt_right">38</td>
<td headers="No education" class="gt_row gt_right">–</td>
<td headers="Primary" class="gt_row gt_right">–</td>
<td headers="Total" class="gt_row gt_right" style="border-left-width: 2px; border-left-style: solid; border-left-color: grey50;">53</td></tr>
    <tr><td headers="doe_catalan" class="gt_row gt_left">(0.2,0.4]</td>
<td headers="Secondary" class="gt_row gt_right">2</td>
<td headers="Complementary" class="gt_row gt_right">–</td>
<td headers="Vocational" class="gt_row gt_right">16</td>
<td headers="University" class="gt_row gt_right">61</td>
<td headers="No education" class="gt_row gt_right">1</td>
<td headers="Primary" class="gt_row gt_right">–</td>
<td headers="Total" class="gt_row gt_right" style="border-left-width: 2px; border-left-style: solid; border-left-color: grey50;">78</td></tr>
    <tr><td headers="doe_catalan" class="gt_row gt_left">(0.4,0.6]</td>
<td headers="Secondary" class="gt_row gt_right">4</td>
<td headers="Complementary" class="gt_row gt_right">2</td>
<td headers="Vocational" class="gt_row gt_right">11</td>
<td headers="University" class="gt_row gt_right">73</td>
<td headers="No education" class="gt_row gt_right">1</td>
<td headers="Primary" class="gt_row gt_right">1</td>
<td headers="Total" class="gt_row gt_right" style="border-left-width: 2px; border-left-style: solid; border-left-color: grey50;">88</td></tr>
    <tr><td headers="doe_catalan" class="gt_row gt_left">(0.6,0.8]</td>
<td headers="Secondary" class="gt_row gt_right">–</td>
<td headers="Complementary" class="gt_row gt_right">–</td>
<td headers="Vocational" class="gt_row gt_right">14</td>
<td headers="University" class="gt_row gt_right">95</td>
<td headers="No education" class="gt_row gt_right">–</td>
<td headers="Primary" class="gt_row gt_right">–</td>
<td headers="Total" class="gt_row gt_right" style="border-left-width: 2px; border-left-style: solid; border-left-color: grey50;">109</td></tr>
    <tr><td headers="doe_catalan" class="gt_row gt_left">(0.8,1]</td>
<td headers="Secondary" class="gt_row gt_right">–</td>
<td headers="Complementary" class="gt_row gt_right">3</td>
<td headers="Vocational" class="gt_row gt_right">4</td>
<td headers="University" class="gt_row gt_right">89</td>
<td headers="No education" class="gt_row gt_right">–</td>
<td headers="Primary" class="gt_row gt_right">–</td>
<td headers="Total" class="gt_row gt_right" style="border-left-width: 2px; border-left-style: solid; border-left-color: grey50;">96</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::


## Items



::: {#tbl-items-summary .cell tbl-cap='Lexical frequencies. Mean, standard error, and 95% confidence interval of lexical frequencies of items included in the Catalan and Spanish lists, reported separately for identical cognates, non-identical cognates, and non-cognates.'}

```{.r .cell-code}
items <- left_join(items, select(bvq_data$pool, te, item, language, class, semantic_category))

items %>%
    unnest(list) %>%
    summarise(n = n(),
              across(c(freq, n_phon, lv),
                     lst(mean, sd, min, max)),
              .by = c(language, list)) |> 
    gt(groupname_col = "language", 
       rowname_col = "list") %>%
    fmt_number(-c(list, n),
               decimals = 1) %>%
    tab_spanner("Frequency (Zipf)",
                columns = starts_with("freq_")) %>%
    tab_spanner("Length (Phonemes)",
                columns = starts_with("n_phon_")) %>%
    tab_spanner("Levenshtein (cognateness)", 
                columns = starts_with("lv_")) %>%
    cols_merge_uncert(col_val = freq_mean,
                      col_uncert = freq_sd) %>%
    cols_merge_uncert(col_val = n_phon_mean,
                      col_uncert = n_phon_sd) %>%
    cols_merge_uncert(col_val = lv_mean, 
                      col_uncert = lv_sd) %>%
    cols_merge_range(col_begin = freq_min, 
                     col_end = freq_max) %>%
    cols_merge_range(col_begin = n_phon_min, 
                     col_end = n_phon_max) %>%
    cols_merge_range(col_begin = lv_min,
                     col_end = lv_max) %>%
    cols_label(list = "",
               n = md("*N*"),
               freq_mean = md("Mean ± SD"),
               freq_min = md("Range"),
               n_phon_mean = md("Mean ± SD"),
               n_phon_min = md("Range"),
               lv_mean = md("Mean ± SD"),
               lv_min = md("Range")) %>%
    tab_style(cell_text(style = "italic"),
              cells_column_labels(everything())) %>%
    tab_style(cell_text(weight = "bold"),
              cells_column_spanners(everything()))
```

::: {.cell-output-display}
```{=html}
<div id="botzpmpmmy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#botzpmpmmy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#botzpmpmmy thead, #botzpmpmmy tbody, #botzpmpmmy tfoot, #botzpmpmmy tr, #botzpmpmmy td, #botzpmpmmy th {
  border-style: none;
}

#botzpmpmmy p {
  margin: 0;
  padding: 0;
}

#botzpmpmmy .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#botzpmpmmy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#botzpmpmmy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#botzpmpmmy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#botzpmpmmy .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#botzpmpmmy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#botzpmpmmy .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#botzpmpmmy .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#botzpmpmmy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#botzpmpmmy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#botzpmpmmy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#botzpmpmmy .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#botzpmpmmy .gt_spanner_row {
  border-bottom-style: hidden;
}

#botzpmpmmy .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#botzpmpmmy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#botzpmpmmy .gt_from_md > :first-child {
  margin-top: 0;
}

#botzpmpmmy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#botzpmpmmy .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#botzpmpmmy .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#botzpmpmmy .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#botzpmpmmy .gt_row_group_first td {
  border-top-width: 2px;
}

#botzpmpmmy .gt_row_group_first th {
  border-top-width: 2px;
}

#botzpmpmmy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#botzpmpmmy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#botzpmpmmy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#botzpmpmmy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#botzpmpmmy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#botzpmpmmy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#botzpmpmmy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#botzpmpmmy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#botzpmpmmy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#botzpmpmmy .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#botzpmpmmy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#botzpmpmmy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#botzpmpmmy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#botzpmpmmy .gt_left {
  text-align: left;
}

#botzpmpmmy .gt_center {
  text-align: center;
}

#botzpmpmmy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#botzpmpmmy .gt_font_normal {
  font-weight: normal;
}

#botzpmpmmy .gt_font_bold {
  font-weight: bold;
}

#botzpmpmmy .gt_font_italic {
  font-style: italic;
}

#botzpmpmmy .gt_super {
  font-size: 65%;
}

#botzpmpmmy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#botzpmpmmy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#botzpmpmmy .gt_indent_1 {
  text-indent: 5px;
}

#botzpmpmmy .gt_indent_2 {
  text-indent: 10px;
}

#botzpmpmmy .gt_indent_3 {
  text-indent: 15px;
}

#botzpmpmmy .gt_indent_4 {
  text-indent: 20px;
}

#botzpmpmmy .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-style: italic;" scope="col" id="&lt;em&gt;N&lt;/em&gt;"><em>N</em></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;" scope="colgroup" id="Frequency (Zipf)">
        <span class="gt_column_spanner">Frequency (Zipf)</span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;" scope="colgroup" id="Length (Phonemes)">
        <span class="gt_column_spanner">Length (Phonemes)</span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;" scope="colgroup" id="Levenshtein (cognateness)">
        <span class="gt_column_spanner">Levenshtein (cognateness)</span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Mean ± SD">Mean ± SD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Range">Range</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Mean ± SD">Mean ± SD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Range">Range</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Mean ± SD">Mean ± SD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-style: italic;" scope="col" id="Range">Range</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="8" class="gt_group_heading" scope="colgroup" id="Catalan">Catalan</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">A</th>
<td headers="Catalan stub_1_1 n" class="gt_row gt_right">167</td>
<td headers="Catalan stub_1_1 freq_mean" class="gt_row gt_right">6.0 ± 0.2</td>
<td headers="Catalan stub_1_1 freq_min" class="gt_row gt_right">5.2–6.5</td>
<td headers="Catalan stub_1_1 n_phon_mean" class="gt_row gt_right">5.2 ± 1.6</td>
<td headers="Catalan stub_1_1 n_phon_min" class="gt_row gt_right">2.0–11.0</td>
<td headers="Catalan stub_1_1 lv_mean" class="gt_row gt_right">0.4 ± 0.3</td>
<td headers="Catalan stub_1_1 lv_min" class="gt_row gt_right">0.0–1.0</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">B</th>
<td headers="Catalan stub_1_2 n" class="gt_row gt_right">151</td>
<td headers="Catalan stub_1_2 freq_mean" class="gt_row gt_right">6.0 ± 0.2</td>
<td headers="Catalan stub_1_2 freq_min" class="gt_row gt_right">5.2–6.5</td>
<td headers="Catalan stub_1_2 n_phon_mean" class="gt_row gt_right">5.1 ± 1.6</td>
<td headers="Catalan stub_1_2 n_phon_min" class="gt_row gt_right">2.0–10.0</td>
<td headers="Catalan stub_1_2 lv_mean" class="gt_row gt_right">0.3 ± 0.2</td>
<td headers="Catalan stub_1_2 lv_min" class="gt_row gt_right">0.0–0.9</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">C</th>
<td headers="Catalan stub_1_3 n" class="gt_row gt_right">149</td>
<td headers="Catalan stub_1_3 freq_mean" class="gt_row gt_right">6.0 ± 0.2</td>
<td headers="Catalan stub_1_3 freq_min" class="gt_row gt_right">5.2–6.5</td>
<td headers="Catalan stub_1_3 n_phon_mean" class="gt_row gt_right">5.2 ± 1.6</td>
<td headers="Catalan stub_1_3 n_phon_min" class="gt_row gt_right">2.0–9.0</td>
<td headers="Catalan stub_1_3 lv_mean" class="gt_row gt_right">0.4 ± 0.3</td>
<td headers="Catalan stub_1_3 lv_min" class="gt_row gt_right">0.0–1.0</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">D</th>
<td headers="Catalan stub_1_4 n" class="gt_row gt_right">156</td>
<td headers="Catalan stub_1_4 freq_mean" class="gt_row gt_right">6.0 ± 0.2</td>
<td headers="Catalan stub_1_4 freq_min" class="gt_row gt_right">5.2–6.5</td>
<td headers="Catalan stub_1_4 n_phon_mean" class="gt_row gt_right">5.2 ± 1.6</td>
<td headers="Catalan stub_1_4 n_phon_min" class="gt_row gt_right">2.0–10.0</td>
<td headers="Catalan stub_1_4 lv_mean" class="gt_row gt_right">0.3 ± 0.3</td>
<td headers="Catalan stub_1_4 lv_min" class="gt_row gt_right">0.0–0.9</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="8" class="gt_group_heading" scope="colgroup" id="Spanish">Spanish</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub">A</th>
<td headers="Spanish stub_1_5 n" class="gt_row gt_right">168</td>
<td headers="Spanish stub_1_5 freq_mean" class="gt_row gt_right">6.0 ± 0.2</td>
<td headers="Spanish stub_1_5 freq_min" class="gt_row gt_right">5.2–6.5</td>
<td headers="Spanish stub_1_5 n_phon_mean" class="gt_row gt_right">5.5 ± 1.4</td>
<td headers="Spanish stub_1_5 n_phon_min" class="gt_row gt_right">3.0–10.0</td>
<td headers="Spanish stub_1_5 lv_mean" class="gt_row gt_right">0.4 ± 0.3</td>
<td headers="Spanish stub_1_5 lv_min" class="gt_row gt_right">0.0–1.0</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub">B</th>
<td headers="Spanish stub_1_6 n" class="gt_row gt_right">157</td>
<td headers="Spanish stub_1_6 freq_mean" class="gt_row gt_right">6.0 ± 0.2</td>
<td headers="Spanish stub_1_6 freq_min" class="gt_row gt_right">5.2–6.5</td>
<td headers="Spanish stub_1_6 n_phon_mean" class="gt_row gt_right">5.5 ± 1.6</td>
<td headers="Spanish stub_1_6 n_phon_min" class="gt_row gt_right">3.0–10.0</td>
<td headers="Spanish stub_1_6 lv_mean" class="gt_row gt_right">0.3 ± 0.2</td>
<td headers="Spanish stub_1_6 lv_min" class="gt_row gt_right">0.0–1.0</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub">C</th>
<td headers="Spanish stub_1_7 n" class="gt_row gt_right">156</td>
<td headers="Spanish stub_1_7 freq_mean" class="gt_row gt_right">6.0 ± 0.2</td>
<td headers="Spanish stub_1_7 freq_min" class="gt_row gt_right">5.2–6.5</td>
<td headers="Spanish stub_1_7 n_phon_mean" class="gt_row gt_right">5.5 ± 1.6</td>
<td headers="Spanish stub_1_7 n_phon_min" class="gt_row gt_right">3.0–10.0</td>
<td headers="Spanish stub_1_7 lv_mean" class="gt_row gt_right">0.4 ± 0.3</td>
<td headers="Spanish stub_1_7 lv_min" class="gt_row gt_right">0.0–1.0</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub">D</th>
<td headers="Spanish stub_1_8 n" class="gt_row gt_right">163</td>
<td headers="Spanish stub_1_8 freq_mean" class="gt_row gt_right">6.0 ± 0.2</td>
<td headers="Spanish stub_1_8 freq_min" class="gt_row gt_right">5.2–6.5</td>
<td headers="Spanish stub_1_8 n_phon_mean" class="gt_row gt_right">5.5 ± 1.5</td>
<td headers="Spanish stub_1_8 n_phon_min" class="gt_row gt_right">3.0–10.0</td>
<td headers="Spanish stub_1_8 lv_mean" class="gt_row gt_right">0.4 ± 0.3</td>
<td headers="Spanish stub_1_8 lv_min" class="gt_row gt_right">0.0–1.0</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::


### Lexical frequency

Word lexical frequency ($Frequency$): lexical frequency of the word in its corresponding language, expressed as Zipf scores [@van2014subtlex; @zipf1949human]. This variable ranges from 0 to 7, and follows and approximates a normal distribution, with most values in a corpus ranging from 3 to 5 points. Lexical frequencies were extracted from the English corpora of the [CHILDES](https://childes.talkbank.org/) database using the [{childesr}](https://langcog.github.io/childes-db-website/) R package. Available words in the corpora were then mapped to their Catalan and Spanish translations. Therefore, the Catalan and Spanish word-forms of the same translation equivalent had the same value for lexical frequency. Responses to words with missing lexical frequencies were excluded from analyses of the total number of items). 



::: {.cell}

```{.r .cell-code}
mean_sd <- function(x, ...) {
    m <- mean(x, ...)
    s <- sd(x, ...)
    data.frame(mean = m, ymin = m-s, ymax = m+s)
}

items %>% 
    distinct(te, .keep_all = TRUE) %>% 
    ggplot(aes(x = freq)) +
    geom_histogram(fill = "grey60",
                   colour = "white",
                   bins = 30)  +
    labs(x = "Lexical frequency (Zipf score)", y = "Number of words") +
    theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        legend.title = element_blank()
    )
```

::: {.cell-output-display}
![Distribution of lexical frequency across items. Lexical frequencies were extracted from the English CHILDES corpora, and mapped to their Catalan and Spanish translations. Therefore Catalan and Spanish word-forms that belong to the same translation equivalent had the same lexical frequency. The black dot and interval indicate the mean and standard deviation.](index_files/figure-html/fig-items-freq-1.png){#fig-items-freq width=6000}
:::
:::


### Word length

Word length ($Length$), computed as the number of phonemes in the phonological transcription of the word-form in [SAMPA](https://en.wikipedia.org/wiki/SAMPA) format.



::: {.cell}

```{.r .cell-code}
items %>%
    count(language, n_phon) %>%
    ggplot(aes(x = n_phon, y = n)) +
    facet_wrap(~language, ncol = 1) +
    geom_col(fill = "grey60", colour = "white") +
    geom_text(aes(label = n),
              position = position_nudge(y = 5),
              colour = "black") +
    labs(x = "Phonemes",
         y = "Number of words",
         fill = "Language",
         shape = "Language",
         colour = "Language") +
    scale_x_continuous(breaks = 1:15) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_line(),
          legend.title = element_blank())
```

::: {.cell-output-display}
![Distribution of number of phonemes across items](index_files/figure-html/fig-items-nphon-1.png){#fig-items-nphon width=7000}
:::
:::


### Levenshtein (cognateness)

Phonological similarity between translation equivalents was computed as the normalised Levenshtein similarity between the word-form and its translation in the other language [@levenshtein1966binary]. This score is calculated by first calculating the Levenshtein distance between the two transcriptions (number of insertions, deletions or replacements needed for the shortest transcription to become identical to the longer transcription), then dividing the resulting value by the length of the longest transcription, and finally subtracting this value from 1. This results in a proportion that indicates how much the two phonological transcriptions of the translation equivalent are similar to each other, ranging from 0% (no similarity at all) to 100% (both transcriptions are identical) [see @floccia2018vocabulary; @fourtassi2020growth; @laing2022phonological for similar approaches]. 


::: {.cell}

```{.r .cell-code}
items %>% 
    distinct(te, .keep_all = TRUE) %>% 
    mutate(lv = cut(lv, breaks = seq(0, 1, 0.1), include.lowest = TRUE)) %>% 
    count(lv) %>% 
    mutate(prop = n/sum(n),
           labels = paste0(
               n, " (",
               scales::percent(prop, accuracy = 0.1), 
               ")")) %>% 
    ggplot(aes(x = lv, n)) +
    geom_col(fill = "grey60", colour = "white") +
    geom_text(aes(label = labels),
              position = position_nudge(y = 3), 
              size = 3.5) +
    labs(x = "Levenshtein distance", y = "Number of words") +
    theme(
        panel.grid.major.x = element_blank(),
        legend.position = "top"
    )
```

::: {.cell-output-display}
![Distribution of Levenshtein similarities across items](index_files/figure-html/fig-items-lv-1.png){#fig-items-lv width=9000}
:::
:::


### Frequency by word length


::: {.cell}

```{.r .cell-code}
items %>% 
    drop_na(freq) %>% 
    distinct(te, .keep_all = TRUE) %>% 
    count(n_phon, freq) %>% 
    ggplot(aes(n_phon, freq)) +
    geom_point(size = 2,
               alpha = 0.5,
               colour = "grey60", 
               shape = 1, 
               stroke = 1,
               position = position_jitter(width = 0.1)) +
    geom_smooth(method = "lm",
                formula = "y ~ x" ,
                colour = "black") +
    labs(
        x = "# Phonemes", 
        y = "Lexical frequency (Zipf score)", 
        colour = "Language", 
        fill = "Language"
    ) +
    scale_x_continuous(breaks = seq(0, 16, 1)) +
    theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
    ) 
```

::: {.cell-output-display}
![Association betwen number of lexical frequency and number of phonemes](index_files/figure-html/fig-items-frequency-phonemes-1.png){#fig-items-frequency-phonemes width=8000}
:::
:::


### Frequency by Levenshtein


::: {.cell}

```{.r .cell-code}
items %>% 
    distinct(te, .keep_all = TRUE) %>% 
    drop_na(freq) %>% 
    count(freq, lv) %>% 
    ggplot(aes(freq, lv)) +
    geom_point(size = 2,
               shape = 1, 
               colour = "grey60",
               stroke = 1) +
    geom_smooth(method = "lm", 
                formula = "y ~ x", 
                colour ="black") +
    labs(y = "Levenshtein similarity", 
         x = "Lexical frequency (Zipf score)", 
         colour = "Language", 
         fill = "Language") +
    scale_y_continuous(labels = scales::percent) +
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank()
    )
```

::: {.cell-output-display}
![Association betwen lexical frequency and phonological similarity](index_files/figure-html/fig-items-frequency-lv-1.png){#fig-items-frequency-lv width=7000}
:::
:::


### Word length by Levenshtein


::: {.cell}

```{.r .cell-code}
items %>% 
    distinct(te, .keep_all = TRUE) %>% 
    drop_na(freq) %>% 
    count(n_phon, lv) %>% 
    ggplot(aes(n_phon, lv, size = n)) +
    geom_point(shape = 1,
               stroke = 1,
               colour = "grey60") +
    geom_smooth(method = "lm",
                formula = "y ~ x",
                size = 1,
                colour = "black") +
    labs(x = "# Phonemes", 
         y = "Levenshtein similarity", 
         colour = "Language", 
         fill = "Language") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(0, 16, 1)) +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) 
```

::: {.cell-output-display}
![Association between number of phonemes and phonological similarity](index_files/figure-html/fig-items-nphon-lv-1.png){#fig-items-nphon-lv width=7000}
:::
:::



### Word exposure index by lexical frequency

We created a predictor ($Exposure$) to account for the exposure rate of each child to each of the words their parents responded to, weighted by the child's exposure to the language the word belongs to. the exposure of the $i$-th child to the $j$-th word measure is the product of the lexical frequency of the word (Zipf score) and the child’s degree of exposure to the corresponding language (a proportion) (see @eq-exposure).

$$
\textbf{Exposure}_{ij} = \textbf{Frequency}_i \times \textbf{DoE}_{j}
$$ {#eq-exposure}

For instance, for a child who is reportedly exposure to Catalan 80% of the time, and to Spanish 20% of the time, the expected exposure to the word *cavall* (horse, in Catalan, with a lexical frequency of ) would be , while that of its translation to Spanish *caballo* would be  (see @eq-exposure-example):

$$
\begin{aligned}
\textbf{Exposure}_{\textbf{cavall, Catalan}} &= \textbf{Frequency}_{cavall} \times \textbf{DoE}_{Catalan} = 6.14 \times 0.8 = 4.91 \\
\textbf{Exposure}_{\textbf{caballo, Spanish}} &= \textbf{Frequency}_{caballo} \times \textbf{DoE}_{Spanish} = 6.14 \times 0.2 = 1.23
\end{aligned}
$$ {#eq-exposure-example}


::: {.cell warnings='false'}

```{.r .cell-code}
responses %>%
    mutate(
        exposure = cut(exposure,
                       breaks = seq(0, 9, 1),
                       include.lowest = TRUE)) %>%
    count(exposure, language) %>%
    ggplot(aes(exposure, n)) +
    facet_wrap( ~ language) +
    geom_col(colour = "white",
             fill = "grey60") +
    labs(x = "Exposure-weighted frequency\n(Frequency \u00d7 DoE)",
         y = "Number of observations",
         fill = "Language") +
    
    responses %>%
    mutate(exposure = cut(exposure,
                          breaks = seq(0, 9, 1),
                          include.lowest = TRUE),
           freq = cut(freq, 
                      breaks = seq(0, 9, 0.25), 
                      include.lowest = TRUE)) %>%
    count(exposure, freq, language) %>%
    ggplot(aes(x = freq,
               y = exposure,
               size = n)) +
    facet_wrap( ~ language) +
    geom_point(colour = "grey60",
               alpha = 1/2) +
    labs(x = "Frequency (Zipf score)",
         y = "Exposure-weighted frequency\\
         (Frequency \u00d7 DoE)",
         colour = "Language",
         fill = "Language",
         size = "Number of observations") +
    theme(panel.grid = element_blank()) +
    plot_layout(ncol = 1) &
    scale_colour_manual(values = clrs[c(1, 4)]) &
    scale_fill_manual(values = clrs[c(1, 4)]) &
    guides(fill = "none", colour = "none") &
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none")
```

::: {.cell-output-display}
![Relationship between lexical frequency and language exposure-weighted lexical frequency](index_files/figure-html/fig-exposure-distribution-1.png){#fig-exposure-distribution width=13000}
:::
:::



## Data analysis

We used multilevel ordinal regression to model the cumulative probability of a *No* response, a *Understands* response, or a *Understands and Says* response [@burkner2019ordinal] using the *logit* link function. The likelihood of the cumulative probability distribution of the responses is defined by @eq-likelihood (see @fig-likelihood for a graphical representation). 


$$
\begin{aligned}
\textbf{Likelihood:} \\
y_{ij} &\sim \text{Cumulative}(p_{k})
\end{aligned}
$$ {#eq-likelihood}


where:

- $y$ is an observed response ($y \in \{\text{No, Understands, Understands and Says}\}$)
- $i$ is the participant index
- $j$ is the translation equivalent (TE) index
- $p_{k}$ is a probability ($p \in (0, 1)$) that indicates the threshold $k$ ($k \in (1, 2)$)) between two response categories in the latent distribution
$p_{k}$ is then estimated using a logit regression model as indicated in @eq-linear.



::: {.cell}

```{.r .cell-code}
dist_df <- tribble(~mean, ~sd, 0,   1)

thresholds <- data.frame(
    ymin = c(-Inf, -1.5, 1.5),
    ymax = c(-1.5, 1.5, Inf),
    xmin = -Inf,
    xmax = Inf,
    .category = str_wrap(c(
        "No", 
        "Understands",
        "Understands and Says"
    ),
    width = 12)
)

thresholds %>%
    ggplot(aes(ymin = ymin, ymax = ymax,
               xmin = xmin, xmax = xmax,
               fill = .category)) +
    geom_rect(alpha = 0.5) +
    stat_dist_slab(
        data = dist_df, 
        aes(dist = "norm", arg1 = mean, arg2 = sd),
        position = "dodge", inherit.aes = FALSE,
        fill = NA, colour = "black"
    ) +
    geom_hline(yintercept = c(-1.5, 1.5), 
               linetype = "dotted",
               linewidth = 1) +
    geom_text(aes(label = .category, y = c(-3, 0, 3), x = 1),
              vjust = 1, hjust = 0.5) +
    annotate(
        geom = "text",
        label = c("Threshold~1~(p[1])", "Threshold~2~(p[2])"),
        y = c(-1.5, 2), parse = TRUE,
        x = 0.5, angle = 90, vjust = -0.5
    ) +
    labs(y = "Hypothetical response-generating latent distribution",
         x = "Response probability",
         fill = "Response type",
         colour = "Response type") +
    coord_flip() +
    scale_fill_manual(values = clrs[c(1, 4, 5)]) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(limits = c(-4, 4)) +
    theme(panel.grid = element_blank(),
          legend.position = "none")
```

::: {.cell-output-display}
![Graphical representation of the assumed latent distribution that generates observed responses in the questionnaire. Cumulative ordinal regression assumes that observed categorical responses (*No*, *Understands* and *Understands and Says* are generated from a common latent (i.e., unobserved) distribution. For threotical reasons, we refer to the dimension captured by such distribution as *Familiarity*. *Familiarity* describes a given participant's familiarity with a given word. When familiarity is low (below some threshold 1), the reulting response is *No*. When familiarity is higher than such threshold 1, but lower than some threshold 2, the resulting result is *Understands* (i.e., comprehension). When familiarity exceeds threshold 2, the resulting response is *Understands and Says* (i.e., comprehension and production). These thresholds are estimated in the cumulative regression model as intercepts 1, and 2. The rest of the coefficients in the regression model shift the response thresholds, making comprehension or production more likely or less likely. See @burkner2019ordinal for a detailed description of cumulative ordinal regression and its implementation.](index_files/figure-html/fig-likelihood-1.png){#fig-likelihood width=7000}
:::
:::



To test our hypotheses, we included several predictors in the regression model as fixed effects: the main effects of participant age ($Age$), number of phonemes ($Phonemes$), word exposure index ($Exposure$), and of phonological similarity ($Levenshtein$), the two-way interactions $Age \times Exposure$, $Age \times Levenshtein$, and $Exposure \times Leveshtein$, and the three-way interaction $Age \times Exposure \times Leveshtein$. We also included crossed random effects for participants and translation equivalents to account for the repeated measures in our dataset--each participant provided responses to multiple translation equivalents, and each translation equivalent was responded to by multiple participants [@gelman2020regression]. For both grouping variables, we included random intercepts, random slopes, and correlation parameters for all predictors were repeated measures were observed in our dataset [@barr2013random, see @eq-linear]. 

$$
\begin{aligned}
\textbf{Linear model:} \\
logit(p_{k}) = \text{ln} \frac{p_{k}}{1-p_{k}} &= (\beta_{0_{k}} + u_{0_{i_{k}}} + w_{0_{j_{k}}}) + \\
& (\beta_{1} + u_{1_{i}} + w_{1_{j}}) · \text{Age}_{i} + & \\
& (\beta_{2} + u_{2_{i}} + w_{2_{j}}) · \text{Phonemes}_{ij} + & \\
& (\beta_{3} + u_{3_{i}} + w_{3_{j}}) · \text{Exposure}_{ij} + & \\
& (\beta_{4} + u_{4_{i}}) · \text{Levenshtein}_{ij} + & \\
& (\beta_{5} + u_{5_{i}} + w_{3_{j}}) · (\text{Age}_{i} \times \text{Exposure}_{ij}) + & \\
& (\beta_{6} + u_{6_{i}}) · (\text{Age}_{i} \times \text{Levenshtein}_{ij}) + & \\
& (\beta_{7} + u_{7_{i}}) · (\text{Exposure}_{ij} \times \text{Levenshtein}_{ij}) & \\
& (\beta_{8} + u_{8_{i}}) · (\text{Age}_{i} \times \text{Exposure}_{ij} \times \text{Levenshtein}_{ij}) & \\
\end{aligned}
$$ {#eq-linear}

where:

- $i$ and $j$ index the participant and translation equivalent (TE)
- $\beta_{0_k}$ is the fixed coefficient of the regression model for the intercept of threshold $k$
- $u_{0_{i}}$ and $w_{0_{j}}$ are the by-participant and by-TE adjustments for $\beta_{0_{k}}$ (i.e., random intercepts), respectively
- $\beta_{1-8}$ are the fixed coefficients of the regression model for the predictors of interest
- $u_{1-8_{i}}$ and $w_{1-3_{j}}$ are the by-participant and by-TE adjustments for$\beta_{1-8}$ (i.e., random slopes), respectively

We used the Bayesian framework to estimate the parameters in our model. This involves using the Bayes theorem to compute a distribution (*posterior distribution*) that describes what values of each parameter in the model are more likely given the data (*likelihood*, see @eq-likelihood), and previous knowledge about such distribution (*prior distribution*, see @eq-prior) [@mcelreath2020statistical]. This posterior distribution not only informs about the most likely values of our regression coefficients of interest, but also about the uncertainty around such estimations. We used a weakly informative prior for our parameters, with the exception of the main effect of $Age$, for which we specified a strongly informative prior based on previous literature about how age affects the acquisition of words [see @eq-prior].

$$
\begin{aligned}
\\
\textbf{Prior:} \\
\beta_{0_{k}} &\sim \mathcal{N}(-0.25, 0.1) & [\mbox{Intercept/response category threshold}] \\
\beta_{1} &\sim \mathcal{N}(1, 0.1) & [\mbox{Age population-level coefficient}]\\
\beta_{2-8} &\sim \mathcal{N}(0, 1) & [\mbox{Rest of population-level coefficients}] \\
u_{0-8_{i}} &\sim \mathcal{N}(0, \sigma_{u_{0-8_{i}}}) & [\mbox{Participant-level coefficient variability}] \\
w_{0-3_{j}} &\sim \mathcal{N}(0, \sigma_{w_{0-3_{j}}}) & [\mbox{TE-level coefficient variability}] \\\\
&&\mbox{[Participant-level coefficient variability]} \\ \\
\Bigg(\begin{smallmatrix}
u_{k_{0}} \\ 
u_{1_{i}} \\ 
\vdots \\ 
u_{8_{i}} 
\end{smallmatrix}\Bigg) &\sim \mathcal{N} 
\Bigg(\Bigg(\begin{smallmatrix}0 \\
0 \\ 
\vdots \\
0\end{smallmatrix}\Bigg), \Sigma_{u}\Bigg) \\
\Sigma_{u} &= \Bigg(\begin{smallmatrix} \\
\rho_{u_{0}} & \rho_{u_{0}} \sigma_{u_{0_{k}}} \sigma_{u_{1}} & \dots & \rho_{u_{0}} \sigma_{u_{0}} \sigma_{w_{8}}\\ 
\rho_{u_{1}} \sigma_{u_{1}} \sigma_{u_{0}} & \rho_{u_{1}} & \dots & \rho_{u_{1}} \sigma_{u_{1}} \sigma_{u_{8}}\\ 
\vdots & \vdots & \vdots & \vdots \\
\rho_{8} \sigma_{u_{8}} \sigma_{u_{0_{k}}} & \dots & \dots & \rho_{u_{8}} \end{smallmatrix}\Bigg) \\
\sigma_{u_{0-8}} &\sim \mathcal{N_{+}}(1, 0.1) \\
\rho_{u} &\sim LKJcorr(2) \\
\\
&&\mbox{[TE-level coefficient variability]} \\ \\
\Bigg(\begin{smallmatrix}
w_{k_{0}}\\ 
w_{1_{j}} \\ 
\vdots \\ 
w_{3_{j}} 
\end{smallmatrix}\Bigg) &\sim \mathcal{N} \Bigg(\Bigg(\begin{smallmatrix}
0\\ 
0 \\ 
\vdots \\
0 
\end{smallmatrix}\Bigg), \Sigma_{w}\Bigg) \\
\Sigma_{w} &= \Bigg(\begin{smallmatrix} \\
\rho_{w_{0}} & \rho_{w_{0}} \sigma_{w_{0_{k}}} \sigma_{w_{1}} & \dots & \rho_{w_{0}} \sigma_{w_{0}} \sigma_{w_{3}}\\ 
\rho_{w_{1}} \sigma_{w_{1}} \sigma_{w_{0}} & \rho_{w_{1}} & \dots & \rho_{w_{1}} \sigma_{w_{1}} \sigma_{w_{3}}\\ 
\vdots & \vdots & \vdots & \vdots \\
\rho_{3} \sigma_{w_{3}} \sigma_{w_{0_{k}}} & \dots & \dots & \rho_{w_{3}} \end{smallmatrix}\Bigg) \\
\sigma_{w_{0-3}} &\sim \mathcal{N_{+}}(1, 0.1) \\
\rho_{w_{0-3}} &\sim LKJcorr(2)
\end{aligned}
$$ {#eq-prior}

where:

- $\rho_{u_{0-8}}$ and $\rho_{w_{0-3}}$ indicate the correlation parameters between the by-participant and by-TE adjustments, respectively
- $\sigma_{u_{0-8}}^2$ and $\sigma_{w_{0-3}}^2$ indicate the variance of the by-participant and by-TE variance of the adjustments, respectively
- $\mathcal{N}$ indicates a normal distribution, $\mathcal{N}_{+}$ indicates a truncated normal distribution with only positive values, and $LKJcorr$ indicates a [LKJ correlation distribution](https://mc-stan.org/docs/2_22/functions-reference/lkj-correlation.html) [@lewandowski2009generating].


::: {.panel-tabset}

#### Prior-predicted mean


::: {.cell}

```{.r .cell-code}
nd <- expand.grid(n_phon_std = 0,
                  age_std = (10:36-mean(responses$age))/(sd(responses$age)),
                  exposure_std = 0,
                  lv_std = 0)

prior_epreds <- add_epred_draws(nd, 
                                model_fit_prior,
                                ndraws = 50, 
                                re_formula = NA) |> 
    filter(.category != "No") |> 
    pivot_wider(names_from = ".category", values_from = ".epred") |> 
    mutate(Understands = Understands + `Understands and Says`) |> 
    pivot_longer(c(Understands, `Understands and Says`),
                 names_to = ".category",
                 values_to = ".epred") |> 
    mutate(.category = factor(.category,
                              levels = c("Understands",
                                         "Understands and Says"),
                              labels = c("Comprehension\n(Understands)",
                                         "Production\n(Understands and Says)")))

prior_epreds |> 
    ggplot(aes(x = age_std, y = .epred)) +
    facet_wrap(~ .category) +
    geom_line(linewidth = 3/4,
              alpha = 1/2,
              aes(group = .draw),
              colour = "grey50") + 
    labs(x = "Age (months)", 
         y = "Probability of acquisition",
         colour = "Credible interval", 
         fill = "Credible interval") +
    scale_x_continuous(breaks = scale(seq(10, 36, 4), 
                                      mean(responses$age), 
                                      sd(responses$age))[, 1],
                       labels = seq(10, 36, 4)) +
    theme(legend.position = "top",
          axis.ticks.x = element_line(colour = "black"),
          panel.grid = element_blank())
```

::: {.cell-output-display}
![Expected prior-predicted mean.](index_files/figure-html/fig-prior-mean-1.png){#fig-prior-mean width=90%}
:::
:::



#### Prior-predictions


::: {.cell}

```{.r .cell-code}
nd <- expand.grid(
    n_phon_std = 0,
    age_std = (10:36-mean(responses$age))/(sd(responses$age)),
    exposure_std = 0,
    lv_std = 0
)

m <- add_predicted_draws(nd, model_fit_prior, ndraws = NULL, re_formula = NA)

m %>% 
    count(age_std, .prediction) %>% 
    pivot_wider(names_from = .prediction, values_from = n) %>% 
    mutate(Understands = Understands + `Understands and Says`) %>% 
    pivot_longer(c(No, Understands, `Understands and Says`),
                 names_to = ".prediction", 
                 values_to = "n") %>%
    ggplot(aes(x = age_std, y = n, fill = .prediction)) +
    geom_col(position = position_fill()) +
    labs(
        x = "Age (months)", 
        y = "Probability of acquisition",
        colour = "Category", 
        fill = "Category"
    ) +
    scale_x_continuous(
        breaks = (seq(10, 36, 4)-mean(responses$age))/(sd(responses$age)),
        labels = seq(10, 36, 4)
    ) +
    scale_fill_manual(values = clrs[c(1, 4, 5)]) +
    scale_y_continuous(labels = scales::percent) +
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.75),
        strip.background = element_rect(fill = "grey", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA)
    )
```

::: {.cell-output-display}
![Expected prior predictions](index_files/figure-html/fig-prior-predictions-1.png){#fig-prior-predictions width=7000}
:::
:::


:::



### Model settings

-   OS: Windows 10 x64 (build 22621), x86_64-w64-mingw32/x64 (64-bit)
-   R version: R version 4.2.2 (2022-10-31 ucrt)
-   Algorithm: Hamiltonian Monte Carlo (No U-turn Sampler)
-   Engine: Stan (`brms` interface, 2.19.2)
-   `brms` backend: CmdStanR (`cmdstanr` )
-   Chains: 4
-   Cores: 4
-   Iterations: 1,000 (500)

More details in Appendix: Session Info.

### R code (`brms`)



::: {.cell}

```{.r .cell-code}
print(model_fit$formula)
```

::: {.cell-output .cell-output-stdout}
```
response ~ age_std * exposure_std * lv_std + n_phon_std + (1 + age_std * exposure_std * lv_std + n_phon_std | id) + (1 + age_std * exposure_std + n_phon_std | te) 
```
:::
:::

:::

# Results

## Descriptive statistics

::: panel-tabset

### By number of phonemes


::: {.cell lab='fig-responses-age-phonemes'}

```{.r .cell-code}
responses %>% 
    mutate(
        age = floor(age),
        n_phon = cut_interval(n_phon, n = 4)
    ) %>%
    count(age, n_phon, response) %>% 
    ggplot(aes(age, n, fill = response)) +
    facet_wrap(~n_phon) +
    geom_col(position = position_fill()) +
    labs(x = "Age (months)", y = "Proportion of responses", fill = "Response") +
    scale_fill_manual(values = clrs[c(1, 4, 5)]) +
    scale_y_continuous(labels = scales::percent) +
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank()
    )
```

::: {.cell-output-display}
![Proportion of responses to each category by age and number of phonemes](index_files/figure-html/unnamed-chunk-16-1.png){width=7000}
:::
:::


### By exposure


::: {.cell lab='fig-responses-age-exposure'}

```{.r .cell-code}
responses %>% 
    mutate(
        age = floor(age),
        exposure = cut_interval(exposure, n = 4)
    ) %>%
    count(age, exposure, response) %>% 
    ggplot(aes(age, n, fill = response)) +
    facet_wrap(~exposure) +
    geom_col(position = position_fill()) +
    labs(x = "Age (months)", y = "Proportion of responses", fill = "Response") +
    scale_fill_manual(values = clrs[c(1, 4, 5)]) +
    scale_y_continuous(labels = scales::percent) +
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank()
    )
```

::: {.cell-output-display}
![Proportion of responses to each category by age and exposure.](index_files/figure-html/unnamed-chunk-17-1.png){width=7000}
:::
:::


### By Levenshtein


::: {.cell lab='fig-responses-age-lv'}

```{.r .cell-code}
responses %>%
    mutate(
        age = floor(age),
        exposure = cut_interval(exposure, n = 4, labels = paste0("Q", 1:4)),
        lv = cut_interval(lv, n = 4)
    ) %>%
    group_by(te, lv, exposure, response) %>%
    count(lv, exposure, response) %>%
    pivot_wider(names_from = response,
                values_from = n,
                values_fill = 0) %>%
    mutate(
        `Understands` = `Understands` + `Understands and Says`,
        n_total = sum(No, `Understands`, `Understands and Says`)
    ) %>%
    pivot_longer(
        c(`No`, `Understands`, `Understands and Says`),
        names_to = "response",
        values_to = "n"
    ) %>%
    filter(response != "No") %>%
    mutate(prop = n / n_total) %>%
    # group_by(age, lv, exposure, response) %>%
    # summarise(prop_mean = mean(prop), .groups = "drop") %>%
    ggplot(aes(exposure, prop, colour = lv, fill = lv)) +
    facet_wrap(~ response, ncol = 1) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    stat_dots(aes(group = lv),
              position = position_dodge(width = 1),
              layout = "swarm",
              side = "both",
              colour = "grey80",
              fill = "grey80"
    ) +
    stat_summary(geom = "errorbar",
                 fun.data = "mean_se",
                 position = position_dodge(width = 1),
                 width = 0.25) +
    stat_summary(geom = "point",
                 fun = "mean",
                 position = position_dodge(width = 1),
                 size = 2) +
    labs(y = "Proportion of responses", 
         x = "Exposure",
         fill = "Levenshtein", 
         colour = "Levenshtein") +
    scale_colour_manual(values = clrs[c(1, 4, 3, 5)]) +
    scale_fill_manual(values = clrs[c(1, 4, 3, 5)]) +
    
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank()
    )
```

::: {.cell-output-display}
![Proportion of responses to each category by age and Levenhstein similarity](index_files/figure-html/unnamed-chunk-18-1.png){width=7000}
:::
:::

:::


## Fixed effects


::: {#tbl-coefs .cell tbl-cap='Posterior distribution of regression coefficients.' tbl-note='Median: median of the posterior distribution in the probability scale. 95\% HDI: 95\% highest density interval of the distribution; narrowest range of values that contains 95\% of the distribution, and therefore is 95\% likely to contain the true value, given the data. On top of each distribution, we indicate the proportion of posterior samples in the 95\% HDI that fall into the region of practical equivalence (ROPE). This proportion indicates the probability of the true value of the coefficient being equivalent to zero. Lower values indicate that the true value is unlikely to be zero of equivalent to zero.'}

```{.r .cell-code}
# summarise posterior draws
intercept_label <- "Intercepts (at 22 months)"

model_summary |>
    select(.variable_name, .median, .lower, .upper, .rope_overlap, .type) |>
    mutate(across(.median:.upper,
                  \(x) ifelse(.type==intercept_label, plogis(x), x/4))) |> 
    gt(groupname_col = ".type", 
       rowname_col = ".variable_name") |>
    fmt_percent(.rope_overlap, decimals = 3) |> 
    fmt_number(c(.median:.upper), decimals = 3) |>
    cols_merge(c(.lower, .upper),
               pattern = "[{1}, {2}]") |>
    cols_label(
        .variable_name = "Parameter",
        .median = "Median",
        .lower = md("95% HDI"),
        .rope_overlap = md("*p*(H0)")
    ) |>
    tab_style(cell_text(weight = "bold"),
              cells_column_labels(columns = 1:5)) |>
    tab_style(cell_text(align = "left"),
              cells_title(groups = "subtitle")) |> 
    tab_style(cell_text(size = "small"), 
              list(cells_body(),
                   cells_row_groups(),
                   cells_stub())) |> 
    tab_style(cell_text(style = "italic"),
              cells_row_groups())
```

::: {.cell-output-display}
```{=html}
<div id="vujddouzut" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vujddouzut table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#vujddouzut thead, #vujddouzut tbody, #vujddouzut tfoot, #vujddouzut tr, #vujddouzut td, #vujddouzut th {
  border-style: none;
}

#vujddouzut p {
  margin: 0;
  padding: 0;
}

#vujddouzut .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vujddouzut .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vujddouzut .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vujddouzut .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vujddouzut .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vujddouzut .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vujddouzut .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vujddouzut .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vujddouzut .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vujddouzut .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vujddouzut .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vujddouzut .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vujddouzut .gt_spanner_row {
  border-bottom-style: hidden;
}

#vujddouzut .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#vujddouzut .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vujddouzut .gt_from_md > :first-child {
  margin-top: 0;
}

#vujddouzut .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vujddouzut .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vujddouzut .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#vujddouzut .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#vujddouzut .gt_row_group_first td {
  border-top-width: 2px;
}

#vujddouzut .gt_row_group_first th {
  border-top-width: 2px;
}

#vujddouzut .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vujddouzut .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vujddouzut .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vujddouzut .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vujddouzut .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vujddouzut .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vujddouzut .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#vujddouzut .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vujddouzut .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vujddouzut .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vujddouzut .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vujddouzut .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vujddouzut .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vujddouzut .gt_left {
  text-align: left;
}

#vujddouzut .gt_center {
  text-align: center;
}

#vujddouzut .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vujddouzut .gt_font_normal {
  font-weight: normal;
}

#vujddouzut .gt_font_bold {
  font-weight: bold;
}

#vujddouzut .gt_font_italic {
  font-style: italic;
}

#vujddouzut .gt_super {
  font-size: 65%;
}

#vujddouzut .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#vujddouzut .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vujddouzut .gt_indent_1 {
  text-indent: 5px;
}

#vujddouzut .gt_indent_2 {
  text-indent: 10px;
}

#vujddouzut .gt_indent_3 {
  text-indent: 15px;
}

#vujddouzut .gt_indent_4 {
  text-indent: 20px;
}

#vujddouzut .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Median">Median</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="95% HDI">95% HDI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="&lt;em&gt;p&lt;/em&gt;(H0)"><em>p</em>(H0)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="font-size: small; font-style: italic;" scope="colgroup" id="Intercepts (at 22 months)">Intercepts (at 22 months)</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_1" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Comprehension and Production</th>
<td headers="Intercepts (at 22 months) stub_1_1 .median" class="gt_row gt_right" style="font-size: small;">0.438</td>
<td headers="Intercepts (at 22 months) stub_1_1 .lower" class="gt_row gt_right" style="font-size: small;">[0.379, 0.496]</td>
<td headers="Intercepts (at 22 months) stub_1_1 .rope_overlap" class="gt_row gt_right" style="font-size: small;">8.842%</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Comprehension</th>
<td headers="Intercepts (at 22 months) stub_1_2 .median" class="gt_row gt_right" style="font-size: small;">0.936</td>
<td headers="Intercepts (at 22 months) stub_1_2 .lower" class="gt_row gt_right" style="font-size: small;">[0.920, 0.949]</td>
<td headers="Intercepts (at 22 months) stub_1_2 .rope_overlap" class="gt_row gt_right" style="font-size: small;">0.000%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="font-size: small; font-style: italic;" scope="colgroup" id="Slopes">Slopes</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_3" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Age (+1 SD, 4.87, months)</th>
<td headers="Slopes stub_1_3 .median" class="gt_row gt_right" style="font-size: small;">0.405</td>
<td headers="Slopes stub_1_3 .lower" class="gt_row gt_right" style="font-size: small;">[0.357, 0.451]</td>
<td headers="Slopes stub_1_3 .rope_overlap" class="gt_row gt_right" style="font-size: small;">0.000%</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Exposure (+1 SD, 1.81)</th>
<td headers="Slopes stub_1_4 .median" class="gt_row gt_right" style="font-size: small;">0.233</td>
<td headers="Slopes stub_1_4 .lower" class="gt_row gt_right" style="font-size: small;">[0.201, 0.268]</td>
<td headers="Slopes stub_1_4 .rope_overlap" class="gt_row gt_right" style="font-size: small;">0.000%</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Cognateness (+1 SD, 0.26)</th>
<td headers="Slopes stub_1_5 .median" class="gt_row gt_right" style="font-size: small;">0.058</td>
<td headers="Slopes stub_1_5 .lower" class="gt_row gt_right" style="font-size: small;">[0.014, 0.104]</td>
<td headers="Slopes stub_1_5 .rope_overlap" class="gt_row gt_right" style="font-size: small;">3.711%</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Length (+1 SD, 1.56 phonemes)</th>
<td headers="Slopes stub_1_6 .median" class="gt_row gt_right" style="font-size: small;">−0.062</td>
<td headers="Slopes stub_1_6 .lower" class="gt_row gt_right" style="font-size: small;">[−0.086, −0.036]</td>
<td headers="Slopes stub_1_6 .rope_overlap" class="gt_row gt_right" style="font-size: small;">0.000%</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Age × Exposure</th>
<td headers="Slopes stub_1_7 .median" class="gt_row gt_right" style="font-size: small;">0.071</td>
<td headers="Slopes stub_1_7 .lower" class="gt_row gt_right" style="font-size: small;">[0.039, 0.104]</td>
<td headers="Slopes stub_1_7 .rope_overlap" class="gt_row gt_right" style="font-size: small;">0.000%</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Age × Cognateness</th>
<td headers="Slopes stub_1_8 .median" class="gt_row gt_right" style="font-size: small;">0.014</td>
<td headers="Slopes stub_1_8 .lower" class="gt_row gt_right" style="font-size: small;">[0.000, 0.026]</td>
<td headers="Slopes stub_1_8 .rope_overlap" class="gt_row gt_right" style="font-size: small;">98.474%</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Exposure × Cognateness</th>
<td headers="Slopes stub_1_9 .median" class="gt_row gt_right" style="font-size: small;">−0.057</td>
<td headers="Slopes stub_1_9 .lower" class="gt_row gt_right" style="font-size: small;">[−0.069, −0.046]</td>
<td headers="Slopes stub_1_9 .rope_overlap" class="gt_row gt_right" style="font-size: small;">0.000%</td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_center gt_stub" style="font-size: small;">Age × Exposure × Cognateness</th>
<td headers="Slopes stub_1_10 .median" class="gt_row gt_right" style="font-size: small;">−0.018</td>
<td headers="Slopes stub_1_10 .lower" class="gt_row gt_right" style="font-size: small;">[−0.027, −0.010]</td>
<td headers="Slopes stub_1_10 .rope_overlap" class="gt_row gt_right" style="font-size: small;">97.474%</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::

::: {.cell fig.heigh='7'}

```{.r .cell-code}
model_draws %>%
    ggplot(aes(.value, .variable_name)) +
    annotate(geom = "rect", ymin = -Inf, ymax = Inf,
             xmin = rope_interval["lower"],
             xmax = rope_interval["upper"],
             colour = NA, alpha = 0.2, fill = clrs[5],) +
    geom_vline(xintercept = 0, size = 1, colour = clrs[5]) +
    stat_slab(
        aes(fill = stat(abs(x) < rope_interval["upper"]),
            colour = stat(abs(x) < rope_interval["upper"]),),
        size = 1, 
        position = position_nudge(y = 0.15)
    ) +
    geom_errorbar(
        data = model_summary,
        aes(xmin = .lower, 
            xmax = .upper, 
            x = .median),
        width = 0.15
    ) +
    geom_point(data = model_summary, 
               aes(x = .median),
               size = 1.5) +
    geom_text(
        data = model_summary,
        aes(x = .median, 
            y = .variable_name,
            label = glue::glue(
                "{round(.median, 1)}",
                "[{round(.lower, 1)}, {round(.upper, 1)}]",
                .sep = " ")),
        colour = "black",
        position = position_nudge(y = -0.3), size = 3
    ) +
    labs(
        x = "Logistic regression coefficient estimate (Logit scale)",
        y = "Variable", 
        fill = "Overlaps with ROPE",
        colour = "Overlaps with ROPE"
    ) +
    scale_x_continuous(breaks = seq(-3, 3, 0.2)) +
    scale_fill_manual(
        values = clrs[c(1, 4)],
        labels = c("No", "Yes")
    ) +
    scale_colour_manual(
        values = clrs[c(1, 4)],
        labels = c("No", "Yes")
    ) +
    theme(
        legend.position = "top",
        legend.justification = c(1, 1),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_line(
            colour = "grey85",
            linetype = "dotted"
        ),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()
    )
```

::: {.cell-output-display}
![Marginal posterior distribution of the regression coefficients of the fixed effects in the extended model. Distributions show the estimated likelihood density of each value in the parameter space (X-axis) of each coefficient (Y-axis). Intervals show the 95% highest density interval (*HDI*) of each distribution. This interval is the narrowest range of values that contains 95% of the distribution, and therefore is 95% likely to contain the true value, given the data. the mean and HDI limits are indicated below each distribution. Coefficients were transformed from the logit scale to the probability scale for interpretability. We used the divide-by-four rule to get the maximum change in probability of correct response associated with a unit increase in this variable, i.e. the derivative of the logistic function in its mid-point.](index_files/figure-html/fig-results-fixed-1.png){#fig-results-fixed width=9000}
:::
:::



## Marginal effects


::: {.cell}

```{.r .cell-code}
clrs_reorder <- c("#58508d", "#003f5c", "#122A36", "#ffa600", "#ff6361", "#bc5090")

model_epreds |> 
    mutate(age = (age_std * sd(responses$age)) + mean(responses$age),
           exposure = factor(exposure_std,
                             levels = unique(exposure_std),
                             labels = c("Low exposure (-1 SD)",
                                        "Mean exposure",
                                        "High exposure (+1 SD)"),
                             ordered = TRUE),
           lv = factor(lv_std,
                       levels = unique(lv_std),
                       labels = paste0(c(0, 50, 100), "%"),
                       ordered = TRUE),
           .category = ifelse(.category=="Understands",
                              "Comprehension",
                              "Production")) |> 
    ggplot(aes(age, .value, colour = lv, fill = lv)) +
    facet_grid(.category~exposure) +
    annotate(geom = "rect",
             xmin = min(responses$age),
             xmax = max(responses$age),
             ymin = -Inf,
             ymax = Inf,
             fill = "grey95",
             colour = NA) +
    annotate(geom = "text",
             x = mean(responses$age),
             y = 0,
             hjust = -0.1,
             vjust = 0,
             label = "Observed age range",
             size = 2) +
    geom_vline(xintercept = mean(responses$age), 
               linetype = "dotted") +
    geom_hline(yintercept = 0.5,
               linetype = "dotted") +
    geom_line(aes(group = interaction(lv, .draw)),
              linewidth = 0.5,
              alpha = 1/10) +
    stat_summary(fun = median, geom = "line", linewidth = 0.75) +
    labs(x = "Age (months)",
         y = "Probability of acquisition",
         linetype = "Cognateness (phonological similarity)",
         colour = "Cognateness (phonological similarity)",
         fill = "Cognateness (phonological similarity)") +
    guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
    scale_colour_manual(values = clrs_reorder[c(4, 5, 1)]) +
    scale_fill_manual(values = clrs_reorder[c(4, 5, 1)]) +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 50, 5)) +
    theme(legend.position = "top",
          legend.key.size = unit(1, "cm"),
          legend.key.height = unit(1, "cm"),
          axis.text.x = element_text(size = 9),
          panel.grid = element_blank(),
          panel.border = element_rect(fill = NA, 
                                      colour = "black", 
                                      linewidth = 0.75))
```

::: {.cell-output-display}
![Posterior marginal effects. Thin lines correspond to 50 mean predictions. Thicker lines indicate the median of those predictions. Different colours indicate different levels of cognateness (phonological similarity). Predictions are presented separately for different degrees of word exposure index: little exposure to the word, mean exposure, and high exposure. Predictions for Comprehension are show on top and predictions for Comprehension and Production are shown on the bottom. In-sample predictions lie inside the grey rectangles.](index_files/figure-html/fig-marginal-1.png){#fig-marginal width=100%}
:::
:::


## Marginal effects

In order to examine the impact of the word exposure index on the effect of phonological similarity we used the [`marginaleffects`](https://vincentarelbundock.github.io/marginaleffects/index.html) R package to compute the average marginal effect of the Levenshtein distance on the probability of acquisition (comprehension or production) for the range of observed values of the word exposure index in our dataset.



::: {.cell}

```{.r .cell-code}
nd <- marginaleffects::datagrid(
    model = model_fit,
    n_phon_std = 0,
    lv_std = 0,
    exposure_std = seq(-1.5, 1.5, length.out = 100),
    age_std = 0,
    id = NA,
    te = NA
)

marg_effects <- marginaleffects::marginaleffects(
    model_fit,
    newdata = nd, 
    re_formula = NA,
    ndraws = 50,
    vcov = FALSE,
    type = "link"
)

marg_effects %>% 
    marginaleffects::posteriordraws() %>%
    as_tibble() %>% 
    filter(term=="lv_std") %>% 
    mutate(draw = draw/4) %>% 
    ggplot(aes(exposure_std, draw)) +
    geom_hline(yintercept = 0, size = 1, colour = "grey") +
    geom_line(aes(group = drawid), 
              alpha = 1, 
              size = 0.75,
              colour = "grey") +
    # stat_summary(fun.data = median_hdi, geom = "ribbon",
    #              colour = NA, alpha = 0.5, fill = clrs[1]) +
    stat_summary(fun = mean, 
                 geom = "line", 
                 size = 1, 
                 color = clrs[1]) +
    labs(
        x = "Word exposure index",
        y = "Marginal effect of phonological similarity\n(Levenshtein distance)"
    ) +
    scale_x_continuous(
        labels = scales::label_number(style_positive = "plus", suffix = " SD"),
        breaks = seq(-2, 2, 0.5)
    ) +
    scale_y_continuous(
        labels = scales::label_percent(style_positive = "plus", ),
        breaks = seq(-1, 1, 0.05)
    ) +
    theme(
        legend.position = "top",
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.75)
    )
```

::: {.cell-output-display}
![Average marginal effect of phonological similarity (Levenshtein distance) on the probability of comprehension and production across the range of values of exposure. The observed range of values of the word exposure index is indicated in the X-axis, and the estimated average marginal effect of phonological similarity (Levenshtein distance) is indicated in the Y-axis. This quantity is generated by averaging the derivative of the regression coefficient of the predictor Levenshtein across the range of values of all predictors but Exposure. We represent the uncertainty associated with the resulting estimate by generating multiple values of this marginal effect by drawing multiple samples from the posterior distribution of the model coefficients, calculating the average marginal effect for each posterior draw, and plotting the result as a thin grey line. Closer lines indicate lower uncertainty over the true value of the average marginal effect of Levenshein, while more distant lines indicate higher uncertainty.](index_files/figure-html/fig-marginal-effect-1.png){#fig-marginal-effect width=7000}
:::
:::



## Random effects

### Participant-level effects

::: panel-tabset

#### Intercept


::: {.cell}

```{.r .cell-code}
re_id <- ranef(model_fit)$id[,,1] %>% 
    as_tibble() %>% 
    rownames_to_column("id") %>%
    left_join(distinct(model_fit$data, id),
              by = join_by(id)) %>% 
    janitor::clean_names() %>% 
    mutate(
        across(
            .cols = estimate:q97_5,
            .fns = list(
                Comprehension = ~plogis(. + fixef(model_fit)[1]),
                Production = ~plogis(. + fixef(model_fit)[2])
            ), 
            .names = "{.col}__{.fn}"
        )
    ) %>% 
    select(id, ends_with("Comprehension"), ends_with("Production")) %>% 
    pivot_longer(
        -id, 
        names_to = c(".value", "type"),
        names_sep = "__"
    ) 

re_id %>% 
    ggplot() +
    aes(x = estimate, y = reorder(id, estimate), 
        xmin = q2_5, xmax = q97_5,
        fill = type, colour = type) +
    facet_wrap(~type) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA) +
    geom_line(aes(group = 1), size = 1) +
    geom_vline(xintercept = 0.5, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior probability of acquisition",
        y = "Participant", 
        colour = "Type"
    ) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior probability of acquisition by participant](index_files/figure-html/fig-results-random-id-intercept-1.png){#fig-results-random-id-intercept width=7000}
:::
:::


#### Age slope


::: {.cell}

```{.r .cell-code}
re_age <- ranef(model_fit)$id[,,"age_std"] 

re_age %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(id = unique(model_fit$data$id)) %>% 
    relocate(id) %>% 
    ggplot() +
    aes(x = estimate, y = reorder(id, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant", 
    ) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of age by participant](index_files/figure-html/fig-results-random-id-age-1.png){#fig-results-random-id-age width=7000}
:::
:::


#### # Phonemes slope


::: {.cell}

```{.r .cell-code}
re_n_phon <- ranef(model_fit)$id[,,"n_phon_std"] 

re_n_phon %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(id = unique(model_fit$data$id)) %>% 
    relocate(id) %>% 
    ggplot() +
    aes(
        x = estimate, y = reorder(id, estimate),
        xmin = q2_5, xmax = q97_5
    ) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant", 
    ) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of the number of phonemes by participant](index_files/figure-html/fig-results-random-id-nphon-1.png){#fig-results-random-id-nphon width=7000}
:::
:::


#### Exposure slope


::: {.cell}

```{.r .cell-code}
re_freq <- ranef(model_fit)$id[,,"exposure_std"] 

re_freq %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(id = unique(model_fit$data$id)) %>% 
    relocate(id) %>% 
    ggplot() +
    aes(x = estimate, y = reorder(id, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant", 
    ) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of lexical frequency by participant](index_files/figure-html/fig-results-random-id-exposure-1.png){#fig-results-random-id-exposure width=7000}
:::
:::



#### Levenshtein slope


::: {.cell}

```{.r .cell-code}
re_lv <- ranef(model_fit)$id[,,"lv_std"]

re_lv %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    mutate(id = unique(model_fit$data$id)) %>%
    relocate(id) %>%
    ggplot() +
    aes(x = estimate, y = reorder(id, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant",
    ) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of phonological similarity by participant](index_files/figure-html/fig-results-random-id-lv-1.png){#fig-results-random-id-lv width=7000}
:::
:::


#### Doe-by-Levenshtein slope


::: {.cell}

```{.r .cell-code}
# | label: fig-results-random-id-exposure-lv
# | message: false
# | warning: false
# | fig-width: 7
# | fig-cap: "Posterior random slope of age by participant"
re_exposure_lv <- ranef(model_fit)$id[,,"exposure_std:lv_std"]

re_lv %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    mutate(id = unique(model_fit$data$id)) %>%
    relocate(id) %>%
    ggplot() +
    aes(x = estimate, y = reorder(id, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant",
    ) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![](index_files/figure-html/unnamed-chunk-26-1.png){width=7000}
:::
:::


#### Age-by-DoE slope


::: {.cell}

```{.r .cell-code}
re_age_exposure <- ranef(model_fit)$id[,,"age_std:exposure_std"]

re_age_exposure %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    mutate(id = unique(model_fit$data$id)) %>%
    relocate(id) %>%
    ggplot() +
    aes(x = estimate, y = reorder(id, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant",
    ) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of the age-by-language degree of exposure interaction by participant](index_files/figure-html/fig-results-random-id-age-doe-1.png){#fig-results-random-id-age-doe width=7000}
:::
:::


#### Age-by-Levenshtein slope


::: {.cell}

```{.r .cell-code}
re_age_lv <- ranef(model_fit)$id[,,"age_std:lv_std"]

re_age_lv %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    mutate(id = unique(model_fit$data$id)) %>%
    relocate(id) %>%
    ggplot() +
    aes(x = estimate, y = reorder(id, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant",
    ) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of the age-by-phonological similarity interaction by participant](index_files/figure-html/fig-results-random-id-age-lv-1.png){#fig-results-random-id-age-lv width=7000}
:::
:::


#### Age-by-DoE-by-Levenshtein slope


::: {.cell}

```{.r .cell-code}
re_age_exposure_lv <- ranef(model_fit)$id[,,"age_std:exposure_std:lv_std"]

re_age_exposure_lv %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    mutate(id = unique(model_fit$data$id)) %>%
    relocate(id) %>%
    ggplot() +
    aes(x = estimate, y = reorder(id, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant",
    ) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of the age-by-exposure-by-phonological similarity interaction by participant](index_files/figure-html/fig-results-random-id-age-exposure-lv-1.png){#fig-results-random-id-age-exposure-lv width=7000}
:::
:::



:::



### Item-level effects

::: panel-tabset

#### Intercept


::: {.cell}

```{.r .cell-code}
re_te <- ranef(model_fit)$te[,,"Intercept"] %>% 
    as_tibble() %>% 
    rownames_to_column("te") %>% 
    janitor::clean_names() %>% 
    mutate(
        across(
            .cols = where(is.numeric),
            .fns = list(
                Comprehension = ~plogis(. + fixef(model_fit)[1]),
                Production = ~plogis(. + fixef(model_fit)[2])
            ), .names = "{.col}__{.fn}"
        )
    ) %>% 
    select(-c(estimate:q97_5)) %>% 
    pivot_longer(
        where(is.numeric), 
        names_to = c(".value", "type"),
        names_sep = "__"
    ) %>% 
    mutate(te = as.numeric(te)) 

re_te %>% 
    ggplot() +
    aes(x = estimate,
        y = reorder(te, estimate), 
        xmin = q2_5,
        xmax = q97_5,
        colour = type, 
        fill = type) +
    facet_wrap(~type) +
    geom_ribbon(aes(group = 1), 
                alpha = 0.5, 
                colour = NA) +
    geom_line(aes(group = 1), size = 1) +
    geom_vline(xintercept = 0.5, 
               colour = "black", 
               linetype = "dotted") +
    labs(x = "Posterior probability of acquisition",
         y = "TE", 
         colour = "Type") +
    scale_x_continuous(limits = c(0, 1),
                       labels = scales::percent) +
    scale_fill_manual(values = clrs[c(1, 4)]) +
    scale_colour_manual(values = clrs[c(1, 4)]) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior probability of acquisition by translation equivalent](index_files/figure-html/fig-results-random-te-1.png){#fig-results-random-te width=7000}
:::
:::


#### Age slope


::: {.cell}

```{.r .cell-code}
re_age <- ranef(model_fit)$te[,,"age_std"]

re_age %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(te = unique(model_fit$data$te)) %>% 
    relocate(te) %>% 
    ggplot() +
    aes(x = estimate, y = reorder(te, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant", 
    ) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of age by translation equivalent](index_files/figure-html/fig-results-random-te-age-1.png){#fig-results-random-te-age width=7000}
:::
:::


#### # Phonemes slope


::: {.cell}

```{.r .cell-code}
re_n_phon <- ranef(model_fit)$te[,,"n_phon_std"]

re_n_phon %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(te = unique(model_fit$data$te)) %>% 
    relocate(te) %>% 
    ggplot() +
    aes(x = estimate, y = reorder(te, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant", 
    ) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of the number of phonemes by translation equivalent](index_files/figure-html/fig-results-random-te-nphon-1.png){#fig-results-random-te-nphon width=7000}
:::
:::


#### Exosure slope


::: {.cell}

```{.r .cell-code}
re_doe <- ranef(model_fit)$te[,,"exposure_std"]

re_doe %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    mutate(te = unique(model_fit$data$te)) %>%
    relocate(te) %>%
    ggplot() +
    aes(x = estimate, y = reorder(te, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant",
    ) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of age by participant](index_files/figure-html/fig-results-random-te-exposure-1.png){#fig-results-random-te-exposure width=7000}
:::
:::


#### Age-by-exposure slope


::: {.cell}

```{.r .cell-code}
re_age_doe <- ranef(model_fit)$te[,,"age_std:exposure_std"]

re_age_doe %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(te = unique(model_fit$data$te)) %>% 
    relocate(te) %>% 
    ggplot() +
    aes(x = estimate, y = reorder(te, estimate),
        xmin = q2_5, xmax = q97_5) +
    geom_ribbon(aes(group = 1), alpha = 0.5, color = NA, fill = clrs[1]) +
    geom_line(aes(group = 1), size = 1, colour = clrs[1]) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    labs(
        x = "Posterior random slope estimate",
        y = "Participant", 
    ) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
```

::: {.cell-output-display}
![Posterior random slope of age by participant](index_files/figure-html/fig-results-random-te-age-exposure-1.png){#fig-results-random-te-age-exposure width=7000}
:::
:::


:::


## Model diagnostics

### Traceplots


::: {.cell}

```{.r .cell-code}
gather_draws(model_fit, `b_.*`, `sd_te__.*`, regex = TRUE) %>%
    mutate(.chain = paste("Chain ", .chain)) %>%
    ggplot(aes(.iteration, .value, colour = .chain)) +
    facet_wrap(~.variable, scales = "free_y") +
    annotate(geom = "rect", colour = NA,
             xmin = 0, xmax = dim(model_fit$fit)[1]/2,
             ymin = -Inf, ymax = Inf,
             alpha = 0.25) +
    geom_line() +
    labs(
        x = "Iteration",
        y = "Sample value",
        colour = "Chain"
    ) +
    scale_colour_manual(values = clrs[c(1, 3, 4, 5)]) +
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 7)
    )
```

::: {.cell-output-display}
![](index_files/figure-html/fig-diagnostics-traceplots-1.png){#fig-diagnostics-traceplots width=12000}
:::
:::




### Model convergence


::: {.cell}

```{.r .cell-code}
model_convergence |> 
    ggplot(aes(.rhat)) +
    geom_histogram(binwidth = 0.001, colour = "white", fill = "grey60") +
    geom_vline(xintercept = 1.01, linetype = "dashed") +
    labs(x = "R-hat",
         y = "MCMC samples") +
    
    model_convergence |> 
    ggplot(aes(.neff)) +
    geom_histogram(binwidth = 0.1, colour = "white", fill = "grey60") +
    geom_vline(xintercept = 1.01, linetype = "dashed") +
    labs(x = "Effective sample size ratio",
         y = "MCMC samples") +
    
    plot_layout(ncol = 1) &
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "none",
          panel.grid = element_blank())
```

::: {.cell-output-display}
![MCMC convergence diagnostic of all parameters in the model. A: distribution of the Gelman-Rubin (R-hat) scores. B: distribution of the ratio of effective sa](index_files/figure-html/fig-rhats-neffs-1.png){#fig-rhats-neffs width=100%}
:::
:::


# Appendix

## Session info


::: {.cell}

```{.r .cell-code}
sessionInfo()
```

::: {.cell-output .cell-output-stdout}
```
R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 22621)

Matrix products: default

locale:
[1] LC_COLLATE=Spanish_Spain.utf8  LC_CTYPE=Spanish_Spain.utf8   
[3] LC_MONETARY=Spanish_Spain.utf8 LC_NUMERIC=C                  
[5] LC_TIME=Spanish_Spain.utf8    

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] beeswarm_0.4.0    quarto_1.2        collapse_1.9.6    tidybayes_3.0.2  
 [5] cmdstanr_0.5.3    brms_2.19.2       Rcpp_1.0.10       testthat_3.1.3   
 [9] lubridate_1.9.2   forcats_1.0.0     stringr_1.5.0     dplyr_1.1.2      
[13] purrr_1.0.1       readr_2.1.4       tidyr_1.3.0       tibble_3.2.1     
[17] tidyverse_2.0.0   tarchetypes_0.7.6 targets_1.1.3     ggplot2_3.4.2    
[21] gt_0.9.0          patchwork_1.1.1  

loaded via a namespace (and not attached):
  [1] backports_1.4.1        plyr_1.8.8             igraph_1.4.3          
  [4] splines_4.2.2          svUnit_1.0.6           crosstalk_1.2.0       
  [7] listenv_0.9.0          rstantools_2.3.1       inline_0.3.19         
 [10] digest_0.6.31          htmltools_0.5.5        fansi_1.0.4           
 [13] magrittr_2.0.3         checkmate_2.2.0        base64url_1.4         
 [16] tzdb_0.3.0             globals_0.16.2         RcppParallel_5.1.7    
 [19] matrixStats_0.63.0     xts_0.13.1             timechange_0.2.0      
 [22] prettyunits_1.1.1      colorspace_2.1-0       ggdist_3.1.1          
 [25] xfun_0.39              callr_3.7.3            crayon_1.5.2          
 [28] jsonlite_1.8.5         zoo_1.8-12             glue_1.6.2            
 [31] gtable_0.3.3           V8_4.3.0               distributional_0.3.2  
 [34] pkgbuild_1.4.0         rstan_2.26.22          abind_1.4-5           
 [37] scales_1.2.1           mvtnorm_1.1-3          miniUI_0.1.1.1        
 [40] xtable_1.8-4           stats4_4.2.2           StanHeaders_2.26.22   
 [43] DT_0.27                htmlwidgets_1.6.2      threejs_0.3.3         
 [46] arrayhelpers_1.1-0     posterior_1.4.1        ellipsis_0.3.2        
 [49] pkgconfig_2.0.3        loo_2.6.0              farver_2.1.1          
 [52] sass_0.4.6             janitor_2.2.0          utf8_1.2.3            
 [55] here_1.0.1             labeling_0.4.2         tidyselect_1.2.0      
 [58] rlang_1.1.1            reshape2_1.4.4         later_1.3.1           
 [61] munsell_0.5.0          tools_4.2.2            cli_3.6.1             
 [64] generics_0.1.3         evaluate_0.21          fastmap_1.1.1         
 [67] yaml_2.3.7             processx_3.8.1         knitr_1.43            
 [70] fs_1.6.2               future.callr_0.8.1     future_1.32.0         
 [73] nlme_3.1-162           mime_0.12              xml2_1.3.4            
 [76] brio_1.1.3             compiler_4.2.2         bayesplot_1.10.0      
 [79] shinythemes_1.2.0      rstudioapi_0.14        curl_5.0.1            
 [82] stringi_1.7.12         ps_1.7.5               Brobdingnag_1.2-9     
 [85] lattice_0.21-8         Matrix_1.5-4.1         commonmark_1.9.0      
 [88] markdown_1.7           shinyjs_2.1.0          tensorA_0.36.2        
 [91] vctrs_0.6.3            pillar_1.9.0           lifecycle_1.0.3       
 [94] furrr_0.3.1            bridgesampling_1.1-2   insight_0.19.2        
 [97] data.table_1.14.8      httpuv_1.6.11          R6_2.5.1              
[100] promises_1.2.0.1       renv_0.15.4            gridExtra_2.3         
[103] parallelly_1.36.0      codetools_0.2-19       colourpicker_1.2.0    
[106] gtools_3.9.4           rprojroot_2.0.3        withr_2.5.0           
[109] marginaleffects_0.12.0 shinystan_2.6.0        mgcv_1.8-42           
[112] parallel_4.2.2         hms_1.1.3              grid_4.2.2            
[115] coda_0.19-4            snakecase_0.11.0       rmarkdown_2.22        
[118] shiny_1.7.4            base64enc_0.1-3        dygraphs_1.1.1.6      
```
:::
:::

