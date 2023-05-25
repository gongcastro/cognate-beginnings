#' Generate cumulative learning instances
#' 
generate_eli <- function(conditions = c("**Cognate**", "**Non-cognate**"),
                         items = c("gat /gat/",
                                   "gato /gato/",
                                   "gos /gos/",
                                   "perro /ˈpe.ro/"),
                         age = 0:34,
                         freq_month = 5,
                         freq_beta = 0.1,
                         parallel_beta = 1,
                         threshold = 500,
                         l1_doe = 0.65)
{
    
    freq_adj <- freq_month / (length(age)-1)
    
    item_df <- tibble(te = conditions,
                      l1.item = items[c(1, 3)],
                      l2.item = items[c(2, 4)]) |>
        mutate(lv = c(stringdist::stringsim("gat", "gato"),
                      stringdist::stringsim("gos", "pero"))) |>
        mutate(l1.doe = l1_doe, l2.doe = 1-l1_doe)
    
    eli <- expand_grid(item_df,
                       age = age) |>
        mutate(n_month = rpois(n(), freq_adj) * age,
               l1.h0_eli = n_month * l1.doe,
               l2.h0_eli = n_month * l2.doe,
               l1.h1_eli = l1.h0_eli + parallel_beta*(lv * l2.h0_eli),
               l2.h1_eli = l2.h0_eli + parallel_beta*(lv * l1.h0_eli),
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

#' Generate simulated ages-of-acquisition from cumulative learning instances
#' 
generate_aoa <- function(eli, threshold = 500) {
    
    aoa <- eli |>
        mutate(aoa = age[which.min(abs(.env$threshold-eli))],
               aoa = case_when(aoa <= min(age) ~ NA, 
                               aoa >= max(age) ~ NA,
                               .default = aoa),
               .by = c(te, hypothesis, language)) |>
        rename_with(\(x) gsub("_eli_aoa", "_aoa", x)) |>
        distinct(pick(-c(age, eli))) |>
        arrange(te, language) |>
        mutate(threshold = .env$threshold)
    
    return(aoa)
}
