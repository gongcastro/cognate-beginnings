library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

conflict_prefer("filter", "dplyr")

clrs <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")

logistic <- function(x, slope, mid, upper) {
    upper / (1 + exp(-slope * (x - mid)))
}



token <- c("porc", "gat")
it <- items |>
    filter(item %in% token) |> 
    select(item, te, meaning, ipa, lv, freq)
n_month <- 24
age <- seq(34)
doe <- c(0.65, 0.35)


elis_df <- it |> 
    expand_grid(age, doe) |> 
    mutate(exposure = freq*doe,
           exposure_std = scale(exposure,
                                mean(responses$exposure),
                                sd(responses$exposure))[,1],
           lambda = n_month/freq,
           lambda_wgt = lambda * doe,
           eli = lambda_wgt + runif(n = n(),
                                    min = -lambda_wgt*0.50,
                                    max = lambda_wgt*0.50)) |> 
    group_by(te, item, doe) |> 
    mutate(eli_cum = cumsum(eli)) |> 
    arrange(item, doe, age) |> 
    group_by(te, doe) |> 
    mutate(aoa = age[which.min(abs(eli_cum-n_month))],
           prob = logistic(age, 1, aoa, 1)) 



ggplot(elis_df, aes(age, eli_cum, colour = as.factor(doe))) +
    facet_wrap(~meaning) +
    geom_step() +
    geom_point(data = distinct(elis_df, item, doe, aoa),
               aes(x = aoa,
                   y = 0)) +
    ggplot(elis_df, aes(age, prob, colour = as.factor(doe))) +
    facet_wrap(~meaning) +
    geom_hline(yintercept = 0.5) + 
    geom_line(aes(group = as.factor(doe)),
                 linewidth = 1) + 
    geom_point(data = distinct(elis_df, te, meaning, doe, aoa),
               aes(x = aoa,
                   y = 0),
               size = 5) +
    
    plot_layout(ncol = 1) &
    scale_colour_manual(values = clrs[c(1, 4)])

post <- posterior_predictions_re(
    model = model_fit_4,
    data = responses,
    levels = c(173, 161),
    group = "te",
    age_std = scale(seq(0, 50, 1),
                    mean(responses$age),
                    sd(responses$age)),
    exposure_std = unique(elis_df$exposure_std),
    n_phon_std = 0) |>
    mutate(lv = lv_std * sd(data$lv) + mean(data$lv),
           age = age_std * sd(data$age) + mean(data$age)) |>
    select(te,
           age,
           exposure_std,
           lv,
           .category = group,
           .draw = drawid,
           .value = draw) |> 
    group_by(te, .category) |> 
    mutate(aoa = age[which.min(abs(.value - 0.5))]) |> 
    ungroup() |> 
    left_join(distinct(items, te, meaning, freq)) |> 
    mutate(te = as.factor(te),
           doe = rescale_variable(exposure_std, 
                                  mean(responses$exposure),
                                  sd(responses$exposure))/freq) |> 
    filter(.category = "Understands")





x <- seq(6, 35, 0.1)
slope <- c(0.75)
mid <- c(20, 24)
upper <- c(1)

logistic_data <- expand_grid(x, slope, mid, upper) %>% 
    mutate(y = logistic(x, slope, mid, upper),
           values = paste0("slope = ", round(slope, 2),
                           ", mid = ", round(mid, 2)))

ggplot(logistic_data, aes(x, y, colour = values)) +
    facet_wrap(~upper, labeller = labeller(upper = ~paste0("upper = ", .))) +
    geom_line(linewidth = 1.25) +
    labs(colour = "Parameter values") +
    guides(colour = guide_legend(ncol = 2)) +
    scale_color_manual(values = clrs[c(1, 4, 5)]) +
    theme(panel.grid = element_line(linetype = "dotted", colour = "grey"),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification = c("left", "top"))

n_month <- 438300
freqs <- childes[childes$token %in% token,][, c("token", "freq_million")] |> 
    mutate(lambda = n_month / freq_million)
thres <- 10000
age <- seq(6, 35, 1)
language <- c("English", "Spanish", "English", "Spanish")
te <- c("flower-flor", "flower-flor", "cow-vaca", "cow-vaca")
word <- c("flower", "flor", "cow", "vaca")
token <- c("flower", "cow")
doe <- c(0.80, 0.20, 0.80, 0.20)
lambda <- rep(freqs$lambda, each = 2)

elis_df <- tibble(te, word, language, doe, lambda, lambda_wgt = lambda*doe) |> 
    expand_grid(age) |> 
    mutate(eli = rpois(n(), lambda_wgt)) |> 
    group_by(te, word) |> 
    mutate(eli_cum = cumsum(eli)) |> 
    group_by(te) |> 
    mutate(aoa_te = age[which.min(abs(eli_cum-thres))]) |> 
    group_by(word) |> 
    mutate(
        aoa = age[which.min(abs(eli_cum-thres))],
        eli_cum_corrected = ifelse(
            (aoa > aoa_te) & (age >= aoa_te) & (word=="flor"),
            eli_cum*1.5,
            eli_cum
        ),
        aoa_corrected = ifelse(aoa > aoa_te,
                               age[which.min(abs(eli_cum_corrected-thres))],
                               aoa),
    ) |> 
    ungroup() |> 
    mutate(y = logistic(age, 1, aoa, 1),
           y_corrected = logistic(age, 1, aoa_corrected, 1))


ggplot(elis_df, aes(age, eli_cum, colour = language)) +
    facet_wrap(~te) +
    geom_hline(yintercept = thres,
               size = 1, 
               colour = "grey") +
    geom_line(size = 1) +
    geom_segment(data = distinct(elis_df, language, word, te, aoa),
                 aes(x = aoa, xend = aoa,
                     y = 0, yend = thres),
                 size = 0.5,
                 linetype = "dotted") +
    
    # corrected
    geom_line(aes(y = eli_cum_corrected), size = 1) +
    geom_segment(data = distinct(elis_df, language, word, te, aoa_corrected),
                 aes(x = aoa_corrected, xend = aoa_corrected,
                     y = 0, yend = thres),
                 size = 0.5,
                 linetype = "dotted") +
    
    theme(axis.title.x = element_blank(),
          strip.text = element_blank()) 

ggplot(elis_df, aes(age, y, colour = language)) +
    facet_wrap(~te) +   
    geom_hline(yintercept = 0.5,
               size = 1, 
               colour = "grey") +
    geom_line(size = 1.25) +
    labs(colour = "Parameter values") +
    guides(colour = guide_legend(ncol = 2)) +
    
    plot_layout(ncol = 1, guides = "collect") &
    scale_color_manual(values = clrs[c(1, 4, 5)]) &
    theme(panel.grid = element_line(linetype = "dotted", colour = "grey"),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification = c("left", "top")) 


