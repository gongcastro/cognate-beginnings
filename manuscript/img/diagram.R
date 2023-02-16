logistic <- function(x, slope, mid, upper) {
    upper / (1 + exp(-slope * (x - mid)))
}


x <- seq(6, 30, 0.1)
slope <- c(0.75)
mid <- c(20, 24, )
upper <- c(1)

logistic_data <- expand_grid(x, slope, mid, upper) %>% 
    mutate(y = logistic(x, slope, mid, upper),
           values = paste0("slope = ", round(slope, 2),
                           ", mid = ", round(mid, 2)))

ggplot(logistic_data, aes(x, y, colour = values)) +
    facet_wrap(~upper, labeller = labeller(upper = ~paste0("upper = ", .))) +
    geom_line(size = 1.25) +
    labs(colour = "Parameter values") +
    guides(colour = guide_legend(ncol = 2)) +
    scale_color_manual(values = clrs[c(1, 4, 5)]) +
    theme(panel.grid = element_line(linetype = "dotted", colour = "grey"),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification = c("left", "top"))


age <- seq(6, 32, 1)
language <- c("English", "Spanish", "English", "Spanish")
te <- c("flower-flor", "flower-flor", "cow-vaca", "cow-vaca")
word <- c("flower", "flor", "cow", "vaca")
doe <- c(0.80, 0.20, 0.80, 0.20)
lambda <- c(5, 10, 5, 10)

thres <- 30

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
    geom_hline(yintercept = 30,
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
          strip.text = element_blank()) +
    
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


