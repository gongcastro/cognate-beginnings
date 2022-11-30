library(marginaleffects)

nd <- datagrid(
    model = model_fit_4,
    n_phon_std = 0,
    lv_std = 0,
    exposure_std = c(-1, 0, 1),
    age_std = 0,
    id = NA,
    te = NA
)

emmeans <- marginaleffects(
    model_fit_4,
    newdata = nd, 
    re_formula = NA,
    ndraws = 25,
    vcov = FALSE, 
)

emmeans %>% 
    posteriordraws() %>%
    as_tibble() %>% 
    mutate(
        exposure_std = factor(
            exposure_std, 
            levels = unique(nd$exposure_std), 
            labels = c("Exposure: -1 SD", "Exposure: mean", "Exposure: +1 SD")
        ),
        # lv_std = factor(
        #     lv_std,
        #     levels = unique(nd$lv_std), 
        #     labels = paste0(c(0, 50, 100), "% similarity")
        # )
    ) %>% 
    ggplot(aes(exposure_std, dydx, colour = lv_std)) +
    facet_grid(group~term) +
    geom_line(aes(group = interaction(drawid, lv_std)))
