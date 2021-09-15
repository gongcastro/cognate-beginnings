# utils

# custom ggplot theme
theme_custom <- function(){
    theme_minimal() +
        theme(
            panel.grid = element_line(colour = "grey", linetype = "dotted"),
            axis.line = element_line(colour = "black"),
            text = element_text(size = 12, colour = "black"),
            axis.text = element_text(colour = "black")
        )
}

# dark GitHub theme
theme_github <- function(){
    theme_dark() +
        theme(
            text = element_text(colour = "white", size = 12),
            axis.text = element_text(colour = "white", size = 8),
            legend.title = element_text(colour = "white", size = 10),
            legend.text = element_text(colour = "white", size = 10),
            legend.background = element_rect(fill = "#0D1117"),
            strip.text = element_text(size = 10),
            legend.key = element_rect(fill = "#0D1117", colour = "#0D1117"),
            strip.background = element_rect(fill = "#161B22"),
            plot.background = element_rect(fill = "#0D1117"),
            panel.background = element_rect(fill = "#0D1117"),
            panel.border = element_rect(fill = "transparent", colour = "#0D1117")
        )
}


# get multilex data ----
get_credentials <- function(
){
    email <- "gonzalo.garciadecastro@upf.edu"
    ml_connect(
        google_email = email,
        formr_email = email,
        formr_password = key_get("formr", email)
    )
}

get_multilex <- function(
    update = FALSE,
    type = "understands"
){
    get_credentials()
    
    p <- ml_participants()
    r <- ml_responses(p, update = update)
    l <- ml_logs(p, r) %>% 
        mutate(
            age_group =  as.factor(
                case_when(
                    between(age, 19, 24) ~ 21,
                    between(age, 24, 28) ~ 25,
                    between(age, 28, 34) ~ 30
                )),
            age_group = paste0(age_group, " months")
        ) %>% 
        filter(age_group != "NA months") %>% 
        select(id, time, time_stamp, age_group, age, lp, dominance, version,
               completed, doe_catalan, doe_spanish, doe_others)
    
    v <- ml_vocabulary(p, r, scale = c("prop", "count"), by = "id_exp") %>% 
        filter(type==type)
    
    m <- list(
        participants = p, responses = r, logs = l,
        vocabulary = v, pool = multilex::pool
    )
    
    return(m)
}

# adjusted proportion, SE, and CI ----
# from Gelman, Hill & Vehtari (2020)
prop_adj <- function(y, n) (y+2)/(n+4)
prop_se_adj <- function(y, n) {
    prop <- prop_adj(y, n)
    sqrt(prop*(1-prop)/(n+4))
}
prop_ci_adj <- function(y, n, conf = 0.95){
    prop <- prop_adj(y, n)
    se <- sqrt(prop*(1-prop)/(n+4))
    ci <- list(prop + qnorm((1-conf)/2)*se, prop + qnorm(1-(1-conf)/2)*se)
    ci[[1]] <- ifelse(ci[[1]]<0, 0, ci[[1]])
    ci[[2]] <- ifelse(ci[[2]]>1, 1, ci[[2]])
    return(ci)
}

# transform logit scale to probability
logit_to_prob <- function(x) {
    exp(x) / (1 + exp(x))
}


