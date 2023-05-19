# get bvq participant data
p <- bvq_participants()

# get bvq questionnaire responses
r <- bvq_responses(p)

# merge participant data with questionnaire responses
edu_dict <- c("noeducation" = "No education",
                "primary" = "Primary",
                "secondary" = "Secondary",
                "complementary" = "Complementary",
                "vocational" = "Vocational",
                "university" = "University")

l <- bvq_logs(p, r) |>
    mutate(across(starts_with("edu_"), 
                  function(x) {
                      factor(x, 
                             levels = names(edu_dict),
                             ordered = TRUE) |> 
                          as.numeric()
                  }),
           # get maximum educational attainment of parents
           edu_parent = apply(cbind(edu_parent1, edu_parent2), 1,
                              function(x) max(x, na.rm = FALSE)),
           # recode it as factor
           edu_parent = factor(edu_parent,
                               levels = 1:6, 
                               labels = edu_dict)) |>
    select(id, time, date_finished, age,
           lp, dominance, version, completed, doe_catalan,
           doe_spanish, doe_others, edu_parent)

pool <- bvq::pool

# get list of all relevant datasets
bvq_data <- list(participants = p,
                 responses = r,
                 logs = l,
                 pool = pool)

saveRDS(bvq_data, "data-raw/bvq.rds")