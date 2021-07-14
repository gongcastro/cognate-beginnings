library(tidyverse)
library(stringdist)
library(janitor)
library(xlsx)

d <- multilex::pool %>% 
  drop_na(cognate) %>% 
  select(-ipa) %>% 
  rename(ipa = ipa_flat) %>% 
  pivot_wider(id_cols = c(te, cognate), names_from = language, values_from = c(label, ipa), values_fn = list) %>% 
  clean_names() %>% 
  unnest(cols = c(label_catalan, label_spanish, ipa_catalan, ipa_spanish)) %>% 
  mutate(
    n = ifelse(nchar(ipa_catalan) > nchar(ipa_spanish), nchar(ipa_catalan), nchar(ipa_spanish)),
    dist = stringdist(ipa_catalan, ipa_spanish, method = "lv"),
    dist_norm = dist/n
  )

ggplot(d, aes(cognate, dist_norm, colour = cognate, fill = cognate)) +
  geom_violin() +
  geom_point(size = 2, alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = "white", colour = "black")

write.xlsx(
  d,
  "C:/Users/U155880/Documents/pool.xlsx",
  sheetName = "pool",
  col.names = TRUE,
  append = FALSE
)
