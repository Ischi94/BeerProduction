library(tidyverse)

# Picasso: "Drink to me, drink to my health. You know I can’t drink anymore.”  

# Get the Data

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# first keep it simple and look at brewer sizes per year
# we want a time series vs. total_barrels per brewer size
# before that, we divide brewer size in 3 categories: small, medium, large
brewer_quant <- brewer_size %>% {quantile(.$total_barrels, na.rm = T)}

brewer_size %>% 
  filter(total_barrels != is.na(total_barrels)) %>% 
  mutate(size = if_else(total_barrels <= brewer_quant[[2]], "small",
                                      if_else(total_barrels >= brewer_quant[[4]], "large",
                                              "medium"))) %>% 
  group_by(year, size) %>% 
  count() %>% 
  group_by(year) %>% 
  mutate(prop_n = n/ sum(n)) %>% 
  ggplot() +
#  geom_line(aes(year, prop_n, colour = size))
#  geom_area(aes(year, prop_n, fill = size)) +
  geom_col(aes(year, prop_n, fill = size), width = 1) +
  scale_fill_manual(values = c("#ad8599", "#e09952", "#75abbd")) +
  theme_minimal()

# add consumption per capita/ 
# source: https://eu.usatoday.com/story/money/2019/09/14/how-much-beer-did-the-average-person-drink-in-every-state/40109241/

tibble(states = unique(beer_states$state)[1:51], 
       per_capita = c(26.9, 28.7, 23.4, 26.8, 25.1, 28.8, 20.0,
                      NA, 28.4, 26.4, 23.8, 30.6, 31.2, 26.4,
                      27.7, 23.1, 25.8, 22.9, 28.0, 22.9, 19.6,
                      33.8, 24.7, 28.5, 27.2, 30.4, 40.8, 25.8,
                      38.2, 32.5, 39.8, 20.1, 28.8, 33.2, 21.5,
                      26.9, 24.6, 29.8, 25.9, 23.2, 30.6, 38.2,
                      24.5, 30.9, 18.6, 24.5, 33.3, 24.3, 33.6,
                      27.3, 30.1))

