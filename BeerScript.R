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
#  geom_area(aes(year, n, fill = size))
  geom_col(aes(year, prop_n, fill = size)) +
  scale_fill_manual(values = c("red", "yellow", "purple")) +
  theme_minimal()



