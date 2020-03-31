library(tidyverse)
library(USAboundaries)
library(sf)
library(biscale)

# Picasso: "Drink to me, drink to my health. You know I can’t drink anymore.”  

# Get the Data

# brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
# beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
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


# now we want to add a map where we show the the per capita consumption per state and 
# the consumption per state
# add consumption per capita per state
# source: https://eu.usatoday.com/story/money/2019/09/14/how-much-beer-did-the-average-person-drink-in-every-state/40109241/

consumpt <- tibble(state = unique(beer_states$state)[1:51], 
       per_capita = c(26.9, 28.7, 23.4, 26.8, 25.1, 28.8, 20.0,
                      NA, 28.4, 26.4, 23.8, 30.6, 31.2, 26.4,
                      27.7, 23.1, 25.8, 22.9, 28.0, 22.9, 19.6,
                      33.8, 24.7, 28.5, 27.2, 30.4, 40.8, 25.8,
                      38.2, 32.5, 39.8, 20.1, 28.8, 33.2, 21.5,
                      26.9, 24.6, 29.8, 25.9, 23.2, 30.6, 38.2,
                      24.5, 30.9, 18.6, 24.5, 33.3, 24.3, 33.6,
                      27.3, 30.1))

# Get the US boundaries 
boundaries <- us_states(states = unique(beer_states$state)[1:51])


# bind with state consumption data
# Get total production and percentage of beer per state, year, and type
beer_states %>% 
  filter(state != "total" & state != "AK" & state != "HI" & year == 2018) %>% 
  group_by(state) %>% 
  count(name = "production", wt = barrels) %>% 
  # group_by(year) %>% 
  # mutate(proportion = production/sum(production, na.rm = TRUE)*100) %>% 
  left_join(consumpt, by= c("state")) %>% 
  filter(per_capita != is.na(per_capita)) %>% 
  left_join(boundaries, by = c("state" = "state_abbr")) %>% 
  bi_class(x = production, y = per_capita , style = "quantile", dim = 3) %>%
  ggplot() +
  geom_sf(aes(geometry=geometry, fill = bi_class),
          color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme()
  # my_theme


  
  # geom_sf_label(aes(label = state,  geometry = geometry),
  #               label.size = 0, size = 2, alpha = 0.6,
  #               label.padding = unit(0.1, "lines")) +
  # coord_sf(xlim = c(-180, -65), expand = FALSE) +
  # facet_wrap(~type, ncol = 2) +
  # ggtitle("Total US Beer Production/Use in 2019") +
  # scale_fill_viridis_c() +
  # theme(axis.title = element_blank())

  

