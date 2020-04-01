library(tidyverse)
library(USAboundaries)
library(sf)
library(biscale)
library(cowplot)
library(gganimate)

# Get the Data

# brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
# beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# first keep it simple and look at brewer sizes per year
# I want a time series vs. total_barrels per brewer size
# before that, I divide brewer size in 3 categories: small, medium, large
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


# New Idea ----------------------------------------------------------------


# I just had an idea
brewer_size %>% 
  filter(total_barrels != is.na(total_barrels)) %>% 
  mutate(size = if_else(total_barrels <= brewer_quant[[2]], "small",
                        if_else(total_barrels >= brewer_quant[[4]], "large",
                                "medium"))) %>% 
  group_by(year, size) %>% 
  count(wt = n_of_brewers) %>% 
  group_by(size) %>% 
  arrange(size) %>% 
  mutate(rate_change = ((n-lag(n))/n)*100) %>% # ROC = ((B-A)/A)*100)
  ggplot(aes(year, rate_change)) +
  geom_segment(aes(xend = year, y = 0, yend = rate_change),
               colour = "grey") +
  geom_point(aes(colour = size), size=3, show.legend = F) +
  facet_grid( ~ size) +
  scale_fill_manual(values = c("#ad8599", "#e09952", "#75abbd")) +
  scale_x_continuous(breaks=seq(2009, 2019, 2)) +
  labs(title = "Year: {as.integer(frame_time)}",
       y = "Rate of Change [%]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45), 
        axis.title.x = element_blank()) +
  transition_time(year)



# first plot
brewer_size %>% 
  filter(total_barrels != is.na(total_barrels)) %>% 
  mutate(size = if_else(total_barrels <= brewer_quant[[2]], "small",
                        if_else(total_barrels >= brewer_quant[[4]], "large",
                                "medium"))) %>% 
  group_by(year, size) %>% 
  count(wt = n_of_brewers) %>% 
  ggplot(aes(year, n, colour = size)) +
  geom_point(size = 3, show.legend = F) +
  geom_line(size = 1.3, show.legend = F) +
  scale_fill_manual(values = c("#ad8599", "#e09952", "#75abbd")) +
  geom_text(aes(x = 2019, label = size), 
            hjust = -0.5, show.legend = F, size = 5) +
  coord_cartesian(clip = "off", xlim = c(2009, 2021)) +
  scale_x_continuous(breaks=seq(2009, 2019, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45), 
        axis.title.x = element_blank()) +
  transition_reveal(year) +
  labs(title = "Year: {as.integer(frame_along)}", 
       y = "Number of Brewers") 
  


# now you could actually see how many breweries fall within the categories
# To do: 
# - make a gif with a line chart with large, medium, small
# - add another plot where you can see the rate of change 
#    (what ever that means, animated as well)
# this is going to be messy, don't know if cowplot works with gganimate
# Idea for rate of change: 
# calculate daily rate of change...
  # ROC = ((B-A)/A)*100)

# Map ---------------------------------------------------------------------

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
# produce plot
map <- beer_states %>% 
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
  labs(title = "Can you keep pace?",
       subtitle = "Beer production and per-capita consumption, USA 2018", 
       caption = "Alcohol and Tobacco Tax and Trade Bureau (TTB)\n
       USAtoday\n
       Gregor Mathes 2020") +
  bi_theme(base_size = 12, font_color = "#262626") +
  theme(plot.margin = unit(c(.1, .5, .2, .1), "cm"),
        plot.title = element_text(hjust = 0.5, 
                                  margin = margin(t = 0,
                                                  b = .2,
                                                  unit = "cm"),),
        plot.subtitle = element_text(size= 10, 
                                     hjust = 0.5,
                                     debug = F, 
                                     margin = margin(t = 0,
                                                     b = .3,
                                                     unit = "cm"),),
        plot.caption = element_text(size = 7,
                                    hjust = 1,
                                    margin = margin(t = 0,
                                                    b = 0,
                                                    unit = "cm"),
                                    lineheight = .5,
                                    color = "#939184"))

  
map_legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Production ",
                    ylab = "Consumption ",
                    size = 8)  

# combine map with legend
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(map_legend, 0, 0, 0.35, 0.35)

ggsave(filename = "keep_pace.png")
