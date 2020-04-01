library(tidyverse)
library(USAboundaries)
library(sf)
library(biscale)
library(cowplot)

beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')


# I want to map where I show  the per capita consumption per state and 
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
