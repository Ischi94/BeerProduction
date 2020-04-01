library(tidyverse)
library(gganimate)

# Get the Data

brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')

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
  labs(y = "Rate of Change [%]") +
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

