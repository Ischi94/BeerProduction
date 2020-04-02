library(tidyverse)
library(gganimate) # for gifs
library(magick) # for combining of gifs

# Get the Data
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')

# load my theme
source("my_theme.R")

# first keep it simple and look at brewer sizes per year
# I want a time series vs. number of brewers per brewer size
# for that, I divide brewer size in 3 categories: small, medium, large
brewer_quant <- brewer_size %>% {quantile(.$total_barrels, na.rm = T)}


# how did the number of each size change over time?
nr_change <- brewer_size %>% 
  filter(total_barrels != is.na(total_barrels)) %>% 
  mutate(size = if_else(total_barrels <= brewer_quant[[2]], "small",
                        if_else(total_barrels >= brewer_quant[[4]], "large",
                                "medium"))) %>% 
  group_by(year, size) %>% 
  count(wt = n_of_brewers) %>% 
  ggplot(aes(year, n, colour = size)) +
  geom_point(size = 3) +
  geom_line(size = 1.3) +
  geom_segment(aes(xend = 2019.3, yend = n, group = size),
               linetype = 2, colour = 'grey') + 
  scale_fill_manual(values = c("#ad8599", "#e09952", "#75abbd")) +
  geom_text(aes(x = 2019, label = size), 
            hjust = -0.5, show.legend = F, size = 5) +
  coord_cartesian(clip = "off", xlim = c(2009, 2021)) +
  scale_x_continuous(breaks=seq(2009, 2019, 1)) +
  my_theme +
  transition_reveal(year) +
  labs(title = "Year: {as.integer(frame_along)}", 
       y = "Number of Brewers") 

# animate nr_change to a gif
nr_gif <- animate(nr_change, width = 240*1.91, height = 310/2)


# visualise rate of change (roc) for each size
roc <- brewer_size %>% 
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
               colour = "grey", size = 1.65) +
  geom_point(aes(fill = size), size = 3.5,
             shape = 21, colour = "black") +
  geom_hline(yintercept = 0) +
  facet_grid( ~ size) +
  scale_colour_manual(values = c("#ad8599", "#e09952", "#75abbd")) +
  scale_x_continuous(breaks=seq(2009, 2019, 2)) +
  labs(y = "Rate of Change [%]") +
  coord_cartesian(ylim = c(-20, 100)) +
  my_theme +
  transition_time(year)

# animate roc to a gif
roc_gif <- animate(roc, width = 240*1.91, height = 310/2)

# combine gifs to one
roc_mgif <- image_read(roc_gif)
nr_mgif <- image_read(nr_gif)

# stack them
final_gif <- image_append(c(roc_mgif[1], nr_mgif[1]), stack = T)

# iterate
for(i in 2:100){
  combined <- image_append(c(roc_mgif[i], nr_mgif[i]), stack = T)
  final_gif <- c(final_gif, combined)
}

# save the gif
anim_save("brewers_vs_time.gif", final_gif)



