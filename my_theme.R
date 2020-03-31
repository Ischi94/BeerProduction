


my_theme <-   theme(
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # add a subtle grid
      # panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8, hjust = 0),
      plot.title = element_text(size = 14, hjust = 0, 
                                margin = margin(t = -0.3,
                                                b = .1,
                                                unit = "cm"),),
      plot.subtitle = element_text(size = 9, hjust = 0,
                                   debug = F, 
                                   margin = margin(t = 0,
                                                   b = -.25,
                                                   unit = "cm"),),
      plot.title.position = "plot",
      # captions
      plot.caption = element_text(size = 6,
                                  margin = margin(t = -0.4,
                                                  b = 0,
                                                  unit = "cm"),
                                  lineheight = .4,
                                  color = "#939184") )
