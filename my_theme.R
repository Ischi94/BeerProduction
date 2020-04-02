my_theme <-   theme(
      # remove legend
      legend.position = "none",
      # background
      strip.background = element_blank(), 
      panel.background = element_blank(),
      # axis
      axis.text.x = element_text(angle = 45, size = 7.5),
      axis.text.y = element_text(size = 9),
      # axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 11),
      # remove grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # # borders and margins
      # plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      # panel.border = element_blank(),
      # panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      strip.text = element_text(size = 11),
      plot.title = element_text(size = 14, hjust = 0.5, 
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
                                  color = "#939184"))
