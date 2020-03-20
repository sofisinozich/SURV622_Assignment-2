library(ggplot2)

# Set the default theme for plots
  if ('extrafont' %in% installed.packages()) {  
    extrafont::loadfonts(device = 'win')
    .font_to_use <- 'Arial Narrow'
  } else {
    .font_to_use <- "sans"
  }

  theme_set(
    theme_minimal(base_family = .font_to_use, base_size = 12) + 
      theme(plot.title.position = 'plot',
            plot.title = element_text(family = .font_to_use,
                                      face = "bold", size = 14),
            axis.title = element_text(family = .font_to_use,
                                      face = 'bold', size = 12,
                                      angle = 0),
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text = element_text(size = 10),
            strip.text = element_text(family = .font_to_use,
                                        size = 12, angle = 0),
            strip.text.y.left = element_text(family = .font_to_use,
                                             size = 12, angle = 0))
  )
  
  rm(.font_to_use)
  
# Update the default color of geoms to match Maryland colors
  .maryland_red <- "#cf102d"

  ggplot2::update_geom_defaults('bar', list(colour = .maryland_red,
                                            fill = .maryland_red))
  ggplot2::update_geom_defaults('col', list(colour = .maryland_red,
                                            fill = .maryland_red))
  ggplot2::update_geom_defaults('point', list(colour = .maryland_red,
                                              fill = .maryland_red))
  ggplot2::update_geom_defaults('line', list(colour = .maryland_red, size = 1.5))
