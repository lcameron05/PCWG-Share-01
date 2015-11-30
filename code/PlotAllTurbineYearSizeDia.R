PlotAllTurbineYearSizeDia <- function(df,
                                   plot.label,
                                   
                                   ouput.dir = file.path(getwd(),'analysis','all')){
  # plots metadata for PCWG Share 01 
  #
  # Args:
  # *.error summary of errors
  #
  # Returns:
  # a ggplot2 plot item
  
  require(ggplot2)

  # supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  p <- ggplot(df,
              aes(x = year.of.measurement,
                  y = turbine.height,
                  size = turbine.dia,
                  fill = "a",
                  group = "a")) + 
    geom_point(alpha = 0.25,
               colour ='black',
               shape = 21) +
    geom_jitter(position = position_jitter(width = .125),
                colour ='black',
                shape = 21,
                alpha = 0.25) + 
    scale_size_continuous(breaks=c(40, 60, 80, 100, 120),
                          range = c(3, 9)) +
    guides(size = guide_legend(title = "Diameter (m)")) +
    scale_fill_brewer(type = "qual",
                      palette = 7,
                      guide = FALSE) +
    ggtitle("Turbines in the Data Sets") +
    labs(x = "Year of Measurement",
         y = "Turbine Hub Height (m)")
  
  # colour by specific power
#   p <- p +
#     aes(colour = turbine.spec.power) +
#     scale_colour_distiller(name="Specific power (w/m2)",
#                            palette = "PuBu",
#                            type = "seq") + 
#     guides(colour = guide_legend(override.aes = list(alpha = 1)))
#     
  
  #+ fig.height = 4, fig.width = 6
  print(p)
  makeFootnote(plot.label)
  
  png(filename = file.path(output.dir,
                           "TurbineYearSizeDia.png"),
      width = 6, 
      height = 4, 
      units = "in", 
      pointsize = 12, 
      res = 300,
      bg = "white")
  print(p)
  makeFootnote(plot.label)
  dev.off()  
  # turn warnings back on
  options(warn = oldw)
}