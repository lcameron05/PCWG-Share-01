PlotAllTurbineLocations <- function(df,
                                    plot.label,
                                    output.dir){

  # plots baseline errors
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
  
  p <- ggplot(data = df,
              aes(x = Geography.country)) +
    geom_histogram() +
    scale_x_discrete(drop = FALSE,
                     name = "Country") +
    scale_y_continuous(name = "Count")
    
  print(p)
  makeFootnote(plot.label)
  
  png(filename = file.path(output.dir,
                           "AllTurbineLocations.png"),
      width = 6, 
      height = 4, 
      units = "in", 
      pointsize = 12, 
      res = 300,
      bg = "white")
  print(p)
  makeFootnote(plot.label,
               base.size = 8)
  dev.off()
  
  # turn warnings back on
  options(warn = oldw)
}