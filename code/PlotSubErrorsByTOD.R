PlotSubErrorsByTOD <- function(df,
                            plot.label,
                            output.dir = getwd()){
  # plots errors in a PCWG Share 01 file by normalized wind speed bin
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
  
  p1 <- ggplot(data = df,
               aes(x = x.min,
                   xend = x.max,
                   y = error.val.pc,
                   yend = error.val.pc,
                   colour = correction)) + 
    geom_segment() + 
    facet_wrap(~ error.name, ncol = 1) + 
    scale_color_brewer(type = "qual",
                       palette=1,
                      name = "Corrections Applied") +
    ggtitle("Error by Time of Day") +
    labs(x = "Hour (uncorrected for time zone)",
         y = "Error (Predicted - Actual, %)")
  
  print(p1)
  makeFootnote(plot.label)
  
  png(filename = file.path(output.dir,
                           "ErrorByTOD.png"),
      width = 6, 
      height = 4, 
      units = "in", 
      pointsize = 12, 
      res = 300,
      bg = "white")
  print(p1)
  makeFootnote(plot.label)
  dev.off()
  
  # turn warnings back on
  options(warn = oldw)
  
}