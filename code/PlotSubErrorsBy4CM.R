PlotSubErrorsBy4CM <- function(df,
                           plot.label,
                           output.dir = getwd()){
  # plots errors in a PCWG Share 01 file by calendar month
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
  
  # plot the data by bin
  p1 <- ggplot(data = df,
               aes(x = correction,
                   y = error.val.pc,
                   color = error.name)) + 
    geom_point() +
    facet_grid(WS.cell ~ Ti.cell) + 
    scale_color_brewer(type="qual",
                       palette = 7,
                       name = "Error type") +
    ggtitle("Four Cell Matrix Normalized Error") +
    labs(x = "Corrections Applied",
         y = "Error (Predicted - Actual, %)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p1)
  makeFootnote(plot.label)
  
  png(filename = file.path(output.dir,
                           "ErrorBy4CM.png"),
      width = 6, 
      height = 4, 
      units = "in", 
      pointsize = 12, 
      res = 300,
      bg = "white")
  print(p1)
  makeFootnote(plot.label,
               base.size = 8)
  dev.off()
  
  # turn warnings back on
  options(warn = oldw)
  
}