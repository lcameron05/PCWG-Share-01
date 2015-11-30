PlotAllErrorsByWSBin_BoxNWhiskers <- function(df,
                              plot.label,
                              ouput.dir = file.path(getwd(),'analysis','all')){
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
  
  # create a bin label
  df$x.label <- factor(x = paste0(df$x.min, "-",df$x.max),
                       ordered = TRUE)
  
  # plot boxplots
  p1 <- ggplot(data = df,
               aes(x = x.label,
                   y = error.val.pc,
                   colour = correction)) + 
    geom_boxplot(outlier.size = 0.6) + 
    facet_wrap(~ error.name, ncol = 1)  + 
    scale_color_brewer(type = "qual",
                      palette = 1,
                      name = "Corrections Applied") +
    ggtitle("Error By Wind Speed Bin") +
    labs(x = "Normalized Wind Speed (binned)",
         y = "Error (Predicted - Actual, %)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #+ fig.height = 4, fig.width = 6
  print(p1)
  makeFootnote(plot.label)
  
  png(filename = file.path(output.dir,
                           "AllErrorsByWSBin_BoxNWhiskers.png"),
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