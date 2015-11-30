PlotAllChangeInErrorsByWSBin <- function(df,
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
  p <- ggplot(data = df,
               aes(x = x.label,
                   y = error.delta.pc,
                   colour = correction)) + 
    geom_hline(yintercept=0) +
    geom_boxplot(outlier.size = 0.6) + 
    facet_wrap(~ error.name,
               scales = 'free_y',
               ncol = 1)  +
    scale_color_brewer(type = "qual",
                       palette = 1,
                       name = "Corrections Applied") +
    ggtitle("Change in Normalized Error in All Data Sets") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Error By Wind Speed Bin") +
    labs(x = "Normalized Wind Speed (binned)",
         y = "Change in Normalized Error Compared to Baseline")
  
  #+ fig.height = 4, fig.width = 6
  print(p)
  makeFootnote(plot.label)
  
  png(filename = file.path(output.dir,
                           "ChangeInAllErrorsByWSBin.png"),
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