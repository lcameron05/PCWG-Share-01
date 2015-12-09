PlotAllErrorsByWSBin_BoxNWhiskers <- function(df,
                                              data.range = "all",
                                              sw.version = "",
                                              sw.version.logic="equals",
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
  
  # filter by Range (all, inner, outer)
  df <- df[(df$range == data.range),]
  
  # filter by software version
  if (sw.version == ""){
    # get all versions
  } else {
    # get specific software version
    df <- SelectDatabySWVersion(df,
                                sw.version,
                                sw.version.logic)
  }
  
  # create the plot label
  plot.label <- labelAggregate(as.character(NROW(unique(df$data.file))),
                               df$sw.version,
                               made.by)
  
  # create a bin label
  df$x.label <- factor(x = paste0(df$x.min, "-",df$x.max),
                       ordered = TRUE)
  
  # figure out how many series we have to plot
  n.lines <- NROW(unique(df$data.file))
  
  # figure out the corrections that we have
  corrections = levels(df$correction)
  
  for (correction in corrections){
    # subset the data
    sub <- df[((df$correction == correction) &
                 (df$error.name == "NME")),]
    sub <- sub[!is.na(sub$error.val.pc),]
    
    # plot boxplots
    plot.title <- paste0("Error By Wind Speed Bin for ", 
                         data.range,
                         " Data")
    plot.subtitle <- paste0("Using ", 
                            correction, ". ", 
                            n.lines,
                            " data sets found.")
    
    p1 <- ggplot(data = sub,
                 aes(x = x.label,
                     y = error.val.pc)) + 
      geom_boxplot(outlier.size = 0.6) + 
      guides(colour = FALSE) +
      ggtitle(bquote(atop(.(plot.title), 
                          atop(italic(.(plot.subtitle)), "")))) + 
      scale_x_discrete(name = "Normalized Wind Speed (binned)") +
      scale_y_continuous(name = "Normalized Mean Error (Predicted - Actual, %)") +
      coord_cartesian(ylim = c(-15, 15)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    #+ fig.height = 4, fig.width = 6
    print(p1)
    makeFootnote(plot.label)
    
    if (sw.version == ""){
      filename = paste0("AllErrorsByWSBin_BoxNWhiskers_",
                        correction,
                        "_",
                        data.range,
                        "_allSWversions.png")
    } else {
      filename = paste0("AllErrorsByWSBin_BoxNWhiskers_",
                        correction,
                        "_",
                        data.range, 
                        "_SWVersion",
                        sw.version.logic,
                        sw.version,
                        ".png")
    }
    
    png(filename = file.path(output.dir,
                             filename),
        width = 6, 
        height = 4, 
        units = "in", 
        pointsize = 10, 
        res = 300,
        bg = "white")
    print(p1)
    makeFootnote(plot.label,
                 base.size = 6)
    dev.off()
    
  }
  
  # turn warnings back on
  options(warn = oldw)
}