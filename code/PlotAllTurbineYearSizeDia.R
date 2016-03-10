PlotAllTurbineYearSizeDia <- function(df,
                                      sw.version = "",
                                      sw.version.logic = "equals",
                                      ouput.dir = file.path(getwd(),'analysis','all'),
                                      made.by = ""){
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
  
  # filter by software version
  if (sw.version == ""){
    # get all versions
  } else {
    # get specific software version
    df <- SelectDatabySWVersion(df,
                                sw.version,
                                sw.version.logic)
  }
  
  # continue if we have data
  if (NROW(df)>0){
    
    # create the plot label
    plot.label <- labelAggregate(as.character(NROW(unique(df$data.file))),
                                 df$sw.version,
                                 made.by)
    
    # plot the results
    p <- ggplot(df,
                aes(x = year.of.measurement,
                    y = turbine.height,
                    size = turbine.dia,
                    fill = sw.version,
                    group = sw.version)) + 
      geom_point(alpha = 0.25,
                 colour ='black',
                 shape = 21) +
      geom_jitter(position = position_jitter(width = .125),
                  colour ='black',
                  shape = 21,
                  alpha = 0.25) + 
      scale_size_continuous(breaks=c(40, 60, 80, 100, 120),
                            range = c(min(df$turbine.dia,na.rm=TRUE)/10, 
                                      max(df$turbine.dia,na.rm=TRUE)/10),
                            name = "Diameter (m)") +
      ggtitle("Turbines in the Data Sets") +
      labs(x = "Year of Measurement",
           y = "Turbine Hub Height (m)")
    
    if (sw.version.logic == "equals"){
      p <- p + scale_fill_discrete(guide=FALSE)
    } else {
      p <- p + scale_fill_discrete(name = "Software\nVersion")
    }
    
    #+ fig.height = 4, fig.width = 6
    print(p)
    makeFootnote(plot.label)
    
    if (sw.version == ""){
      filename = paste0("TurbineYearSizeDia_allSWversions.png")
    } else {
      filename = paste0("TurbineYearSizeDia_SWVersion",
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
    print(p)
    makeFootnote(plot.label,
                 base.size = 6)
    dev.off()  
  } else {
    message("No data found with the requested software version")
  }
  
  # turn warnings back on
  options(warn = oldw)
}