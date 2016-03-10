PlotAllDataSources <- function(df,
                               sw.version = "",
                               sw.version.logic = "equals",
                               ouput.dir = file.path(getwd(),'analysis','all'),
                               made.by = ""){
  
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
  
  # get the maximum number of tests in any one country
  nmax <- max(aggregate(cbind(count = data.type)~data.supplier.type,
                        data = df[!(is.na(df$data.supplier.type)),],
                        FUN = length)$count,
              na.rm = TRUE)
  
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
    plot.label <- labelAggregate(as.character(NROW(df)),
                                 df$sw.version,
                                 made.by)
    
    # plot
    p <- ggplot(data = df,
                aes(x = data.supplier.type)) +
      geom_histogram() +
      scale_x_discrete(drop = FALSE,
                       name = "Data Sources") +
      scale_y_continuous(name = "Count")
    
    if (sw.version.logic == "equals"){
      
    } else {
      p <- p + 
        aes(fill = sw.version) +
        scale_fill_discrete(name = "Software\nVersion")
    }
    
    print(p)
    makeFootnote(plot.label)
    
    if (sw.version == ""){
      filename = paste0("DataSources_allSWversions.png")
    } else {
      filename = paste0("DataSources_SWVersion",
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