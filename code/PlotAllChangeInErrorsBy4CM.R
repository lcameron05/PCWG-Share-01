PlotAllChangeInErrorsBy4CM <- function(df.in,
                                       sw.version = "",
                                       sw.version.logic = "equals",
                                       error.name = "",
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
  
  # work out the change compared to the baseline
  df <- NULL
  for (file in unique(df.in$data.file)){
    sub <- df.in[df.in$data.file == file,]
    for (ds in unique(sub$correction)){
      for (WS.cell in unique(sub$WS.cell)){
        for (Ti.cell in unique(sub$Ti.cell)){
          # get the reference NME and NMAE
          baseline <- sub[(sub$correction == "Baseline") & 
                            (sub$WS.cell == WS.cell) &
                            (sub$Ti.cell == Ti.cell),]
          # get the NME and NMAE for this cell
          new <- sub[(sub$correction == ds) & 
                       (sub$WS.cell == WS.cell) &
                       (sub$Ti.cell == Ti.cell),]
          delta <- data.frame(new,
                              error.delta.pc = c(abs(new$error.val.pc[new$error.name == "NME"]) - abs(baseline$error.val.pc[baseline$error.name == "NME"]),
                                                 abs(new$error.val.pc[new$error.name == "NMAE"]) - abs(baseline$error.val.pc[baseline$error.name == "NMAE"])))
          df <- rbind(df,
                      delta)
        }
      }
    }
  }
  
  # filter df by software version
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
    
    #filter by error type
    if (error.name == ""){
      
    } else if (error.name == "NME"){
      df <- df[(df$error.name == "NME"),] 
    } else if (error.name == "NMAE"){
      df <- df[(df$error.name == "NMAE"),] 
    }
    
    # plot the data by bin
    p1 <- ggplot(data = df,
                 aes(x = correction,
                     y = error.delta.pc)) + 
      geom_hline(yintercept=0) +
      geom_boxplot(outlier.size = 0.6) + 
      facet_grid(WS.cell ~ Ti.cell,
                 as.table = FALSE) +
      labs(x = "Corrections Applied",
           y = "|Normalized Error with Corrections|\n - |Baseline Normalized Error| (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # change plot by error type
    if (error.name == ""){
      p1 <- p1 + 
        aes(color = error.name) +
        ggtitle("Change in Magnitude of Errors") +
        scale_color_brewer(type="qual",
                           palette = 7,
                           name = "Error type")
      base.filename <- "ChangeInErrorBy4CM"
    } else if (error.name == "NME"){
      p1 <- p1 + 
        ggtitle("Change in Magnitude of Normalized Mean Error (NME)")
      
      base.filename <- "ChangeInNMEBy4CM"
    } else if (error.name == "NMAE"){
      p1 <- p1 + 
        ggtitle("Change in Magnitude of Normalized Mean Absolute Error (NMAE)")
      
      base.filename <- "ChangeInNMAEBy4CM"
    }
    
    
    print(p1)
    makeFootnote(plot.label)
    if (sw.version == ""){
      filename = paste0(base.filename,
                        "allSWversions.png")
    } else {
      filename = paste0(base.filename,
                        "_SWVersion",
                        sw.version.logic,
                        sw.version,
                        ".png")
    }
    
    png(filename = file.path(output.dir,
                             filename),
        width = 6, 
        height = 5, 
        units = "in", 
        pointsize = 10, 
        res = 300,
        bg = "white")
    print(p1)
    makeFootnote(plot.label,
                 base.size = 6)
    dev.off()
  } else {
    message("No data found with the requested software version")
  }
  
  # turn warnings back on
  options(warn = oldw)
  
}