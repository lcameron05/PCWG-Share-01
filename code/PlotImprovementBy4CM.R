PlotImprovementBy4CM <- function(df,
                                 sw.version = "",
                                 sw.version.logic = "equals",
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
    
    # create a "cell" value that we can look for
    df$cell <- paste0(df$WS.cell,
                      ", ",
                      df$Ti.cell)
    
    # get the reference data
    df.ref <- df[(df$error.name == "NME") & (df$correction == "Baseline"),
                 c("data.file","cell","error.val.pc")]
    colnames(df.ref)[colnames(df.ref) == 'error.val.pc'] <- 'baseline.error.val.pc'
    
    df.other <- df[(df$error.name == "NME")&(df$correction != "Baseline"),
                   c("data.file","cell","correction","error.val.pc")]
    
    df2 <- merge(df.other,
                 df.ref,
                 by = c("data.file","cell"))
    
    df2$imp <- (abs(df2$error.val.pc) < abs(df2$baseline.error.val.pc))
    
    df.imp <- aggregate(cbind(label=imp)~correction+cell,
                        data = df2,
                        FUN = function(x){
                          n = length(as.numeric(x))
                          pos = sum(as.numeric(x),na.rm = TRUE)
                          return(paste0("n = ", n ,"\n", 
                                        round(100*pos/n), "% improve"))
                        })
    
    max.error = ceiling(max(abs(df2$baseline.error.val.pc),
                            abs(df2$error.val.pc),
                            na.rm = TRUE))
    
    ptspermm <- 72 / 25.4
    
    p <- ggplot(data = df2,
                aes(x = baseline.error.val.pc,
                    y = error.val.pc)) +
      geom_hline(yintercept = 0,
                 colour = "black") +
      geom_vline(xintercept = 0,
                 colour = "black") +
      geom_point(aes(colour = abs(error.val.pc) < abs(baseline.error.val.pc)),
                 size= 1.5,
                 alpha = 0.75) +
      scale_colour_brewer(type = "qual",
                          palette= 7,
                          drop = TRUE,
                          name = "Improved?") +
      facet_grid(cell ~ correction) +
      geom_text(data = df.imp, 
                x = 0.95*-max.error,
                y = 0.95*max.error,
                aes(label=label), 
                colour = "black",
                inherit.aes = FALSE, 
                parse = FALSE,
                hjust = 0,
                vjust = 1,
                size=6/ptspermm ) +
      scale_y_continuous(name = "Normalized Mean Error with Corrections (%)") +
      scale_x_continuous(name = "Baseline Normalized Mean Error (%)") +
      coord_cartesian(xlim = c(-max.error, max.error),
                      ylim = c(-max.error,max.error)) 
    theme(legend.position="bottom",
          aspect.ratio = 1)
    
    #+ fig.height = 6, fig.width = 6
    print(p)
    makeFootnote(plot.label,
                 base.size = 6)
    
    if (sw.version == ""){
      filename = paste0("ImprovementBy4CM_allSWversions.png")
    } else {
      filename = paste0("ImprovementBy4CM_SWVersion",
                        sw.version.logic,
                        sw.version,
                        ".png")
    }
    
    png(filename = file.path(output.dir,
                             filename),
        width = 6, 
        height = 6, 
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