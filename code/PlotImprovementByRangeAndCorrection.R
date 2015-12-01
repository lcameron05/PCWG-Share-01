PlotImprovementByRangeAndCorrection <- function(df,
                                                plot.label,
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
  
  # get the reference data
  df.ref <- df[(df$error.name == "NME") & (df$correction == "Baseline"),
               c("data.file","range","error.val.pc")]
  colnames(df.ref)[colnames(df.ref) == 'error.val.pc'] <- 'baseline.error.val.pc'
  
  
  
  df.other <- df[(df$error.name == "NME")&(df$correction != "Baseline"),
                 c("data.file","range","correction","error.val.pc")]
  
  df2 <- merge(df.other,
               df.ref,
               by = c("data.file","range"))
  
  df2$imp <- (df2$error.val.pc < df2$baseline.error.val.pc)
  
  df.imp <- aggregate(cbind(label=imp)~correction+range,
                      data = df2,
                      FUN = function(x){
                        n = length(as.numeric(x))
                        pos = sum(as.numeric(x),na.rm = TRUE)
                        return(paste0("n = ", n ,"\n", 
                                      round(100*pos/n), "% improved"))
                      })
  
  ptspermm <- 72 / 25.4
  
  p <- ggplot(data = df2,
              aes(x = baseline.error.val.pc,
                  y = error.val.pc)) +
    geom_abline(intercept=0, 
                slope=1,
                colour = "blue") +
    geom_point(aes(colour = (error.val.pc < baseline.error.val.pc))) +
    scale_colour_brewer(type = "qual",
                      palette= 7,
                      drop = TRUE,
                      name = "Improved?") +
    facet_grid(range~correction) +
    geom_text(data=df.imp, 
              aes(x=5, y=-10, label=label), 
              colour="black",
              inherit.aes=FALSE, 
              parse=FALSE,
              hjust = 1,
              vjust = 0,
              size=8/ptspermm ) +
    scale_y_continuous(name = "Normalized Mean Error (%)") +
    scale_x_continuous(name = "Baseline Normalized Mean Error (%)") +
    theme(legend.position="bottom")
  
  #+ fig.height = 4, fig.width = 6
  print(p)
  makeFootnote(plot.label)
  
  png(filename = file.path(output.dir,
                           "ImprovementByRangeAndCorrection.png"),
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