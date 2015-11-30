PlotAllErrorsByWSBin_Lines <- function(df,
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
  require(RColorBrewer)
  
  # supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  # create a bin label
  df$x.label <- factor(x = paste0(df$x.min, "-",df$x.max),
                       ordered = TRUE)
  
  # figure out the corrections that we have
  corrections = levels(df$correction)
  
  for (correction in corrections){
    # subset the data
    sub <- df[((df$correction == correction) &
                 (df$error.name == "NME")),]
    sub <- sub[!is.na(sub$error.val.pc),]
    
    # figure out how many series we have to plot
    n.lines <- NROW(unique(sub$data.file))
    lines.palette <- colorRampPalette(brewer.pal(8,"Paired"))(n.lines)
      
    # plot boxplots
    plot.title <- "Error By Wind Speed Bin"
    plot.subtitle <- paste0("Using ", correction, ". ", n.lines, " data sets found.")
    
    p <- ggplot(data = sub,
                aes(x = x.label,
                    y = error.val.pc,
                    group = data.file,
                    colour = data.file)) + 
      geom_hline(yintercept = 0) +
      geom_line() + 
      #stat_summary(fun.y=sum, geom="line") +
      scale_color_manual(values = lines.palette) +
      guides(colour = FALSE) +
      ggtitle(bquote(atop(.(plot.title), 
                          atop(italic(.(plot.subtitle)), "")))) + 
      scale_x_discrete(name = "Normalized Wind Speed (binned)") +
      scale_y_continuous(name = "Error (Predicted - Actual, %)")+
      coord_cartesian(ylim = c(-15, 15)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    #+ fig.height = 4, fig.width = 6
    print(p)
    makeFootnote(plot.label)
    
    png(filename = file.path(output.dir,
                             paste0("AllErrorsByWSBin_Lines_", correction, ".png")),
        width = 6, 
        height = 4, 
        units = "in", 
        pointsize = 12, 
        res = 300,
        bg = "white")
    print(p)
    makeFootnote(plot.label)
    dev.off()
    
  }
  
  # turn warnings back on
  options(warn = oldw)
}