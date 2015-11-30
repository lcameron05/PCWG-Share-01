PlotAllOuterErrorsTurbulenceCorrection <- function(df,
                                                   plot.label,
                                                   output.dir = getwd()){
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
  
  # get the data we want
  df <- df[((df$correction == "Baseline") | (df$correction == "Turbulence Correction")) &
             (df$range == "Outer") & 
             (df$error.name == "NME"),]
  
  df$error.val.pc.cut <- cut(x = df$error.val.pc,
                             breaks = c(-100,
                                        seq(from = -2.75,
                                            to = 2.75,
                                            by = 0.5),
                                        100),
                             labels = c("< -2.75",
                                        paste(seq(from = -2.5,
                                                  to = 2.5,
                                                  by = 0.5)),
                                        ">2.75"))
  
  # plot the NME
  p <- ggplot(data = as.data.frame(table(df$error.val.pc.cut,
                                         droplevels(df$correction))),
              aes(x = Var1,
                  y = Freq,
                  fill = Var2)) +
    geom_vline(aes(xintercept = which(levels(Var1) %in% '0'))) +
    geom_bar(stat = 'identity',
             position = "dodge") +
    scale_x_discrete(drop = FALSE,
                     name = "Error (Predicted - Actual, %)") +
    scale_y_continuous(name = "Count") +
    scale_fill_brewer(type = "qual",
                      palette= 7,
                      drop = TRUE,
                      name = "Corrections") +
    ggtitle("Outer Range Normalized Error")
  
  print(p)
  makeFootnote(plot.label)
  
  png(filename = file.path(output.dir,
                           "OuterRangeTurbulenceCorrectionHistogram.png"),
      width = 6, 
      height = 4, 
      units = "in", 
      pointsize = 12, 
      res = 300,
      bg = "white")
  print(p)
  makeFootnote(plot.label,
               base.size = 8)
  dev.off()
  
  # turn warnings back on
  options(warn = oldw)
  
}