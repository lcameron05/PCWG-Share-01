CSVToRange <- function(text.in){
  # supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  xs <- as.numeric(unlist(strsplit(gsub(pattern = "-",
                                        replacement = ",",
                                        x = text.in,
                                        fixed = TRUE),
                                   split = ",")))
  out <- data.frame(x.min = xs[seq(1, length(xs), 2)],
             x.max = xs[seq(2, length(xs), 2)])
  
  # turn warnings back on
  options(warn = oldw)
  
  return(out)
}