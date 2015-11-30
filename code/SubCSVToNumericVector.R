subCSVToNumericVector <- function(text.in){
  
  # supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  vec <- as.numeric(unlist(strsplit(text.in,split = ", ")))
  
  # turn warnings back on
  options(warn = oldw)
  
  return(vec)
  
}