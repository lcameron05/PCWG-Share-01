CSVToCharacterVector <- function(text.in){
  
  # supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  vec <- unlist(strsplit(text.in,split = ", "))
  
  # turn warnings back on
  options(warn = oldw)
  
  return(vec)
}