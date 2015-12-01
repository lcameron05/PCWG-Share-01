NullToNA <- function(x){
  # function to return NA if a single input is NULL
  
  if(is.null(x)){
    return(NA)
  } else {
    return(x)
  }
  
}