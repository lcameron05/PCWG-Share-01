TextList <- function(vec){
 
  if (NROW(vec) > 2){
    vec[NROW(vec)] <- paste("and", vec[NROW(vec)])
    out <- paste(vec, collapse = ", ")
  } else if (NROW(vec) == 2){
    vec[NROW(vec)] <- paste("and", vec[NROW(vec)])
    out <- paste(vec, collapse = " ")
  } else {
    out <- vec
  }
  return(out)
}