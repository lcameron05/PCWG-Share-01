TextList <- function(vec){
 
  if (NROW(vec) > 1){
    vec[NROW(vec)] <- paste0("and ", vec[NROW(vec)])
    out <- paste(vec, collapse = ", ")
  } else
  {
    out <- vec
  }
  return(out)
}