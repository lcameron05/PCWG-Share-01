VersionStr <- function(verstr){
  require(stringr)
  str_extract(verstr,"\\d+(\\.\\d+)+")
}