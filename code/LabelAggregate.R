labelAggregate <- function(n.sub,
                           versions,
                           made.by){
  
  label.text = paste0(n.sub, " submissions from version ",
                      TextList(as.character(unique(versions)))," \n",
                      "Created ", format(Sys.time(), 
                                         "%B %d %Y at %H:%M"),
                      " by ", made.by)
  
}