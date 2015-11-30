labelAggregate <- function(n.sub,
                        made.by){
  
  label.text = paste0("Data from ", n.sub, " submissions\n",
                     "Created ", format(Sys.time(), "%B %d %Y at %H:%M"), " by ", made.by)
  
}