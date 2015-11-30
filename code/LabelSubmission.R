labelSubmission <- function(in.sub,
                        made.by){
  
  # create the label
  label.text = paste0("Data sets ", in.sub$random.ID, "\n",
                     "Created ", format(Sys.time(), "%B %d %Y at %H:%M"), " by ", made.by)
  
}