ReadSubmissionData <- function(wb){
  # reads a PCWG Share 01 file and returns data about the submission
  #
  # Args:
  # data.file: name of the data file to be read
  #
  # Returns:
  # data.frame containing data from the "submission" tab
  
  require(XLConnect)
  # get data
  data.sub = data.frame(sw.version = NullToNA(readWorksheet(wb, 
                                                            sheet = "Submission",
                                                            region = "C8",
                                                            simplify = TRUE,
                                                            header = FALSE)),
                        export.complete = NullToNA(readWorksheet(wb, 
                                                                 sheet = "Submission",
                                                                 region = "C9",
                                                                 simplify = TRUE,
                                                                 header = FALSE,
                                                                 colTypes = c(XLC$DATA_TYPE.BOOLEAN))),
                        random.ID = paste(as.character(readWorksheet(wb,
                                                                     sheet = "Submission",
                                                                     region = "C12:BC12",
                                                                     simplify = TRUE,
                                                                     header = FALSE)),
                                          collapse=", ",
                                          sep=" "),
                        stringsAsFactors=FALSE)
}

# note that there are some problems with XLConnect on a mac: see 
# https://github.com/s-u/rJava/issues/37 for details of how to fix them
# (download the Apple Java 1.6 release and install it)