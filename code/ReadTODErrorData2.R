ReadTODErrorData2 <- function(wb,sheet){
  # reads a PCWG Share 01 file and returns data about the baseline accuracy.
  # works on data files from version 0.5.9 and higher
  #
  # Args:
  # data.file: name of the data file to be read
  #
  # Returns:
  # data.frame containing data from the "Meta Data" tab
  
  require(XLConnect)
  setMissingValue(wb," ")
  
  # get data...
  # By normalized wind speed bin
  bin = paste(as.character(readWorksheet(wb,
                                         sheet = sheet,
                                         region = "D19:AA19",
                                         header = FALSE,
                                         autofitCol = FALSE,
                                         autofitRow= FALSE)),
              collapse=", ",
              sep=" ")
  data.count = paste(as.character(readWorksheet(wb,
                                                sheet = sheet,
                                                region = "D20:AA20",
                                                header = FALSE,
                                                autofitCol = FALSE,
                                                autofitRow= FALSE)),
                     collapse=", ",
                     sep=" ")
  NME = paste(as.character(readWorksheet(wb,
                                         sheet = sheet,
                                         region = "D21:AA21",
                                         header = FALSE,
                                         autofitCol = FALSE,
                                         autofitRow= FALSE)),
              collapse=", ",
              sep=" ")
  NMAE = paste(as.character(readWorksheet(wb,
                                          sheet = sheet,
                                          region = "D22:AA22",
                                          header = FALSE,
                                          autofitCol = FALSE,
                                          autofitRow= FALSE)),
               collapse=", ",
               sep=" ")
  
  # return data
  data.error = data.frame(sheet.name = sheet,
                          bin = as.character(bin),
                          data.count = as.character(data.count),
                          NME = as.character(NME),
                          NMAE = as.character(NMAE),
                          stringsAsFactors=FALSE)
}

# note that there are some problems with XLConnect on a mac: see 
# https://github.com/s-u/rJava/issues/37 for details of how to fix them
# (download the Apple Java 1.6 release and install it)