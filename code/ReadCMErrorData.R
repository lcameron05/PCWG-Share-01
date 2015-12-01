ReadCMErrorData <- function(wb,sheet){
  # reads a PCWG Share 01 file and returns data about the error
  #
  # Args:
  # data.file: name of the data file to be read
  #
  # Returns:
  # data.frame containing data from the "Meta Data" tab
  
  require(XLConnect)
  setMissingValue(wb," ")
  
  # get data...
  # By calendar month
  bin = paste(as.character(readWorksheet(wb,
                                         sheet = sheet,
                                         region = "D15:O15",
                                         header = FALSE,
                                         autofitCol = FALSE,
                                         autofitRow= FALSE)),
              collapse=", ",
              sep=" ")
  data.count = paste(as.character(readWorksheet(wb,
                                                sheet = sheet,
                                                region = "D16:O16",
                                                header = FALSE,
                                                autofitCol = FALSE,
                                                autofitRow= FALSE)),
                     collapse=", ",
                     sep=" ")
  NME = paste(as.character(readWorksheet(wb,
                                         sheet = sheet,
                                         region = "D17:O17",
                                         header = FALSE,
                                         autofitCol = FALSE,
                                         autofitRow= FALSE)),
              collapse=", ",
              sep=" ")
  NMAE = paste(as.character(readWorksheet(wb,
                                          sheet = sheet,
                                          region = "D18:O18",
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