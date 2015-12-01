ExpandErrorBy4CMDF <-function(df.in,
                                 sheets){
  # create empty DF
  # note that we need to overwrite the cell structure in the excel sheet
  cell.levels <- c("LWS-LTI","LWS-HTI","HWS-LTI","HWS-HTI")
  df.out <- data.frame(correction = factor(x=character(),
                                        levels = sheets),
                       cell = factor(x=character(),
                                     levels = cell.levels),
                       data.count = double(),
                       error.name = factor(x=character(),
                                           levels = c("NME","NMAE")),
                       error.val = double(),
                       stringsAsFactors=FALSE)
  df.out <-rbind(df.out,
                 data.frame(correction = factor(x=df.in$sheet.name,
                                             levels = sheets),
                            cell = factor(x = c("LWS-LTI","LWS-HTI","HWS-LTI","HWS-HTI"),
                                          levels = cell.levels),
                            data.count = CSVToNumericVector(df.in$data.count),
                            error.name = as.factor("NME"),
                            error.val = CSVToNumericVector(df.in$NME),
                            stringsAsFactors=FALSE),
                 data.frame(correction = factor(x=df.in$sheet.name,
                                             levels = sheets),
                            cell = factor(c("LWS-LTI","LWS-HTI","HWS-LTI","HWS-HTI"),
                                          levels = cell.levels),
                            data.count = CSVToNumericVector(df.in$data.count),
                            error.name = as.factor("NMAE"),
                            error.val = CSVToNumericVector(df.in$NMAE),
                            stringsAsFactors=FALSE))
  
  # convert errrors into percentages
  df.out$error.val.pc <- df.out$error.val * 100
  
  # create extra levels to capture low/high WS/Ti
  df.out$WS.cell <- ""
  df.out$Ti.cell <- ""
  df.out$WS.cell[grep("LWS", df.out$cell)] <- "Low WS"
  df.out$WS.cell[grep("HWS", df.out$cell)] <- "High WS"
  df.out$Ti.cell[grep("LTI", df.out$cell)] <- "Low Ti"
  df.out$Ti.cell[grep("HTI", df.out$cell)] <- "High Ti"
  
  # factorize the Ti and WS bins
  df.out$Ti.cell = factor(df.out$Ti.cell, levels=c("Low Ti","High Ti"))
  df.out$WS.cell = factor(df.out$WS.cell, levels=c("Low WS","High WS"))
  
  return(df.out)
  
}