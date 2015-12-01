ExpandErrorByCMDF <-function(df.in,
                                sheets){
  # create empty DF
  month.levels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dev")
  df.out <- data.frame(correction = factor(x=character(),
                                        levels = sheets),
                       month = factor(x=character(),
                                      levels = month.levels),
                       data.count = double(),
                       error.name = factor(x=character(),
                                           levels = c("NME","NMAE")),
                       error.val = double(),
                       stringsAsFactors=FALSE)
  df.out <-rbind(df.out,
                 data.frame(correction = factor(x=df.in$sheet.name,
                                             levels = sheets),
                            month = factor(CSVToCharacterVector(df.in$bin),
                                           levels = month.levels),
                            data.count = CSVToNumericVector(df.in$data.count),
                            error.name = as.factor("NME"),
                            error.val = CSVToNumericVector(df.in$NME),
                            stringsAsFactors=FALSE),
                 data.frame(correction = factor(x=df.in$sheet.name,
                                             levels = sheets),
                            month = factor(CSVToCharacterVector(df.in$bin),
                                           levels = month.levels),
                            data.count = CSVToNumericVector(df.in$data.count),
                            error.name = as.factor("NMAE"),
                            error.val = CSVToNumericVector(df.in$NMAE),
                            stringsAsFactors=FALSE))
  
  # convert errrors into percentages
  df.out$error.val.pc <- df.out$error.val * 100
  
  return(df.out)
}