ReadErrorData <- function(wb){
  
  sheets = c("Baseline",
             "REWS",
             "Turbulence Correction",
             "REWS & Turbulence Correction",
             "Power Deviation Matrix")
  
  # get errors
  by.WS <- NULL
  by.TOD <- NULL
  by.CM <- NULL
  by.WD <- NULL
  by.Range <- NULL
  by.4CM <- NULL
  for (sheet in sheets){
    # by wind speed
    by.WS <- rbind(by.WS,
                   subExpandErrorByWSDF(subReadWSErrorData(wb,sheet),
                                        sheets))
    # by time of day
    by.TOD <- rbind(by.TOD,
                    subExpandErrorByTODDF(subReadTODErrorData(wb,sheet),
                                          sheets))
    
    # by calendar month
    by.CM <- rbind(by.CM,
                   subExpandErrorByCMDF(subReadCMErrorData(wb,sheet),
                                        sheets))
    
    # by wind direction
    by.WD <- rbind(by.WD,
                   subExpandErrorByWDDF(subReadWDErrorData(wb,sheet),
                                        sheets))
    
    # by range
    by.Range <- rbind(by.Range,
                   subExpandErrorByRangeDF(subReadRangeErrorData(wb,sheet),
                                        sheets))
    
    # from four-cell matrix
    by.4CM <- rbind(by.4CM,
                    subExpandErrorBy4CMDF(subRead4CMErrorData(wb,sheet),
                                            sheets))
    
  }
  
  # pack up the errors and return them
  errors <- list(by.WS = by.WS,
                 by.TOD = by.TOD,
                 by.CM = by.CM,
                 by.WD = by.WD,
                 by.Range = by.Range,
                 by.4CM = by.4CM)
}