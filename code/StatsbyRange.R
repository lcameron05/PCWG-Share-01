StatsByRange <- function(df,
                         error.name = "NME",
                         sw.version,
                         sw.version.logic = "from"){
  
  # filter by error name (NME, NMAE)
  df <- df[(df$error.name == error.name),]
  
  # filter by software version
  if (sw.version == ""){
    # get all versions
  } else {
    # get specific software version
    df <- SelectDatabySWVersion(df,
                                sw.version,
                                sw.version.logic)
  }
  
  # continue if we have data
  if (NROW(df)>0){
    # get the stats
    print("Mean")
    print(aggregate(error.val.pc ~ range + correction,
                    data = df,
                    FUN = function(x){mean(x,na.rm = TRUE)}))
    
    print("Standard Deviation")
    print(aggregate(error.val.pc ~ range + correction,
                    data = df,
                    FUN = function(x){sd(x,na.rm = TRUE)}))
  } else {
    message("No data found with the requested software version")
  }
  
  
  
}