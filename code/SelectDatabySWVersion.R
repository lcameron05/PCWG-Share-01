SelectDatabySWVersion <- function(df,
                                  sw.version,
                                  sw.version.logic = "equals"){
  if (sw.version.logic == "equals"){
    df <- df[as.logical(sapply(VersionStr(df$sw.version),
                               function(x){
                                 compareVersion(x,
                                                sw.version)
                               })==0),] }
  else if (sw.version.logic == "from"){
    df <- df[as.logical(sapply(VersionStr(df$sw.version),
                               function(x){
                                 compareVersion(x,
                                                sw.version)
                               })>=0),] }
  else if (sw.version.logic == "to"){
    df <- df[as.logical(sapply(VersionStr(df$sw.version),
                               function(x){
                                 compareVersion(x,
                                                sw.version)
                               })<=0),] }
}