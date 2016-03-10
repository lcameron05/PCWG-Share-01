MapAllTurbineLocations <- function(df,
                                   sw.version = "",
                                   sw.version.logic = "equals",
                                   code.dir,
                                   ouput.dir = file.path(getwd(),'analysis','all'),
                                   made.by = ""){
  
  # plots baseline errors
  #
  # Args:
  # *.error summary of errors
  #
  # Returns:
  # a ggplot2 plot item
  
  require(rgdal)        # for readOGR(...)
  require(RColorBrewer) # for brewer.pal(...)
  require(ggplot2)
  
  # supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  # get the maximum number of tests in any one country
  nmax <- max(aggregate(cbind(count = data.type)~Geography.country,
                        data = df[!(is.na(df$Geography.country)),],
                        FUN = length)$count,
              na.rm = TRUE)
  
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
    
    # create the plot label
    plot.label <- labelAggregate(as.character(NROW(df)),
                                 df$sw.version,
                                 made.by)
    
    # read world map
    world <- readOGR(dsn=file.path(code.dir,"worldmap"),
                     layer="TM_WORLD_BORDERS_SIMPL-0.3",
                     verbose=FALSE)
    # get countries
    countries <- world@data
    countries <- cbind(id=rownames(countries),countries)
    
    # get the data count
    counts <- aggregate(cbind(count = data.type)~Geography.country,
                        data = df[!(is.na(df$Geography.country)),],
                        FUN = length)
    # change the names of some of the countries to correspond to the 
    counts <- data.frame(sapply(counts,
                                FUN = function(x){
                                  x<-gsub("US","United States",x)
                                  x<-gsub("UK","United Kingdom",x)
                                  return(x)
                                },
                                simplify = FALSE))
    counts$count <- as.numeric(levels(counts$count))[counts$count]
    counts$Geography.country <- as.character(levels(counts$Geography.country))[counts$Geography.country]
    
    # combine the data count with the country outlines
    countries <- merge(countries,
                       counts,
                       by.x="NAME", 
                       by.y="Geography.country", 
                       all.x=T)
    
    #countries$count(is.na(countries$count)) <- o
    
    # now create the data frame to plot
    map.df <- fortify(world)
    map.df <- merge(map.df,
                    countries,
                    by="id")
    
    # create the plot
    p <- ggplot(map.df, 
                aes(x=long,
                    y=lat,
                    group=group)) +
      geom_polygon(aes(fill=count),
                   size = 0.125)+
      geom_path(colour="grey50",
                size = 0.125)+
      scale_fill_gradientn(name="N. Data Sets",
                           colours=rev(brewer.pal(9,"Spectral")),
                           limits =c(0,nmax),
                           na.value="white")+
      coord_fixed() +
      scale_x_continuous(name = "",
                         breaks = c(-180,-90,0,90,180),
                         labels = c("180 E","90 E","0","90 W","180 W")) +
      scale_y_continuous(name = "",
                         breaks = c(-90,-45,0,45,90),
                         labels = c("90 S","45 S","0","45 N","90 N"))
    
    print(p)
    
    makeFootnote(plot.label)
    
    if (sw.version == ""){
      filename = paste0("AllTurbineLocations_Map_allSWversions.png")
    } else {
      filename = paste0("AllTurbineLocations_Map_SWVersion",sw.version,".png")
    }
    
    png(filename = file.path(output.dir,
                             filename),
        width = 6, 
        height = 4, 
        units = "in", 
        pointsize = 12, 
        res = 300,
        bg = "white")
    print(p)
    makeFootnote(plot.label,
                 base.size = 6)
    dev.off()
  } else {
    message("No data found with the requested software version")
  }
  
  # turn warnings back on
  options(warn = oldw)
}
