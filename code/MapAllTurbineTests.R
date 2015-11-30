MapAllTurbineLocations <- function(df,
                                   plot.label,
                                   code.dir,
                                   output.dir){
  
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
  
  # read world map
  world <- readOGR(dsn=file.path(code.dir,"worldmap"),
                   layer="TM_WORLD_BORDERS_SIMPL-0.3")
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
                              }))
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
    scale_fill_gradientn(name="N. Tests",
                         colours=rev(brewer.pal(9,"Spectral")),
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
  
  png(filename = file.path(output.dir,
                           "AllTurbineLocations_Map.png"),
      width = 6, 
      height = 4, 
      units = "in", 
      pointsize = 12, 
      res = 300,
      bg = "white")
  print(p)
  makeFootnote(plot.label,
               base.size = 8)
  dev.off()
  
  # turn warnings back on
  options(warn = oldw)
}
