#functions for drawing pictures
#画出每个航次的轨迹
plotSegs <- function(l) {
  
  for (i in (1:nrow(l[,.N,tripid]))) {
    trip = l[tripid == i]
    dev.new()
    plot(trip$lon1,trip$lat1)
  }
  
}

#dt must have a lon and lat column

getMap<-function(dt,zoomsize){
  
  lon=dt$lon
  lat=dt$lat
  
  centerX=0.5*(max(lon)+min(lon))
  centerY=0.5*(max(lat)+min(lat))
  
  #p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomsize,source='osm'))
  p<-ggmap(get_googlemap(center = c(centerX,centerY),zoom=zoomsize))
  
  return(p)
  
  
  
}

