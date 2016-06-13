#functions for drawing pictures
#画出每个航次的轨迹
plotSegs <- function(l) {
  for (i in (1:nrow(l[,.N,tripid]))) {
    trip = l[tripid == i]
    dev.new()
    plot(trip$lon1,trip$lat1)
  }
}

g=fread('zerogrids.csv')
p=getMap(g,7)
p=p+geom_point(data = g,aes(x=lon,y=lat))
p