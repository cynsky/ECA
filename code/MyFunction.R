
# grid the points with a specific sacle(e.g. 10)
# points must be a data.table class
# points include for columns: time,lon,lat,sog
# return points with pid and gid
# the default scale is set to 10

setPoints<-function(points,scale){
  
  n=nrow(points)
  
  points[,pid:=seq(1,n,1)]
  points[,g.lon:=floor(lon*scale)/scale]
  points[,g.lat:=floor(lat*scale)/scale]
  points[,gid:=paste(floor(lon*scale)/scale,floor(lat*scale)/scale,sep='_')]
  
  setkey(points,time)
  
  return(points)
  
}

# points:pid,gid,lon,lat,time,...
# just add two points together


setLines<-function(points){
  
  n=nrow(points)
  setkey(points,time)
  dt1=points[1:(n-1),list(lon,lat,time,sog,pid,gid,g.lon,g.lat)]
  dt2=points[2:n,list(lon,lat,time,sog,pid,gid,g.lon,g.lat)]
  lines=cbind(dt1,dt2)
  setnames(lines,c('lon1','lat1','time1','sog1','pid1','gid1','g.lon1','g.lat1','lon2','lat2','time2','sog2','pid2','gid2','g.lon2','g.lat2'))
  lines[,lid:=seq(1,n-1)]
  
  return(lines)
  
}

#lines:data.table
#speed unit:knot(nm/h)

addLineSpeed<-function(lines,time_threshold,dist_threshold){ 
  
  lines=lines[,timespan:=abs(time2-time1)*1.0]
  lines=lines[,distance:=distance(lon1,lat1,lon2,lat2)]
  lines[,avgspeed1:=round((sog1+sog2)/2,1)]
  lines[,avgspeed2:=round((distance/1852)*10/(timespan/3600))]# lavgspeed 与 sog 单位相同 海里/小时*10
  lines[,avgspeed:=avgspeed1];
  lines[(distance/1852>dist_threshold)|(timespan>time_threshold),avgspeed:=avgspeed2] 
  return(lines)
  
}


gridALine<-function(line,scale){
  
#   scale=10
#   line=lines[2,]
  # line=lines[761,]
  timespan=abs(line$time2-line$time1)
  lid=line$lid
  
  #interpolate at grid.lon and grid.lat
  
  if(ceiling(min(line$lon1,line$lon2)*scale)/scale<floor(max(line$lon1,line$lon2)*scale)/scale){
  
  seq.lon=seq(ceiling(min(line$lon1,line$lon2)*scale)/scale,floor(max(line$lon1,line$lon2)*scale)/scale,by=1/scale)
  lon.interp.lat=approx(c(line$lon1,line$lon2),c(line$lat1,line$lat2),xout=seq.lon)$y
  lon.interp.time=approx(c(line$lon1,line$lon2),c(line$time1,line$time2),xout=seq.lon)$y
  lon.dt=data.table(cbind(seq.lon,lon.interp.lat,lon.interp.time))
  }else{
    lon.dt=line[lon1=='na',list(lon1,lat1,time1)] #get an empty data.table with 3
  }
  
  if(ceiling(min(line$lat1,line$lat2)*scale)/scale<floor(max(line$lat1,line$lat2)*scale)/scale){
    
  seq.lat=seq(ceiling(min(line$lat1,line$lat2)*scale)/scale,floor(max(line$lat1,line$lat2)*scale)/scale,by=1/scale)
  lat.interp.lon=approx(c(line$lat1,line$lat2),c(line$lon1,line$lon2),xout=seq.lat)$y
  lat.interp.time=approx(c(line$lat1,line$lat2),c(line$time1,line$time2),xout=seq.lat)$y
  lat.dt=data.table(cbind(lat.interp.lon,seq.lat,lat.interp.time))
  
  }else{
    lat.dt=line[lon1=='na',list(lon1,lat1,time1)] #get an empty data.table with 3
  }
  
  #remove duplicate points
  
  setnames(lon.dt,c('lon','lat','time'))
  setnames(lat.dt,c('lon','lat','time'))
  
  dt.inline=distinct(rbind(lon.dt,lat.dt))
  
  # add start and end point of the original line
  
  dt1=line[,list(lon1,lat1,time1)]
  dt2=line[,list(lon2,lat2,time2)]
  setnames(dt.inline,c('lon','lat','time'))
  setnames(dt1,c('lon','lat','time'))
  setnames(dt2,c('lon','lat','time'))
  points=rbind(dt1,dt.inline,dt2)
  setkey(points,time)
  
  #here the lines is grided line, each line only belong to an individual grid.
  
  points[,sog:=0]
  lines=setLines(setPoints(points,scale))
 
  # use average value to determine the grid where the line belongs to
  
  lines[,grid.x:=floor((lon1+lon2)/2*scale)/scale]
  lines[,grid.y:=floor((lat1+lat2)/2*scale)/scale]
  lines[,gid:=paste(grid.x,grid.y,sep='_')]
  
  lines[,percent:=abs(time2-time1)/timespan]
  lines[,lid:=line$lid]
  
  gridPercent=lines[,list(gid,grid.x,grid.y,percent,time1,lid)]
  
  #gridPercent:gid,grid.x,grid.y,percent
  
  return(gridPercent)
  
}


# get the percentage of line in each grid


gridLines<-function(lines,scale){
  
  gridPercent=data.table()
  
  for (i in (1:nrow(lines))) {
    
    print(i)
    
    line=lines[i,]
    dt=gridALine(line,scale)
    gridPercent=rbind(dt,gridPercent)
    
    
  }
  
  return(gridPercent)
  
}


# input data.table stops is with sog ==0 and h

getclusters2<-function(stops,e,MinPts){
  
  
  #distmatrix=geodetic.distance.dataframe(stops,lon,lat)
  
  distmatrix=dist(as.matrix(stops[,list(lon,lat)]))
  
  model <- dbscan(as.dist(distmatrix),eps=e,MinPts=MinPts)  # 50000 for archor places5
  stops=cbind(stops,cluster=model$cluster)
  return(stops)
  
}

sample_n<-function(dt,n){
  
   dt=dt[sample((1:nrow(dt)),n),]
  
   return(dt)
  
}

drawClst<-function(dt){
  library(ggplot2)
  p <-dtplot(dt)
  # p=getMap(x[cluster>0],5)
  p=p+geom_point(data=dt,size=1, aes(colour=factor(cluster)),shape=3)
  p=p+geom_text(data=dt,size=3,col='black',aes(x=lon+0.05,y=lat+0.05,label=cluster))
  p
}

getCircle <- function(center = c(0,0),r = 1, npoints = 100){
  
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
  
}
#经纬度为单位
p2pdist<-function(lon1,lat1,lon2,lat2){
  pt1=SpatialPoints(coords=data.table(lon=lon1,lat=lat1))
  pt2=SpatialPoints(coords=data.table(lon=lon2,lat=lat2))
  return(gDistance(pt1,pt2))
  
}



#--------------------------------
# cut data in a specific area
# @dt data.table type, must have lon,lat column
# @lonBtm,@latBtm are the lon and lat of the bottom point of the rectangle area. 
# @lonUp,@latUp are the lon and lat of the up point of the rectangle area. 

#get circle data----------
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

