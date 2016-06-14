

#default scale is 10
setPoints <- function(points,scale = 10) {
  n = nrow(points)
  
  points[,pid:= seq(1,n,1)]
  points[,g.lon:= floor(lon * scale) / scale]
  points[,g.lat:= floor(lat * scale) / scale]
  points[,gid:= paste(floor(lon * scale) / scale,floor(lat * scale) / scale,sep =
                         '_')]
  
  setkey(points,time)
  
  return(points)
  
}

# points:pid,gid,lon,lat,time,...
# just add two points together


setLines <- function(points) {
  n = nrow(points)
  setkey(points,time)
  dt1 = points[1:(n - 1),list(lon,lat,time,sog,pid,gid,g.lon,g.lat)]
  dt2 = points[2:n,list(lon,lat,time,sog,pid,gid,g.lon,g.lat)]
  lines = cbind(dt1,dt2)
  setnames(
    lines,c(
      'lon1','lat1','time1','sog1','pid1','gid1','g.lon1','g.lat1','lon2','lat2','time2','sog2','pid2','gid2','g.lon2','g.lat2'
    )
  )
  lines[,lid:= seq(1,n - 1)]
  
  return(lines)
  
}

#lines:data.table
#speed unit:knot(nm/h)

addLineSpeed <- function(lines,time_threshold = 600,dist_threshold = 2) {
  lines = lines[,timespan:= abs(time2 - time1) * 1.0]
  lines = lines[,distance:= distance(lon1,lat1,lon2,lat2)]
  lines[,avgspeed1:= round((sog1 + sog2) / 2,1)]
  lines[,avgspeed2:= round((distance / 1852) * 10 / (timespan / 3600))]# lavgspeed 与 sog 单位相同 海里/小时*10
  lines[,avgspeed:= avgspeed1];
  lines[(distance / 1852 > dist_threshold) |
          (timespan > time_threshold),avgspeed:= avgspeed2]
  return(lines)
  
}
#distance of two points,单位米
distance <- function(lon1,lat1,lon2,lat2) {
  radlat1 = rad(lat1);
  radlat2 = rad(lat2);
  delta_lon = rad(lon2 - lon1);
  top1 = cos(radlat2) * sin(delta_lon);
  top2 = cos(radlat1) * sin(radlat2) - sin(radlat1) * cos(radlat2) * cos(delta_lon);
  top = sqrt(top1 * top1 + top2 * top2);
  bottom = sin(radlat1) * sin(radlat2) + cos(radlat1) * cos(radlat2) * cos(delta_lon);
  delta_sigma = atan2(top,bottom);
  distance = delta_sigma * 6378137.0;
  return (distance);
  
}

#计算弧度
rad <- function(d) {
  return (d * pi / 180);
  
}

#time_threshold 位两相邻轨迹点之间的时间间隔，单位为天，默认值为3天 72 小时
#返回值为加入了tripid的lines

segTra <- function(l,time_threshold = 3) {
  setkey(l,lid)
  bl = l[timespan / 3600 / 24 >= time_threshold,list(lid,time1,time2,timespan)]
  lids = bl$lid
  if (nrow(bl) > 1) {
    l[,tripid:= 0]#分割的line的tripid=0
    for (i in (1:(nrow(bl) - 1))) {
      l[(lid > lids[i]) & (lid < lids[i + 1]),tripid:= (i + 1)]
    }
    
    #第一个和最后一个tripid
    l[(lid < lids[1]),tripid:= 1]
    l[(lid > lids[nrow(bl)]),tripid:= (nrow(bl) + 1)]
   
  }else if (nrow(bl) == 1) {
    l[,tripid:= 0]#分割的line的tripid=0
    #第一个和最后一个tripid
    l[(lid < lids[1]),tripid:= 1]
    l[(lid > lids[nrow(bl)]),tripid:= (nrow(bl) + 1)]

  }else{
    l[,tripid:= 1]
    
  }
  return(l)
}

getships <- function(shipfile) {
  #ships=fread('D:/Rprojects/ships/ships.txt',sep=',') # build year should be include
  ships = fread(shipfile,sep = ',')
  ships = ships[!is.na(mmsi)]
  # ships$mmsi<-as.character(ships$mmsi)
  setkey(ships,mmsi)
  return (ships)

}

getMap<-function(dt,zoomsize){
  

  lon=dt$lon
  lat=dt$lat
  
  centerX=0.5*(max(lon)+min(lon))
  centerY=0.5*(max(lat)+min(lat))
  
  p<-ggmap(get_googlemap(location=c(centerX,centerY),zoom=zoomsize))
  
  return(p)

}

gridALine<-function(line,scale=10){
  
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


gridLines<-function(lines,scale=10){
  
  gridPercent=data.table()
  
  for (i in (1:nrow(lines))) {
    if(i%%1000==0){
      print(i)
    }
    
    
    line=lines[i,]
    dt=gridALine(line,scale)
    gridPercent=rbind(dt,gridPercent)
    
    
  }
  
  return(gridPercent)
  
}

traEmission<-function(ship,lines,ef){
  
  
  
  
  
  
}

