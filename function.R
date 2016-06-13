

#default scale is 10
setPoints<-function(points,scale=10){
  
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

addLineSpeed<-function(lines,time_threshold=600,dist_threshold=2){ 
  
  lines=lines[,timespan:=abs(time2-time1)*1.0]
  lines=lines[,distance:=distance(lon1,lat1,lon2,lat2)]
  lines[,avgspeed1:=round((sog1+sog2)/2,1)]
  lines[,avgspeed2:=round((distance/1852)*10/(timespan/3600))]# lavgspeed 与 sog 单位相同 海里/小时*10
  lines[,avgspeed:=avgspeed1];
  lines[(distance/1852>dist_threshold)|(timespan>time_threshold),avgspeed:=avgspeed2] 
  return(lines)
  
}
#distance of two points,单位米
distance<-function(lon1,lat1,lon2,lat2){
  
  radlat1=rad(lat1);
  radlat2=rad(lat2);
  delta_lon=rad(lon2-lon1);
  top1=cos(radlat2)*sin(delta_lon);
  top2=cos(radlat1)*sin(radlat2)-sin(radlat1)*cos(radlat2)*cos(delta_lon);
  top=sqrt(top1*top1+top2*top2);
  bottom=sin(radlat1)*sin(radlat2)+cos(radlat1)*cos(radlat2)*cos(delta_lon);
  delta_sigma=atan2(top,bottom);
  distance=delta_sigma*6378137.0;
  return (distance);
  
}

#计算弧度
rad<-function(d){
  
  return (d*pi/180);
  
}

#time_threshold 位两相邻轨迹点之间的时间间隔，单位为天，默认值为3天 72 小时。
#返回值为加入了tripid的lines
segTra<-function(l,time_threshold=3){
  setkey(l,lid)
  bl=l[timespan/3600/24>=time_threshold,list(lid,time1,time2,timespan)]
  lids=bl$lid
  if(nrow(bl)>1){
     l[,tripid:=0]#分割的line的tripid=0
  for(i in (1:(nrow(bl)-1))){
    
    l[(lid>lids[i])&(lid<lids[i+1]),tripid:=(i+1)]
    
  }
  #第一个和最后一个tripid
  l[(lid<lids[1]),tripid:=1]
  l[(lid>lids[nrow(bl)]),tripid:=(nrow(bl)+1)]
    
  }else if(nrow(bl)==1){
    
    l[,tripid:=0]#分割的line的tripid=0
    #第一个和最后一个tripid
    l[(lid<lids[1]),tripid:=1]
    l[(lid>lids[nrow(bl)]),tripid:=(nrow(bl)+1)]
    
    
  }else{
    
    l[,tripid:=1]
  }
 
  
  return(l)
  
}

getships<-function(shipfile){
  
  #ships=fread('D:/Rprojects/ships/ships.txt',sep=',') # build year should be include
  ships=fread(shipfile,sep=',')
  ships=ships[!is.na(mmsi)]
  # ships$mmsi<-as.character(ships$mmsi)
  setkey(ships,mmsi)
  return (ships)
  
}

#识别缺失轨迹航段。条件1：



