

#default scale is 10
setPoints <- function(points,scale = 100) {
  n = nrow(points)
  
  points[,pid:= seq(1,n,1)]
  points[,g.lon:= floor(lon * scale) / scale]
  points[,g.lat:= floor(lat * scale) / scale]
  points[,gid:= paste(floor(lon * scale) / scale,floor(lat * scale) / scale,sep =
                         '_')]
  
  #setkey(points,time)
  
  return(points)
  
}

# points:pid,gid,lon,lat,time,...
# just add two points together


setLines <- function(points) {
  n = nrow(points)
  setkey(points,time)
  dt1 = points[1:(n - 1),list(mmsi,status,lon,lat,time,sog,pid,gid,g.lon,g.lat)]
  dt2 = points[2:n,list(lon,lat,time,status,sog,pid,gid,g.lon,g.lat)]
  lines = cbind(dt1,dt2)
  setnames(
    lines,c('mmsi','status1','lon1','lat1','time1','sog1','pid1','gid1','g.lon1','g.lat1','lon2','lat2','time2','status2','sog2','pid2','gid2','g.lon2','g.lat2'
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
  lines[(distance / 1852 > dist_threshold) &
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

addTrip <- function(l,time_threshold = 3) {
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

#emission of a ship

shipEmission<-function(ship,lines,mBaseEF,auxEF,boiEF){
  auxPowerdt = fread('data/auxpw.csv',sep=',',header = TRUE)
  boiPowerdt = fread('data/boilerpw.csv',sep=',',header = TRUE)
  #low load adjustment multipler
  llaFactordt = fread('data/LowLoadAdjustmentFactors.csv',sep=',',header = TRUE)
  sSpeed=ship$speed*10#service speed
  pw=ship$powerkw
  MCR=round(pw/0.9)
  DWT=ship$dwt
  #计算船舶在每种航速下的能耗
  em=lines[,list(.N,duration=sum(timespan)),list(speed=round(avgspeed))]
  em[,load.main:=round((speed*0.94/sSpeed)^3,2)]#load.main=main engine load factor
  # plot(em$load.main)
  #operation modes:1 at berth, 2 anchored, 3 manoeuvering, 4 slow-steaming, 5 normal cruising
  #imo 2014,p122
  em[,mode:=0]
  em[speed<10,mode:=1]
  em[speed>=1&speed<=30,mode:=2]
  em[speed>30&load.main<0.2,mode:=3]
  em[load.main>=0.2&load.main<=0.65,mode:=4]
  em[load.main>0.65,mode:=5]
  em[mode==3&load.main<0.02,load.main:=0.02]
  #e[mode==3&load.main*100<19.5&load.main*100>1.5,load.main:=0.2]
  
  em[,loadId:=100*load.main] # to join with low load factor table
  em[load.main>0.195|load.main<0.015,loadId:=20]#only load with in (0.02,0.2) need adject
  
  #----------------calculate emission factors------------------
  
  llaFactordt[,loadId:=Load]
  setkey(llaFactordt,loadId)
  setkey(em,loadId)
  em=data.table(left_join(em,llaFactordt[,list(loadId,CO2,PM2.5,SOx,NOx)],by='loadId'))
  setnames(em,c('loadId','speedid', 'segments','duration','load.main','mode','llaCO2','llaPM2.5','llaSOx','llaNOx'))
  
  #main engine emission:kw*n*g/kwh*n*s/3600/1000/1000: tons
  em[,meCO2:=MCR*load.main*mBaseEF$CO2*llaCO2*duration/3600/1000/1000]
  em[,mePM2.5:=MCR*load.main*mBaseEF$PM2.5*llaPM2.5*duration/3600/1000/1000]
  em[,meSOx:=MCR*load.main*mBaseEF$SOx*llaSOx*duration/3600/1000/1000]
  em[,meNOx:=MCR*load.main*mBaseEF$NOx*llaNOx*duration/3600/1000/1000]
  
  #-----IMO 2014 中辅机功率没有分SRZ和SEA两种模式，只是提供了一种在海模式的功率-----
  #-----如果要分这两种模式，可以参考port 2009中的处理方式---------------------------
  #------------aux engine-----------
  
  auxPower=auxPowerdt[ShipClass==ship$type_en&CapacityFrom<=DWT&CapacityTo>=DWT]
  
  em[,aePM2.5:=0]
  em[,aeNOx:=0]
  em[,aeSOx:=0]
  em[,aeCO2:=0]
  
  em[mode==1,aePM2.5:=auxPower$Berth*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==1,aeNOx:=auxPower$Berth*auxEF$NOx*duration/3600/1000/1000]
  em[mode==1,aeSOx:=auxPower$Berth*auxEF$SOx*duration/3600/1000/1000]
  em[mode==1,aeCO2:=auxPower$Berth*auxEF$CO2*duration/3600/1000/1000]
  
  em[mode==2,aePM2.5:=auxPower$Anchorage*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==2,aeNOx:=auxPower$Anchorage*auxEF$NOx*duration/3600/1000/1000]
  em[mode==2,aeSOx:=auxPower$Anchorage*auxEF$SOx*duration/3600/1000/1000]
  em[mode==2,aeCO2:=auxPower$Anchorage*auxEF$CO2*duration/3600/1000/1000]
  
  em[mode==3,aePM2.5:=auxPower$Maneuvering*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==3,aeNOx:=auxPower$Maneuvering*auxEF$NOx*duration/3600/1000/1000]
  em[mode==3,aeSOx:=auxPower$Maneuvering*auxEF$SOx*duration/3600/1000/1000]
  em[mode==3,aeCO2:=auxPower$Maneuvering*auxEF$CO2*duration/3600/1000/1000]
  
  em[mode==5,aePM2.5:=auxPower$Sea*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==5,aeNOx:=auxPower$Sea*auxEF$NOx*duration/3600/1000/1000]
  em[mode==5,aeSOx:=auxPower$Sea*auxEF$SOx*duration/3600/1000/1000]
  em[mode==5,aeCO2:=auxPower$Sea*auxEF$CO2*duration/3600/1000/1000]
  
  em[mode==4,aePM2.5:=auxPower$Sea*auxEF$PM2.5*duration/3600/1000/1000]
  em[mode==4,aeNOx:=auxPower$Sea*auxEF$NOx*duration/3600/1000/1000]
  em[mode==4,aeSOx:=auxPower$Sea*auxEF$SOx*duration/3600/1000/1000]
  em[mode==4,aeCO2:=auxPower$Sea*auxEF$CO2*duration/3600/1000/1000]
  
  #------------boiler engine-----------
  
  boiPower=boiPowerdt[ShipClass==ship$type_en&CapacityFrom<=DWT&CapacityTo>=DWT]
  
  em[,boPM2.5:=0]
  em[,boNOx:=0]
  em[,boSOx:=0]
  em[,boCO2:=0]
  
  em[mode==1,boPM2.5:=boiPower$Berth*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==1,boNOx:=boiPower$Berth*boiEF$NOx*duration/3600/1000/1000]
  em[mode==1,boSOx:=boiPower$Berth*boiEF$SOx*duration/3600/1000/1000]
  em[mode==1,boCO2:=boiPower$Berth*boiEF$CO2*duration/3600/1000/1000]
  
  em[mode==2,boPM2.5:=boiPower$Anchorage*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==2,boNOx:=boiPower$Anchorage*boiEF$NOx*duration/3600/1000/1000]
  em[mode==2,boSOx:=boiPower$Anchorage*boiEF$SOx*duration/3600/1000/1000]
  em[mode==2,boCO2:=boiPower$Anchorage*boiEF$CO2*duration/3600/1000/1000]
  
  em[mode==3,boPM2.5:=boiPower$Maneuvering*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==3,boNOx:=boiPower$Maneuvering*boiEF$NOx*duration/3600/1000/1000]
  em[mode==3,boSOx:=boiPower$Maneuvering*boiEF$SOx*duration/3600/1000/1000]
  em[mode==3,boCO2:=boiPower$Maneuvering*boiEF$CO2*duration/3600/1000/1000]
  
  em[mode==5,boPM2.5:=boiPower$Sea*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==5,boNOx:=boiPower$Sea*boiEF$NOx*duration/3600/1000/1000]
  em[mode==5,boSOx:=boiPower$Sea*boiEF$SOx*duration/3600/1000/1000]
  em[mode==5,boCO2:=boiPower$Sea*boiEF$CO2*duration/3600/1000/1000]
  
  em[mode==4,boPM2.5:=boiPower$Sea*boiEF$PM2.5*duration/3600/1000/1000]
  em[mode==4,boNOx:=boiPower$Sea*boiEF$NOx*duration/3600/1000/1000]
  em[mode==4,boSOx:=boiPower$Sea*boiEF$SOx*duration/3600/1000/1000]
  em[mode==4,boCO2:=boiPower$Sea*boiEF$CO2*duration/3600/1000/1000]
  em=em[,mmsi:=ship$mmsi]
  setkey(em,speedid)
  return(em)
  
}

shipProxy<-function(ship,points){
  
  sSpeed=ship$speed*10#service speed
  pw=ship$powerkw
  MCR=round(pw/0.9)
  DWT=ship$dwt
  auxPowerdt = fread('data/auxpw.csv',sep=',',header = TRUE)
  boiPowerdt = fread('data/boilerpw.csv',sep=',',header = TRUE)
  auxPower=auxPowerdt[ShipClass==ship$type_en&CapacityFrom<=DWT&CapacityTo>=DWT]
  boiPower=boiPowerdt[ShipClass==ship$type_en&CapacityFrom<=DWT&CapacityTo>=DWT]
  dt2=points#points为已经网格化的轨迹点
  
  dt2[,mp:=0]
  dt2[,ap:=0]
  dt2[,bp:=0]
  dt2[,load.main:=round((sog*0.94/sSpeed)^3,2)]
  dt2[,mp:=round((sog*0.94/sSpeed)^3*MCR)]
  #set ship status: 1 for berth,2for anchor,3for maneuvering,4for lowCruise,5for highCruise
  dt2[,mode:=0]
  dt2[sog<10,mode:=1]
  dt2[sog>=10&sog<=30,mode:=2]
  dt2[sog>30&load.main<0.2,mode:=3]
  dt2[load.main>=0.2&load.main<=0.65,mode:=4]
  dt2[load.main>0.65,mode:=5]
  
  dt2[mode==1,ap:=auxPower$Berth]
  dt2[mode==2,ap:=auxPower$Anchorage]
  dt2[mode==3,ap:=auxPower$Maneuvering ]
  dt2[mode==4,ap:=auxPower$Sea]
  dt2[mode==5,ap:=auxPower$Sea]
  
  dt2[mode==1,bp:=boiPower$Berth]
  dt2[mode==2,bp:=boiPower$Anchorage]
  dt2[mode==3,bp:=boiPower$Maneuvering ]
  dt2[mode==4,bp:=boiPower$Sea]
  dt2[mode==5,bp:=boiPower$Sea]
  
  dt2[mode==3&load.main<0.02,load.main:=0.02]
  dt2[,tp:=(mp+ap+bp)]
  proxy=dt2[,list(idx=sum(tp)/sum(dt2$tp)),list(gid,g.lon,g.lat)]
  return(proxy)
 
}

getCls<-function(dt,eps,minpnts){
  m1=dt[sog1==0&status1==5,list(lon1,lat1)]
  cls=dbscan(m1,eps,minpnts)
  dtcls=cbind(dt[sog1==0&status1==5],cls[[1]])
  return (dtcls)
  
}


#------break a trip into segments-------------
#ammsi该船舶mmsi号
#points0去掉了sog<0和大于1.5倍设计航速的轨迹点全集
#speedscale=1.3#计算的平均航速不能大于1.3倍设计航速
#shipspeed,船舶设计航速或者服务航速
segmentOneShip<-function(ammsi,points0,clusters,shipspeed,speedscale){
  
  shipp=points0[mmsi==ammsi]#points0已去掉了sog>1.5speed 的轨迹点
  aship=l[mmsi==ammsi]
  aship0=data.table(left_join(aship,points0[,list(time1=time,status1=status,sog1=sog,lon1=lon,lat1=lat,pid1=pid,gid1=gid)],'pid1'))
  aship0=data.table(left_join(aship0,points0[,list(time2=time,status2=status,sog2=sog,lon2=lon,lat2=lat,pid2=pid,gid2=gid)],'pid2'))
  
  #--------去掉每个trip的不合理轨迹点，并根据停留区域分割轨迹------
  
  tripids=aship0[tripid>0,.N,tripid]$tripid#tripid=0时为trip分割航段
  shipsegments=data.table(gid=0,mmsi=0,time=0,status=0,sog=0,lon=0,lat=0,tripid=0,cls=0,segid=0,scls=0,ecls=0)[mmsi<0]
  for(id in tripids){
    print(id)
    atrip=aship0[tripid==id]
    #去掉不合理平均航速的轨迹点
    atrip=delectBigAvgSpeed(shipp,shipspeed,speedscale,atrip)
    tripp=atrip[,list(mmsi,time=time1,status=status1,sog=sog1,lon=lon1,lat=lat1,gid=gid1,tripid)]
    tripp=rbind(tripp,atrip[nrow(atrip),list(mmsi,time=time2,status=status2,sog=sog2,lon=lon2,lat=lat2,gid=gid2,tripid)])
    tripseg=addSegment(tripp,clusters)
    shipsegments=rbind(shipsegments,tripseg)
  }
  
  
  return(shipsegments)
  
}

#tripp:mmsi,time,lon,lat,sog,gid
#clusters: gid, cls
#result:gid,mmsi,time,status,sog,lon,lat,tripid,cls,segid,scls,ecls


addSegment<-function(tripp,clusters){
  tripc=data.table(left_join(tripp,clusters[,list(gid,cls)],'gid'))
  #虽然在停留区，但是如果航速大于0，也不算在停留，
  #防止有些船经过某个停留区而被误认为是停留区的点
  tripc=tripc[is.na(cls),cls:=0][cls>0&sog>0,cls:=0]
  setkey(tripc,mmsi,time)
  tripc[,segid:=0]
  tripc[,scls:=0]
  tripc[,ecls:=0]
  tmpc=unique(c(1,tripc[cls!=0,which=TRUE],nrow(tripc)))#这样对两个端点有点影响，但是基本可以忽略不计
  np=length(tmpc)
  #seg=0代表点为端点或者为停留点
  for(i in (1:(np-1))){
    if((tmpc[i+1]-tmpc[i])>1){
      tripc[tmpc[i]:tmpc[i+1],segid:=i]
      tripc[tmpc[i]:tmpc[i+1],scls:=tripc[tmpc[i]]$cls]
      tripc[tmpc[i]:tmpc[i+1],ecls:=tripc[tmpc[i+1]]$cls]
    }
  }
  
  tripc[tmpc,segid:=0]#所有端点和停留点segid都为0
  #所有scls与ecls相同的，其seg也应该为0
  tripc[scls==ecls,segid:=0]
  return(tripc)
  
}
#only for one ship
#segment with gid generated by setpoints()
getSegmentLines<-function(segments){
  
  tripids=segments[tripid>0,.N,tripid]$tripid
  sl0=data.table(lid=0,mmsi=0,pid1=0,pid2=0,timespan=0,distance=0,avgspeed1=0,avgspeed2=0,avgspeed=0)[mmsi<0]
  for(id in tripids){
    
    temp=setLines(segments[tripid==id])#segment lines
    temp=addLineSpeed(temp)
    sl0=rbind(sl0,temp[,list(lid,mmsi,pid1,pid2,timespan,distance,avgspeed1,avgspeed2,avgspeed)])
    
  }
  #用第一个点代表线段的属性
  sl=data.table(left_join(sl0,segments[,list(pid1=pid,time1=time,status1=status,sog1=sog,lon1=lon,lat1=lat,tripid1=tripid,
                                             segid1=segid,scls1=scls,ecls1=ecls,cls1=cls,gid1=gid,g.lon1=g.lon,g.lat1=g.lat)]),'pid1')[,V2:=NULL]
  sl=data.table(left_join(sl,segments[,list(pid2=pid,time2=time,status2=status,sog2=sog,lon2=lon,lat2=lat,tripid2=tripid,
                                            segid2=segid,scls2=scls,ecls2=ecls,cls2=cls,gid2=gid,g.lon2=g.lon,g.lat2=g.lat)]),'pid2')[,V2:=NULL]
  setkey(sl,mmsi,tripid1,segid1,time1)
  
  return(sl)
  
  
}

#points:mmsi,time,status,sog,lon,lat,g.lon,g.lat,gid
detectStayArea<-function(points,eps=0.03,minpnt=10,clusterscale=1000,gridscale=100){
  
  cp1=points[sog==0&status%in%c(5)];#1为锚泊，5为锚链系泊
  cp2=setPoints(cp1,scale=clusterscale);
  cp3=cp2[,list(.N,lon=mean(g.lon),lat=mean(g.lat)),gid]
  m1=cp3[,list(lon,lat)]
  cls=dbscan(m1,eps,minpnt)
  cp5=cbind(cp3,cls[[1]])
  
  cpoints=setPoints(cp5[V2>0,list(lon,lat,cls=V2)],gridscale)#网格尺度与点相同都为0.01，目前有306个网格处于停顿状态
  clusters=cpoints[,list(.N),list(gid,cls)]
  
  return(clusters[,list(gid,cls)])
}
#去掉不合格点,第一次去掉直线中的所有点
#第二次如果还有航速不对还去掉这两个端点
#shipp一条船的轨迹点
delectBigAvgSpeed<-function(shipp,shipspeed,speedscale=1.3,atrip){
  tid=atrip[1]$tripid
  while(nrow(atrip[avgspeed>speedscale*shipspeed])>0){
    
    deletep=unique(c(atrip[avgspeed>speedscale*shipspeed,]$pid1,atrip[avgspeed>speedscale*shipspeed,]$pid2))
    tripp=data.table(pid=unique(c(atrip$pid1,atrip$pid2)))
    remainp=tripp[!pid%in%deletep]
    newp=data.table(inner_join(remainp,shipp,'pid'))
    atrip=setLines(newp)
    atrip=addLineSpeed(atrip)
    atrip[,tripid:=tid]
    
  }

  return(atrip)
  
}


#两点间插值
#两个点之间距离较大时插值算法，一般两点距离大于5海里
#r 为 或者网格半径，几个网格
#ln为缺失轨迹航段
#similarshipp：为用于轨迹推测的大数据点，mmsi,time,status,sog,lon,lat,gid,g.lon,g.lat,tripid,segid,scls,ecls,cls

#找一个缺失航段的最初参考轨迹点

getRefPoints<-function(ln,similarshipp,r=3,samedirection=1,scale=100){
  
  refpoints=similarshipp
 
  area1=refpoints[lon>=(ln$g.lon1-r/scale)&lon<=(ln$g.lon1+r/scale)
                  &lat>=(ln$g.lat1-r/scale)&lat<=(ln$g.lat1+r/scale),list(mmsi,time,lon,lat,sog,tripid,segid)][segid>0]
  area2=refpoints[lon>=ln$g.lon2-r/scale&lon<=ln$g.lon2+r/scale
                  &lat>=ln$g.lat2-r/scale&lat<=ln$g.lat2+r/scale,list(mmsi,time,lon,lat,sog,tripid,segid)][segid>0]
  
  t1=area1[,list(time1=median(time)),list(mmsi,tripid,segid)]#各个航次在第一个点的时间
  t2=area2[,list(time2=median(time)),list(mmsi,tripid,segid)]#各个航次在第二个点的时间
  t12=data.table(inner_join(t1,t2,c('mmsi','tripid','segid')))
  if(samedirection){
    t12=t12[time2>time1]#同方向
  }
  
  refp=data.table(inner_join(refpoints,t12,c('mmsi','tripid','segid')))[time<=time2&time>=time1]
  refp=refp[!(mmsi==ln$mmsi&tripid==ln$tripid1&segid==ln$segid1)]#去掉自己的tripid和segid的
  #考虑到停留点问题
  if(ln$sog1>10&ln$sog2>10){
    refp=refp[sog>10]
  }
  return(refp)
}

#refp 为初步refpoints，需要进行一步分析去掉不合理的segid
#这里主要用segid的航行距离dbscan聚类，取成员最多类为参考轨迹
#dbscan中，eps为0.2*mediam（所有seg距离），0.2可以改，
#dbscan中的minpnt为个数，minpnt设置

refineRefpoints<-function(refp,epsscale=0.2,minpnt=5){
  
  #---利用每个seg的航行距离,剥离不合理的seg----
  #聚类算法，只用一个类中seg最多的类为refpoints
  dist=data.table(mmsi=0,tripid=0,segid=0,totaldist=0,totaltime=0)[tripid<0]
  segs=refp[,.N,list(mmsi,tripid,segid)]
  
  for(i in (1:nrow(segs))){
    
    seg=refp[mmsi==segs[i]$mmsi&tripid==segs[i]$tripid&segid==segs[i]$segid]
    setkey(seg,mmsi,time)
    rp=setLines(seg[,list(mmsi,time,status,sog,lon,lat,pid,gid,g.lon,g.lat)])
    rl=addLineSpeed(rp)
    temp=cbind(seg[1,list(mmsi,tripid,segid)],totaldist=sum(rl$distance),totaltime=sum(rl$timespan))
    dist=rbind(dist,temp)
    
  }
  
  #聚类分析得到较合理的参考轨迹点，这里用距离为参数
  mediantime=median(dist$totaltime)
  dist=dist[abs(totaltime-mediantime)/mediantime<=3]
  refcls=dbscan(dist[,totaldist],epsscale*median(dist$totaldist),minpnt)
  dist2=dist[,refcls:=refcls[[1]]]
  top1cls=dist2[,.N,refcls][N==max(N),]$refcls[1]
  refp2=data.table(inner_join(refp,dist2[refcls==top1cls,],c('mmsi','tripid','segid')))
  
 # refp2=refp2[sog>10]
  return(refp2)
  
}

#主要用于内部函数
#missln为轨迹缺失的线段
#gridnum为每个网格插值个数
#scale为网格尺度，一般为100，也就是0.01*0.01


addMissPoints<-function(missln,refpoints,scale=100,gridnum=2){
  ln=missln
  refp2=refpoints
  lonspan=abs(ln$lon2-ln$lon1)
  latspan=abs(ln$lat2-ln$lat1)
  
  if(lonspan>=latspan){
    
    endp1=ln$lon1
    endp2=ln$lon2
    
    maxlon=max(endp1,endp2)
    minlon=min(endp1,endp2)
    
    bins=seq(from=(floor(minlon*scale)+1)/scale,to=(floor(maxlon*scale)/scale),by=1/(scale*gridnum))
    
    len=length(bins)
    #分组标识
    refp2[,bin:=0]
    for(i in (1:(len-1))){
      refp2[lon>=bins[i]&lon<bins[i+1],bin:=i]
    }
    addp=refp2[bin>0,list(lon=median(lon),lat=median(lat),sog=round(median(sog))),bin]
    setkey(addp,bin)
    addp[,bin:=NULL]
    addp[,mmsi:=ln$mmsi]
    addp[,tripid:=ln$tripid1]
    addp[,segid:=ln$segid1]
    addp[,status:=ln$status1]
    addp[,scls:=ln$scls1]
    addp[,ecls:=ln$ecls1]
    addp[,cls:=ln$cls1]
    
    
    #-----计算时间和速度，速度为平均速度，时间根据平均速度和距离计算------
    firstp=ln[,list(lon=lon1,lat=lat1,sog=sog1,mmsi,tripid=tripid1,segid=segid1,
                    status=status1,scls=scls1,ecls=ecls1,cls=cls1)]
    lastp=ln[,list(lon=lon2,lat=lat2,sog=sog2,mmsi,tripid=tripid2,segid=segid2,
                   status=status2,scls=scls2,ecls=ecls2,cls=cls2)]
    
    addp2=rbind(firstp,addp,lastp)
    setkey(addp2,lon)
    temp0=data.table(id=0,dd=0)[id<0]
    
    for(i in (1:(nrow(addp2)-1))){
      
      fp=addp2[i]
      sp=addp2[i+1]
      dd=distance(fp$lon,fp$lat,sp$lon,sp$lat)
      temp=data.table(id=i,dd=dd)
      temp0=rbind(temp0,temp)
      
    }
    
    avgspeed=sum(temp0$dd)*10/1852/(ln$timespan/3600)
    
    for (i in (1:(nrow(addp2)-2))){
      sumd=sum(temp0[1:i,]$dd)
      temp0[i,time:=(ln$time1+(sumd/1852)*10/(avgspeed)*3600)]
    }
    
    addp=cbind(addp[,sog:=round(avgspeed)],time=temp0[1:nrow(addp),]$time)
    
    addp=addp[,list(mmsi,time,status,sog,lon,lat,tripid,segid,scls,ecls,cls)]
    
  }else{
    
    endp1=ln$lat1
    endp2=ln$lat2
    
    maxlat=max(endp1,endp2)
    minlat=min(endp1,endp2)
    
    bins=seq(from=(floor(minlat*scale)+1)/scale,to=(floor(maxlat*scale)/scale),by=(1/(scale*gridnum)))
    
    len=length(bins)
    #分组标识
    refp2[,bin:=0]
    for(i in (1:(len-1))){
      refp2[lat>=bins[i]&lat<bins[i+1],bin:=i]
    }
    addp=refp2[bin>0,list(lon=median(lon),lat=median(lat),sog=round(median(sog))),bin]
    setkey(addp,bin)
    addp[,bin:=NULL]
    addp[,mmsi:=ln$mmsi]
    addp[,tripid:=ln$tripid1]
    addp[,segid:=ln$segid1]
    addp[,status:=ln$status1]
    addp[,scls:=ln$scls1]
    addp[,ecls:=ln$ecls1]
    addp[,cls:=ln$cls1]
    
    #-----计算时间和速度，速度为平均速度，时间根据平均速度和距离计算------
    firstp=ln[,list(lon=lon1,lat=lat1,sog=sog1,mmsi,tripid=tripid1,segid=segid1,
                    status=status1,scls=scls1,ecls=ecls1,cls=cls1)]
    lastp=ln[,list(lon=lon2,lat=lat2,sog=sog2,mmsi,tripid=tripid2,segid=segid2,
                   status=status2,scls=scls2,ecls=ecls2,cls=cls2)]
    
    addp2=rbind(firstp,addp,lastp)
    setkey(addp2,lon)
    temp0=data.table(id=0,dd=0)[id<0]
    
    for(i in (1:(nrow(addp2)-1))){
      
      fp=addp2[i]
      sp=addp2[i+1]
      dd=distance(fp$lon,fp$lat,sp$lon,sp$lat)
      temp=data.table(id=i,dd=dd)
      temp0=rbind(temp0,temp)
      
    }
    
    avgspeed=sum(temp0$dd)*10/1852/(ln$timespan/3600)
    
    for (i in (1:(nrow(addp2)-2))){
      sumd=sum(temp0[1:i,]$dd)
      temp0[i,time:=(ln$time1+(sumd/1852)*10/(avgspeed)*3600)]
    }
    
    addp=cbind(addp[,sog:=round(avgspeed)],time=temp0[1:nrow(addp),]$time)
    addp=addp[,list(mmsi,time,status,sog,lon,lat,tripid,segid,scls,ecls,cls)]
  }
  
  return(addp)
  
}

