
#多个处理器，多线程技术

library('doParallel')
library('foreach')
library('data.table')
library('dplyr')
library('sp')
library('dbscan')
library('ggplot2')
library('ggmap')
library('ggthemes')
#---------basic data input-------------
# read ships
shipfile='D://share/ships/ships.csv'
ships=getships(shipfile);dim(ships);head(ships);setkey(ships,mmsi)
#container with all important field is not NAN.
containers=ships[!is.na(speed)&!is.na(powerkw)&!is.na(dwt)&!is.na(mmsi)&speed>0&type_en=='Container']
eFactordt = fread('data/EmissionFactors.txt',sep=' ',header = TRUE)
fcFactordt = fread('data/FuelCorrectionFactors.csv',sep=',',header = TRUE)

#-----提取在区域内的轨迹点-------------
cl=makeCluster(12)#利用12个处理器
registerDoParallel(cl)
getDoParWorkers()
dt=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0)[mmsi<0]
filenames=fread(input = 'D://share/Git/Rprojects/ECA/filename',header = TRUE)#博懋东部沿海集装箱船数据
system.time(dt<-foreach(i=1:nrow(filenames),.combine='rbind',.packages=c('data.table')) %dopar% { 
  fread(input = paste('D://share/AIS/containers/',filenames[i]$name,sep = ''))[,list(mmsi,time,status,sog,lon,lat)]
})

ships=inner_join(dt[,.N,mmsi],ships[!is.na(speed)&!is.na(powerkw)&!is.na(dwt)],'mmsi')#确保有AIS数据的船舶都有完整数据
setkey(dt,mmsi,time)

#排放控制区中的点
polygon.points=fread(input ='D://share/Git/Rprojects/ECA/polygon' )
idx.array=point.in.polygon(dt$lon,dt$lat,polygon.points$x,polygon.points$y)
points=cbind(dt,idx.array)[idx.array>0,list(mmsi,time,status,sog,lon,lat)]
#中国东部沿海
#points=cbind(dt,idx.array)

points=data.table(inner_join(points,ships[!is.na(speed)&!is.na(powerkw)&!is.na(dwt),list(mmsi)],'mmsi'))#保证所有AIS点对应船舶都有船舶技术数据
points=points[sog>=0,]#航速不小于0

#-----------get points0--------------------------
points0=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0)[mmsi<0]
mmsis=points[,.N,mmsi]$mmsi

system.time(points0<-foreach(i=(1:length(mmsis)),.combine='rbind',.packages=c('data.table')) %dopar% {
 
  p1=points[mmsi==mmsis[i],list(mmsi,time,status,sog,lon,lat)];
  p1=p1[sog>=0&sog<1.5*10*ships[mmsi==p1[1]$mmsi]$speed]
  
})

scale=100
points0=setPoints(points0,scale)
setkey(points0,mmsi,time)

#----------trip 分割 每船 72小时----------------
#gridPoints=setPoints(points,scale)
# 数据处理：segment trajectory，在后边添加tripid,其中tripid==0表示为分割segment
stopCluster(cl)#释放内存
cl=makeCluster(12)#利用12个处理器
registerDoParallel(cl)
getDoParWorkers()

gridPoints=points0
mmsis=gridPoints[,.N,mmsi][N>1000,]#这里要处理轨迹点极度缺少的问题
n=nrow(mmsis)
l=data.table(lid=0,mmsi=0,pid1=0,pid2=0,timespan=0,distance=0,avgspeed1=0,avgspeed2=0,avgspeed=0,tripid=0)[mmsi<0]

system.time(l<-foreach(i=(1:n),.combine='rbind',.packages=c('data.table','dplyr','sp','dbscan')) %dopar% {
  
  p1=points0[mmsi==mmsis$mmsi[i],];
  l1=setLines(p1);
  l1=addLineSpeed(l1);
  #轨迹分段
  l1=addTrip(l1)
  l1=l1[,list(lid,mmsi,pid1,pid2,timespan,distance,avgspeed1,avgspeed2,avgspeed,tripid)]
  
})

setkey(l,mmsi,lid)

#-------get clusters and segments--------

#针对每条船舶每个tripid 进行segment 分割
#points 为总的数据点，去掉了sog小于0的点。
stopCluster(cl)
cl=makeCluster(10)#利用12个处理器
registerDoParallel(cl)
getDoParWorkers()

clusters=detectStayArea(points0,0.03,10,1000,100)
mmsis=l[,.N,mmsi]$mmsi
segments=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0,tripid=0,segid=0,scls=0,ecls=0,cls=0)[mmsi<0]

system.time(segments<-foreach(i=(1:length(mmsis)),.combine='rbind',.packages=c('data.table','dplyr','sp','dbscan')) %dopar% {
  ammsi=mmsis[i]#ammsi=209075000
  shipspeed=ships[mmsi==ammsi]$speed*10
  speedscale=1.3#计算的平均航速不能大于1.3倍设计航速
  shipsegments=segmentOneShip(ammsi,points0,clusters,shipspeed,speedscale)
  shipseg=shipsegments[,list(mmsi,time,status,sog,lon,lat,tripid,segid,scls,ecls,cls)]
})

setkey(segments,mmsi,time)


#-----缺失轨迹插值--------
#-----单船循环处理--------
segments=setPoints(segments,scale);setkey(segments,mmsi,time)
addedPoints=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0,tripid=0,segid=0,scls=0,ecls=0,cls=0)[mmsi<0]
mmsis=segments[,.N,mmsi]$mmsi
system.time(addedpoints<-foreach(j=(1:length(mmsis)),.combine='rbind',.packages=c('data.table','dplyr','sp','dbscan')) %dopar% {
  
  ammsi=mmsis[j]
  ss=segments[mmsi==ammsi]
  sl=getSegmentLines(ss)
  missLines=sl[distance>5*1852]#所有船舶
  #端点平均航速与距离时间平均航速在5%以内，不用插值
  if(nrow(missLines)>0){
    
  missLines=missLines[abs(avgspeed1-avgspeed2)/avgspeed1>0.05]
  # get points from ships with similar dwt and same type
  shipdwt=ships[mmsi==missLines[1]$mmsi]$dwt#第一艘船
  refmmsis=ships[dwt>=0.9*shipdwt&dwt<=1.1*shipdwt]$mmsi
  refpoints=segments[mmsi%in%refmmsis]
  
  for(i in (1:nrow(missLines))){
    print(i)
    
    ln=missLines[i,] 
    #不包括ln所在segment
    refp=getRefPoints(ln,refpoints,r=3,samedirection=1,scale=100)
    if(nrow(refp)>0){
      #---利用每个seg的航行距离,剥离不合理的seg----
      refp2=refineRefpoints(refp,epsscale=0.1,minpnt=3)
      #-----加入缺失点------
      addp=addMissPoints(ln,refp2,100,2)
      # addedPoints=rbind(addedPoints,addp)
      
    }else{
      
      addp==data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0,tripid=0,segid=0,scls=0,ecls=0,cls=0)[mmsi<0]
      
    }
  } 
  }
  
})



stopCluster(cl)

