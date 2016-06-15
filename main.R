
library('data.table')
library('dplyr')
library('sp')
library('dbscan')
library('ggplot2')
library('ggmap')
library('ggthemes')
# read ships
shipfile='D://share/ships/ships.csv'
ships=getships(shipfile);dim(ships);head(ships);setkey(ships,mmsi)
eFactordt = fread('data/EmissionFactors.txt',sep=' ',header = TRUE)
fcFactordt = fread('data/FuelCorrectionFactors.csv',sep=',',header = TRUE)
#提取在区域内的轨迹点
filenames=fread(input = 'D://share/Git/Rprojects/ECA/filename',header = TRUE)
dt=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0)[mmsi<0]
for (filename in filenames$name){
  
  temp=fread(input = paste('D://share/AIS/containers/',filename,sep = ''))[,list(mmsi,time,status,sog,lon,lat)]
  dt=rbind(dt,temp)
  
}
setkey(dt,mmsi,time)
polygon.points=fread(input ='D://share/Git/Rprojects/ECA/polygon' )
idx.array=point.in.polygon(dt$lon,dt$lat,polygon.points$x,polygon.points$y)
points=cbind(dt,idx.array)[idx.array>0,]
points=points[sog<260&sog>=0,]#删掉航速大于26节的轨迹点
scale=25
gridPoints=setPoints(points,scale)
# 数据处理：segment trajectory，在后边添加tripid,其中tripid==0表示为分割segment

mmsis=gridPoints[,.N,mmsi][N>100,]
n=nrow(mmsis)
l=data.table(lid=0,mmsi=0,pid1=0,pid2=0,timespan=0,distance=0,avgspeed1=0,avgspeed2=0,avgspeed=0,tripid=0)[mmsi<0]
for(i in (1:n)){
  if(i%%100==0){
    print(i)
  }
  p2=gridPoints[mmsi==mmsis$mmsi[i],];
  l1=setLines(p2);
  l1=addLineSpeed(l1);
  l1=l1[avgspeed<=250,]#航速不能超过25节
  #轨迹分段
  l1=segTra(l1)
  l1=l1[,list(lid,mmsi,pid1,pid2,timespan,distance,avgspeed1,avgspeed2,avgspeed,tripid)]
  l=rbind(l,l1)
}

setkey(l,mmsi,lid)

#----排放及其空间分布计算
em1=data.table(mmsi=0,speedid=0,segments=0,duration=0,mode=0,meCO2=0,mePM2.5=0,meSOx=0,meNOx=0,
             aePM2.5=0,aeNOx=0,aeSOx=0,aeCO2=0,boPM2.5=0,boNOx=0,boSOx=0,boCO2=0)[mmsi<0]
for(i in (1:n)){
  if(i%%10==0){
    print(i)
  }
  shipmmsi=mmsis$mmsi[i]
  ship=ships[mmsi==shipmmsi,]
  lines=l[mmsi==shipmmsi&tripid>0,]#不包括轨迹线段在排放控制区意外的点
  mBaseEF=eFactordt[Engine=='Main'&Sulfur=='2.7'&EngineType=='MSD'&IMOTier=='Tier1',list(CO2,PM2.5,SOx,NOx)]
  auxEF=eFactordt[Engine=='Aux'&Sulfur=='0.5'&EngineType=='MSD'&IMOTier=='Tier1',list(CO2,PM2.5,SOx,NOx)]
  boiEF=eFactordt[Engine=='Boiler'&Sulfur=='0.5'&EngineType=='Steamship',list(CO2,PM2.5,SOx,NOx)]
  em=shipEmission(ship,lines,mBaseEF,auxEF,boiEF)
  em=em[,mmsi:=shipmmsi]
  em2=em[,list(mmsi,speedid,segments,duration,mode,meCO2,mePM2.5,meSOx,meNOx,
               aePM2.5,aeNOx,aeSOx,aeCO2,boPM2.5,boNOx,boSOx,boCO2)]
  em1=rbind(em1,em2)
 
}
  
#其中的idx是表示该网格占对应船舶的能耗的比例
sproxy=data.table(mmsi=0,gid=0,g.lon=0,g.lat=0,idx=0)[mmsi<0]
for(i in (1:n)){
  if(i%%100==0){
    print(i)
  }
  shipmmsi=mmsis$mmsi[i]
  ship=ships[mmsi==shipmmsi,]
  gpoints=gridPoints[mmsi==shipmmsi,]
  proxy=shipProxy(ship,gpoints)
  proxy=proxy[,mmsi:=shipmmsi]
  sproxy=rbind(sproxy,proxy)
}

ge=data.table(mmsi=0,gid=0,g.lon=0,g.lat=0,idx=0,CO2=0,PM2.5=0,SOx=0,NOx=0)[mmsi<0,]

for(i in (1:n)){
  if(i%%100==0){
    print(i)
  }
  shipmmsi=mmsis$mmsi[i]
  shipe=em1[mmsi==shipmmsi,]
  proxyship=sproxy[mmsi==shipmmsi,]
  totalEmission=shipe[,list(totalCO2=sum(meCO2+aeCO2+boCO2),totalPM2.5=sum(mePM2.5+aePM2.5+boPM2.5),
                          totalSOx=sum(meSOx+aeSOx+boSOx),totalNOx=sum(meNOx+aeNOx+boNOx))]
  e.grid=proxyship[,list(gid,g.lon,g.lat,idx,CO2=idx*totalEmission$totalCO2,PM2.5=idx*totalEmission$totalPM2.5,
                     SOx=idx*totalEmission$totalSOx,NOx=idx*totalEmission$totalNOx)]
  
  e.grid=e.grid[,mmsi:=shipmmsi]
  ge=rbind(ge,e.grid)
}

ge.total=ge[,list(CO2=sum(CO2),PM2.5=sum(PM2.5),SOx=sum(SOx),NOx=sum(NOx)),list(gid,g.lon,g.lat)]

plotGrid(e.grid)

# #--------计算排放:利用每个航速所用的时间来计算，而不是针对每个航段 --------
# shipmmsi=mmsis[1]
# ship=ships[mmsi==shipmmsi]
# sSpeed=ship$speed*10#service speed
# pw=ship$powerkw
# MCR=round(pw/0.9)
# DWT=ship$dwt
# #计算船舶在每种航速下的能耗
# em=l[tripid>0,list(.N,duration=sum(timespan)),list(speed=round(avgspeed))]
# em[,load.main:=round((speed*0.94/sSpeed)^3,2)]#load.main=main engine load factor
# plot(em$load.main)
# #operation modes:1 at berth, 2 anchored, 3 manoeuvering, 4 slow-steaming, 5 normal cruising
# #imo 2014,p122
# em[,mode:=0]
# em[speed<10,mode:=1]
# em[speed>=1&speed<=30,mode:=2]
# em[speed>30&load.main<0.2,mode:=3]
# em[load.main>=0.2&load.main<=0.65,mode:=4]
# em[load.main>0.65,mode:=5]
# em[mode==3&load.main<0.02,load.main:=0.02]
# #e[mode==3&load.main*100<19.5&load.main*100>1.5,load.main:=0.2]
# 
# em[,loadId:=100*load.main] # to join with low load factor table
# em[load.main>0.195|load.main<0.015,loadId:=20]#only load with in (0.02,0.2) need adject
# 
# #----------------calculate emission factors------------------
# 
# llaFactordt[,loadId:=Load]
# setkey(llaFactordt,loadId)
# setkey(em,loadId)
# em=data.table(left_join(em,llaFactordt[,list(loadId,CO2,PM2.5,SOx,NOx)],by='loadId'))
# setnames(em,c('loadId','speedid', 'segments','duration','load.main','mode','llaCO2','llaPM2.5','llaSOx','llaNOx'))
# 
# #main engine emission:kw*n*g/kwh*n*s/3600/1000/1000: tons
# em[,meCO2:=MCR*load.main*mBaseEF$CO2*llaCO2*duration/3600/1000/1000]
# em[,mePM2.5:=MCR*load.main*mBaseEF$PM2.5*llaPM2.5*duration/3600/1000/1000]
# em[,meSOx:=MCR*load.main*mBaseEF$SOx*llaSOx*duration/3600/1000/1000]
# em[,meNOx:=MCR*load.main*mBaseEF$NOx*llaNOx*duration/3600/1000/1000]
# 
# #-----IMO 2014 中辅机功率没有分SRZ和SEA两种模式，只是提供了一种在海模式的功率-----
# #-----如果要分这两种模式，可以参考port 2009中的处理方式---------------------------
# #------------aux engine-----------
# 
# auxPower=auxPowerdt[ShipClass==ship$type_en&CapacityFrom<DWT&CapacityTo>DWT]
# 
# em[,aePM2.5:=0]
# em[,aeNOx:=0]
# em[,aeSOx:=0]
# em[,aeCO2:=0]
# 
# em[mode==1,aePM2.5:=auxPower$Berth*auxEF$PM2.5*duration/3600/1000/1000]
# em[mode==1,aeNOx:=auxPower$Berth*auxEF$NOx*duration/3600/1000/1000]
# em[mode==1,aeSOx:=auxPower$Berth*auxEF$SOx*duration/3600/1000/1000]
# em[mode==1,aeCO2:=auxPower$Berth*auxEF$CO2*duration/3600/1000/1000]
# 
# em[mode==2,aePM2.5:=auxPower$Anchorage*auxEF$PM2.5*duration/3600/1000/1000]
# em[mode==2,aeNOx:=auxPower$Anchorage*auxEF$NOx*duration/3600/1000/1000]
# em[mode==2,aeSOx:=auxPower$Anchorage*auxEF$SOx*duration/3600/1000/1000]
# em[mode==2,aeCO2:=auxPower$Anchorage*auxEF$CO2*duration/3600/1000/1000]
# 
# em[mode==3,aePM2.5:=auxPower$Maneuvering*auxEF$PM2.5*duration/3600/1000/1000]
# em[mode==3,aeNOx:=auxPower$Maneuvering*auxEF$NOx*duration/3600/1000/1000]
# em[mode==3,aeSOx:=auxPower$Maneuvering*auxEF$SOx*duration/3600/1000/1000]
# em[mode==3,aeCO2:=auxPower$Maneuvering*auxEF$CO2*duration/3600/1000/1000]
# 
# em[mode==5,aePM2.5:=auxPower$Sea*auxEF$PM2.5*duration/3600/1000/1000]
# em[mode==5,aeNOx:=auxPower$Sea*auxEF$NOx*duration/3600/1000/1000]
# em[mode==5,aeSOx:=auxPower$Sea*auxEF$SOx*duration/3600/1000/1000]
# em[mode==5,aeCO2:=auxPower$Sea*auxEF$CO2*duration/3600/1000/1000]
# 
# em[mode==4,aePM2.5:=auxPower$Sea*auxEF$PM2.5*duration/3600/1000/1000]
# em[mode==4,aeNOx:=auxPower$Sea*auxEF$NOx*duration/3600/1000/1000]
# em[mode==4,aeSOx:=auxPower$Sea*auxEF$SOx*duration/3600/1000/1000]
# em[mode==4,aeCO2:=auxPower$Sea*auxEF$CO2*duration/3600/1000/1000]
# 
# #------------boiler engine-----------
# 
# boiPower=boiPowerdt[ShipClass==ship$type_en&CapacityFrom<DWT&CapacityTo>DWT]
# 
# em[,boPM2.5:=0]
# em[,boNOx:=0]
# em[,boSOx:=0]
# em[,boCO2:=0]
# 
# em[mode==1,boPM2.5:=boiPower$Berth*boiEF$PM2.5*duration/3600/1000/1000]
# em[mode==1,boNOx:=boiPower$Berth*boiEF$NOx*duration/3600/1000/1000]
# em[mode==1,boSOx:=boiPower$Berth*boiEF$SOx*duration/3600/1000/1000]
# em[mode==1,boCO2:=boiPower$Berth*boiEF$CO2*duration/3600/1000/1000]
# 
# em[mode==2,boPM2.5:=boiPower$Anchorage*boiEF$PM2.5*duration/3600/1000/1000]
# em[mode==2,boNOx:=boiPower$Anchorage*boiEF$NOx*duration/3600/1000/1000]
# em[mode==2,boSOx:=boiPower$Anchorage*boiEF$SOx*duration/3600/1000/1000]
# em[mode==2,boCO2:=boiPower$Anchorage*boiEF$CO2*duration/3600/1000/1000]
# 
# em[mode==3,boPM2.5:=boiPower$Maneuvering*boiEF$PM2.5*duration/3600/1000/1000]
# em[mode==3,boNOx:=boiPower$Maneuvering*boiEF$NOx*duration/3600/1000/1000]
# em[mode==3,boSOx:=boiPower$Maneuvering*boiEF$SOx*duration/3600/1000/1000]
# em[mode==3,boCO2:=boiPower$Maneuvering*boiEF$CO2*duration/3600/1000/1000]
# 
# em[mode==5,boPM2.5:=boiPower$Sea*boiEF$PM2.5*duration/3600/1000/1000]
# em[mode==5,boNOx:=boiPower$Sea*boiEF$NOx*duration/3600/1000/1000]
# em[mode==5,boSOx:=boiPower$Sea*boiEF$SOx*duration/3600/1000/1000]
# em[mode==5,boCO2:=boiPower$Sea*boiEF$CO2*duration/3600/1000/1000]
# 
# em[mode==4,boPM2.5:=boiPower$Sea*boiEF$PM2.5*duration/3600/1000/1000]
# em[mode==4,boNOx:=boiPower$Sea*boiEF$NOx*duration/3600/1000/1000]
# em[mode==4,boSOx:=boiPower$Sea*boiEF$SOx*duration/3600/1000/1000]
# em[mode==4,boCO2:=boiPower$Sea*boiEF$CO2*duration/3600/1000/1000]
# setkey(em,speedid)
# 
# totalEmission=em[,list(totalCO2=sum(meCO2+aeCO2+boCO2),totalPM2.5=sum(mePM2.5+aePM2.5+boPM2.5),
#                        totalSOx=sum(meSOx+aeSOx+boSOx),totalNOx=sum(meNOx+aeNOx+boNOx))]
# #----另一种计算方式网格分配的方式：利用点的位置以及功率等-----
# #----每个点的主辅机功率，主机功率为3次方，辅机和锅炉功率可以航行模式查表确定------
# dt2=p#其中p为一条船舶的所有点
# dt2[,mp:=0]
# dt2[,ap:=0]
# dt2[,bp:=0]
# dt2[,mp:=round((sog*0.94/sSpeed)^3*MCR)]
# #set ship status: 1 for berth,2for anchor,3for maneuvering,4for lowCruise,5for highCruise
# dt2[,mode:=0]
# dt2[sog<10,mode:=1]
# dt2[sog>=10&sog<=30,mode:=2]
# dt2[sog>30&load.main<0.2,mode:=3]
# dt2[load.main>=0.2&load.main<=0.65,mode:=4]
# dt2[load.main>0.65,mode:=5]
# 
# dt2[mode==1,ap:=auxPower$Berth]
# dt2[mode==2,ap:=auxPower$Anchorage]
# dt2[mode==3,ap:=auxPower$Maneuvering ]
# dt2[mode==4,ap:=auxPower$Sea]
# dt2[mode==5,ap:=auxPower$Sea]
# 
# dt2[mode==1,bp:=boiPower$Berth]
# dt2[mode==2,bp:=boiPower$Anchorage]
# dt2[mode==3,bp:=boiPower$Maneuvering ]
# dt2[mode==4,bp:=boiPower$Sea]
# dt2[mode==5,bp:=boiPower$Sea]
# 
# dt2[,tp:=(mp+ap+bp)]
# proxy=dt2[,list(idx=sum(tp)/sum(dt2$tp)),list(gid,g.lon,g.lat)]
# e.grid=proxy[,list(gid,g.lon,g.lat,idx,CO2=idx*totalEmission$totalCO2,PM2.5=idx*totalEmission$totalPM2.5,
#                    SOx=idx*totalEmission$totalSOx,NOx=idx*totalEmission$totalNOx)]
# #每个网格乘以总排放

#聚类发现集装箱码头，并画出泊位位置，如有岸电等情况
#p0航速为0的点，其中有可能在锚地或者泊位，再视觉确认
p0=points[sog==0&status==5,];dim(p0)
p01=setPoints(p0,1000)
p.grids=p01[,list(.N,lon=mean(lon),lat=mean(lat)),list(gid,g.lon,g.lat)];dim(p.grids)
plot(p.grids$lon,p.grids$lat)

p=getMap(p.grids,6)
p=p+geom_point(data=p.grids,aes(x=lon,y=lat))
p

write.csv(p.grids,file = 'zerogrids.csv')

write.csv(ge.total,file = 'ge.total.csv')
#缺失轨迹
missLine=l[,list(lid,tripid,sog1,sog2,avgspeed1,avgspeed2,avgspeed,timespan,distance)][distance>=2*1852&tripid>0]
dim(missLine)

