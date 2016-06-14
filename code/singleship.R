#calculate the emission of a single trip in ECA

lines=l # the lines of a single ship in ECA

auxPowerdt = fread('data/auxpw.csv',sep=',',header = TRUE)
boiPowerdt = fread('data/boilerpw.csv',sep=',',header = TRUE)
eFactordt = fread('data/EmissionFactors.txt',sep=' ',header = TRUE)
fcFactordt = fread('data/FuelCorrectionFactors.csv',sep=',',header = TRUE)
#low load adjustment multipler
llaFactordt = fread('data/LowLoadAdjustmentFactors.csv',sep=',',header = TRUE)

scale=10 # grid size 

dt=lines[tripid>0,list(lid,timespan,avgspeed)]

#----------------------
mmsi=ship$mmsi
power=ship$powerkw
MCR=power/0.9
sSpeed=ship$speed*10 # 采用服务航速代替设计航速，以考虑主机de-rating 
DWT=ship$dwt
mBaseEF=eFactordt[Engine=='Main'&Sulfur=='2.7'&EngineType=='MSD'&IMOTier=='Tier1',list(CO2,PM2.5,SOx,NOx)]
auxEF=eFactordt[Engine=='Aux'&Sulfur=='0.5'&EngineType=='MSD'&IMOTier=='Tier1',list(CO2,PM2.5,SOx,NOx)]
boiEF=eFactordt[Engine=='Boiler'&Sulfur=='0.5'&EngineType=='Steamship',list(CO2,PM2.5,SOx,NOx)]

cargoLoad=1 # the ship is loaded
weatherFactor=1 # shiping near shore 1.1, or 1.15 if shipping in ocean
hullFactor=1 # resistance caused by hull roughness, more than four years 0.09

dt[,load.main:=round(((avgspeed/(sSpeed/0.95))^3)*cargoLoad^(2/3)*weatherFactor*hullFactor,2)]

#set ship status: 1 for berth,2for anchor,3for maneuvering,4for lowCruise,5for highCruise
dt[,model:=0]
dt[avgspeed<10,model:=1]
dt[avgspeed>=10&avgspeed<=30,model:=2]
dt[avgspeed>30&load.main<0.2,model:=3]
dt[load.main>=0.2&load.main<=0.65,model:=4]
dt[load.main>0.65,model:=5]

# main engine load is not less than 0.02 under maneuvering status
dt[load.main<0.02&model==3,load.main:=0.02]

dt[,loadId:=100*load.main] # to join with low load factor table
dt[load.main>0.195|load.main<0.015,loadId:=20]#only load with in (0.02,0.2) need adject

#----------------calculate emission factors------------------

llaFactordt[,loadId:=Load]
setkey(llaFactordt,loadId)
setkey(dt,loadId)
dt=data.table(left_join(dt,llaFactordt[,list(loadId,CO2,PM2.5,SOx,NOx)],by='loadId'))
setnames(dt,c('loadId','lid', 'timespan','avgspeed','load.main','model','llaCO2','llaPM2.5','llaSOx','llaNOx'))

#main engine emission:kw*n*g/kwh*n*s/3600/1000/1000: tons
dt[,meCO2:=MCR*load.main*mBaseEF$CO2*llaCO2*timespan/3600/1000/1000]
dt[,mePM2.5:=MCR*load.main*mBaseEF$PM2.5*llaPM2.5*timespan/3600/1000/1000]
dt[,meSOx:=MCR*load.main*mBaseEF$SOx*llaSOx*timespan/3600/1000/1000]
dt[,meNOx:=MCR*load.main*mBaseEF$NOx*llaNOx*timespan/3600/1000/1000]

# co2 to oil, 3.18 from (losengle port,2009), 

#------------aux engine-----------

auxPower=auxPowerdt[ShipClass=='Container'&CapacityFrom<DWT&CapacityTo>DWT]

dt[,aePM2.5:=0]
dt[,aeNOx:=0]
dt[,aeSOx:=0]
dt[,aeCO2:=0]

#-----IMO 2014 中辅机功率没有分SRZ和SEA两种模式，只是提供了一种在海模式的功率-----
#-----如果要分这两种模式，可以参考port 2009中的处理方式---------------------------

dt[model==1,aePM2.5:=auxPower$Berth*auxEF$PM2.5*timespan/3600/1000/1000]
dt[model==1,aeNOx:=auxPower$Berth*auxEF$NOx*timespan/3600/1000/1000]
dt[model==1,aeSOx:=auxPower$Berth*auxEF$SOx*timespan/3600/1000/1000]
dt[model==1,aeCO2:=auxPower$Berth*auxEF$CO2*timespan/3600/1000/1000]

dt[model==2,aePM2.5:=auxPower$Anchorage*auxEF$PM2.5*timespan/3600/1000/1000]
dt[model==2,aeNOx:=auxPower$Anchorage*auxEF$NOx*timespan/3600/1000/1000]
dt[model==2,aeSOx:=auxPower$Anchorage*auxEF$SOx*timespan/3600/1000/1000]
dt[model==2,aeCO2:=auxPower$Anchorage*auxEF$CO2*timespan/3600/1000/1000]

dt[model==3,aePM2.5:=auxPower$Maneuvering*auxEF$PM2.5*timespan/3600/1000/1000]
dt[model==3,aeNOx:=auxPower$Maneuvering*auxEF$NOx*timespan/3600/1000/1000]
dt[model==3,aeSOx:=auxPower$Maneuvering*auxEF$SOx*timespan/3600/1000/1000]
dt[model==3,aeCO2:=auxPower$Maneuvering*auxEF$CO2*timespan/3600/1000/1000]

dt[model==5,aePM2.5:=auxPower$Sea*auxEF$PM2.5*timespan/3600/1000/1000]
dt[model==5,aeNOx:=auxPower$Sea*auxEF$NOx*timespan/3600/1000/1000]
dt[model==5,aeSOx:=auxPower$Sea*auxEF$SOx*timespan/3600/1000/1000]
dt[model==5,aeCO2:=auxPower$Sea*auxEF$CO2*timespan/3600/1000/1000]

dt[model==4,aePM2.5:=auxPower$Sea*auxEF$PM2.5*timespan/3600/1000/1000]
dt[model==4,aeNOx:=auxPower$Sea*auxEF$NOx*timespan/3600/1000/1000]
dt[model==4,aeSOx:=auxPower$Sea*auxEF$SOx*timespan/3600/1000/1000]
dt[model==4,aeCO2:=auxPower$Sea*auxEF$CO2*timespan/3600/1000/1000]

#------------boiler engine-----------

boiPower=boiPowerdt[ShipClass=='Container'&CapacityFrom<DWT&CapacityTo>DWT]

dt[,boPM2.5:=0]
dt[,boNOx:=0]
dt[,boSOx:=0]
dt[,boCO2:=0]

#-----IMO 2014 中辅机功率没有分SRZ和SEA两种模式，只是提供了一种在海模式的功率-----
#-----如果要分这两种模式，可以参考port 2009中的处理方式---------------------------

dt[model==1,boPM2.5:=boiPower$Berth*boiEF$PM2.5*timespan/3600/1000/1000]
dt[model==1,boNOx:=boiPower$Berth*boiEF$NOx*timespan/3600/1000/1000]
dt[model==1,boSOx:=boiPower$Berth*boiEF$SOx*timespan/3600/1000/1000]
dt[model==1,boCO2:=boiPower$Berth*boiEF$CO2*timespan/3600/1000/1000]

dt[model==2,boPM2.5:=boiPower$Anchorage*boiEF$PM2.5*timespan/3600/1000/1000]
dt[model==2,boNOx:=boiPower$Anchorage*boiEF$NOx*timespan/3600/1000/1000]
dt[model==2,boSOx:=boiPower$Anchorage*boiEF$SOx*timespan/3600/1000/1000]
dt[model==2,boCO2:=boiPower$Anchorage*boiEF$CO2*timespan/3600/1000/1000]

dt[model==3,boPM2.5:=boiPower$Maneuvering*boiEF$PM2.5*timespan/3600/1000/1000]
dt[model==3,boNOx:=boiPower$Maneuvering*boiEF$NOx*timespan/3600/1000/1000]
dt[model==3,boSOx:=boiPower$Maneuvering*boiEF$SOx*timespan/3600/1000/1000]
dt[model==3,boCO2:=boiPower$Maneuvering*boiEF$CO2*timespan/3600/1000/1000]

dt[model==5,boPM2.5:=boiPower$Sea*boiEF$PM2.5*timespan/3600/1000/1000]
dt[model==5,boNOx:=boiPower$Sea*boiEF$NOx*timespan/3600/1000/1000]
dt[model==5,boSOx:=boiPower$Sea*boiEF$SOx*timespan/3600/1000/1000]
dt[model==5,boCO2:=boiPower$Sea*boiEF$CO2*timespan/3600/1000/1000]

dt[model==4,boPM2.5:=boiPower$Sea*boiEF$PM2.5*timespan/3600/1000/1000]
dt[model==4,boNOx:=boiPower$Sea*boiEF$NOx*timespan/3600/1000/1000]
dt[model==4,boSOx:=boiPower$Sea*boiEF$SOx*timespan/3600/1000/1000]
dt[model==4,boCO2:=boiPower$Sea*boiEF$CO2*timespan/3600/1000/1000]

#--------------分配到不同的网格中,以下方法对轨迹的完整性要求较--------
#--------------计算量非常大，需要采用分布式计算-----------------------

glines=gridLines(lines,scale)
#gridized emissions
ge=data.table(inner_join(glines,dt,by='lid'))
#calculate each grid segment using percent
ge2=cbind(ge[,1:11,with=F],ge[,16:27,with=FALSE]*ge$percent)
#sum for each grid
ge3=ge2[,list(mePM2.5=sum(mePM2.5),meNOx=sum(meNOx),meSOx=sum(meSOx),meCO2=sum(meCO2),
              aePM2.5=sum(aePM2.5),aeNOx=sum(aeNOx),aeSOx=sum(aeSOx),aeCO2=sum(aeCO2),
              boPM2.5=sum(boPM2.5),boNOx=sum(boNOx),boSOx=sum(boSOx),boCO2=sum(boCO2)),by=list(gid,grid.x,grid.y)]

ge4=ge2[,list(PM2.5=sum(mePM2.5+aePM2.5+boPM2.5),NOx=sum(meNOx+aeNOx+boNOx),
              SOx=sum(meSOx+aeSOx+boSOx),CO2=sum(meCO2+aeCO2+boCO2)),by=list(gid,grid.x,grid.y)]

#emission of each air pollutants

ge3[,list(mePM2.5=sum(mePM2.5),meNOx=sum(meNOx),meSOx=sum(meSOx),meCO2=sum(meCO2),
          aePM2.5=sum(aePM2.5),aeNOx=sum(aeNOx),aeSOx=sum(aeSOx),aeCO2=sum(aeCO2),
          boPM2.5=sum(boPM2.5),boNOx=sum(boNOx),boSOx=sum(boSOx),boCO2=sum(boCO2))]

#----画图------
temp1=cbind(lines[tripid>0,list(lid,time1,time2)],dt[,list(avgspeed,load.main)])
temp2=cbind(temp1[1:(nrow(temp1)-1),list(time2,avgspeed1=avgspeed,load.main1=load.main)],temp1[2:nrow(temp1),list(time1,avgspeed2=avgspeed,load.main2=load.main)])
#在地图上画空间分布
temp3=ge3[,list(lon=grid.x,lat=grid.y,meCO2,aeCO2,boCO2,mePM2.5,aePM2.5,boPM2.5,meNOx,aeNOx,boNOx,meSOx,aeSOx,boSOx)]

#--主机油耗空间分布----
dev.new()
#p=getMap(temp3,6)
centerX=0.5*(max(temp3$lon)+min(temp3$lon))
centerY=0.5*(max(temp3$lat)+min(temp3$lat))
#p<-ggmap(get_map(location=c(centerX,centerY),zoom=6,source='google',maptype = 'roadmap'))
p=ggplot()
p=p+geom_rect(data=temp3,aes(xmin=lon,xmax=lon+1/scale,ymin=lat,ymax=lat+1/scale,fill=meCO2/3.18),color='black',size=0.01)
p=p+scale_fill_gradient('主机能 \n 耗(吨）',low='green',high='red')
p=p+labs(x="经度",y="纬度")+
  theme(legend.position=c(0.8,0.2))
p


#----另一种计算方式网格分配的方式：利用点的位置以及功率等-----
#----每个点的主辅机功率，主机功率为3次方，辅机和锅炉功率可以航行模式查表确定------
dt2=p
dt2[,mp:=0]
dt2[,ap:=0]
dt2[,bp:=0]
dt2[,mp:=round((sog*0.94/sSpeed)^3*MCR)]
#set ship status: 1 for berth,2for anchor,3for maneuvering,4for lowCruise,5for highCruise
dt2[,model:=0]
dt2[sog<10,model:=1]
dt2[sog>=10&sog<=30,model:=2]
dt2[sog>30&load.main<0.2,model:=3]
dt2[load.main>=0.2&load.main<=0.65,model:=4]
dt2[load.main>0.65,model:=5]

dt2[model==1,ap:=auxPower$Berth]
dt2[model==2,ap:=auxPower$Anchorage]
dt2[model==3,ap:=auxPower$Maneuvering ]
dt2[model==4,ap:=auxPower$Sea]
dt2[model==5,ap:=auxPower$Sea]

dt2[model==1,bp:=boiPower$Berth]
dt2[model==2,bp:=boiPower$Anchorage]
dt2[model==3,bp:=boiPower$Maneuvering ]
dt2[model==4,bp:=boiPower$Sea]
dt2[model==5,bp:=boiPower$Sea]

dt2[,tp:=(mp+ap+bp)]
proxy=dt2[,list(idx=sum(tp)/sum(dt2$tp)),list(gid,g.lon,g.lat)]
#每个网格乘以总排放



