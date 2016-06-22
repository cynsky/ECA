#get file for each container in 2015 

shipfile='D://share/ships/ships.csv'
ships=getships(shipfile);dim(ships);head(ships);setkey(ships,mmsi)
containers=ships[!is.na(speed)&!is.na(powerkw)&!is.na(dwt)&!is.na(mmsi)&speed>0&type_en=='Container']
setkey(containers,mmsi)
dirs=list.dirs('D://share/AIS/globalTraj')
i=201500#记录月份
#filedir=dirs[1]
for(filedir in dirs[2:13]){
  i=i+1
  
  d=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0)[mmsi<0]
  filenames=list.files(filedir,full.names = TRUE)
  for(filename in filenames){
    
    d0=fread(filename,sep = '@');setnames(d0,c('mmsi','time','status','sog','lon','lat'))
    setkey(d0,mmsi)
    d1=data.table(inner_join(d0,containers[,list(mmsi)],'mmsi'))
    d=rbind(d,d1)
    
  }
  write.csv(d,paste(filedir,i,sep = '/'))
}
#读入2015年所有集装箱船舶AIS数据
filenames=list.files('D://share/AIS/globalContainer2015',full.names = TRUE)
d=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0)[mmsi<0]
for(filename in filenames[7:12]){
  
  d0=fread(filename)[,list(mmsi,time,status,sog,lon,lat)]
  d=rbind(d,d0)
  
}
setkey(d,mmsi,time)
#去掉轨迹点少于1000
#去掉航速小于0和航速大于1.5倍设计航速的报告点
#每天船舶一年轨迹进行分割:获取轨迹小于

ship1=d0[mmsi=='209098000']
plot(ship1$time,ship1$sog)
dev.new()
plot(ship1$lon,ship1$lat)






