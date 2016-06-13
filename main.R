
library('data.table')
library('dplyr')
library('sp')
library('dbscan')
library('ggplot2')
# read ships

shipfile='D://share/ships/ships.txt'
ships=getships(shipfile);dim(ships);head(ships);setkey(ships,mmsi)

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
points=points[sog<260,]#删掉航速大于26节的轨迹点

#聚类发现集装箱码头，并画出泊位位置，如有岸电等情况
# p0航速为0的点，其中有可能在锚地或者泊位，再视觉确认
p0=points[sog==0&status==5,];dim(p0)
p01=setPoints(p0,1000)
p.grids=p01[,list(.N,lon=mean(lon),lat=mean(lat)),list(gid,g.lon,g.lat)];dim(p.grids)
plot(p.grids$lon,p.grids$lat)

p=getMap(p.grids,6)
p=p+geom_point(data=p.grids,aes(x=lon,y=lat))
p

write.csv(p.grids,file = 'zerogrids.csv')

# 数据处理：segment trajectory，在后边添加tripid,其中tripid==0表示为分割segment

mmsis=points[,.N,mmsi]$mmsi
p1=points[mmsi==mmsis[1],];dim(p1);
p=setPoints(p1)
l=setLines(p);
l=addLineSpeed(l);

l=l[avgspeed>=250,]#航速不能超过25节

#轨迹分段
l=segTra(l)

missLine=l[,list(lid,tripid,sog1,sog2,avgspeed1,avgspeed2,avgspeed,timespan,distance)][distance>=2*1852&tripid>0]
dim(missLine)






