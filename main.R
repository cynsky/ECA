
library('data.table')
library('dplyr')
library('sp')

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

# 数据处理：segment trajectory


mmsis=points[,.N,mmsi]$mmsi
p1=points[mmsi==mmsis[1],];dim(p1)

p=setPoints(p1)
l=setLines(p)
l=addLineSpeed(l)

plot(p1$lon,p1$lat)

dt.sample=sample_n(dt,10000)
plot(dt.sample$lon,dt.sample$lat)



