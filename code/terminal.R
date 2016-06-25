#识别在排放控制区的码头和锚地，可以说是停留点

cp1=points[sog==0&status%in%c(5)];dim(p2)#1为锚泊，5为锚链系泊
cp2=setPoints(cp1,scale=1000);p3[,.N,gid]
cp3=cp2[,list(.N,lon=mean(g.lon),lat=mean(g.lat)),gid]
m1=cp3[,list(lon,lat)]
# kNN(m1[1:10,],3)
# kNNdist(m1[1:100],3)
# kNNdistplot(m1,10)
dev.new()
plot(cp3$lon,cp3$lat)
cls=dbscan(m1,0.03,10)
cp5=cbind(cp3,cls[[1]])

p=ggplot()
p=p+geom_point(data=cp5[V2>0],aes(x=lon,y=lat,col=as.factor(V2)))
p=p+geom_point(data=sample_n(p3,10000),aes(x=lon,y=lat))
p=p+scale_fill_gradient('cluster',low='green',high='red')
p=p+geom_text(data=cp5[V2>0,list(V2,lon=mean(lon),lat=mean(lat)),V2],aes(x=lon,y=lat,label=V2),size=4)
p

cpoints=setPoints(p5[V2>0,list(lon,lat,cls=V2)],100)#网格尺度与点相同都为0.01，目前有306个网格处于停顿状态
clusters=cpoints[,list(.N),list(gid,cls)]
#--------------------------
scale=100
gridPoints=setPoints(points,scale)
gridPoints[,cls:=0]
gridPoints=data.table(inner_join(gridPoints,p6[,list(gid,cls)],'gid'))
# 数据处理：segment trajectory，在后边添加tripid,其中tripid==0表示为分割segment

mmsis=gridPoints[,.N,mmsi][N>100,]
n=nrow(mmsis)
l=data.table(lid=0,mmsi=0,pid1=0,pid2=0,timespan=0,distance=0,avgspeed1=0,avgspeed2=0,avgspeed=0,tripid=0)[mmsi<0]
for(i in (1:n)){
  if(i%%100==0){
    print(i)
  }
  i=1
  p2=gridPoints[mmsi==mmsis$mmsi[i],];
  
  
  l1=setLines(p2,scale);
  l1=addLineSpeed(l1);
  l1=l1[avgspeed<=250,]#航速不能超过25节
  #轨迹分段
  l1=segTra(l1)
  l1=l1[,list(lid,mmsi,pid1,pid2,timespan,distance,avgspeed1,avgspeed2,avgspeed,tripid)]
  l=rbind(l,l1)
}

setkey(l,mmsi,lid)









write.csv(p4,'zerogrids.csv')
plot(cls[[1]])
distance(120,31,120.01,31.01)

