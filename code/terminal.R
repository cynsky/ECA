#识别在排放控制区的码头和锚地，可以说是停留点

clusters=detectStayArea(points,0.03,10,1000,100)

# kNN(m1[1:10,],3)
# kNNdist(m1[1:100],3)
# kNNdistplot(m1,10)

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

