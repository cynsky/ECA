#识别在排放控制区的码头和锚地，可以说是停留点

p2=points[sog==0&status==5];dim(p2)
p3=setPoints(p2,scale=10000);p3[,.N,gid]
p4=p3[,list(gid,.N,lon=mean(g.lon),lat=mean(g.lat)),gid]
m1=as.matrix(p4[,list(lon,lat)])
kNN(m1[1:10000,],5)
kNNdistplot(m1,20)
cls=dbscan(m1,0.05,100)
p5=cbind(p4,cls[[1]])

p=ggplot()
p=p+geom_point(data=p5[V2>0],aes(x=lon,y=lat,col=V2))
p=p+scale_fill_gradient('cluster',low='green',high='red')
p=p+geom_text(data=p5[V2>0,list(V2,lon=mean(lon),lat=mean(lat)),V2],aes(x=lon,y=lat,label=V2),size=4)
p

write.csv(p4,'zerogrids.csv')
plot(cls[[1]])
distance(120,31,120.07,31.07)

