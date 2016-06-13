#functions for drawing pictures
#画出每个航次的轨迹
plotSegs <- function(l) {
  for (i in (1:nrow(l[,.N,tripid]))) {
    trip = l[tripid == i]
    dev.new()
    plot(trip$lon1,trip$lat1)
  }
}

g=fread('zerogrids.csv')
p=getMap(g,7)
p=p+geom_point(data = g,aes(x=lon,y=lat))
p

filenames=list.files('data/containerFromClarkson/')
containers=data.table(Type='e',Name='',Size=0,Unit='',Dwt=0,GT=0,Flag='',Built=0,Month=0,Builder='', OwnerGroup='')[Size<0,]

for (filename in filenames){
  
  dt=read.csv(paste('data/containerFromClarkson/',filename,sep = ''))
  containers=rbind(containers,dt)
  
  
}
dim(containers);
containers[Size>14000&Size<16500,list(Size,Dwt,GT)]
