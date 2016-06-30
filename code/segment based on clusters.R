#segment one ship trajectory based on clusters,segment 分段的点要在类里面，同事航速要小于0.2节
aship0[,seg:=0]
clusters

shipp=points[mmsi==ammsi]

clsp0=data.table(left_join(shipp,clusters[,list(gid,cls)],'gid'))
clsp=clsp0[cls>0&sog==0]
atrip=aship0[tripid==25]


tripp=atrip[,list(mmsi,time=time1,status=status1,sog=sog1,lon=lon1,lat=lat1,gid=gid1,tripid)]
tripp=rbind(tripp,atrip[nrow(atrip),list(mmsi,time=time2,status=status2,sog=sog2,lon=lon2,lat=lat2,gid=gid2,tripid)])
tripseg=addSegment(tripp,clusters)
tripseg[,.N,segid]

