#calculate the global emission using ais from chuanxun
# this docu only provide the points which are pre-processed
filedir='D://share/AIS/AIS_chuanxun_201409/csvdata/'
filepaths=list.files(filedir,full.names =TRUE)
dt0=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0)[mmsi<0]
for (filepath in filepaths){
  temp=fread(filepath)
  temp=temp[,list(mmsi=unique_ID,time=acquisition_time,status,lon=longitude,lat=latitude,sog=round(speed/100))]
  dt0=rbind(dt0,temp)
}

#setkey(dt0,mmsi,time);setkey(ships,mmsi)
dt0=data.table(inner_join(dt0,ships[,list(mmsi,type_en)],'mmsi'))
dt=dt0[type_en=='Container',list(mmsi,time,status,sog,lon,lat)]
dt.mmsis=dt[,list(avgsog=mean(sog),.N),mmsi][avgsog>=20&avgsog<=250&N>=1000]
points=data.table(inner_join(dt,dt.mmsis[,list(mmsi)],'mmsi'))


#写入硬盘
write.csv(em1,'results/chuanxun_201409_container_global_emission.csv')
write.csv(ge.total,'results/chuanxun_201409_container_grid_emission.csv')
write.csv(ge,'results/chuanxun_201409_container_global_ship_proxy_emission.csv')
