##### ----------------> explore pixel-based mgmt suitability analysis for possible inclusion in paper 10/18/2017

### lets just do January
### for each species and each pixel, need to know 1. total threat, CV of threat.
### will be a dataframe, columns: lat,long, species, total, csv
### final plot will have different symbols for each species

library(rgdal)
library(raster)
library(tidyverse)

threatDir="/Volumes/SDM /Journal_work/threat_normalized_effort"
species=c("ss","sfm","bs","bws","Dws","ts","ows")
ext=extent(145,165,-45,-12)
crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
e_ras=raster(ext,nrow=377,ncol=164,crs=crs)
janDir="/Volumes/SDM /Journal_work/threat_normalized_effort/january_common_extent"
outDir="/Volumes/SDM /Lacie backup October 2016/Lacie share/bycatch paper/Journal for Nature Conservation/back_with_revisions/spatio_temporal_dynamics"

######## february #######
a=list.files(threatDir,pattern = "*.tif$")
b=grep(".tif",a,value = T)
c=grep("*02_",b,value=T)

for(ras in c){
  d=raster(paste0(threatDir,"/",ras))
  extent(d)=ext
  d[is.na(d[])]<-0
  d=resample(d,e_ras,method="ngb")
  name=gsub(".tif","",ras)
  #assign(name,d)
  writeRaster(d,paste0(janDir,"/",name),overwrite=T)
}

a=list.files(janDir,pattern=".grd$",full.names = T)
a=grep("*02_",a,value=T)
for(sp in species){
  print(sp)
  c=grep(sp,a,value = T)
  c=grep("*02_",c,value = T)
  d=stack(c)
  e=calc(d,fun = sum)
  f=calc(d,fun = sd)
  sumname=paste0(sp,"_total")
  sdname=paste0(sp,"_sd")
  assign(sumname,e)
  assign(sdname,f)
}
sdd=ls(pattern = "*_sd")

for(sd in sdd){
  a=rasterToPoints(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=as.data.frame(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=get(sd)
  colnames(a)[3]="sd"
  b=sd%>%strsplit(.,"_")
  b=b[[1]][1]
  a$species=b
  assign(sd,a)
}

sdDF=do.call("rbind",list(bs_sd,bws_sd,Dws_sd,ows_sd,sfm_sd,ss_sd,ts_sd))

sdd=ls(pattern = "*_total")
for(sd in sdd){
  a=rasterToPoints(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=as.data.frame(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=get(sd)
  colnames(a)[3]="total"
  b=sd%>%strsplit(.,"_")
  b=b[[1]][1]
  a$species=b
  assign(sd,a)
}

total=do.call("rbind",list(bs_total,bws_total,Dws_total,ows_total,sfm_total,ss_total,ts_total))
total$sd=sdDF$sd
full=total
full$cv=full$sd/(full$total/10)
full_no_zero=full[complete.cases(full),]

png(filename = paste0(outDir,"/Feb.png"),width=1100,height=1100,units='px',pointsize=35)
trial=ggplot()+geom_point(data=full_no_zero, aes(x=total,y=cv,color=species))
trial=trial+xlab("Magnitude of threat")+ylab("Variability of bycatch threat")+ggtitle("Management implications of the spatio-temporal dynamics of February interactions")
trial
dev.off()
 
####### March ######
a=list.files(threatDir,pattern = "*.tif$")
b=grep(".tif",a,value = T)
c=grep("*03_",b,value=T)

for(ras in c){
  d=raster(paste0(threatDir,"/",ras))
  extent(d)=ext
  d[is.na(d[])]<-0
  d=resample(d,e_ras,method="ngb")
  name=gsub(".tif","",ras)
  #assign(name,d)
  writeRaster(d,paste0(janDir,"/",name),overwrite=T)
}

a=list.files(janDir,pattern=".grd$",full.names = T)
a=grep("*03_",a,value=T)
for(sp in species){
  print(sp)
  c=grep(sp,a,value = T)
  c=grep("*03_",c,value = T)
  d=stack(c)
  e=calc(d,fun = sum)
  f=calc(d,fun = sd)
  sumname=paste0(sp,"_total")
  sdname=paste0(sp,"_sd")
  assign(sumname,e)
  assign(sdname,f)
}
sdd=ls(pattern = "*_sd")

for(sd in sdd){
  a=rasterToPoints(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=as.data.frame(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=get(sd)
  colnames(a)[3]="sd"
  b=sd%>%strsplit(.,"_")
  b=b[[1]][1]
  a$species=b
  assign(sd,a)
}

sdDF=do.call("rbind",list(bs_sd,bws_sd,Dws_sd,ows_sd,sfm_sd,ss_sd,ts_sd))

sdd=ls(pattern = "*_total")
for(sd in sdd){
  a=rasterToPoints(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=as.data.frame(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=get(sd)
  colnames(a)[3]="total"
  b=sd%>%strsplit(.,"_")
  b=b[[1]][1]
  a$species=b
  assign(sd,a)
}

total=do.call("rbind",list(bs_total,bws_total,Dws_total,ows_total,sfm_total,ss_total,ts_total))
total$sd=sdDF$sd
full=total
full$cv=full$sd/(full$total/10)
full_no_zero=full[complete.cases(full),]

png(filename = paste0(outDir,"/Mar.png"),width=1100,height=1100,units='px',pointsize=35)
trial=ggplot()+geom_point(data=full_no_zero, aes(x=total,y=cv,color=species))
trial=trial+xlab("Magnitude of threat")+ylab("Variability of bycatch threat")+ggtitle("Management implications of the spatio-temporal dynamics of March interactions")
trial
dev.off()

####### April ######
a=list.files(threatDir,pattern = "*.tif$")
b=grep(".tif",a,value = T)
c=grep("*04_",b,value=T)

for(ras in c){
  d=raster(paste0(threatDir,"/",ras))
  extent(d)=ext
  d[is.na(d[])]<-0
  d=resample(d,e_ras,method="ngb")
  name=gsub(".tif","",ras)
  #assign(name,d)
  writeRaster(d,paste0(janDir,"/",name),overwrite=T)
}

a=list.files(janDir,pattern=".grd$",full.names = T)
a=grep("*04_",a,value=T)
for(sp in species){
  print(sp)
  c=grep(sp,a,value = T)
  c=grep("*04_",c,value = T)
  d=stack(c)
  e=calc(d,fun = sum)
  f=calc(d,fun = sd)
  sumname=paste0(sp,"_total")
  sdname=paste0(sp,"_sd")
  assign(sumname,e)
  assign(sdname,f)
}
sdd=ls(pattern = "*_sd")

for(sd in sdd){
  a=rasterToPoints(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=as.data.frame(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=get(sd)
  colnames(a)[3]="sd"
  b=sd%>%strsplit(.,"_")
  b=b[[1]][1]
  a$species=b
  assign(sd,a)
}

sdDF=do.call("rbind",list(bs_sd,bws_sd,Dws_sd,ows_sd,sfm_sd,ss_sd,ts_sd))

sdd=ls(pattern = "*_total")
for(sd in sdd){
  a=rasterToPoints(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=as.data.frame(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=get(sd)
  colnames(a)[3]="total"
  b=sd%>%strsplit(.,"_")
  b=b[[1]][1]
  a$species=b
  assign(sd,a)
}

total=do.call("rbind",list(bs_total,bws_total,Dws_total,ows_total,sfm_total,ss_total,ts_total))
total$sd=sdDF$sd
full=total
full$cv=full$sd/(full$total/10)
full_no_zero=full[complete.cases(full),]

png(filename = paste0(outDir,"/Apr.png"),width=1100,height=1100,units='px',pointsize=35)
trial=ggplot()+geom_point(data=full_no_zero, aes(x=total,y=cv,color=species))
trial=trial+xlab("Magnitude of threat")+ylab("Variability of bycatch threat")+ggtitle("Management implications of the spatio-temporal dynamics of April interactions")
trial
dev.off()

####### May ######
a=list.files(threatDir,pattern = "*.tif$")
b=grep(".tif",a,value = T)
c=grep("*05_",b,value=T)

for(ras in c){
  d=raster(paste0(threatDir,"/",ras))
  extent(d)=ext
  d[is.na(d[])]<-0
  d=resample(d,e_ras,method="ngb")
  name=gsub(".tif","",ras)
  #assign(name,d)
  writeRaster(d,paste0(janDir,"/",name),overwrite=T)
}

a=list.files(janDir,pattern=".grd$",full.names = T)
a=grep("*05_",a,value=T)
for(sp in species){
  print(sp)
  c=grep(sp,a,value = T)
  c=grep("*05_",c,value = T)
  d=stack(c)
  e=calc(d,fun = sum)
  f=calc(d,fun = sd)
  sumname=paste0(sp,"_total")
  sdname=paste0(sp,"_sd")
  assign(sumname,e)
  assign(sdname,f)
}
sdd=ls(pattern = "*_sd")

for(sd in sdd){
  a=rasterToPoints(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=as.data.frame(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=get(sd)
  colnames(a)[3]="sd"
  b=sd%>%strsplit(.,"_")
  b=b[[1]][1]
  a$species=b
  assign(sd,a)
}

sdDF=do.call("rbind",list(bs_sd,bws_sd,Dws_sd,ows_sd,sfm_sd,ss_sd,ts_sd))

sdd=ls(pattern = "*_total")
for(sd in sdd){
  a=rasterToPoints(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=as.data.frame(get(sd))
  assign(sd,a)
}

for(sd in sdd){
  a=get(sd)
  colnames(a)[3]="total"
  b=sd%>%strsplit(.,"_")
  b=b[[1]][1]
  a$species=b
  assign(sd,a)
}

total=do.call("rbind",list(bs_total,bws_total,Dws_total,ows_total,sfm_total,ss_total,ts_total))
total$sd=sdDF$sd
full=total
full$cv=full$sd/(full$total/10)
full_no_zero=full[complete.cases(full),]

png(filename = paste0(outDir,"/May.png"),width=1100,height=1100,units='px',pointsize=35)
trial=ggplot()+geom_point(data=full_no_zero, aes(x=total,y=cv,color=species))
trial=trial+xlab("Magnitude of threat")+ylab("Variability of bycatch threat")+ggtitle("Management implications of the spatio-temporal dynamics of May interactions")
trial
dev.off()
