########### ---------------------------------------------------> April E. Reside's code to create GIFs ####
# setwd('/home/jc214262/ConsPlan/WT_cluster/pics_for_giff/') 
# system('convert -delay 100 *.png WTC_landscape.gif')
# 
# ###### spp gifs
# setwd('/home/jc214262/ConsPlan/WT_cluster/Species_future_models_realised/amphibians/Austrochaperina_fryi/') 
# system(paste('convert -delay 100 *.png ',spp,'_WTC.gif'))
# 
# 
# 
# 
# sdm.dir = '/home/jc214262/ConsPlan/WT_cluster/Species_future_models_realised/'
# taxa = c("amphibians", "birds","mammals","reptiles")
# tax = taxa[1]
# 
# #for (tax in taxa[4]) {    cat(tax,'\n') 
# pngDir="/Volumes/SDM /Journal_work/github_repo/ETBF_bycatch_NatCon/ETBF_bycatch_app/data/pngs"
# species=c("ss","sfm","bs","bws","Dws","ts","ows")
# outdir="/Volumes/SDM /Journal_work/github_repo/ETBF_bycatch_NatCon/ETBF_bycatch_app/data/gifs";dir.create(outdir)
# 
# 	for(spp in species) { 
# 	  Dir=paste0("/Volumes/SDM /Journal_work/github_repo/ETBF_bycatch_NatCon/ETBF_bycatch_app/data/pngs/",spp);setwd(Dir)
# 		#pngfile = paste(outdir,"/",spp,sep='')
# 		setwd(Dir)
# 			system('convert -delay 40 *.png species.gif')		### this is the only line you need!!!!
# 			file.rename('species.gif', paste0(spp,".gif"))
# 	}
# 	
	#####
	########### ---------------------------------------------------> Heather's code  ####

	##### 1. make some pngs
	### preparing rasters, write out as pngs
	library(caTools)
	library(raster)
	library(sp)
	library(tidyverse)
	library(rworldmap)
	library(rgdal)
	library(maps)
	library(mapdata)
	library(RColorBrewer)
	library(leaflet)
	library(zoo)

	species=c("ss","sfm","bs","bws","Dws","ts","ows")
	
	sdmDir="/Volumes/SDM /Journal_work/github_repo/ETBF_bycatch_NatCon/ETBF_bycatch_app/data/SDMs";setwd(sdmDir)
	pngDir="/Volumes/SDM /Journal_work/github_repo/ETBF_bycatch_NatCon/ETBF_bycatch_app/data/pngs"
	for(sp in species){
	  dirpath=paste0(pngDir,"/",sp);dir.create(dirpath)
	}
	
	make_png=function(r,name, date,sp){ ### does what it says
	  
	  png(paste0(pngDir,"/",sp,"/",name,".png"), width=5, height=7, units="in", res=400)
	  par(ps=10) #settings before layout
	  layout(matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE), heights=c(4,1), widths=7)
	  par(cex=1) # layout has the tendency change par()$cex, so this step is important for control
	  
	  par(mar=c(4,4,1,1)) # I usually set my margins before each plot
	  pal <- colorRampPalette(c("#A50026", "#FFFFBF", "#313695"))
	  ncolors <- 100
	  
	  breaks <- seq(0,1,,ncolors+1)
	  image(r, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(140,173),ylim=c(-50,-9))
	  map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
	  contour(r, add=TRUE, col="black",levels=c(.5))
	  contour(r, add=TRUE, col="black",levels=c(.75))
	  mtext(paste0(as.character(date)), side=3, line=0)
	  box()
	  
	  levs <- breaks[-1] - diff(breaks)/2
	  image(x=levs, y=1, z=as.matrix(levs), col=pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
	  mtext(paste0("Habitat suitability"), side=3, line=0.2)
	  
	  box()
	  
	  dev.off() # closes device
	}
	
	for(ras in list.files(sdmDir)){
	  r=raster(ras)
	  name=gsub(".asc","",ras)
	  a=strsplit(name,"_")
	  b=a[[1]][2]
	  sp=a[[1]][1]
	  date=as.yearmon(b,"%Y%m")
	  make_png(r=r,name=name,date=date,sp=sp)
	}
	
###### 2. make some gifs ####
	pngDir="/Volumes/SDM /Journal_work/github_repo/ETBF_bycatch_NatCon/ETBF_bycatch_app/data/pngs"
	species=c("ss","sfm","bs","bws","Dws","ts","ows")
	outdir="/Volumes/SDM /Journal_work/github_repo/ETBF_bycatch_NatCon/ETBF_bycatch_app/data/gifs";#dir.create(outdir)
	
	for(spp in species) { 
	  Dir=paste0("/Volumes/SDM /Journal_work/github_repo/ETBF_bycatch_NatCon/ETBF_bycatch_app/data/pngs/",spp);setwd(Dir)
	  #pngfile = paste(outdir,"/",spp,sep='')
	  setwd(Dir)
	  system('convert -delay 40 *.png species.gif')		### this is the only line you need!!!!
	  file.rename('species.gif', paste0(spp,".gif"))
	}
	
	