 #
 #
 #
 #
 source("~/workspace/LPJ_Utilities/src/read.input.r")
 source("~/workspace/LPJ_Utilities/src/map.r")
 source("crops2band.r")
 library(maps)
 library(fields)
 
 DEBUG<-F
 Reload_cft<-FALSE
 PIORITY<-"IRRIG" #"RAINFED", "NOPIRO"
 NUM_CROPS<-26
 NUM_TOTAL_CROPS<-52
 REMAIN<-0.9
 
 grid.file<-"/home/sinan/workspace/LPJmL/LPJmL_Global/inputs2013/grid.bin"
 cft.file<-"/home/sinan/workspace/input_downscale/INPUTS/cft1700_2010_new.bin"
 cow.file<-"cow_mg_2006_full.bin"

 cft.name52<-c("Temperate_Cereals_r","Rice_r","Maize_r","Tropical_Cereals_r","Pulses_r","Temperate_Roots_r","Potatoes_r","Tropical_Roots_r","Sunflower_r",
  "Soybeans_r","Groundnuts_r","Rapeseed_r","Sugar_Cane_r","Citrus_r",
  "Non_Citrus_Orchards_r","Date_Palm_r","Olives_r","Nuts_Trees_r",
  "Grapes_r","Vegetables_r","Cotton_r","Fodder_grass_r","Others_r",
  "Manage_Grasslands_r","Bioenergy_Grass_r","Bioenergy_Tree_r",
  
"Temperate_Cereals_i","Rice_i","Maize_i","Tropical_Cereals_i", "Pulses_i",
"Temperate_Roots_i","Potatoes_i","Tropical_Roots_i","Sunflower_i",
  "Soybeans_i","Groundnuts_i","Rapeseed_i","Sugar_Cane_i","Citrus_i",
  "Non_Citrus_Orchards_i","Date_Palm_i","Olives_i","Nuts_Trees_i",
  "Grapes_i","Vegetables_i","Cotton_i","Fodder_grass_i","Others_i",
  "Manage_Grasslands_i","Bioenergy_Grass_i","Bioenergy_Tree")
 
 allcrop_band<-which(cft.name52!="Fodder_grass_i"&cft.name52!="Manage_Grasslands_i"&cft.name52!="Fodder_grass_r"&cft.name52!="Manage_Grasslands_r")
 
 grass_band<-c(22,24,48,50)
 
 COL<- colorRampPalette(c("darkgreen","chartreuse","lemonchiffon","goldenrod1","goldenrod2","goldenrod3","goldenrod"))
 #---------
 #from latitude to area
 #---------
 deg2area<-function(lat,res=0.5){
	 deg2rad <- function(deg){
		 return (deg*pi*0.00555555555555)
	 }
	 area<-(111e3*res)*(111e3*res)*cos(deg2rad(lat))/10000#ha
	 return(area)
  }
 
 
#=========
#load LPJmL files
#=========
 read.input.grid(grid.file)
 cow<-read.input.yearband(cow.file,2,1901,1)
country_code_table<-read.csv("country_cow.csv")
if(Reload_cft==TRUE){
	cft<-array(NA,c(67420,52))
	for(i in 1:52)	{
		cft[,i]<-read.input.yearband(cft.file,2,2000,i)
		print(i)
	}
	save(cft,file="cft.Rdata")
}else{
	load("cft.Rdata")
}

#=========
#load land grab information
#=========
loadLandGrabList<-function(){
	landgrab_table<-read.csv("crops.prod.af.csv")
	crop_name<<-unique(as.character(landgrab_table$crop))
	landgrab_list<-list()
	for(i in 1:length(crop_name)){
		landgrab_list[[crop_name[i]]]<-list()
			landgrab_list[[crop_name[i]]][["cow_iso3"]]<-as.vector(as.character(
												landgrab_table$ISO3[landgrab_table[,"crop"]==crop_name[i]]))
			landgrab_list[[crop_name[i]]][["hectares"]]<-as.vector(as.numeric(
												landgrab_table$hectares[landgrab_table[,"crop"]==crop_name[i]]))
			landgrab_list[[crop_name[i]]][["band_search"]]<- croplist[[crop_name[i]]][[1]]
			landgrab_list[[crop_name[i]]][["band_alloc"]] <- croplist[[crop_name[i]]][[2]]
	}
	return(landgrab_list)
}
	
#=======
#
#
#=======


getThisCountry<-function(cft,cow,this.country){
	this.country.cft<-list()
	this.country.cft[["cft"]]<-cft[which(cow==this.country),]
	this.country.cft[["lon"]]<-lon[which(cow==this.country)]
	this.country.cft[["lat"]]<-lat[which(cow==this.country)]
	this.country.cft[["area_total"]]<-deg2area(this.country.cft[["lat"]])
	this.country.cft[["index_origin"]]<-which(cow==this.country)
	return(this.country.cft)
}

getThisBandArea<-function(countrycft,thisband){
	this.band.cft<-list()
	chosen.ind<<-which(countrycft[["cft"]][,thisband]>0 & countrycft[["cft"]][,thisband]<REMAIN,arr.ind=2)
 	if(is.null(dim(chosen.ind))==FALSE)   chosen.ind<-unique(chosen.ind[,1])	
 	totalfrac<-rowSums(countrycft[["cft"]])
 	chosen.ind<-intersect(chosen.ind,which(totalfrac<REMAIN))
 	#if(length(chosen.ind)==1)  this.band.cft[["cft"]]<-sum(countrycft[["cft"]][chosen.ind,])
 	
 	if(length(chosen.ind)==0) {
 		#cat("No", cft.name52[thisband],"!\n")
 		return(NULL)
 	}
 	
 	else 	this.band.cft[["cft"]]<-totalfrac[chosen.ind]
	this.band.cft[["lon"]]<-countrycft[["lon"]][chosen.ind]
	this.band.cft[["lat"]]<-countrycft[["lat"]][chosen.ind]
	this.band.cft[["area_total"]]<-countrycft[["area_total"]][chosen.ind]
	this.band.cft[["index_origin"]]<-countrycft[["index_origin"]][chosen.ind]
	return(this.band.cft)

}


getallArea<-function(countrycft){
	this.band.cft<-list()
 	totalfrac<-rowSums(countrycft[["cft"]])
 	chosen.ind<-which(totalfrac<REMAIN)
 	this.band.cft[["cft"]]<-totalfrac[chosen.ind]
 	this.band.cft[["lon"]]<-countrycft[["lon"]][chosen.ind]
	this.band.cft[["lat"]]<-countrycft[["lat"]][chosen.ind]
	this.band.cft[["area_total"]]<-countrycft[["area_total"]][chosen.ind]
	this.band.cft[["index_origin"]]<-countrycft[["index_origin"]][chosen.ind]

	return(this.band.cft)
}



getGrabbedArea<-function(landgrab_list,this.crop,this.country){
#	hectaresind<-which(landgrab_list[[this.crop]][["cow_iso3"]]==this.country)
#	return(sum(landgrab_list[[this.crop]][["hectares"]][hectaresind]))
	return(landgrab_list[[this.crop]][["hectares"]][which(landgrab_list[[this.crop]][["cow_iso3"]]==this.country)])
}
	
getUsedArea<-function(cft_list){
	return(cft_list$area_total*cft_list$cft)
}
getAvailArea<-function(cft_list){
	val<-cft_list$area_total*(REMAIN-cft_list$cft)
	if(any(val<0)){ 
		browser() 
		stop("area < 0")
	}	
	return(val)
}

getProportion<-function(cft_list){
	getAvailArea(cft_list)/sum(getAvailArea(cft_list))
}


allocGrabLand<-function(cft_list,alloc_band,toassign){
	if(toassign!=0){
		proportion<-getProportion(cft_list)
		area_avail<-sum(getAvailArea(cft_list))
		if(DEBUG==TRUE){
			cft.test<-new.cft
		}
		if(area_avail>toassign){
			
			for(i in 1:length(alloc_band)){
				val<-(toassign*proportion)/length(alloc_band)/cft_list[["area_total"]]
				new.cft[cft_list[["index_origin"]],alloc_band[i]]<<-new.cft[cft_list[["index_origin"]],alloc_band[i]]+val
				#old cft also being added the assign value so that for next step the assigned value will appeared on old cft.
				#cft[cft_list[["index_origin"]],alloc_band[i]]<<-cft[cft_list[["index_origin"]],alloc_band[i]]+new.cft[cft_list[["index_origin"]],alloc_band[i]]
			}
			cat("Assigning:",sum(val*cft_list[["area_total"]]),"*",length(alloc_band),"to",cft.name52[alloc_band])
			
			if(DEBUG==TRUE){
				if(round(sum(new.cft-cft.test))!=round(toassign)){
					browser()
					stop("TEST FAILURE: sum(new)!=toassign ",sum(new.cft-cft.test),"/",toassign)
				}
				
			}
			return(0)		
		}else{
			for(i in 1:length(alloc_band)){
				val<-(area_avail*proportion)/length(alloc_band)/cft_list[["area_total"]]
				new.cft[cft_list[["index_origin"]],alloc_band[i]]<<-new.cft[cft_list[["index_origin"]],alloc_band[i]]+val
				#cft[cft_list[["index_origin"]],alloc_band[i]]<<-cft[cft_list[["index_origin"]],alloc_band[i]]+new.cft[cft_list[["index_origin"]],alloc_band[i]]
			}
			cat("Assigning:",sum(val),"*",length(alloc_band),"to",cft.name52[alloc_band])
			if(DEBUG==TRUE){
				check_area<-sum(new.cft-cft.test)
				if(round(check_area)!=round(area_avail)){ 
					browser() 
					stop("TEST FAILURE: sum(new)!=toassign ",check_area,"/",area_avail)}
				}
			
			return(toassign-area_avail)		
		}
	}else{
		return(0)
	}

	
	
}

assignTheCropArea<-function(countrycft,landgrab_list,crop,country){
	to_assign<-getGrabbedArea(landgrab_list,crop,country)
	band_search<-landgrab_list[[crop]][["band_search"]]
	band_alloc<-landgrab_list[[crop]][["band_alloc"]]
	band_search_irrig<-band_search[band_search>NUM_CROPS]
	band_search_rainf<-band_search[band_search<=NUM_CROPS]
	band_alloc_irrig<-band_alloc[band_alloc>NUM_CROPS]
	band_alloc_rainf<-band_alloc[band_alloc<=NUM_CROPS]
	
	if(PIORITY=="IRRIG"){
		cat("1st -> ")
		have_irrig_list<-getThisBandArea(countrycft,band_search_irrig)
		have_irrig_availarea<-getAvailArea(have_irrig_list)
		cat("Area [only irrigated]:",sum(have_irrig_availarea),"hectares     ")		
		to_assign<-allocGrabLand(have_irrig_list,band_alloc_irrig,to_assign)
		cat("......(",to_assign,")\n",sep="")

		cat("2nd -> ")
		have_rainf_list<-getThisBandArea(countrycft,band_search_rainf)
		have_rainf_availarea<-getAvailArea(have_rainf_list)
		cat("Area [only rainfed]:",sum(have_rainf_availarea),"hectares     ")	
		to_assign<-allocGrabLand(have_rainf_list,band_alloc_rainf,to_assign)
		cat("......(",to_assign,")\n",sep="")
		
		cat("3rd -> ")
		have_allcrop_list<-getThisBandArea(countrycft,allcrop_band)
		have_allcrop_availarea<-getAvailArea(have_allcrop_list)
		cat("Area [all crops]:",sum(have_allcrop_availarea),"hectares     ")	
		to_assign<-allocGrabLand(have_allcrop_list,band_alloc_irrig,to_assign) # assign to irrigated 
		cat("......(",to_assign,")\n",sep="")
		
		cat("4rd -> ")
		have_mgrass_list<-getThisBandArea(countrycft,24) #band 24 rainfed managed grassland
		have_mgrass_availarea<-getAvailArea(have_mgrass_list)
		cat("Area [managed grassland]:",sum(have_mgrass_availarea),"hectares     ")	
		to_assign<-allocGrabLand(have_allcrop_list,band_alloc_irrig,to_assign) # assign to irrigated 
		cat("......(",to_assign,")\n",sep="")
		
		
		cat("5th -> ")
		have_all_list<-getallArea(countrycft)
		have_all_availarea<-getAvailArea(have_all_list)
		cat("Area [entire country]:",sum(have_all_availarea),"hectares     ")	
		to_assign<-allocGrabLand(have_all_list,c(1:NUM_TOTAL_CROPS),to_assign) # assign to irrigated 
		cat("......(",to_assign,")\n",sep="")		
	}
	
	
	if(PIORITY=="RAINFED"){
#		cat("1st -> ")
#		have_rainf_list<-getThisBandArea(countrycft,band_search_rainf)
#		have_rainf_availarea<-getAvailArea(have_rainf_list)
#		cat("Area [only irrigated]:",sum(have_rainf_availarea),"hectares     ")		
#		to_assign<-allocGrabLand(have_rainf_list,band_alloc_rainf,to_assign)
#		cat("......(",to_assign,")\n",sep="")

#		cat("2nd -> ")
#		have_rainf_list<-getThisBandArea(countrycft,band_search_irrig)
#		have_rainf_availarea<-getAvailArea(have_rainf_list)
#		cat("Area [only rainfed]:",sum(have_rainf_availarea),"hectares     ")	
#		to_assign<-allocGrabLand(have_rainf_list,band_alloc_rainf,to_assign)
#		cat("......(",to_assign,")\n",sep="")
#		
#		cat("3rd -> ")
#		have_allcrop_list<<-getThisBandArea(countrycft,allcrop_band)
#		have_allcrop_availarea<-getAvailArea(have_allcrop_list)
#		if(DEBUG==TRUE){
#			if(any(have_allcrop_list$cft<0) || any(have_allcrop_list$cft>=1)) {stop("\n")}
#			if(any(have_allcrop_availarea<0)) {stop("available area < 0 \n")}
#		}
#		cat("Area [all crops]:",sum(have_allcrop_availarea),"hectares     ")	
#		to_assign<-allocGrabLand(have_allcrop_list,band_alloc_irrig,to_assign) # assign to irrigated 
#		cat("......(",to_assign,")\n",sep="")
#		
#		cat("4rd -> ")
#		have_mgrass_list<<-getThisBandArea(countrycft,24) #band 24 rainfed managed grassland
#		have_mgrass_availarea<-getAvailArea(have_mgrass_list)
#		if(DEBUG==TRUE){
#			if(any(have_mgrass_list$cft<0) || any(have_mgrass_list$cft>=1)) {stop("\n")}
#			if(any(have_mgrass_availarea<0)) {stop("available area < 0 \n")}
#		}
#		cat("Area [managed grassland]:",sum(have_mgrass_availarea),"hectares     ")	
#		to_assign<-allocGrabLand(have_allcrop_list,band_alloc_irrig,to_assign) # assign to irrigated 
#		cat("......(",to_assign,")\n",sep="")
#		
#		
#		cat("5th -> ")
#		have_all_list<-getallArea(countrycft)
#		have_all_availarea<-getAvailArea(have_all_list)
#		if(DEBUG==TRUE){
#			if(any(have_allcrop_list$cft<0) || any(have_allcrop_list$cft>=1)) {stop("\n")}
#			if(any(have_allcrop_availarea<0)) {stop("available area < 0 \n")}
#		}
#		cat("Area [entire country]:",sum(have_all_availarea),"hectares     ")	
#		to_assign<-allocGrabLand(have_all_list,c(1:NUM_TOTAL_CROPS),to_assign) # assign to irrigated 
#		cat("......(",to_assign,")\n",sep="")		
#	}
		
	}
	if(PIORITY=="NOPIRO"){
		
	}
	
	cat("\n")
	if(to_assign!=0) stop("to_assign != 0")
	

}


#=====
#main
#=====
new.cft<<-array(0,dim=dim(cft))
landgrab_list<-loadLandGrabList()
for(this.crop in crop_name){
	cat("----------------------","\n")		
	cat("Assigning: [[", this.crop,"]]\n",sep="")
	cat("Search Band:",cft.name52[landgrab_list[[this.crop]][["band_search"]]],"\n")
	cat("Allocate Band:",cft.name52[landgrab_list[[this.crop]][["band_alloc"]]],"\n")
	cat("----------------------","\n")	
	for(this.country in landgrab_list[[this.crop]][["cow_iso3"]]){
		grabbed.hectares<-getGrabbedArea(landgrab_list,this.crop,this.country)
		this.country.name<-as.character(country_code_table$name[which(country_code_table$iso3==this.country)])
		if(length(this.country.name)!=1) stop("country code is not right: ", this.country)
		this.lpj.cow<-as.numeric(country_code_table$cow[which(country_code_table$iso3==this.country)])
		cat(this.country.name, this.country,"(",this.lpj.cow,") \nGrabbed Area:", grabbed.hectares,"hectares.\n")
		
		this.country.cft<-getThisCountry(cft=cft,cow=cow,this.country=this.lpj.cow)
		
		
		
		#1st fill pixels that contain this crop
		if(landgrab_list[[this.crop]][["band_search"]][1]!=0){#no action taken for crops don't have a corresponding LPJ band
			left.hectares<-assignTheCropArea(this.country.cft,landgrab_list,this.crop,this.country)
		}




#			this.country.this.band.cft<-getThisBandCrops(countrycft=this.country.cft,landgrab_list=landgrab_list)
#			if(is.null(this.country.this.band.cft)==FALSE){
#			assignThisCropPixel(cft_list=this.country.this.band.cft,
#								      bands=landgrab_list[[this.crop]][["lpj_band"]],
#								      totalgrabbed=grabbed.hectares)
			
#			}
				
#		}
	}
	cat("\n\n")
}
	

#col<-rev(COL(100))
#col[1]<-"grey"
#Lon<-sort(unique(lon))
#Lat<-sort(unique(lat))

#for(i in 1:52){
#	bitmap(paste("image/",cft.name52[i],".jpeg",sep=""),res=300,type="jpeg")
#	image.plot(x=Lon,y=Lat,map.build(abs(new.cft[,i])),col=col,xlim=c(-20,60), ylim=c(-40,40))
#	map(add=T)
#	title(paste("cft_frac",cft.name52[i]))
#	dev.off()
# }


 #=========================trash
 
 #	if( THISCROP_COUNT=="ALL"){
#		landgrab_list[[crop_name[i]]][["lpj_band"]]<-as.vector(croplist[[crop_name[i]]])
#	}else{
#		if(THISCROP_COUNT=="ONLY_IRRIG"){
#			indcount<-which(croplist[[crop_name[i]]]>NUM_CROPS)
#		}
#		if(THISCROP_COUNT=="ONLY_RAINFED"){
#			indcount<-which(croplist[[crop_name[i]]]<=NUM_CROPS)
#		}
#		if(length(indcount)==0){
#			landgrab_list[[crop_name[i]]][["lpj_band"]]<-0
#		}else{
#			landgrab_list[[crop_name[i]]][["lpj_band"]]<-as.vector(croplist[[crop_name[i]]][indcount])
#		}
#	}













#assignThisCropPixel<-function(cft_list,bands,totalgrabbed){
#	toassign<-sum(getAvailArea(cft_list))-totalgrabbed
#	cat("available area=", sum(getAvailArea(cft_list)), "||grabbed area=",totalgrabbed,"(hectares).\n")
#	if(toassign<0) stop("not enough space for assigning grabbed land to only
#		     cropping area, make further program for non-cropping area and grassland :-)\n")
#	else toassign<-0
#	
#	area.to.each.cell<-totalgrabbed*getProportion(cft_list)
#	#check length of vectors are the same
#	if(length(area.to.each.cell)!=length(cft_list[["area_total"]])){
#		print(area.to.each.cell)
#		print(cft_list[["area_total"]])
#		stop("length of area.to.each.cell=",length(area.to.each.cell),"cft_list[[area_total]]=",length(cft_list[["area_total"]]))
#	}
#	#area to frac
#	frac.to.each.cell<-area.to.each.cell/cft_list[["area_total"]]
#	frac.to.each.cell.each.band<-frac.to.each.cell/length(bands)
#	for(band in bands){
#		new.cft[cft_list[["index_origin"]],band]<<-new.cft[cft_list[["index_origin"]],band]+frac.to.each.cell.each.band
#	}
#	return(toassign)
#}





