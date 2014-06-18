 #
 #
 #
 #
 source("~/workspace/LPJ_Utilities/src/read.input.r")
 source("~/workspace/LPJ_Utilities/src/map.r")
 source("crops2band.r")
 library(maps)

 Reload_cft<-F
 
 THISCROP_COUNT<-"ONLY_IRRIG" #"ONLY_RAINFED", ONLY_IRRIG
 NUM_CROPS<-26
 NUM_TOTAL_CROPS<-52
 
 grid.file<-"/home/sinan/workspace/LPJmL/LPJmL_Global/inputs2013/grid.bin"
 cft.file<-"/home/sinan/workspace/input_downscale/INPUTS/cft1700_2010_new.bin"
 cow.file<-"/home/sinan/workspace/input_downscale/INPUTS/cow_mg_2006_full.bin"

 cft.name52<-c("Temperate_Cereals(r)","Rice(r)","Maize(r)","Tropical_Cereals(r)","Pulses(r)","Temperate_Roots(r)","Potatoes(r)","Tropical_Roots(r)","Sunflower(r)",
  "Soybeans(r)","Groundnuts(r)","Rapeseed(r)","Sugar_Cane(r)","Citrus(r)",
  "Non_Citrus_Orchards(r)","Date_Palm(r)","Olives(r)","Nuts_Trees(r)",
  "Grapes(r)","Vegetables(r)","Cotton(r)","Fodder_grass(r)","Others(r)",
  "Manage_Grasslands(r)","Bioenergy_Grass(r)","Bioenergy_Tree(r)",
  
"Temperate_Cereals(i)","Rice(i)","Maize(i)","Tropical_Cereals(i)","Pulses(i)",
"Temperate_Roots(i)","Potatoes(i)","Tropical_Roots(i)","Sunflower(i)",
  "Soybeans(i)","Groundnuts(i)","Rapeseed(i)","Sugar_Cane(i)","Citrus(i)",
  "Non_Citrus_Orchards(i)","Date_Palm(i)","Olives(i)","Nuts_Trees(i)",
  "Grapes(i)","Vegetables(i)","Cotton(i)","Fodder_grass(i)","Others(i)",
  "Manage_Grasslands(i)","Bioenergy_Grass(i)","Bioenergy_Tree")
 
 
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
	#load("cft.Rdata")
}

#=========
#load land grab information
#=========
landgrab_table<-read.csv("crops.prod.af.csv")
crop_name<-unique(as.character(landgrab_table$crop))
landgrab_list<-list()
for(i in 1:length(crop_name)){
	landgrab_list[[crop_name[i]]]<-list()
	landgrab_list[[crop_name[i]]][["cow_iso3"]]<-as.vector(as.character(
												landgrab_table$ISO3[landgrab_table[,"crop"]==crop_name[i]]))
	landgrab_list[[crop_name[i]]][["hectares"]]<-as.vector(as.numeric(
												landgrab_table$hectares[landgrab_table[,"crop"]==crop_name[i]]))
	if( THISCROP_COUNT=="ALL"){
		landgrab_list[[crop_name[i]]][["lpj_band"]]<-as.vector(croplist[[crop_name[i]]])
	}else{
		if(THISCROP_COUNT=="ONLY_IRRIG"){
			indcount<-which(croplist[[crop_name[i]]]>NUM_CROPS)
		}
		if(THISCROP_COUNT=="ONLY_RAINFED"){
			indcount<-which(croplist[[crop_name[i]]]<=NUM_CROPS)
		}
		if(length(indcount)==0){
			landgrab_list[[crop_name[i]]][["lpj_band"]]<-0
		}else{
			landgrab_list[[crop_name[i]]][["lpj_band"]]<-as.vector(croplist[[crop_name[i]]][indcount])
		}
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

getThisBandCrops<-function(countrycft, thisband){
	this.band.cft<-list()
	chosen.ind<<-which(countrycft[["cft"]][,thisband]>0,arr.ind=2)
 	if(is.null(dim(chosen.ind))==FALSE)   chosen.ind<-unique(chosen.ind[,1])
	
 	if(length(chosen.ind)==0) return(NULL)
 	if(length(chosen.ind)==1)  this.band.cft[["cft"]]<-sum(countrycft[["cft"]][chosen.ind,])
 	else 	this.band.cft[["cft"]]<-rowSums(countrycft[["cft"]][chosen.ind,])
	this.band.cft[["lon"]]<-countrycft[["lon"]][chosen.ind]
	this.band.cft[["lat"]]<-countrycft[["lat"]][chosen.ind]
	this.band.cft[["area_total"]]<-countrycft[["area_total"]][chosen.ind]
	this.band.cft[["index_origin"]]<-countrycft[["index_origin"]][chosen.ind]
	return(this.band.cft)

	}
}

getHectarOfThisCountry<-function(landgrab_list,this.crop,this.country){
	hectaresind<-which(landgrab_list[[this.crop]][["cow_iso3"]]==this.country)
	return(sum(landgrab_list[[this.crop]][["hectares"]][hectaresind]))
}
	
getUsedArea<-function(cft_list){
	return(cft_list$area_total*cft_list$cft)
}
getAvailArea<-function(cft_list){
	return(cft_list$area_total*(0.9-cft_list$cft))
}

getProportion<-function(cft_list){
	getAvailArea(cft_list)/sum(getAvailArea(cft_list))
}

assignThisCropPixel<-function(cft_list,bands,totalgrabbed){
	toassign<-sum(getAvailArea(cft_list))-totalgrabbed
	cat("available area=", sum(getAvailArea(cft_list)), "||grabbed area=",totalgrabbed,"(hectares).\n")
	if(toassign<0) stop("not enough space for assigning grabbed land to only
		     cropping area, make further program for non-cropping area and grassland :-)\n")
	else toassign<-0
	
	area.to.each.cell<-totalgrabbed*getProportion(cft_list)
	#check length of vectors are the same
	if(length(area.to.each.cell)!=length(cft_list[["area_total"]])){
		print(area.to.each.cell)
		print(cft_list[["area_total"]])
		stop("length of area.to.each.cell=",length(area.to.each.cell),"cft_list[[area_total]]=",length(cft_list[["area_total"]]))
	}
	#area to frac
	frac.to.each.cell<-area.to.each.cell/cft_list[["area_total"]]
	frac.to.each.cell.each.band<-frac.to.each.cell/length(bands)
	for(band in bands){
		new.cft[cft_list[["index_origin"]],band]<<-new.cft[cft_list[["index_origin"]],band]+frac.to.each.cell.each.band
	}
	return(toassign)
	
	
	
}

#=====
#main
#=====
new.cft<<-array(0,dim=dim(cft))
for(this.crop in crop_name){
	cat("----------------------","\n")		
	cat("Assigning the grabbed land of", this.crop,"\n")
	cat("----------------------","\n")	
	for(this.country in landgrab_list[[this.crop]][["cow_iso3"]]){
		grabbed.hectares<-getHectarOfThisCountry(landgrab_list,this.crop,this.country)
		this.country.name<-as.character(country_code_table$name[which(country_code_table$iso3==this.country)])
		if(length(this.country.name)!=1) stop("country code is not right: ", this.country)
		this.lpj.cow<-as.numeric(country_code_table$cow[which(country_code_table$iso3==this.country)])
		cat(this.country.name, this.country,"(",this.lpj.cow,")","\n")
		
		this.country.cft<-getThisCountry(cft=cft,cow=cow,this.country=this.lpj.cow)
		
		#1st fill pixels that contain this crop
		if(landgrab_list[[this.crop]][["lpj_band"]][1]!=0){#no action taken for crops don't have a corresponding LPJ band
			this.country.this.band.cft<-getThisBandCrops(countrycft=this.country.cft,
															thisband=landgrab_list[[this.crop]][["lpj_band"]])
			if(is.null(this.country.this.band.cft)==FALSE){
			assignThisCropPixel(cft_list=this.country.this.band.cft,
								      bands=landgrab_list[[this.crop]][["lpj_band"]],
								      totalgrabbed=grabbed.hectares)
			
			}
				
		}
	}
}
	

col<-rev(COL(100))
col[1]<-"grey"
Lon<-sort(unique(lon))
Lat<-sort(unique(lat))

for(i in 1:52){
	bitmap(paste("image/",cft.name52[i],".jpeg",sep=""),res=300,type="jpeg")
	image.plot(x=Lon,y=Lat,map.build(abs(new.cft[,i])),col=col,xlim=c(-20,60), ylim=c(-40,40))
	map(add=T)
	title(paste("cft_frac",cft.name52[i]))
	dev.off()
 }


 

