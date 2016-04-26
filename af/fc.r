#read Africa runs and picks up certain pixels, writes out maps and table
# source("/home/mfader/_AndereProjekte/LandGrab/NewRuns/land_grab2/fc.r")

#needs in progs.path
#Functions: read.input.r and read.output.r
#Deal list: af.fader.csv
#contry codes: af.fader.csv
#cow full and input grid

#LPJ outputs in data.path

#change paths
#choose of you are calculating the rainfed or the irrigated part in a<-read.csv(...
#look FRESHMATTER!!
#control years
#control Zuordnung crops in nuevo*
#change outtable name
#change directory name output for maps

rm(list=ls(all=TRUE))
library(fields)

NATEFF<-T #NATEFF,SURFACE,MIXED,SPRINKLER,DRIP,IMP
SURFACE<-F
MIXED<-F
SPRINKLER<-F
DRIP<-F
IMP<-F

MUL<-T
UNMUL<-F #MUL or UNMUL multiplied by area or not

IRRIGATED<-T
RAINFED<-F

ncell <- 12079
nbands <- 52
startyear<-1901
year<-c(2000)
band<-c(1:52)


progs.path<-"./"

if(NATEFF) data.path    <-  "./Bananas_NatEff/"
if(SURFACE) data.path   <-  "./Bananas_Surface/"
if(MIXED) data.path     <-  "./Bananas_Mixed/"
if(SPRINKLER) data.path <-  "./Bananas_Sprinkler/"
if(DRIP) data.path      <-  "./Bananas_Drip/"
if(IMP) data.path       <-  "./Bananas_Imp/"

source(paste(progs.path,"read.input.r",sep=""))
source(paste(progs.path,"read.output.r",sep=""))

#with apples in non-citrus fruits
#FRESHMATTER <-  100 / c(88, 87, 88, 88, 90, 24,20, 35,93, 91, 94, 92, 27, 100, 25 ,100 ,100,13,16,70,30,90,20,60,91,25,
#			88, 87, 88, 88, 90, 24,20, 35, 93, 91, 94, 92, 27, 100, 25,100 ,100,13,16,70,30,90,20,60,91,25) 

#with bananas in non-citrus fruits
FRESHMATTER <-  100 / c(88, 87, 88, 88, 90, 24,20, 35,93, 91, 94, 92, 27, 100, 25 ,100 ,100,13,26,70,30,90,20,60,91,25,
			88, 87, 88, 88, 90, 24,20, 35, 93, 91, 94, 92, 27, 100, 25,100 ,100,13,26,70,30,90,20,60,91,25) 
			

find_closest<-function(plon,plat, glon,glat){
	dis<-sqrt((glon-plon)^2+(glat-plat)^2)
	return(which(dis==min(dis))[1])
}
#IRRIGATED
if(IRRIGATED) a<-read.csv(paste(progs.path,"nuevo2_irrig.csv",sep=""),sep=",")

#RAINFED
if(RAINFED) a<-read.csv(paste(progs.path,"nuevo2_rainf.csv",sep=""),sep=",")

print("FLAG##############################")

b<-read.csv(paste(progs.path,"af.fader.csv",sep=""),sep="\t")
lpj_band<-strsplit(as.character(a$LPJ.output.band),split=",")
cow.id<-read.csv(paste(progs.path,"ListCountries.csv",sep=""),sep=";")
cow<-read.input.yearband(filename=paste(progs.path,"cow_mg_2006_full_12079p_h43.bin",sep=""),year=1901,band=1,data.size=2)
read.input.grid(paste(progs.path,"grid_12079p_h43.bin",sep=""))

crop<-as.character(b$crop)

cropnames<-unique(crop)
crop_band_list<-list()

for(i in 1:length(unique(crop))){
	crop_band_list[[cropnames[i]]]<-lpj_band[[which(a$crop==cropnames[i])]]
}


target.country<-unique(as.vector(b$target.country))

#check table country names and LPJ country names by comparing with prepared countrylist
for(i in target.country){
	#cat(i,"--",as.character(cow.id$name[which(cow.id$name==i)]),"\n")
	if(length(as.character(cow.id$name[which(cow.id$name==i)]))!=1)
	cat(i, "can not be found in LPJ cow names\n")
}

print(paste(data.path, "cftfrac.bin", sep=""))
cft_frac<-read.output.harvest(paste(data.path,"cftfrac.bin",sep=""),ncells=ncell,nbands=nbands,
			     startyear=startyear,year=year,data.size=4,band=band)
cft_nir<-read.output.harvest(paste(data.path,"cft_nir.pft.bin",sep=""),ncells=ncell,nbands=nbands,
			     startyear=startyear,year=year,data.size=4,band=band)
pft_harvest<-read.output.harvest(paste(data.path,"pft_harvest.pft.bin",sep=""),ncells=ncell,nbands=nbands,
			     startyear=startyear,year=year,data.size=4,band=band,
			     par=1/0.45*0.01*FRESHMATTER)
pft_irrig<-read.output.harvest(paste(data.path,"pft_irrig.pft.bin",sep=""),ncells=ncell,nbands=nbands,
			     startyear=startyear,year=year,data.size=4,band=band)
cft_water_g<-read.output.harvest(paste(data.path,"cft_consump_water_g.pft.bin",sep=""),ncells=ncell,nbands=nbands,
			     startyear=startyear,year=year,data.size=4,band=band)
cft_water_b<-read.output.harvest(paste(data.path,"cft_consump_water_b.pft.bin",sep=""),ncells=ncell,nbands=nbands,
			     startyear=startyear,year=year,data.size=4,band=band)

#mean over years
outdata<-list()
area<-deg2area(lat) #in ha
if(MUL){
outdata$cft_nir<-rowMeans(cft_nir$data*10*cft_frac$data,dims=2) *area # m3
outdata$cft_water_b<-rowMeans(cft_frac$data*cft_water_b$data*10,dims=2)*area # m3
outdata$cft_water_g<-rowMeans(cft_frac$data*cft_water_g$data*10,dims=2)*area # m3
outdata$yields<-rowMeans(cft_frac$data*pft_harvest$data,dims=2)*area #tons, production
outdata$pft_harvest<-rowMeans(pft_harvest$data,dims=2) #t/ha, yields
outdata$pft_irrig<-rowMeans(cft_frac$data*pft_irrig$data*10,dims=2)*area #m3
outdata$cft_frac2<-rowMeans(cft_frac$data,dims=2) #frac
outdata$cft_frac<-rowMeans(cft_frac$data,dims=2)*area #ha
}

# #without multiplying per area
if(UNMUL){
outdata$cft_nir<-rowMeans(cft_nir$data*10,dims=2) # m3/ha
outdata$cft_water_b<-rowMeans(cft_water_b$data*10,dims=2)# m3/ha
outdata$cft_water_g<-rowMeans(cft_water_g$data*10,dims=2)# m3/ha
outdata$yields<-rowMeans(cft_frac$data*pft_harvest$data,dims=2)*area #tons, production
outdata$pft_harvest<-rowMeans(pft_harvest$data,dims=2) #t/ha, yields
outdata$pft_irrig<-rowMeans(pft_irrig$data*10,dims=2)#m3/ha
outdata$cft_frac2<-rowMeans(cft_frac$data,dims=2) #frac
outdata$cft_frac<-rowMeans(cft_frac$data,dims=2)*area #ha
}

cft_nir_p<-vector()
cft_water_b_p<-vector()
cft_water_g_p<-vector()
yields_p<-vector()
yields_irrig_p<-vector()
pft_harvest_p<-vector()
pft_irrig_p<-vector()
cft_frac_p<-vector()
cft_frac_2<-vector()
area_p<-vector()
#sum over bands for the cases of more than one correspondence, NOTE:make division in posst-processing
for(i in 1:length(b$lon)){
	pos_index<-find_closest(b$lon[i],b$lat[i],lon,lat)
	band_index<-as.numeric(crop_band_list[[as.character(b$crop[i])]])

	cft_nir_p[i]<-sum(outdata$cft_nir[pos_index,band_index]) 
	cft_water_b_p[i]<-sum(outdata$cft_water_b[pos_index,band_index])
	cft_water_g_p[i]<-sum(outdata$cft_water_g[pos_index,band_index])
	yields_p[i]<-sum(outdata$yields[pos_index,band_index])
	yields_irrig_p[i]<-sum(outdata$yields_irrig[pos_index,band_index])
	pft_harvest_p[i]<-sum(outdata$pft_harvest[pos_index,band_index])
	pft_irrig_p[i]<-sum(outdata$pft_irrig[pos_index,band_index])
	cft_frac_p[i]<-sum(outdata$cft_frac[pos_index,band_index])
 	cft_frac_2[i]<-sum(outdata$cft_frac2[pos_index,band_index])
 	area_p[i]<-area[pos_index]
}

#write table
out_table_r<-data.frame(b, "nir_m3_r"=cft_nir_p,"water_b_m3_r"=cft_water_b_p,"water_g_m3_r"=cft_water_g_p,
		      "production_tFM_r"=yields_p, "yield_tFMperHA_r"=pft_harvest_p,
		      "IR_m3_r"=pft_irrig_p,"area_ha_r"=cft_frac_p,"cft_frac"=cft_frac_2,"area_ha_pixel"=area_p)
if(MUL) out_table_i<-data.frame(b, "nir_m3_i"=cft_nir_p,"water_b_m3_i"=cft_water_b_p,"water_g_m3_i"=cft_water_g_p,
		      "production_tFM_i"=yields_p, "yield_tFMperHA_i"=pft_harvest_p,
		      "IR_m3_i"=pft_irrig_p,"area_ha_i"=cft_frac_p)

# #without multiplying per area
if(UNMUL) out_table_i<-data.frame(b, "nir_m3Ph_i"=cft_nir_p,"water_b_m3Ph_i"=cft_water_b_p,"water_g_m3Ph_i"=cft_water_g_p,
		      "production_tFM_i"=yields_p, "yield_tFMperHA_i"=pft_harvest_p,
		      "IR_m3Ph_i"=pft_irrig_p,"area_ha_i"=cft_frac_p)

if(RAINFED) write.csv(file="out_table_rainfed.csv",out_table_r)
if(RAINFED==F & NATEFF==T) write.csv(file="out_table_nateffic.csv",out_table_i)
if(SURFACE) write.csv(file="out_table_surface.csv",out_table_i)
if(UNMUL) write.csv(file="out_table_withoutMultPerArea.csv",out_table_i) #without multiplication per area
if(MIXED) write.csv(file="out_table_mixed.csv",out_table_i)
if(SPRINKLER) write.csv(file="out_table_sprinkler.csv",out_table_i)
if(DRIP) write.csv(file="out_table_drip.csv",out_table_i)
if(IMP) write.csv(file="out_table_imp.csv",out_table_i)

#visualise
for(i in 1:52){
	var_name<-names(outdata)
	for(vn in var_name){
		#browser()
		if(RAINFED) bitmap(paste("image_rainfed/",vn,"_band",i,".jpeg",sep=""))
		if(RAINFED==F & NATEFF==T) bitmap(paste("image_nateffic/",vn,"_band",i,".jpeg",sep=""))
		if(SURFACE) bitmap(paste("image_surface/",vn,"_band",i,".jpeg",sep=""))
		if(UNMUL) bitmap(paste("image_2/",vn,"_band",i,".jpeg",sep="")) #without multiplication per ha
		if(MIXED) bitmap(paste("image_mixed/",vn,"_band",i,".jpeg",sep=""))
		if(SPRINKLER) bitmap(paste("image_sprinkler/",vn,"_band",i,".jpeg",sep=""))
		if(DRIP) bitmap(paste("image_drip/",vn,"_band",i,".jpeg",sep=""))
		if(IMP) bitmap(paste("image_imp/",vn,"_band",i,".jpeg",sep=""))
		image.plot(map.build(outdata[[vn]][,i]))
		dev.off()
	}
}
	
# 1-cft_nir.pft.bin
# output*10*frac*(area.of.grid/100/100)
# 
# NAME->NIR_m3
# 
# 
# 2-pft_irrig.pft.bin
# 
# output*10*frac*(area.of.grid/100/100)
# 
# NAME->IR_m3
# 
# 
# 3-cft_consump_water_g.pft.bin
# 
# output*10*frac*(area.of.grid/100/100)
# 
# NAME->Consump_g_m3
# 
# 
# 4-cft_consump_water_b.pft.bin
# 
# output*10*frac*(area.of.grid/100/100)
# 
# NAME->Consump_b_m3
# 
# 
# 5-cftfrac.bin
# output*(area.of.grid/100/100)
# 
# NAME->area_ha
# 
# 
# 6-pft_harvest.pft.bin
# ((output/0.45)/100)*frac*(area.of.grid/100/100)*W.CONTENT
# NAME->production_tFM
# 
# 7-pft_harvest.pft.bin
# ((output/0.45)/100)*W.CONTENT
# NAME->yield_tFMperHA 
