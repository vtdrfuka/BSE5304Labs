# 
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
LabNo="/Lab04"
#
# Getting our organization on for where we want to put
# Data, external programs, and our project files.
# Things are going to get messy if we don't start issolating
# our data files by Lab
#
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)

# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# WOOO HOOO... took me a few hours to find this function!
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'drfuka@vt.edu' ") 
system("git config --global user.name 'Daniel Fuka' ")
system("git config pull.rebase false")
#
# This week, we discovered some "features" that make removing and 
# re-installing the EcoHydrology Library necessary.
#
setwd(srcdir)
detach("package:EcoHydRology", unload = TRUE)
remove.packages("EcoHydRology", lib="~/R/x86_64-pc-linux-gnu-library/4.2")
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)

setwd(datadir)
#
# Should we do a gage that is easy, or deal with some reality?
#
myflowgage_id="0205551460"  # Old Friendly Gage
myflowgage_id="14216500"    # Most Frustrating Gage... lets do it!
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = "2022-03-01")

#
# This is where some folks had issues... they forgot to check their 
# watershed areas per the homework... though there were ways to fix
# it later with lower resolution DEM pull
#
print(myflowgage$area)
# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

# In the Lab02, we introduced you to a way to quickly get your WX Data 
# for any location in the world way easier than traditional download and
# parsing methods most old people use.
#
source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max="2023-02-01",targElev=myflowgage$elev,
                  method = "IDW",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
#
# Setting the projection information for the specific location
#
proj4_utm = paste0("+proj=utm +zone=", trunc((180+myflowgage$declon)/6+1), " +datum=WGS84 +units=m +no_defs")

# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"

# Now we will build our proj4strings which define our “Coordinate 
# Reference Systems” or CRS in future geographic manipulations. 
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
#
# Double chec

latlon <- cbind(myflowgage$declon,myflowgage$declat)
myflowgage$gagepoint_ll <- SpatialPoints(latlon)
proj4string(myflowgage$gagepoint_ll)=proj4_ll
myflowgage$gagepoint_utm=spTransform(myflowgage$gagepoint_ll,crs_utm)
# Open up maps.google.com to guesstimate area/lengths
url=paste0("https://www.google.com/maps/@",
           myflowgage$declat,",",myflowgage$declon,",18z")
browseURL(url)
# We are going to over estimate our area
# For our search we are going to multiply the area by 6 and
# to get the distance
searchlength=sqrt(myflowgage$area*8)*1000 
pourpoint=SpatialPoints(myflowgage$gagepoint_utm@coords,proj4string = crs_utm)
bboxpts=myflowgage$gagepoint_utm@coords
bboxpts=rbind(bboxpts,bboxpts+searchlength)
bboxpts=rbind(bboxpts,bboxpts-searchlength)
bboxpts
bboxpts=rbind(bboxpts,c(min(bboxpts[,1]),max(bboxpts[,2])))
bboxpts=rbind(bboxpts,c(max(bboxpts[,1]),min(bboxpts[,2])))
bboxpts
bboxpts=SpatialPoints(bboxpts,proj4string = crs_utm)
# From Lab04, get your DEM
mydem=get_aws_terrain(locations=bboxpts@coords, 
                      z = 10, prj = proj4_utm,src ="aws",expand=1)
res(mydem)
plot(mydem)
plot(bboxpts,add=T)
plot(pourpoint,add=T,col="red")

# Write our raster to a geotiff file that can be used with
# OS level hydrological models 
writeRaster(mydem,filename = "mydem.tif",overwrite=T)
# Our quick intro to terminal where the cloud offerings are usually Linux
# ls; cd ~; pwd;  # Linux/Mac 
# dir; cd ; # Windows

#
# I am going to set two different zoom levels so I can inspect 
# the TauDEM Processing below.
#

zoomext=myflowgage$gagepoint_utm@coords
zoomext=rbind(zoomext,zoomext+res(mydem)*100)
zoomext=rbind(zoomext,zoomext-res(mydem)*100)
zoomext=SpatialPoints(zoomext,proj4string = crs_utm)  
zoomext2=myflowgage$gagepoint_utm@coords
zoomext2=rbind(zoomext2,zoomext2+res(mydem)*10)
zoomext2=rbind(zoomext2,zoomext2-res(mydem)*10)
zoomext2=SpatialPoints(zoomext2,proj4string = crs_utm)  
zoom(mydem,ext=zoomext2)
plot(pourpoint,add=T,col="red")

# If you already installed this in your ~/src directory and it 
# worked... you really 
# cd ~/src/      # Set your directory to your home directory
# git clone https://github.com/dtarb/TauDEM.git
# mkdir ~/src/TauDEM/bin
# cd ~/src/TauDEM/src
# sed -i -e 's/MPI_Type_struct/MPI_Type_create_struct/g' linklib.h
## yes, this next line is very small font, but it is one line so...
# sed -i -e 's/MPI_Type_extent(MPI_LONG, \&extent)/MPI_Aint lb\;MPI_Type_get_extent(MPI_LONG, \&lb, \&extent)/g' linklib.h
## Now let's try make again!
# make

rm("old_path")
old_path <- Sys.getenv("PATH")
old_path

if( ! grepl("~/src/TauDEM/bin",old_path)){
  Sys.setenv(PATH = paste(old_path,
                          paste0(Sys.getenv("HOME"),"/src/TauDEM/bin"), 
                          sep = ":"))
}

system("mpirun aread8")

setwd(datadir)
z=raster("mydem.tif")
plot(z)

# Pitremove
system("mpiexec -n 2 pitremove -z mydem.tif -fel mydemfel.tif")
fel=raster("mydemfel.tif")
plot(fel-z)


# D8 flow directions
system("mpiexec -n 2 d8flowdir -p mydemp.tif -sd8 mydemsd8.tif -fel mydemfel.tif",show.output.on.console=F,invisible=F)
p=raster("mydemp.tif")
plot(p)
sd8=raster("mydemsd8.tif")
plot(sd8)

# Contributing area
system("mpiexec -n 2 aread8 -p mydemp.tif -ad8 mydemad8.tif")
ad8=raster("mydemad8.tif")
plot(log(ad8))
zoom(log(ad8),ext=zoomext2)
plot(pourpoint,add=T)

# Grid Network 
system("mpiexec -n 2 gridnet -p mydemp.tif -gord mydemgord.tif -plen mydemplen.tif -tlen mydemtlen.tif")
gord=raster("mydemgord.tif")
plot(gord)
zoom(gord,ext=zoomext2)

# DInf flow directions
system("mpiexec -n 2 dinfflowdir -ang mydemang.tif -slp mydemslp.tif -fel mydemfel.tif",show.output.on.console=F,invisible=F)
ang=raster("mydemang.tif")
plot(ang)
slp=raster("mydemslp.tif")
plot(slp)

# Dinf contributing area
system("mpiexec -n 2 areadinf -ang mydemang.tif -sca mydemsca.tif")
sca=raster("mydemsca.tif")
plot(log(sca))
zoom(log(sca),ext=zoomext2)

targetbasins=3  # Lets figure out some number of sub basins we want
# to break this into
res(mydem)      # Cell Resolution in meters
myflowgage$area # Area in km^2
myflowgage$area * 10^3 * 10^3 / res(mydem)[1]^2 / targetbasins
subthreshold=myflowgage$area * 10^3 * 10^3 / res(mydem)[1]^2 / targetbasins
subthreshold=as.integer(subthreshold)
# Threshold
syscmd=paste0("mpiexec -n 2 threshold -ssa mydemad8.tif -src mydemsrc.tif -thresh ",subthreshold)
system(syscmd)
src=raster("mydemsrc.tif")
plot(src)
zoom(src,ext=zoomext2)
plot(pourpoint,add=T)

outlet=SpatialPointsDataFrame(myflowgage$gagepoint_utm,
                              data.frame(Id=c(1),outlet=paste("outlet",1,sep="")))
writeOGR(outlet,dsn=".",layer="approxoutlets",
         driver="ESRI Shapefile", overwrite_layer=TRUE)
#

# Move Outlets
system("mpiexec -n 2 moveoutletstostrm -p mydemp.tif -src mydemsrc.tif -o approxoutlets.shp -om outlet.shp")

approxpt=readOGR("approxoutlets.shp")
plot(approxpt,add=T, col="blue")
outpt=readOGR("outlet.shp")
plot(outpt,add=T, col="red")

# Contributing area upstream of outlet
# Now that we know the location of an outlet, we can isolate our basin 
#
system("mpiexec -n 2 aread8 -p mydemp.tif -o outlet.shp -ad8 mydemssa.tif")
ssa=raster("mydemssa.tif")
plot(ssa) 

# Threshold
system("mpiexec -n 2 threshold -ssa mydemssa.tif -src mydemsrc1.tif -thresh 2000")
syscmd=paste0("mpiexec -n 2 threshold -ssa mydemssa.tif -src mydemsrc1.tif -thresh ",subthreshold)
system(syscmd)
src1=raster("mydemsrc1.tif")
plot(src1)
zoom(src1,ext=zoomext)

# Stream Reach and Watershed
system("mpiexec -n 2 streamnet -fel mydemfel.tif -p mydemp.tif -ad8 mydemad8.tif -src mydemsrc1.tif -o outlet.shp -ord mydemord.tif -tree mydemtree.txt -coord mydemcoord.txt -net mydemnet.shp -w mydemw.tif")
plot(raster("mydemord.tif"))
zoom(raster("mydemord.tif"),ext=zoomext2)
mydemw=raster("mydemw.tif")
zoom(mydemw,ext=zoomext)
summary(mydemw)
plot(mydemw)

# Trimming, Cropping, and Masking to make life prettier and easier
mydemw=raster("mydemw.tif")
mybasinmask=trim(mydemw,padding=2)
mydem=raster("mydem.tif")
mybasindem=crop(mydem,mybasinmask)
mybasindem=mask(mybasindem,mybasinmask)
plot(mybasindem)

# Make a poly with raster library (slow)
# or from thee command line gdal (fast)
# gdal_polygonize.py -8 mydemw.tif mydemw_poly_gdal.shp
mydemw=rast("mydemw.tif")
mydemw_poly=as.polygons(mydemw,na.rm=T)
plot(mydemw_poly,add=T,col=rainbow(6))

writeVector(mydemw_poly,dsn=".",layer="mydemw",driver="ESRI Shapefile", overwrite_layer=TRUE)
writeVector(mydemw_poly, filename="mydemw.shp", filetype="ESRI Shapefile", layer="mydemw", insert=FALSE,
            overwrite=TRUE)


ssurgo.geom <- SDA_spatialQuery(
  mydemw_poly,
  what = 'mupolygon',
  db = 'SSURGO',
  geomIntersection = TRUE
)

ssurgo.geom_utm=project(ssurgo.geom,crs_utm)
plot(ssurgo.geom_utm,col=rainbow(length(ssurgo.geom_utm)))
plot(mydemw_poly,add=T)
ssurgo.geom_utm_crop=crop(ssurgo.geom_utm,mydemw_poly)

plot(ssurgo.geom_utm_crop,col=rainbow(length(ssurgo.geom_utm)))
plot(mydemw_poly,add=T)
#
# Get a list of the Map Units to use in accessing the soil data
#
unique(ssurgo.geom_utm_crop$mukey)
mukey_statement = format_SQL_in_statement(unique(ssurgo.geom_utm_crop$mukey))
print(mukey_statement)
q_mu2co = paste("SELECT mukey,cokey FROM component WHERE mukey IN ", mukey_statement, sep="")
print(q_mu2co)
mu2co = SDA_query(q_mu2co)
head(mu2co)
summary(mu2co)

# Second associate cokey with ksat_r,awc_r,hzdepb_r from chorizon
cokey_statement = format_SQL_in_statement(unique(mu2co$cokey))
q_co2ch = paste("SELECT cokey,ksat_r,awc_r,hzdepb_r  FROM chorizon WHERE cokey IN ", cokey_statement, sep="")
print(q_co2ch)
co2ch = SDA_query(q_co2ch)
# Last, bring them back together, and aggregate based on max values
# of ksat_r,awc_r, and hzdepb_r
mu2ch=merge(mu2co,co2ch)
View(mu2ch)
summary(mu2ch)
mu2chmax=aggregate(mu2ch,list(mu2ch$mukey),max)
summary(mu2chmax)   	# What should we do with NAs?
# What do we have here vs our model for AWC?
soilmap=merge(ssurgo.geom_utm_crop,mu2chmax,all=T)

par(mfrow=c(2,2))
plot(soilmap,y="ksat_r")
plot(soilmap,y="awc_r")
plot(soilmap,y="hzdepb_r")
plot(mydemw_poly,main="Subbasins",col=rainbow(length(mydemw_poly)))

soilmap$ksat_r[is.na(soilmap$ksat_r)]=mean(soilmap$ksat_r,na.rm=T)
soilmap$awc_r[is.na(soilmap$awc_r)]=mean(soilmap$awc_r,na.rm=T)
soilmap$hzdepb_r[is.na(soilmap$hzdepb_r)]=mean(soilmap$hzdepb_r,na.rm=T)



par(mfrow=c(1,1))
# Lab 04 TI 
slp=raster("mydemslp.tif")
plot(slp,ext=zoomext)
sca=raster("mydemsca.tif")
plot(log(sca),e=zoomext)
TI = log( (sca+1)/(slp+0.00001) )
plot(TI)
zoom(log(TI),e=zoomext)
#
# To make things easier, while adding some confusion
# we will convert raster objects to "terra" supported SpatRasters
# and back again to use features that are unique to each
#
TI_terra=rast(TI)
TI_terra=crop(mask(TI_terra,mydemw_poly),mydemw_poly)
#
# Why would we want to mask the TI to the watershed
# boundaries before we build TI Classes?
# 
TI=raster(TI_terra)
pacman::p_load(classInt)
nTIclass=5 #number of TI classes, currently equal area, can adjust method various ways e.g., classIntervals(v, n = nTIclass, style = "jenks")
v=values(TI)
v=v[!is.na(v)]
brks.qt = classIntervals(v, n = nTIclass, style = "quantile")$brks #length nTIclass+1 of just the numeric breakpoints

TIC = cut(TI, breaks=brks.qt, include.lowest = T, right=T)
plot(TIC)
TIC_terra=rast(TIC)
plot(TIC_terra)


# Lab 04 Calibration
# Building more complex functions
# 

TMWB=BasinData
#
# Our model will
# 1) Calculate PET for the basin via Function
# 2) Calculate the Snow Accumulation and Melt via Function
# 3) Run TMWB via Function 
#
#
# First functions from last week we already have, Wetting, Drying, 
# and Wetting above capacity 
# 
# soil wetting function
soilwetting<-function(AWprev,dP_func,AWC_func){
  AW_func<-AWprev+dP_func
  excess_func<-0.0
  c(AW_func,excess_func)
} 
# soil drying function
soildrying<-function(AWprev,dP_func,AWC_func){
  AW_func=AWprev*exp(dP_func/AWC_func)
  excess_func<-0.0
  c(AW_func,excess_func)
}
# soil_wetting_above_capacity function
soil_wetting_above_capacity<-function(AWprev,dP_func,AWC_func){
  AW_func<-AWC_func
  excess_func<-AWprev+dP_func-AWC_func
  c(AW_func,excess_func)
}

#
# Lets make one out of our Temperature Index Snow Model
#

SFTmp = 3  # referred to as SFTMP in SWAT input (Table 1)
bmlt6 = 4.5   # referred to as SMFMX in SWAT input (Table 1)
bmlt12 = 0.0  # referred to as SMFMN in SWAT input adjusted for season
Tmlt = SFTmp  # Assumed to be same as SnowFall Temperature
Tlag = 1  # referred to as TIMP in SWAT input (Table 1)

TISnow=function(WBData,SFTmp=2,bmlt6=4.5,bmlt12=0.0,Tmlt=3,Tlag=1){
  WBData$AvgTemp=(WBData$MaxTemp-WBData$MinTemp)/2
  WBData$bmlt = (bmlt6 + bmlt12)/2 + (bmlt6 - bmlt12)/2 * 
    sin(2*pi/365*(julian(WBData$date,origin = as.Date("2000-01-01"))-81))
  # Initialize SNO, Tsno as well as the first values of each
  WBData$SNO = 0  # Snow Depth (mm)
  WBData$Tsno = 0  # Snow Temp (C)
  WBData$SNOmlt = 0  # Snow Melt (mm)
  WBData$SNOfall = 0  # Snow Fall (mm)
  attach(WBData)
  for (t in 2:length(date)){
    Tsno[t]= Tsno[t-1] * (1.0-Tlag) +  AvgTemp[t] * Tlag
    if(AvgTemp[t] < SFTmp){
      SNO[t]= SNO[t-1] + P[t]
      #
      # Eeee... I forgot to save my snowfall!
      #
      SNOfall=P[t]
    }  else {
      SNOmlt[t]= bmlt[t] * SNO[t-1] * ((Tsno[t]+MaxTemp[t])/2 - Tmlt) 
      SNOmlt[t]= min(SNOmlt[t],SNO[t-1])
      SNO[t]= SNO[t-1] -SNOmlt[t]
    }
    print(t)
  }
  plot(date,SNO,type="l")
  detach(WBData)
  WBData$Tsno=Tsno
  WBData$SNO=SNO
  WBData$SNOmlt=SNOmlt
  WBData$SNOmlt=SNOfall
  rm(list=c("SNO", "SNOmlt", "Tsno"))
  return(data.frame(Tsno=WBData$Tsno,SNO=WBData$SNO,SNOmlt=WBData$SNOmlt,SNOfall=WBData$SNOfall))
}



SNO_df=TISnow(TMWB)
TMWB$SNO=SNO_df$SNO
TMWB$SNOmlt=SNO_df$SNOmlt
TMWB$SNOfall=SNO_df$SNOfall
TMWB$Tsno=SNO_df$Tsno
detach(TMWB)
#
# Our PET Model we will borrow from EcoHydrology
#
?PET_fromTemp
TMWB$PET=PET_fromTemp(Jday=(1+as.POSIXlt(date)$yday),Tmax_C = MaxTemp,Tmin_C = MinTemp,
                      lat_radians = myflowgage$declat*pi/180) * 1000
plot(date,TMWB$PET)


# Our TMWB Model

attach(TMWB)
detach(TMWB)


TMWB$ET = TMWB$PET # in mm/day
TMWB$AWC=(0.45-0.15)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm
TMWB$dP = TMWB$P-TMWB$ET -TMWB$SNO + TMWB$SNOmlt 

attach(TMWB)# Remember to detach or it gets ugly
plot(date,Qmm,type = "l",col="black")
lines(date,P,type = "l",col="red")
lines(date,Qmm,type = "l",col="black") # We repeat to have Qmm on top of P
lines(date,ET,type = "l",col="blue")
legend("topright", c("P", "Qmm", "ET"), col = c("red", "black", "blue"),
       lty = 1:2, cex = 0.8)
detach(TMWB) # IMPORTANT TO DETACH


TMWB$AWC=(0.45-0.15)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm


TMWB$AW=NA  #Assigns all values in column with “NA” (Not available)
TMWB$AW[1]=250
TMWB$Excess=NA
TMWB$Excess[1]=0
head(TMWB)

# Here we go looping through our functions….

attach(TMWB)
for (t in 2:length(date)){
  if (dP[t]< 0) {  
    values<-soildrying(AW[t-1],dP[t],AWC[t])
  } else if (AW[t-1]+dP[t]>AWC[t]) {
    values<-soil_wetting_above_capacity(AW[t-1],dP[t],AWC[t])
  } else {
    values<-soilwetting (AW[t-1],dP[t],AWC[t])
  }
  AW[t]<-values[1]
  Excess[t]<-values[2]
}

detach(TMWB)
TMWB$AW <-AW
TMWB$Excess<-Excess
rm(list=c("AW","Excess"))

# Calculate Watershed Storage and River Discharge: 
TMWB$Qpred=NA
TMWB$Qpred[1]=0
TMWB$S=NA
TMWB$S[1]=0

attach(TMWB)
fcres=.3   # reservoir coefficient
for (t in 2:length(date)){
  S[t]=S[t-1]+Excess[t]     
  Qpred[t]=fcres*S[t]
  S[t]=S[t]-Qpred[t]
}
detach(TMWB) # IMPORTANT TO DETACH
TMWB$S=S
TMWB$Qpred=Qpred # UPDATE vector BEFORE DETACHING
rm(list=c("S","Qpred"))
View(TMWB)
dev.off()
plot(TMWB$date,TMWB$Qmm,col="black",ylab ="Qmm(mm)",xlab="date",type="l")
lines(TMWB$date,TMWB$Qpred,col="blue",type="l", 
      xlab = "", ylab = "")
legend("topright", c("Qmm(mm)", "Qpred(mm)"), col = c("black", "blue"),
       lty = 1:2, cex = 0.8)

myflowgage$FldCap=.45
myflowgage$WiltPt=.15
myflowgage$Z=1000
TMWB$AWC=(myflowgage$FldCap-myflowgage$WiltPt)*myflowgage$Z # 
TMWB$dP = 0 # Initializing Net Precipitation
TMWB$ET = 0 # Initializing ET
TMWB$AW = 0 # Initializing AW
TMWB$Excess = 0 # Initializing Excess


# Loop to calculate AW and Excess
attach(TMWB)
for (t in 2:length(AW)){
  # This is where Net Precipitation is now calculated
  # Do you remember what Net Precip is? Refer to week 2 notes
  # Update this to reflect the ET model described above
  
  ET[t] = (AW[t-1]/AWC[t-1])*PET[t] # New Model
  dP[t] = P[t] - ET[t] + SNOmlt[t] - SNOfall[t] 
  # From here onward, everything is the same as Week2’s lab
  if (dP[t]<=0) {
    values<-soildrying(AW[t-1],dP[t],AWC[t])
  } else if((dP[t]>0) & (AW[t-1]+dP[t])<=AWC[t]) {
    values<-soilwetting(AW[t-1],dP[t],AWC[t])
  } else {
    values<-soil_wetting_above_capacity(AW[t-1],dP[t],AWC[t])
  }
  AW[t]<-values[1]
  Excess[t]<-values[2]
  print(t)
}
TMWB$AW=AW
TMWB$Excess=Excess
TMWB$dP=dP
TMWB$ET=ET
rm(list=c("AW","dP","ET", "Excess"))
detach(TMWB) # IMPORTANT TO DETACH

# Calculate Watershed Storage and River Discharge, S and Qpred, playing with the reservoir coefficient to try to get Qpred to best match Qmm

TMWB$Qpred=NA
TMWB$Qpred[1]=0
TMWB$S=NA
TMWB$S[1]=0
attach(TMWB)
fcres=.3
for (t in 2:length(date)){
  S[t]=S[t-1]+Excess[t]     
  Qpred[t]=fcres*S[t]
  S[t]=S[t]-Qpred[t]
}
TMWB$S=S
TMWB$Qpred=Qpred # UPDATE vector BEFORE DETACHING
detach(TMWB) # IMPORTANT TO DETACH
rm(list=c("Qpred","S"))

#Make a plot that has Qmm, P,and Qpred over time
plot(TMWB$date,P,col="black")
lines(date,Qmm,type = "l",col="black")
lines(date,Qpred,col="blue")

#
# Functionalizing big big big time
# Here is a great place to make this into a function!
# return(TMWB)

