# 
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
LabNo="/Lab10"
#
# Getting our organization on for where we want to put
# Data, external programs, and our project files.
# Things are going to get messy if we don't start isolating
# our data files by Lab
#
user=Sys.getenv("USER")
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
system(paste0("git config --global user.email '",user,"@vt.edu'")) 
system("git config --global user.name 'Daniel Fuka' ")
system("git config pull.rebase false")
#
# This week, we discovered some "features" that make removing and 
# re-installing the EcoHydrology Library necessary.
#
setwd(srcdir)

#detach("package:EcoHydRology", unload = TRUE)
#remove.packages("EcoHydRology", lib="~/R/x86_64-pc-linux-gnu-library/4.2")
#system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
#install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)
setwd(datadir)
#
# Should we do a gage that is easy, or deal with some reality?
#
myflowgage_id="0205551460"  # Old Friendly Gage
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
                      z = 12, prj = proj4_utm,src ="aws",expand=1)
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
q_co2ch = paste("SELECT cokey,ksat_r,awc_r,hzdepb_r,frag3to10_r FROM chorizon WHERE cokey IN ", cokey_statement, sep="")
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



TMWB=BasinData
#
# Our model will
# 1) Calculate PET for the basin via Function
# 2) Calculate the Snow Accumulation and Melt via Function
# 3) Run TMWB via Function 

source("https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TMWBFuncs.R")
source("https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TISnow.R")
#
# Lets make one out of our Temperature Index Snow Model
#

SNO_df=TISnow(TMWB,SFTmp = 2,bmlt6 = 3,bmlt12 = 0,Tmlt = 3,Tlag = 1)
TMWB$SNO=SNO_df$SNO
TMWB$SNOmlt=SNO_df$SNOmlt
TMWB$SNOfall=SNO_df$SNOfall
TMWB$Tsno=SNO_df$Tsno
#
# Our PET Model we will borrow from EcoHydrology
#

TMWB$PET=PET_fromTemp(Jday=(1+as.POSIXlt(TMWB$date)$yday),
                      Tmax_C = TMWB$MaxTemp,Tmin_C = TMWB$MinTemp,
                      lat_radians = myflowgage$declat*pi/180) * 1000
plot(TMWB$date,TMWB$PET)

TMWBnew=TMWBmodel(TMWB)

#
# Functionalizing big big big time
# Here is a great place to make this into a function!
# return(TMWB)


BasinTMWB_JO=TMWBnew[(month(TMWBnew$date) > 5 
                      & month(TMWBnew$date) < 11),]
attach(BasinTMWB_JO)
plot(dP,Qmm)
detach(BasinTMWB_JO)


# Keep iterating until NSE is as high as you can get for your 
# best estimate to S (Sest)
#
f <- function (x) {
  Sest=x
  return(NSE(Qmm,dP^2/(dP+Sest)))
}
attach(BasinTMWB_JO)
Sest=optimize(f, c(50,500), tol = 0.0001,maximum = TRUE)$maximum
plot(dP,Qmm)
points(dP,dP^2/(dP+Sest),col="red") 
########
detach(BasinTMWB_JO)
CN2 = (1000/85-10)*25.4   # our CN estimate in bold

mysoil_utm <- as(ssurgo.geom_utm_crop, "Spatial")
# Rasterizing for categorical analysis! 
rmysoil_utm=rasterize(mysoil_utm,TIC,field=as.numeric(mysoil_utm$mukey))
unique(rmysoil_utm)
pacman::p_load(circlize)
plot(rmysoil_utm,col=rand_color(length(unique(values(rmysoil_utm)))))
unique(rmysoil_utm)
#


mybasinslp=mask(crop(slp,rmysoil_utm),rmysoil_utm)

#
# Now build an HRU table with the combination of the 1) raster Soils, 2) TIC,
# and 3) slope layers. 
#
hru=ratify(TIC*10^9 + (rmysoil_utm*10^3) + round(mybasinslp*10+1))
unique(values(hru))
sort(unique(values(hru)))
length(unique(values(hru)))
plot(hru,col=rand_color(length(unique(values(hru)))))
# Think of how you will color this plot based on the sediment runoff you will
# calculate later.
#
# Build an HRU attribute table
hru_table = levels(hru)[[1]]
origID = hru_table$ID # preserve data order for later reordering
# metadata parameters from a string... this will make more sense
# after the next "head()" command
hru_table$TIclass = as.numeric(substr(sprintf("%10.0f", hru_table$ID), 1,1))
hru_table$mukey = as.numeric(substr(sprintf("%10.0f", hru_table$ID), 2,7))
hru_table$slp = (as.numeric(substr(sprintf("%10.0f", 
                                             hru_table$ID), 8,10))-1)/10
#
# Calculate the area for each unique soil (mukey) X TIClass combination
# using res(raster) for x and y resolution in units of m
# Note that table() function returns the count of the occurrences of
# unique values in the hru raster cells.
hru_table$areaSQKM = as.vector(round(res(hru)[1]*res(hru)[2]*
                                         table(values(hru))/10^6, 3))

View(mu2co)
View(co2ch)

cokey_statement = format_SQL_in_statement(unique(mu2co$cokey))
q_co2co = paste("SELECT cokey,slopelenusle_r FROM component WHERE cokey IN ", cokey_statement, sep="")
co2co=SDA_query(q_co2co)
# Last, bring them back together, and aggregate based on max values
# of ksat_r,awc_r, and hzdepb_r
mu2ch=merge(mu2co,co2ch)
mu2ch=merge(mu2ch,co2co)
View(mu2ch)

MUSLE_mrg=merge(hru_table,mu2ch)   
MUSLE_mrg$ksat_r=as.numeric(MUSLE_mrg$ksat_r)
MUSLE_mrg$awc_r=as.numeric(MUSLE_mrg$awc_r)
MUSLE_mrg$hzdepb_r=as.numeric(MUSLE_mrg$hzdepb_r)
MUSLE_mrg$slopelenusle_r=as.numeric(MUSLE_mrg$slopelenusle_r)
MUSLE_mrg$frag3to10_r=as.numeric(MUSLE_mrg$frag3to10_r)
MUSLE=aggregate(MUSLE_mrg,list(MUSLE_mrg$ID),mean,na.rm=T)

MUSLE=aggregate(MUSLE_mrg,list(MUSLE_mrg$TIclass),mean,na.rm=T)
#
# Easiest first! Eq. 4:1.1.15 Course Fragment Factor
MUSLE$CFRG=exp(-0.053*MUSLE$frag3to10_r)
MUSLE
#
# LSusle is calculated using eq. 4.1.12
MUSLE$alpha=atan(MUSLE$slp/100)
MUSLE$LSm=.6*(1-exp(-35.835*MUSLE$slp/100))
MUSLE$LS=(MUSLE$slopelenusle_r/22.1)^MUSLE$LSm * (65.41*sin(MUSLE$alpha)^2+4.56*sin(MUSLE$alpha)+0.065)
#
# Pusle
MUSLE$Pusle=.50
#
# Cusle
MUSLE$Cusle=.20
#
# Kusle
MUSLE$Kusle=0.28
#
# Build a constant for those we are not changing day to day
attach(MUSLE)
MUSLE$KCPLSCFRG118=11.8*Kusle*Cusle*Pusle*LS*CFRG
detach(MUSLE)
MUSLE # Make sure values look correct, Pusle, Cusle, Kusle
#
# Now we need to use each of the TIClass Q solutions from Lab06 to calculate
# peak flows (qpeak) and complete the MUSLE Sediment Loss for each class.
# Run Model
#
# Now we need to use Q solutions from Lab06 to calculate
# peak flows (qpeak) and complete the MUSLE Sediment Loss for each class.
# Run Model
#source CNmodel function
Sest

source("https://raw.githubusercontent.com/vtdrfuka/BSE5304_2022/main/functions/CNmodel")
pacman::p_load(data.table)
# We will split into 5 VSA areas represented by 5 TI Classes
nTIclass=5
VSAsol=data.table(TIClass=seq(from=nTIclass,to=1),
                    As=seq(1:nTIclass)*(1/nTIclass),Wetfrac=(1/nTIclass))
VSAsol[,sSratio:=2*(sqrt(1-shift(As))-sqrt(1-As))/Wetfrac-1]
#
VSAsol$sSratio[1]=2*(sqrt(1-0)-sqrt(1-VSAsol$As[1]))/VSAsol$Wetfrac[1]-1
# Calculate TI Class localized sigma and Curve Number
VSAsol[,sigma:=Sest*sSratio]
VSAsol[,CN:=25400/(sigma+254)]
VSAsol

VSAParams=merge(VSAsol,MUSLE,by.x="TIClass",by.y="TIclass")
View(VSAParams)

TIC01=TMWB

# For TIC01 CNavg=VSAParams$CN[1] but confirm
TIC01 = CNmodel(CNmodeldf = TIC01, CNavg=VSAParams$CN[1], 
                  declat=myflowgage$declat,declon=myflowgage$declon)
TIC01$qpeak=TIC01$Qpred/3600/24/1000*myflowgage$area/nTIclass*10^6 #m^3/sec
TIC01$sed=(TIC01$Qpred*TIC01$qpeak*myflowgage$area/nTIclass*100)^.56*MUSLE$KCPLSCFRG118[1]    # Eq. 4:1.1.1 SWAT Theory
