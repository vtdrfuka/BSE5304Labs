setwd(srcdir)
detach("package:SWATmodel", unload = TRUE)
remove.packages("SWATmodel", lib="~/R/x86_64-pc-linux-gnu-library/4.2")
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/SWATmodel/"),repos = NULL)
pacman::p_load(SWATmodel)

setwd(datadir)

build_swat_basic(dirname=myflowgage_id,iyr="2016",nbyr=6,myflowgage$area,
        myflowgage$elev,myflowgage$declat,myflowgage$declon,hist_wx=WXData)
#
runSWAT2012()



source("https://raw.githubusercontent.com/Rojakaveh/Elab_SWATinitcalib/main/build_wgn_file.R") #loading the build_wgn_file func
source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")#loading fillmisswx func
pacman::p_load(rnoaa,SWATmodel)
##-----getting usgs info--------------------------
flowgage_id="04282650" #Little Otter Creek at Ferrisburg, VT.
flowgage=get_usgs_gage(flowgage_id,begin_date = "2010-01-01",end_date= "2021-12-31")
flowgage$flowdata$Qmm=(flowgage$flowdata$flow)/(flowgage$area*1000)#mm
##getting ghcn weather data
WXData=FillMissWX(declat = flowgage$declat,declon = flowgage$declon,StnRadius =30,date_min=min(flowgage$flowdata$mdate),date_max=max(flowgage$flowdata$mdate),method = "IDW",minstns = 3,alfa = 2)
AllDays=data.frame(date=seq(min(flowgage$flowdata$mdate), by = "day", length.out = max(flowgage$flowdata$mdate)-min(flowgage$flowdata$mdate)))
WXData=merge(AllDays,WXData,all=T)
WXData$PRECIP=WXData$P
WXData$PRECIP[is.na(WXData$PRECIP)]=-99
WXData$TMX=WXData$MaxTemp
WXData$TMX[is.na(WXData$TMX)]=-99
WXData$TMN=WXData$MinTemp
WXData$TMN[is.na(WXData$TMN)]=-99
WXData$DATE=WXData$date
#making swat init in the directory with the same name as usgs gagename
build_swat_basic(dirname= flowgage$gagename, iyr=min(year(WXData$DATE),na.rm=T), 
          nbyr=(max(year(WXData$DATE),na.rm=T)-min(year(WXData$DATE),na.rm=T) +1), 
          wsarea=flowgage$area, elev=flowgage$elev, declat=flowgage$declat, declon=flowgage$declon, hist_wx=WXData)
build_wgn_file() #wgn func
runSWAT2012() #run swat 
###--------Calibration----------

source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/SWATmodel/R/readSWAT.R?root=ecohydrology")
save(readSWAT,file="readSWAT.R")
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/EcoHydRology/R/setup_swatcal.R?root=ecohydrology")
source("https://raw.githubusercontent.com/Rojakaveh/Elab_SWATinitcalib/main/calibrationfunc.R")
change_params=""
rm(change_params)
load(paste(path.package("EcoHydRology"), "data/change_params.rda", sep = "/")) #all parameters
calib_range=c("1999-12-31","2021-12-31")
params_select=c(1,2,3,4,5,6,7,8,9,10,11,14,19,21,23,24,32,33)
calib_params=change_params[params_select,]
View(calib_params)
######changing calib range based on prio knowledge 
calib_params$min[1]=0
calib_params$min[2]=0
calib_params$max[2]=1
calib_params$current[2]=0.5
calib_params$max[3]=600
calib_params$min[4]=0.02
calib_params$current[4]=0.03
calib_params$min[9]=0
calib_params$min[10]=0
calib_params$current[9]=2.5
calib_params$current[10]=2.5
calib_params$min[11]=0.01
calib_params$max[11]=1
calib_params$current[11]=0.02
calib_params$min[13]=35
calib_params$min[18]=0.1
calib_params$current[18]=0.2
calib_params$current[17]=0.2
###############
calib_params[1:7]
setup_swatcal(calib_params)
View(calib_params)
rch=3 #rch number for calibration
x=calib_params$current
swat_objective_function_rch(x,calib_range,calib_params,flowgage,rch,save_results=F)
cl <- parallel::makeCluster(16)
outDEoptim<-DEoptim(swat_objective_function_rch,calib_params$min,calib_params$max,
                    DEoptim.control(cluster=cl,strategy = 6,NP = 16,itermax=300,parallelType = 1,
                                    packages = c("SWATmodel","dplyr","EcoHydRology","base","topmodel","utils","cl"),parVar=c("%<%","NSeff","read.fortran","readSWAT","alter_files")),calib_range,calib_params,flowgage,rch)