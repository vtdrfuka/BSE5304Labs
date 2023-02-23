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
#
#

TMWBmodel=function(TMWBdf,fcres=.3,FldCap=.45,WiltPt=.15,Z=1000,
                   SFTmp=2,bmlt6=4.5,bmlt12=0.0,Tmlt=3,Tlag=1){
  # Our TMWB Model
  SNO_df=TISnow(TMWBdf,SFTmp=SFTmp,bmlt6=bmlt6,bmlt12=bmlt12,Tmlt=Tmlt,Tlag=Tlag)
  TMWBdf$SNO=SNO_df$SNO
  TMWBdf$SNOmlt=SNO_df$SNOmlt
  TMWBdf$SNOfall=SNO_df$SNOfall
  TMWBdf$Tsno=SNO_df$Tsno
  attach(TMWBdf)
  TMWBdf$PET=PET_fromTemp(Jday=(1+as.POSIXlt(date)$yday),Tmax_C = MaxTemp,Tmin_C = MinTemp,
                        lat_radians = myflowgage$declat*pi/180) * 1000
  detach(TMWBdf)
  
  TMWBdf$ET = TMWBdf$PET # in mm/day
  TMWBdf$AWC=(0.45-0.15)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm
  TMWBdf$dP = TMWBdf$P-TMWBdf$ET -TMWBdf$SNO + TMWBdf$SNOmlt 
  
  TMWBdf$AWC=(0.45-0.15)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm
  TMWBdf$AW=NA  #Assigns all values in column with “NA” (Not available)
  TMWBdf$AW[1]=250
  TMWBdf$Excess=NA
  TMWBdf$Excess[1]=0
  
  # Here we go looping through our functions….
  
  attach(TMWBdf)
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
  
  detach(TMWBdf)
  TMWBdf$AW <-AW
  TMWBdf$Excess<-Excess
  rm(list=c("AW","Excess"))
  
  # Calculate Watershed Storage and River Discharge: 
  TMWBdf$Qpred=NA
  TMWBdf$Qpred[1]=0
  TMWBdf$S=NA
  TMWBdf$S[1]=0
  
  attach(TMWBdf)
  for (t in 2:length(date)){
    S[t]=S[t-1]+Excess[t]     
    Qpred[t]=fcres*S[t]
    S[t]=S[t]-Qpred[t]
  }
  detach(TMWBdf) # IMPORTANT TO DETACH
  TMWBdf$S=S
  TMWBdf$Qpred=Qpred # UPDATE vector BEFORE DETACHING
  rm(list=c("S","Qpred"))
  
  TMWBdf$AWC=(FldCap-WiltPt)*Z # 
  TMWBdf$dP = 0 # Initializing Net Precipitation
  TMWBdf$ET = 0 # Initializing ET
  TMWBdf$AW = 0 # Initializing AW
  TMWBdf$Excess = 0 # Initializing Excess

  # Loop to calculate AW and Excess
  attach(TMWBdf)
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
  }
  TMWBdf$AW=AW
  TMWBdf$Excess=Excess
  TMWBdf$dP=dP
  TMWBdf$ET=ET
  rm(list=c("AW","dP","ET", "Excess"))
  detach(TMWBdf) # IMPORTANT TO DETACH
  
  # Calculate Watershed Storage and River Discharge, S and Qpred, 
  # playing with the reservoir coefficient to try to get Qpred to best match Qmm
  TMWBdf$Qpred=NA
  TMWBdf$Qpred[1]=0
  TMWBdf$S=NA
  TMWBdf$S[1]=0
  attach(TMWBdf)
  for (t in 2:length(date)){
    S[t]=S[t-1]+Excess[t]     
    Qpred[t]=fcres*S[t]
    S[t]=S[t]-Qpred[t]
  }
  TMWBdf$S=S
  TMWBdf$Qpred=Qpred # UPDATE vector BEFORE DETACHING
  detach(TMWBdf) # IMPORTANT TO DETACH
  rm(list=c("Qpred","S"))
  return(TMWBdf)
}


# NSE from Class
NSE=function(Yobs,Ysim){
  return(1-sum((Yobs-Ysim)^2,na.rm=TRUE)/sum((Yobs-mean(Yobs, na.rm=TRUE))^2, na.rm=TRUE))
}

