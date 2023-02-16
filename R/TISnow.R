#SFTmp = 3  # referred to as SFTMP in SWAT input (Table 1)
#bmlt6 = 4.5   # referred to as SMFMX in SWAT input (Table 1)
#bmlt12 = 0.0  # referred to as SMFMN in SWAT input adjusted for season
#Tmlt = SFTmp  # Assumed to be same as SnowFall Temperature
#Tlag = 1  # referred to as TIMP in SWAT input (Table 1)

TISnow=function(WBData,SFTmp=2,bmlt6=4.5,bmlt12=0.0,Tmlt=3,Tlag=1){
  #  WBData=TMWB
  #  SFTmp = 3  # referred to as SFTMP in SWAT input (Table 1)
  #  bmlt6 = 4.5   # referred to as SMFMX in SWAT input (Table 1)
  #  bmlt12 = 0.0  # referred to as SMFMN in SWAT input adjusted for season
  #  Tmlt = SFTmp  # Assumed to be same as SnowFall Temperature
  #  Tlag = 1  # referred to as TIMP in SWAT input (Table 1)
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
    SNOmlt[t]=0
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
  rm(list=c("SNO", "SNOmlt", "Tsno","SNOfall"))
  return(data.frame(Tsno=WBData$Tsno,SNO=WBData$SNO,SNOmlt=WBData$SNOmlt,SNOfall=WBData$SNOfall))
}
