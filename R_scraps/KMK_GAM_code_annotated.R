library(ncdf4); #library(ncdf.tools)
library(chron); library(lattice); library(RColorBrewer); library(sp); library(dplyr)
library(raster); library(data.table); library(maps); library(maptools)
library(plyr); library(ROCR); library(mgcv); library(rpart); #library(rgdal)
library(reshape2); library(ggplot2); library(ggmap);
library(RGraphics); #library(gpclib); 
library(RgoogleMaps); library(shapefiles); library(scales) #for transparency
library(lme4); library(marmap) #for NOAA bathymetry

library(ks); library(CircStats); library(geosphere)
library(gamlss); library(gamlss.tr); library(gamlss.spatial);library(rgr);  
library(gamlss.cens); library(FField); library(circular); library(forecast)
library(kriging); library(fields); library(adehabitatHR); library(adehabitatLT); library(adehabitatMA); 
library(hyperSpec); library(gstat); library(rgdal)


##Read in trawl survey data :
#setwd("D:/NOAA_TNC/thermal_assemblages/NewNorthSouthPts_June2014/")
AllPts<-read.csv("../Downloads/AllPts.csv", header=T, stringsAsFactors=F)
names(AllPts)
unique(AllPts$SPECIES)
AllPts4<-data.table(AllPts) # library (data.table)


# AllPts4$SPECIES<-as.character(AllPts4$SPECIES)
ALLres5<- AllPts4[,list(biomass=sum(biomass, na.rm=T),value=sum(value, na.rm=F)),by="CRUISE6,STATION,SVVESSEL,SPECIES,YEAR,SEASON,Region,LAT,LON,STRATUM,DEPTH,BOTTEMP,SURFTEMP"]

ALL_abund<-AllPts4[,list(value=mean(value, na.rm=F)),by="YEAR,Region,SPECIES"]
setnames(ALL_abund, old=c("YEAR", "Region", "SPECIES", "value"), new=c("YEAR", "Region", "SPECIES", "MA_abund"))
ALLres2<-merge(ALLres5, ALL_abund, by=c("YEAR", "Region","SPECIES"))

ALLres2$IDxx<-1:2035426
unique(ALLres2$SPECIES)

##Subset out a file with a unique list of lat-long points from survey:
myPoints<-unique(data.frame(ALLres2$LON, ALLres2$LAT))
names(myPoints)<-c('LON', 'LAT')
coordinates(myPoints) <- c('LON', 'LAT')

##Create a grid of the CM region:
grd <- GridTopology(cellcentre.offset=c(-79.95000,29.98016), cellsize=c(0.1,0.07603204), cells.dim=c(250,250))
grdSBT <- as.SpatialPolygons.GridTopology(grd)

##Determine which points from the survey are associated with the CM grid:
TEST<-over(myPoints, grdSBT)
test<-data.frame(TEST)
names(test)<-"grid_id"

myPoints<-unique(data.frame(ALLres2$LON, ALLres2$LAT))
myPoints_2<-cbind(myPoints, test)
names(myPoints_2)<-c("LON", "LAT", "grid_id")

##Add the grid cell ids to the trawl survey dataset:
AllPts_grid<-merge(ALLres2, myPoints_2, by=c("LON", "LAT"))
AllPts_grid<-AllPts_grid[which(is.na(AllPts_grid$grid_id)==F),]
AllPts_grid2<-data.table(AllPts_grid)

Fa<-aggregate(biomass~CRUISE6+STATION+SVVESSEL+SPECIES+YEAR+SEASON+LAT+LON+STRATUM+DEPTH+BOTTEMP+SURFTEMP, data=AllPts_grid2, sum)
Fa_2<-aggregate(value~CRUISE6+STATION+SVVESSEL+SPECIES+YEAR+SEASON+LAT+LON+STRATUM+DEPTH+BOTTEMP+SURFTEMP, data=AllPts_grid2, sum)
Fa_3<-merge(Fa, Fa_2, by=c("CRUISE6", "STATION", "SVVESSEL", "SPECIES", "YEAR", "SEASON", "LAT", "LON", "STRATUM", "DEPTH", "BOTTEMP", "SURFTEMP"))

Fa_abund<-aggregate(value~YEAR+SPECIES, Fa_3, mean)
names(Fa_abund)<-c("YEAR", "SPECIES", "Avalue")
Fa_3<-merge(Fa_3, Fa_abund, by=c("YEAR", "SPECIES"))

Fa_3$biomass<-ifelse(is.na(Fa_3$biomass)==T, 0, Fa_3$biomass)
Fa_3$logB<-ifelse(Fa_3$biomass==0, 0, log(Fa_3$biomass)) # how distinguising biomass of 1 from NA?
Fa_3$PA<-ifelse(Fa_3$biomass==0, 0, 1)

names(Fa_3)<-c("YEAR", "SPECIES", "CRUISE6", "STATION", "SVVESSEL", "SEASON", "LAT", "LON", "STRATUM", "DEPTH", "BOTTEMP", "SURFTEMP", "biomass", "abund", "MA_abund", "logB", "PA")
Fa_3<-Fa_3[which(Fa_3$SPECIES!="Southern Eagle Stingray"),]

#Early data (1968-2002):
allSeasReg_E<-Fa_3[which(Fa_3$YEAR<2003),]
#Late data (2003-2012):
allSeasReg_L<-Fa_3[which(Fa_3$YEAR>2002),]

setwd("/Volumes/COMPATIBLE/NOAA_TNC/thermal_assemblages/NewNorthSouthPts_June2014/Projections_Clusters/SPECIES_ALL_noSeason_noRug/")
allSeasReg<-Fa_3
allSeasReg$IDxx<-1:1727256
allSeasReg_E$IDxx<-1:1334158
allSeasReg_L$IDxx<-1:393098

#Clus_split<-split(Fa_N, Fa_N$Cluster)
Clus_split<-split(allSeasReg, allSeasReg$SPECIES)
allSeasReg_Sp<-allSeasReg[which(allSeasReg$SEASON=="SPRING"),]
Clus_split_Sp<-split(allSeasReg_Sp, allSeasReg_Sp$SPECIES)
allSeasReg_Fa<-allSeasReg[which(allSeasReg$SEASON=="FALL"),]
Clus_split_Fa<-split(allSeasReg_Fa, allSeasReg_Fa$SPECIES)

Clus_split_E<-split(allSeasReg_E, allSeasReg_E$SPECIES)
Clus_split_L<-split(allSeasReg_L, allSeasReg_L$SPECIES)
allSeasReg_E_Sp<-allSeasReg_E[which(allSeasReg_E$SEASON=="SPRING"),]
allSeasReg_E_Fa<-allSeasReg_E[which(allSeasReg_E$SEASON=="FALL"),]
allSeasReg_L_Sp<-allSeasReg_L[which(allSeasReg_L$SEASON=="SPRING"),]
allSeasReg_L_Fa<-allSeasReg_L[which(allSeasReg_L$SEASON=="FALL"),]
Clus_split_E_Sp<-split(allSeasReg_E_Sp, allSeasReg_E_Sp$SPECIES)
Clus_split_E_Fa<-split(allSeasReg_E_Fa, allSeasReg_E_Fa$SPECIES)
Clus_split_L_Sp<-split(allSeasReg_L_Sp, allSeasReg_L_Sp$SPECIES)
Clus_split_L_Fa<-split(allSeasReg_L_Fa, allSeasReg_L_Fa$SPECIES)

####################################################
#Divide the data into a training set and test set.
#training set: 1968 - 2002
#test set: 2003-2012
#1. Fit full GAM and simplified (naive) GAM to the data from 1968-2002.
#2. Predict 2003-2012 with both models.
#3. Compare their MAE.
#Calculate MAE_Naive with RWF--include STRATUM:
#, allSeasReg_L$STRATUM
Clus_split_strat<-split(allSeasReg, list(allSeasReg$SPECIES, allSeasReg_L$STRATUM))
Clus_split_E_strat<-split(allSeasReg_E, list(allSeasReg_E$SPECIES, allSeasReg_L$STRATUM))
Clus_split_L_strat<-split(allSeasReg_L, list(allSeasReg_L$SPECIES, allSeasReg_L$STRATUM))

NFORECAST<-NULL
for(i in 1:length(Clus_split_E_strat)){
  CLUS<-data.frame(Clus_split_strat[i]) # for yellowtail flounder, i = 70, CLUS has only 30 observations. but both CLUS_E and L have way more than that. for i = 5 Am. Shad, CLUS has 546, CLUS_# has 412, CLUS_L has 129
  names(CLUS)<-names(allSeasReg)
  CLUS_E<-data.frame(Clus_split_E_strat[i])
  names(CLUS_E)<-names(allSeasReg)
  CLUS_L<-data.frame(Clus_split_L_strat[i])
  names(CLUS_L)<-names(allSeasReg)
  if(dim(CLUS_E)[1]<3) # basically number of observations? skip if no more than 3
    next 
  if(dim(CLUS_L)[1]<3)
    next 
  CLUS_Eagg<-aggregate(biomass~YEAR+SPECIES+STRATUM, CLUS_E, mean)
  CLUS_L_agg<-aggregate(biomass~YEAR+SPECIES+STRATUM, CLUS_L, mean)
  CLUS_agg<-aggregate(biomass~YEAR+SPECIES+STRATUM, CLUS, mean)
  CLUS_Lagg<-CLUS_agg[which(CLUS_agg$YEAR>2002),]
  if(dim(CLUS_Eagg)[1]<4)
    next 
  if(dim(CLUS_Lagg)[1]==0)
    next
  #if(sum(CLUS_Eagg$biomass==0))
  #next
  Naive_forecast<-rwf(CLUS_Eagg$biomass, drift=F, h=dim(CLUS_Lagg)[1], biasadj=T) # library (forecast). CLUS_Lagg has 10 observations. 2006 and 2007, some biomass 0, 2 have values. Why is drift false? 
  #-dim(CLUS_Eagg)[1]
  Naive_forecast2<-cbind(CLUS_Lagg,as.vector(Naive_forecast$mean))
  NFORECAST<-rbind(NFORECAST, Naive_forecast2)
}  

NFORECAST<-data.frame(NFORECAST)

# MAE: forecast minus observed?
NFORECAST$MAE<-abs(NFORECAST$as.vector.Naive_forecast.mean.-NFORECAST$biomass)
names(NFORECAST)<-c("YEAR", "SPECIES", "STRATUM", "biomassN", "as.vector.Naive_forecast.mean.", "MAE_N") 
TestN<-merge(allSeasReg_L, NFORECAST, by=c("YEAR", "SPECIES", "STRATUM"))
head(TestN)
TestNagg<-aggregate(biomassN~YEAR+SPECIES+STRATUM+CRUISE6+LAT+LON+SVVESSEL+STATION+IDxx, TestN, mean)
TestNagg2<-aggregate(as.vector.Naive_forecast.mean.~YEAR+SPECIES+STRATUM+CRUISE6+LAT+LON+SVVESSEL+STATION+IDxx, TestN, mean)
TestNagg3<-merge(TestNagg, TestNagg2, by=c("YEAR", "SPECIES", "STRATUM", "CRUISE6", "LAT", "LON", "SVVESSEL", "STATION", "IDxx"))
TestNagg3$MAE_N<-TestNagg3$biomass-TestNagg3$as.vector.Naive_forecast.mean.


names(allSeasReg_L)
MAE_rwf<-aggregate(MAE_N~SPECIES, TestNagg3, mean)
write.csv(MAE_rwf, "MAE_rwf_norug_6.csv", row.names=F)

##Naive GAM MODEL:
GAM<-NULL
DEV<-NULL
LOGB_RES<-NULL
NFORECAST1<-NULL

for(i in 1:length(Clus_split_E)){
  CLUS_E<-data.frame(Clus_split_E[i])
  names(CLUS_E)<-names(allSeasReg)
  CLUS_L<-data.frame(Clus_split_L[i])
  names(CLUS_L)<-names(allSeasReg)
  if(dim(CLUS_E)[1]==0)
    next 
  b_PA <- gam(PA~YEAR+STRATUM,data=CLUS_E, family='binomial')
  b_LB <- gam(logB~YEAR+STRATUM,data=CLUS_E, family='gaussian')
  r_sq_PA<-summary(b_PA)[10]
  r_sq_PA<-data.frame(r_sq_PA)
  devexp_PA<-summary(b_PA)[14]
  devexp_PA<-data.frame(devexp_PA)
  ptable_PA<-summary(b_PA)[22]
  ptable_PA<-data.frame(ptable_PA)
  UBRE_PA<-summary(b_PA)[26]
  UBRE_PA<-data.frame(UBRE_PA)
  AIC_PA<-AIC(b_PA)
  
  r_sq_LB<-summary(b_LB)[10]
  r_sq_LB<-data.frame(r_sq_LB)
  devexp_LB<-summary(b_LB)[14]
  devexp_LB<-data.frame(devexp_LB)
  ptable_LB<-summary(b_LB)[22]
  ptable_LB<-data.frame(ptable_LB)
  GCV_LB<-summary(b_LB)[26]
  GCV_LB<-data.frame(GCV_LB)
  AIC_LB<-AIC(b_LB)
  
  Devs<-cbind(CLUS_E$SPECIES[1], devexp_PA, devexp_LB, r_sq_PA, r_sq_LB, UBRE_PA, GCV_LB, AIC_PA, AIC_LB)
  names(Devs)<-c("SPECIES", "dev.expl_PA", "dev.expl_LB", "r.sq_PA", "r.sq_LB", "UBRE_PA", "GCV_LB", "AIC_PA", "AIC_LB")
  DEV<-rbind(DEV, Devs)
  
  #get the predicted probabilities for each sample
  gampred_PA <- predict.gam(b_PA,CLUS_L, type="response")
  gampred_PA<-data.frame(gampred_PA)
  gampred_LB <- predict.gam(b_LB,CLUS_L, type="response") 
  gampred_LB<-data.frame(gampred_LB)
  logBresids<-residuals.gam(b_LB)
  logBresids<-cbind(logBresids, CLUS_E$SPECIES[1])
  LOGB_RES<-rbind(LOGB_RES, logBresids)
  gams<-cbind(gampred_PA, gampred_LB, CLUS_E$SPECIES[1],CLUS_L$IDxx)
  GAM<-rbind(GAM, gams)
  
  Naive_forecast<-rwf(CLUS_E$biomass, drift=T, h=dim(CLUS_L)[1]) # when i = 8120, species is summer flounder. CLUS_L has 6 observations of summer flounder, 3 in 2011, 3 in 2012. vs. line 137
  #-dim(CLUS_Eagg)[1]
  Naive_forecast2<-cbind(CLUS_L,as.vector(Naive_forecast$mean))
  NFORECAST1<-rbind(NFORECAST1, Naive_forecast2)
}

NFORECAST1<-NULL
for(i in 1:length(Clus_split_E)){
  CLUS_E<-data.frame(Clus_split_E[i])
  names(CLUS_E)<-names(allSeasReg)
  CLUS_L<-data.frame(Clus_split_L[i])
  names(CLUS_L)<-names(allSeasReg)
  if(dim(CLUS_E)[1]==0)
    next 
  Naive_forecast<-rwf(CLUS_E$biomass, drift=T, h=dim(CLUS_L)[1], lambda=-0.5) # why lambda = -0.5? Box cox transformation parameter, y ^ -0.5
  #-dim(CLUS_Eagg)[1]
  Naive_forecast2<-cbind(CLUS_L,as.vector(Naive_forecast$mean))
  NFORECAST1<-rbind(NFORECAST1, Naive_forecast2)
}

# this makes a dataframe with residuals and log residuals for each species, probably also each stratum? 1334158 obs, same as full training dataste. final Dev_resid_Naive has one row per species with mean Eresid and GA/m summary stat. 
LOGB_RES_Naive<-data.frame(LOGB_RES)
names(LOGB_RES_Naive)<-c("resid", "SPECIES")
#write.csv(LOGB_RES_Naive, "LOGB_RES_Naive2.csv", row.names=F)
LOGB_RES_Naive$one<-1
LOGB_RES_Naive$resid<-as.character(LOGB_RES_Naive$resid)
LOGB_RES_Naive$resid<-as.numeric(LOGB_RES_Naive$resid)
LOGB_RES_Naive$Eresid<-exp(LOGB_RES_Naive$resid)
meanresid_Naive<-aggregate(Eresid~SPECIES, data=LOGB_RES_Naive, mean)
Dev_resid_Naive<-merge(meanresid_Naive, DEV, by=c("SPECIES"))
#write.csv(Dev_resid_Naive, "Dev_resid_Naive2.csv", row.names=F)

# this is all of the gam predictions, same as full test ds, 393098
GAM_Naive<-data.frame(GAM)
names(GAM_Naive)<-c("gampred_PA", "gampred_LB", "SPECIES", "IDxx")
GAM_Naive<-merge(GAM_Naive, Dev_resid_Naive, by=c("SPECIES"))
GamPreds_Naive<-merge(GAM_Naive, allSeasReg_L, by=c("IDxx", "SPECIES"))

GamPreds_Naive$ThermPred<-GamPreds_Naive$gampred_PA*(exp(GamPreds_Naive$gampred_LB))*GamPreds_Naive$Eresid
GamPreds_Naive$MAE<-abs(GamPreds_Naive$ThermPred-GamPreds_Naive$biomass)
#write.csv(GamPreds_Naive, "GamPreds_Naive2.csv", row.names=F)

MAE_Naive<-aggregate(MAE~SPECIES, GamPreds_Naive, mean)
write.csv(MAE_Naive, "MAE_Naive_norug4.csv", row.names=F)

NFORECAST1<-data.frame(NFORECAST1)
NFORECAST1$MAE<-abs(NFORECAST1$as.vector.Naive_forecast.mean.-NFORECAST1$biomass) # how is NFORECAST1$biomass different from GamPreds biomass

MAE_rwf2<-aggregate(MAE~SPECIES, NFORECAST1, mean)
write.csv(MAE_rwf2, "MAE_rwf_norug_GAM.75.csv", row.names=F)
#


##Naive GAM MODEL with SEASON:
SpringSpec<-data.frame(names(Clus_split_E_Sp))
names(SpringSpec)<-"SPECIES"
FallSpec<-data.frame(names(Clus_split_E_Fa))
names(FallSpec)<-"SPECIES"
allSeasSpring <- merge(allSeasReg, SpringSpec, all.x=F, all.y=F)
allSeasSpring_E<-allSeasSpring[which(allSeasSpring$YEAR<2003),]
Clus_split_E_AllSpring<-split(allSeasSpring_E, allSeasSpring_E$SPECIES)

allSeasFall <- merge(allSeasReg, FallSpec, all.x=F, all.y=F)
allSeasFall_E<-allSeasFall[which(allSeasFall$YEAR<2003),]
Clus_split_E_AllFall<-split(allSeasFall_E, allSeasFall_E$SPECIES)

GAM<-NULL
DEV<-NULL
LOGB_RES<-NULL
NFORECAST1<-NULL

for(i in 1:length(Clus_split_E_AllFall)){
  CLUS_E<-data.frame(Clus_split_E_AllFall[i])
  names(CLUS_E)<-names(allSeasFall)
  CLUS_L<-data.frame(Clus_split_L_Fa[i])
  names(CLUS_L)<-names(allSeasReg)
  if(dim(CLUS_E)[1]==0)
    next 
  b_PA <- gam(PA~YEAR+STRATUM,data=CLUS_E, family='binomial')
  b_LB <- gam(logB~YEAR+STRATUM,data=CLUS_E, family='gaussian')
  r_sq_PA<-summary(b_PA)[10]
  r_sq_PA<-data.frame(r_sq_PA)
  devexp_PA<-summary(b_PA)[14]
  devexp_PA<-data.frame(devexp_PA)
  ptable_PA<-summary(b_PA)[22]
  ptable_PA<-data.frame(ptable_PA)
  UBRE_PA<-summary(b_PA)[26]
  UBRE_PA<-data.frame(UBRE_PA)
  AIC_PA<-AIC(b_PA)
  
  r_sq_LB<-summary(b_LB)[10]
  r_sq_LB<-data.frame(r_sq_LB)
  devexp_LB<-summary(b_LB)[14]
  devexp_LB<-data.frame(devexp_LB)
  ptable_LB<-summary(b_LB)[22]
  ptable_LB<-data.frame(ptable_LB)
  GCV_LB<-summary(b_LB)[26]
  GCV_LB<-data.frame(GCV_LB)
  AIC_LB<-AIC(b_LB)
  
  Devs<-cbind(CLUS_E$SPECIES[1], devexp_PA, devexp_LB, r_sq_PA, r_sq_LB, UBRE_PA, GCV_LB, AIC_PA, AIC_LB)
  names(Devs)<-c("SPECIES", "dev.expl_PA", "dev.expl_LB", "r.sq_PA", "r.sq_LB", "UBRE_PA", "GCV_LB", "AIC_PA", "AIC_LB")
  DEV<-rbind(DEV, Devs)
  
  #get the predicted probabilities for each sample
  gampred_PA <- predict.gam(b_PA,CLUS_L, type="response")
  gampred_PA<-data.frame(gampred_PA)
  gampred_LB <- predict.gam(b_LB,CLUS_L, type="response") 
  gampred_LB<-data.frame(gampred_LB)
  logBresids<-residuals.gam(b_LB)
  logBresids<-cbind(logBresids, CLUS_E$SPECIES[1])
  LOGB_RES<-rbind(LOGB_RES, logBresids)
  gams<-cbind(gampred_PA, gampred_LB, CLUS_E$SPECIES[1],CLUS_L$IDxx)
  GAM<-rbind(GAM, gams)
  
  Naive_forecast<-rwf(CLUS_E$biomass, drift=T, h=dim(CLUS_L)[1])
  Naive_forecast2<-cbind(CLUS_L,as.vector(Naive_forecast$mean))
  NFORECAST1<-rbind(NFORECAST1, Naive_forecast2)
}

# NFORECAST1<-NULL
# for(i in 1:length(Clus_split_E)){
#   CLUS_E<-data.frame(Clus_split_E[i])
#   names(CLUS_E)<-names(allSeasReg)
#   CLUS_L<-data.frame(Clus_split_L[i])
#   names(CLUS_L)<-names(allSeasReg)
#   if(dim(CLUS_E)[1]==0)
#     next 
#   Naive_forecast<-rwf(CLUS_E$biomass, drift=T, h=dim(CLUS_L)[1], lambda=-0.5)
#   #-dim(CLUS_Eagg)[1]
#   Naive_forecast2<-cbind(CLUS_L,as.vector(Naive_forecast$mean))
#   NFORECAST1<-rbind(NFORECAST1, Naive_forecast2)
# }

LOGB_RES_Naive<-data.frame(LOGB_RES)
names(LOGB_RES_Naive)<-c("resid", "SPECIES")
#write.csv(LOGB_RES_Naive, "LOGB_RES_Naive2.csv", row.names=F)
LOGB_RES_Naive$one<-1
LOGB_RES_Naive$resid<-as.character(LOGB_RES_Naive$resid)
LOGB_RES_Naive$resid<-as.numeric(LOGB_RES_Naive$resid)
LOGB_RES_Naive$Eresid<-exp(LOGB_RES_Naive$resid)
meanresid_Naive<-aggregate(Eresid~SPECIES, data=LOGB_RES_Naive, mean)
Dev_resid_Naive<-merge(meanresid_Naive, DEV, by=c("SPECIES"))
#write.csv(Dev_resid_Naive, "Dev_resid_Naive2.csv", row.names=F)

GAM_Naive<-data.frame(GAM)
names(GAM_Naive)<-c("gampred_PA", "gampred_LB", "SPECIES", "IDxx")
GAM_Naive<-merge(GAM_Naive, Dev_resid_Naive, by=c("SPECIES"))
GamPreds_Naive<-merge(GAM_Naive, allSeasReg_L, by=c("IDxx", "SPECIES"))

GamPreds_Naive$ThermPred<-GamPreds_Naive$gampred_PA*(exp(GamPreds_Naive$gampred_LB))*GamPreds_Naive$Eresid
GamPreds_Naive$MAE<-abs(GamPreds_Naive$ThermPred-GamPreds_Naive$biomass)
#write.csv(GamPreds_Naive, "GamPreds_Naive2.csv", row.names=F)

MAE_Naive<-aggregate(MAE~SPECIES, GamPreds_Naive, mean)
names(MAE_Naive)<-c("SPECIES", "MAE_naive")
NFORECAST1<-data.frame(NFORECAST1)
NFORECAST1$MAE<-abs(NFORECAST1$as.vector.Naive_forecast.mean.-NFORECAST1$biomass)
MAE_rwf<-aggregate(MAE~SPECIES, NFORECAST1, mean)
names(MAE_rwf)<-c("SPECIES", "MAE_rwf")
MAE_Naive_rwf<-merge(MAE_Naive, MAE_rwf, by="SPECIES")

# MAE_rwf2<-aggregate(MAE~SPECIES, NFORECAST1, mean)
# write.csv(MAE_rwf2, "MAE_rwf_norug_GAM.75.csv", row.names=F)
# # 



GAM<-NULL
DEV<-NULL
LOGB_RES<-NULL

for(i in 1:length(Clus_split_E_AllFall)){
  CLUS_E<-data.frame(Clus_split_E_AllFall[i])
  names(CLUS_E)<-names(allSeasFall)
  CLUS_L<-data.frame(Clus_split_L_Fa[i])
  names(CLUS_L)<-names(allSeasReg)
  if(dim(CLUS_E)[1]==0)
    next 
  
  b_PA <- gam(PA~YEAR+s(BOTTEMP, k=6)+s(SURFTEMP, k=6)+s(DEPTH, k=6)+STRATUM,data=CLUS_E, family='binomial')
  b_LB <- gam(logB~YEAR+s(BOTTEMP, k=6)+s(SURFTEMP, k=6)+s(DEPTH, k=6)+STRATUM,data=CLUS_E, family='gaussian')
  r_sq_PA<-summary(b_PA)[10]
  r_sq_PA<-data.frame(r_sq_PA)
  devexp_PA<-summary(b_PA)[14]
  devexp_PA<-data.frame(devexp_PA)
  ptable_PA<-summary(b_PA)[22]
  ptable_PA<-data.frame(ptable_PA)
  UBRE_PA<-summary(b_PA)[26]
  UBRE_PA<-data.frame(UBRE_PA)
  AIC_PA<-AIC(b_PA)
  
  r_sq_LB<-summary(b_LB)[10]
  r_sq_LB<-data.frame(r_sq_LB)
  devexp_LB<-summary(b_LB)[14]
  devexp_LB<-data.frame(devexp_LB)
  ptable_LB<-summary(b_LB)[22]
  ptable_LB<-data.frame(ptable_LB)
  GCV_LB<-summary(b_LB)[26]
  GCV_LB<-data.frame(GCV_LB)
  AIC_LB<-AIC(b_LB)
  
  modl_PA<-b_PA
  termPA<-"norug"
  modl_LB<-b_LB
  termLB<-"norug"
  
  DEV_a<-cbind(CLUS_E$SPECIES[1],devexp_PA, devexp_LB, r_sq_PA, r_sq_LB, UBRE_PA, GCV_LB, AIC_PA, AIC_LB)
  names(DEV_a)<-c("SPECIES","dev.expl_PA", "dev.expl_LB", "r.sq_PA", "r.sq_LB", "UBRE_PA", "GCV_LB", "AIC_PA", "AIC_LB")
  DEV<-rbind(DEV, DEV_a)
  
  #get the predicted probabilities for each sample
  gampred_PA <- predict.gam(modl_PA,CLUS_L, type="response")
  gampred_PA<-data.frame(gampred_PA)
  gampred_LB <- predict.gam(modl_LB,CLUS_L, type="response") 
  gampred_LB<-data.frame(gampred_LB)
  logBresids<-residuals.gam(modl_LB)
  logBresids<-cbind(logBresids, CLUS_E$SPECIES[1])
  LOGB_RES<-rbind(LOGB_RES, logBresids)
  gams<-cbind(gampred_PA, gampred_LB, CLUS_E$SPECIES[1],CLUS_L$IDxx)
  GAM<-rbind(GAM, gams)
}

# 
LOGB_RES_Full<-data.frame(LOGB_RES)
names(LOGB_RES_Full)<-c("resid", "SPECIES")

#write.csv(LOGB_RES_Full, "LOGB_RES_Full_GCV_noRug.csv", row.names=F)
LOGB_RES_Full$one<-1
LOGB_RES_Full$resid<-as.character(LOGB_RES_Full$resid)
LOGB_RES_Full$resid<-as.numeric(LOGB_RES_Full$resid)
LOGB_RES_Full$Eresid<-exp(LOGB_RES_Full$resid)
meanresid_Full<-aggregate(Eresid~SPECIES, data=LOGB_RES_Full, mean)
Dev_resid_Full<-merge(meanresid_Full, DEV, by="SPECIES")
#write.csv(Dev_resid_Full, "Dev_resid_Full_GCV3_noRug.csv", row.names=F)

GAM_Full<-data.frame(GAM)
head(GAM_Full)
names(GAM_Full)<-c("gampred_PA", "gampred_LB", "SPECIES", "IDxx")
GAM_Full<-merge(GAM_Full, Dev_resid_Full, by="SPECIES")
GamPreds_Full<-merge(GAM_Full, allSeasReg_L, by=c("IDxx", "SPECIES"))
unique(GamPreds_Full$YEAR)

GamPreds_Full$ThermPred<-GamPreds_Full$gampred_PA*(exp(GamPreds_Full$gampred_LB))*GamPreds_Full$Eresid
max(GamPreds_Full$ThermPred)

GamPreds_Full$MAE<-abs(GamPreds_Full$ThermPred-GamPreds_Full$biomass)
#write.csv(GamPreds_Full, "GamPreds_Full_GCV_noRug.csv", row.names=F)


MAE_Full<-aggregate(MAE~SPECIES, GamPreds_Full, mean)
MAE_All<-merge(MAE_Naive_rwf, MAE_Full, by="SPECIES")
MAE_All$MASE_rwf<-MAE_All$MAE/MAE_All$MAE_rwf
MAE_All$MASE_naive<-MAE_All$MAE/MAE_All$MAE_naive
write.csv(MAE_All, "MAE_All_GCV_noRug_Fall.csv", row.names=F)

GamPreds_Full<-read.csv("GamPreds_Full_GCV_noRug.csv", header=T, stringsAsFactors=F)
MASE<-read.csv("MASE.csv", header=T, stringsAsFactors=F)
GamPreds_Full2<-GamPreds_Full%>%
  left_join(MASE)%>%
  filter(MASE<1)

NFORECAST1$E1<-NFORECAST1$as.vector.Naive_forecast.mean.-NFORECAST1$biomass
GamPreds_Naive$E1<-GamPreds_Naive$ThermPred-GamPreds_Naive$biomass
GamPreds_Full$E2<-GamPreds_Full$ThermPred-GamPreds_Full$biomass
GamPreds_Naive_split<-split(GamPreds_Naive, GamPreds_Naive$SPECIES)
GamPreds_Full_split<-split(GamPreds_Full, GamPreds_Full$SPECIES)
NFORECAST_split<-split(NFORECAST1, NFORECAST1$SPECIES)
#LOGB_RES_Full_split<-split(LOGB_RES_Full, LOGB_RES_Full$SPECIES)
#LOGB_RES_Naive_split<-split(LOGB_RES_Naive, LOGB_RES_Naive$SPECIES)
DM<-NULL
for (i in 1:length(GamPreds_Full_split)){
  NaiveRes<-data.frame(GamPreds_Naive_split[i])
  names(NaiveRes)<-names(GamPreds_Naive)
  FullRes<-data.frame(GamPreds_Full_split[i])
  names(FullRes)<-names(GamPreds_Full)
  if(as.character(FullRes$SPECIES[1])=="Southern Eagle Stingray")
    next 
  DMtest<-dm.test(NaiveRes$E1, FullRes$E2, h=1, power=2, alternative="greater") 
  DMtest1<-data.frame(cbind(as.character(FullRes$SPECIES[1]), DMtest$statistic, DMtest$p.value))
  names(DMtest1)<-c("SPECIES", "Statistic", "p.value")
  DM<-rbind(DM, DMtest1)
}
DM<-data.frame(DM)
write.csv(DM, "DMgreater_Fall.csv", row.names=F)
