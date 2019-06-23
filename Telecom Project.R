#----------------------------------------------------------------------------------#
# ------ Prediction-of-customer-disposition-to-Churn-in-a-Telecom-Company -------- #
#----------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------#
#                          set memory, read file                                   #
#----------------------------------------------------------------------------------#

setwd("c:/Jig18551/Final Project")

tel<-read.csv("c:/...../telecom.csv")

options(scipen=999)

str(tel)

#----------------------------------------------------------------------------------#
#                               Libraries                                          #
#----------------------------------------------------------------------------------#


  library(dplyr)
  library(ggplot2)
  library(pROC)


#----------------------------------------------------------------------------------#
#                             Creating DQR                                         #
#----------------------------------------------------------------------------------#

VarName<-names(tel) #creating Variable Names for dataframe dqr
dqr<-as.data.frame(VarName) #making dataframe dqr
dqr$ClassType<-sapply(tel, class) #adding class type to dataframe dqr
dqr$NoofRecs<-nrow(tel) #adding number of records to dataframe dqr

str(tel)

for(i in 1:ncol(tel))# this is a loop for counting unique values to add to dataframe dqr
{
  dqr$UniqueRecs[i]            <-length(unique(tel[,i]))
  dqr$AvailbleRecs[i]          <-colSums(!is.na(tel[i]))
  dqr$AvailbleRecPercent[i]    <-round(dqr$AvailbleRecs[i] / dqr$NoofRecs[i],2)
  dqr$Missing[i]               <-colSums(is.na(tel[i]))
  dqr$MissingPercent[i]        <-round(dqr$Missing[i] / dqr$NoofRecs[i],2)
  dqr$Minimum[i]               <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',min(tel[,i],na.rm = T),0),2)
  dqr$Maximum[i]               <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',max(tel[,i],na.rm=T),0),2)  
  dqr$Mean[i]                  <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',colMeans(tel[i], na.rm = T),0),2)
  dqr$FifthPercentile[i]       <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',quantile(tel[i],p=0.05,na.rm = T),0),2)
  dqr$TenthPercentile[i]       <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',quantile(tel[i],p=0.10,na.rm = T),0),2)
  dqr$TwentyFifthPercentile[i] <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',quantile(tel[i],p=0.25,na.rm = T),0),2)
  dqr$FiftythPercentile[i]     <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',quantile(tel[i],p=0.5,na.rm = T),0),2)
  dqr$SeventyFifthPercentile[i]<-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',quantile(tel[i],p=0.75,na.rm = T),0),2)
  dqr$NinetythPercentile[i]    <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',quantile(tel[i],p=0.90,na.rm = T),0),2)
  dqr$NinetyFifthPercentile[i] <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',quantile(tel[i],p=0.95,na.rm = T),0),2)
  dqr$NinetyEightthPercentile[i] <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',quantile(tel[i],p=0.98,na.rm = T),0),2)
  dqr$NinetyNinethPercentile[i] <-round(ifelse(class(tel[,i])=='integer' | class(tel[,i])=='numeric',quantile(tel[i],p=0.99,na.rm = T),0),2)
}


write.csv(dqr, "DQR.csv",row.names = F)


str(tel)

#----------------------------------------------------------------------------------#
#             Merging variables to create new derived variable                     #
#----------------------------------------------------------------------------------#

tel$compcalls_Mean<-tel$comp_dat_Mean+tel$comp_vce_Mean
tel$plcdcalls_Mean<-tel$plcd_dat_Mean+tel$plcd_vce_Mean
names(tel)

#----------------------------------------------------------------------------------#
#               Omitting variables with > 40% missing values                       #
#----------------------------------------------------------------------------------#

tele<-tel[,colMeans(is.na(tel))<=0.40]
names(tele)


#--------------  Dropping Insignificant Variables/Duplicates  ---------------------#
#    data definitions file has variables clearly defined & merged variables.       #
#----------------------------------------------------------------------------------#

#comp_vce_Mean,comp_dat_Mean,plcd_dat_Mean,plcd_vce_Mean as these are now captured in compcalls_Mean & plcdcalls_Mean
#drop_vce_Mean,drop_dat_Mean  blck_dat_Mean, drop_vce_Range can be dropped as these are already captured in drop_blk_Mean
#callwait_Range as i already have callwait_Mean
#datovr_Mean,   datovr_Range is already included in ovrrev_Mean.
#mou_opkv_Range, mou_pead_Mean & opk_dat_Mean are all offpeak/peak calls. doesnt have signifance here.
#recv_sms_Mean not significant.

telec<-tele[,-c(3:4,7,9,17,23:24,48,52:54,59:63,69:70)]
str(telec)
names(telec)
summary(telec)

##---------------------------   Data Exploration    ------------------------------##
###------------------  Binning Numeric/Integer Variables  -----------------------###
#----------------------------------------------------------------------------------#

names(telec)
str(telec)

{
  # mou_Mean,	totmrc_Mean,	change_mou,	drop_blk_Mean,	  owylis_vce_Range,	
  
  summary(telec$mou_Mean) #181 NAs
  telec%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
  dat1$N<-unclass(telec%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
  dat1$churn_perc<-dat1$n/dat1$N
  dat1$GreaterThan<-unclass(telec%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
  dat1$LessThan<-unclass(telec%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
  dat1$varname<-rep("mou_Mean",nrow(dat1))
  
  summary(telec$totmrc_Mean)   #181 NAs
  telec%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
  dat2$N<-unclass(telec%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
  dat2$churn_perc<-dat2$n/dat2$N
  dat2$GreaterThan<-unclass(telec%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
  dat2$LessThan<-unclass(telec%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
  dat2$varname<-rep("totmrc_Mean",nrow(dat2))
  
  summary(telec$change_mou)  #414 NAs
  telec%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
  dat3$N<-unclass(telec%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
  dat3$churn_perc<-dat3$n/dat3$N
  dat3$GreaterThan<-unclass(telec%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
  dat3$LessThan<-unclass(telec%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
  dat3$varname<-rep("change_mou",nrow(dat3))
  
  summary(telec$drop_blk_Mean)
  telec%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
  dat4$N<-unclass(telec%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
  dat4$churn_perc<-dat4$n/dat4$N
  dat4$GreaterThan<-unclass(telec%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
  dat4$LessThan<-unclass(telec%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
  dat4$varname<-rep("drop_blk_Mean",nrow(dat4)) 
  
  summary(telec$owylis_vce_Range)
  telec%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
  dat5$N<-unclass(telec%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
  dat5$churn_perc<-dat5$n/dat5$N
  dat5$GreaterThan<-unclass(telec%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min( owylis_vce_Range)))[[2]]
  dat5$LessThan<-unclass(telec%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
  dat5$varname<-rep("owylis_vce_Range",nrow(dat5))  
  
  
  #	months,  totcalls,	eqpdays,    custcare_Mean,	callwait_Mean,	
  
  summary(telec$months)
  telec%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
  dat6$N<-unclass(telec%>%mutate(dec=ntile(months,n=5))%>%count(dec)%>%unname())[[2]]
  dat6$churn_perc<-dat6$n/dat6$N
  dat6$GreaterThan<-unclass(telec%>%mutate(dec=ntile( months,n=5))%>%group_by(dec)%>%summarise(min(months)))[[2]]
  dat6$LessThan<-unclass(telec%>%mutate(dec=ntile(months,n=5))%>%group_by(dec)%>%summarise(max(months)))[[2]]
  dat6$varname<-rep("months",nrow(dat6))
  
  summary(telec$totcalls)
  telec%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
  dat7$N<-unclass(telec%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
  dat7$churn_perc<-dat7$n/dat7$N
  dat7$GreaterThan<-unclass(telec%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min( totcalls)))[[2]]
  dat7$LessThan<-unclass(telec%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
  dat7$varname<-rep("totcalls",nrow(dat7))
  
  summary(telec$eqpdays)   #   1 NA
  telec%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
  dat8$N<-unclass(telec%>%mutate(dec=ntile(eqpdays,n=9))%>%count(dec)%>%unname())[[2]]
  dat8$churn_perc<-dat8$n/dat8$N
  dat8$GreaterThan<-unclass(telec%>%mutate(dec=ntile(eqpdays,n=9))%>%group_by(dec)%>%summarise(min( eqpdays)))[[2]]
  dat8$LessThan<-unclass(telec%>%mutate(dec=ntile(eqpdays,n=9))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
  dat8$varname<-rep("eqpdays",nrow(dat8))
  
  summary(telec$custcare_Mean)
  telec%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
  dat9$N<-unclass(telec%>%mutate(dec=ntile(custcare_Mean,n=7))%>%count(dec)%>%unname())[[2]]
  dat9$churn_perc<-dat9$n/dat9$N
  dat9$GreaterThan<-unclass(telec%>%mutate(dec=ntile(custcare_Mean,n=7))%>%group_by(dec)%>%summarise(min( custcare_Mean)))[[2]]
  dat9$LessThan<-unclass(telec%>%mutate(dec=ntile(custcare_Mean,n=7))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
  dat9$varname<-rep("custcare_Mean",nrow(dat9))    
  
  summary(telec$callwait_Mean)
  telec%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
  dat10$N<-unclass(telec%>%mutate(dec=ntile(callwait_Mean,n=8))%>%count(dec)%>%unname())[[2]]
  dat10$churn_perc<-dat10$n/dat10$N
  dat10$GreaterThan<-unclass(telec%>%mutate(dec=ntile(callwait_Mean,n=8))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
  dat10$LessThan<-unclass(telec%>%mutate(dec=ntile(callwait_Mean,n=8))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
  dat10$varname<-rep("callwait_Mean",nrow(dat10))
  
  
  #	iwylis_vce_Mean,ccrndmou_Range,	adjqty,   ovrrev_Mean,	rev_Mean,	
  
  summary(telec$iwylis_vce_Mean)
  telec%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
  dat11$N<-unclass(telec%>%mutate(dec=ntile(iwylis_vce_Mean,n=9))%>%count(dec)%>%unname())[[2]]
  dat11$churn_perc<-dat11$n/dat11$N
  dat11$GreaterThan<-unclass(telec%>%mutate(dec=ntile(iwylis_vce_Mean,n=9))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
  dat11$LessThan<-unclass(telec%>%mutate(dec=ntile(iwylis_vce_Mean,n=9))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
  dat11$varname<-rep("iwylis_vce_Mean",nrow(dat11))
  
  summary(telec$ccrndmou_Range)
  telec%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
  dat12$N<-unclass(telec%>%mutate(dec=ntile(ccrndmou_Range,n=7))%>%count(dec)%>%unname())[[2]]
  dat12$churn_perc<-dat12$n/dat12$N
  dat12$GreaterThan<-unclass(telec%>%mutate(dec=ntile(ccrndmou_Range,n=7))%>%group_by(dec)%>%summarise(min( ccrndmou_Range)))[[2]]
  dat12$LessThan<-unclass(telec%>%mutate(dec=ntile(ccrndmou_Range,n=7))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
  dat12$varname<-rep("ccrndmou_Range",nrow(dat12))
  
  summary(telec$adjqty)
  telec%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat13
  dat13$N<-unclass(telec%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
  dat13$churn_perc<-dat13$n/dat13$N
  dat13$GreaterThan<-unclass(telec%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min( adjqty)))[[2]]
  dat13$LessThan<-unclass(telec%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
  dat13$varname<-rep("adjqty",nrow(dat13))
  
  summary(telec$ovrrev_Mean)   #181 NAs
  telec%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat14
  dat14$N<-unclass(telec%>%mutate(dec=ntile(ovrrev_Mean,n=8))%>%count(dec)%>%unname())[[2]]
  dat14$churn_perc<-dat14$n/dat14$N
  dat14$GreaterThan<-unclass(telec%>%mutate(dec=ntile(ovrrev_Mean,n=8))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
  dat14$LessThan<-unclass(telec%>%mutate(dec=ntile(ovrrev_Mean,n=8))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
  dat14$varname<-rep("ovrrev_Mean",nrow(dat14)) 
  
  summary(telec$rev_Mean)   #181 NAs
  telec%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat15
  dat15$N<-unclass(telec%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
  dat15$churn_perc<-dat15$n/dat15$N
  dat15$GreaterThan<-unclass(telec%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
  dat15$LessThan<-unclass(telec%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
  dat15$varname<-rep("rev_Mean",nrow(dat15))
  
  
  #	ovrmou_Mean,avg3mou,  	avgmou,	avg3qty,	avgqty,	avg6mou,	avg6qty,
  
  summary(telec$ovrmou_Mean)   #181 NAs
  telec%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat16
  dat16$N<-unclass(telec%>%mutate(dec=ntile(ovrmou_Mean,n=8))%>%count(dec)%>%unname())[[2]]
  dat16$churn_perc<-dat16$n/dat16$N
  dat16$GreaterThan<-unclass(telec%>%mutate(dec=ntile(ovrmou_Mean,n=8))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
  dat16$LessThan<-unclass(telec%>%mutate(dec=ntile(ovrmou_Mean,n=8))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
  dat16$varname<-rep("ovrmou_Mean",nrow(dat16))
  
  summary(telec$avg3mou)
  telec%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat17
  dat17$N<-unclass(telec%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
  dat17$churn_perc<-dat17$n/dat17$N
  dat17$GreaterThan<-unclass(telec%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min( avg3mou)))[[2]]
  dat17$LessThan<-unclass(telec%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
  dat17$varname<-rep("avg3mou",nrow(dat17))
  
  summary(telec$avgmou)
  telec%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
  dat18$N<-unclass(telec%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
  dat18$churn_perc<-dat18$n/dat18$N
  dat18$GreaterThan<-unclass(telec%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
  dat18$LessThan<-unclass(telec%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
  dat18$varname<-rep("avgmou",nrow(dat18))
  
  summary(telec$avg3qty)
  telec%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat19
  dat19$N<-unclass(telec%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
  dat19$churn_perc<-dat19$n/dat19$N
  dat19$GreaterThan<-unclass(telec%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
  dat19$LessThan<-unclass(telec%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
  dat19$varname<-rep("avg3qty",nrow(dat19))
  
  summary(telec$avgqty)
  telec%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
  dat20$N<-unclass(telec%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
  dat20$churn_perc<-dat20$n/dat20$N
  dat20$GreaterThan<-unclass(telec%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
  dat20$LessThan<-unclass(telec%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
  dat20$varname<-rep("avgqty",nrow(dat20))
  
  summary(telec$avg6mou)    # 2056 NA's
  telec%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat21
  dat21$N<-unclass(telec%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
  dat21$churn_perc<-dat21$n/dat21$N
  dat21$GreaterThan<-unclass(telec%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
  dat21$LessThan<-unclass(telec%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
  dat21$varname<-rep("avg6mou",nrow(dat21))
  
  summary(telec$avg6qty)    # 2056 NA's
  telec%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
  dat22$N<-unclass(telec%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
  dat22$churn_perc<-dat22$n/dat22$N
  dat22$GreaterThan<-unclass(telec%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
  dat22$LessThan<-unclass(telec%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
  dat22$varname<-rep("avg6qty",nrow(dat22))
  
  
  #		roam_Mean,	 da_Mean,   adjmou,	totrev,	adjrev,	avgrev, 
  
  summary(telec$roam_Mean)   #181 NAs
  telec%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
  dat23$N<-unclass(telec%>%mutate(dec=ntile(roam_Mean,n=6))%>%count(dec)%>%unname())[[2]]
  dat23$churn_perc<-dat23$n/dat23$N
  dat23$GreaterThan<-unclass(telec%>%mutate(dec=ntile(roam_Mean,n=6))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
  dat23$LessThan<-unclass(telec%>%mutate(dec=ntile(roam_Mean,n=6))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
  dat23$varname<-rep("roam_Mean",nrow(dat23))
  
  summary(telec$da_Mean)   #181 NAs
  telec%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
  dat24$N<-unclass(telec%>%mutate(dec=ntile(da_Mean,n=8))%>%count(dec)%>%unname())[[2]]
  dat24$churn_perc<-dat24$n/dat24$N
  dat24$GreaterThan<-unclass(telec%>%mutate(dec=ntile(da_Mean,n=8))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
  dat24$LessThan<-unclass(telec%>%mutate(dec=ntile(da_Mean,n=8))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
  dat24$varname<-rep("da_Mean",nrow(dat24))
  
  summary(telec$adjmou)
  telec%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
  dat25$N<-unclass(telec%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
  dat25$churn_perc<-dat25$n/dat25$N
  dat25$GreaterThan<-unclass(telec%>%mutate(dec=ntile( adjmou,n=10))%>%group_by(dec)%>%summarise(min( adjmou)))[[2]]
  dat25$LessThan<-unclass(telec%>%mutate(dec=ntile( adjmou,n=10))%>%group_by(dec)%>%summarise(max( adjmou)))[[2]]
  dat25$varname<-rep("adjmou",nrow(dat25))
  
  summary(telec$totrev)
  telec%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
  dat26$N<-unclass(telec%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
  dat26$churn_perc<-dat26$n/dat26$N
  dat26$GreaterThan<-unclass(telec%>%mutate(dec=ntile( totrev,n=10))%>%group_by(dec)%>%summarise(min( totrev)))[[2]]
  dat26$LessThan<-unclass(telec%>%mutate(dec=ntile( totrev,n=10))%>%group_by(dec)%>%summarise(max( totrev)))[[2]]
  dat26$varname<-rep("totrev",nrow(dat26))
  
  summary(telec$adjrev)
  telec%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
  dat27$N<-unclass(telec%>%mutate(dec=ntile( adjrev,n=10))%>%count(dec)%>%unname())[[2]]
  dat27$churn_perc<-dat27$n/dat27$N
  dat27$GreaterThan<-unclass(telec%>%mutate(dec=ntile( adjrev,n=10))%>%group_by(dec)%>%summarise(min( adjrev)))[[2]]
  dat27$LessThan<-unclass(telec%>%mutate(dec=ntile( adjrev,n=10))%>%group_by(dec)%>%summarise(max( adjrev)))[[2]]
  dat27$varname<-rep("adjrev",nrow(dat27))
  
  summary(telec$avgrev)
  telec%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
  dat28$N<-unclass(telec%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
  dat28$churn_perc<-dat28$n/dat28$N
  dat28$GreaterThan<-unclass(telec%>%mutate(dec=ntile( avgrev,n=10))%>%group_by(dec)%>%summarise(min( avgrev)))[[2]]
  dat28$LessThan<-unclass(telec%>%mutate(dec=ntile( avgrev,n=10))%>%group_by(dec)%>%summarise(max( avgrev)))[[2]]
  dat28$varname<-rep("avgrev",nrow(dat28))
  
  
  #	"compcalls_Mean"   "plcdcalls_Mean"  
  
  summary(telec$compcalls_Mean)
  telec%>%mutate(dec=ntile(compcalls_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
  dat29$N<-unclass(telec%>%mutate(dec=ntile(compcalls_Mean,n=5))%>%count(dec)%>%unname())[[2]]
  dat29$churn_perc<-dat29$n/dat29$N
  dat29$GreaterThan<-unclass(telec%>%mutate(dec=ntile( compcalls_Mean,n=5))%>%group_by(dec)%>%summarise(min(compcalls_Mean)))[[2]]
  dat29$LessThan<-unclass(telec%>%mutate(dec=ntile( compcalls_Mean,n=5))%>%group_by(dec)%>%summarise(max(compcalls_Mean)))[[2]]
  dat29$varname<-rep("compcalls_Mean",nrow(dat29))
  
  summary(telec$plcdcalls_Mean)
  telec%>%mutate(dec=ntile(plcdcalls_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat30
  dat30$N<-unclass(telec%>%mutate(dec=ntile(plcdcalls_Mean,n=5))%>%count(dec)%>%unname())[[2]]
  dat30$churn_perc<-dat30$n/dat30$N
  dat30$GreaterThan<-unclass(telec%>%mutate(dec=ntile( plcdcalls_Mean,n=5))%>%group_by(dec)%>%summarise(min(plcdcalls_Mean)))[[2]]
  dat30$LessThan<-unclass(telec%>%mutate(dec=ntile(plcdcalls_Mean,n=5))%>%group_by(dec)%>%summarise(max(plcdcalls_Mean)))[[2]]
  dat30$varname<-rep("plcdcalls_Mean",nrow(dat30))
  


DataCon<-rbind(dat1,	dat2,	  dat3,	  dat4,	  dat5,	  dat6,	  dat7,	  dat8,	  dat9,	  dat10,	
               dat11,	dat12,	dat13,	dat14,  dat15,	dat16,	dat17,	dat18,	dat19,	dat20,	
               dat21,	dat22,	dat23,	dat24,	dat25,	dat26,	dat27,	dat28,	dat29,	dat30)

#write.csv(DataCon,"ContinuousProfiling28March.csv",row.names = F)
}

##---------------------------   Data Exploration    ------------------------------##
### ----------------      Binning factor variables      -------------------------###
#----------------------------------------------------------------------------------#

names(telec)
str(telec)
{
  # "income"   crclscod,	asl_flag,	prizm_social_one,	area,	refurb_new,
  
  summary(telec$income)   #  16528  NA's
  #telec$income<-as.factor(telec$income)
  telec%>%count(churn,levels=income)%>%filter(churn==1)->datC1
  datC1$N<-unclass(telec%>%filter(income%in%datC1$levels)%>%count(income))[[2]]
  datC1$ChurnPerc<-datC1$n/datC1$N
  datC1$Varname<-rep("income",nrow(datC1))
  
  summary(telec$crclscod)
  telec%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC2
  datC2$N<-unclass(telec%>%filter(crclscod%in%datC2$levels)%>%count(crclscod))[[2]]
  datC2$ChurnPerc<-datC2$n/datC2$N
  datC2$Varname<-rep("crclscod",nrow(datC2))
  
  summary(telec$asl_flag)
  telec%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC3
  datC3$N<-unclass(telec%>%filter(asl_flag%in%datC3$levels)%>%count(asl_flag))[[2]]
  datC3$ChurnPerc<-datC3$n/datC3$N
  datC3$Varname<-rep("asl_flag",nrow(datC3))
  
  summary(telec$prizm_social_one) # 4751 NAs
  telec%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC4
  datC4$N<-unclass(telec%>%filter(prizm_social_one%in%datC4$levels)%>%count(prizm_social_one))[[2]]
  datC4$ChurnPerc<-datC4$n/datC4$N
  datC4$Varname<-rep("prizm_social_one",nrow(datC4))
  
  summary(telec$area)  #  18 NA's
  telec%>%count(churn,levels=area)%>%filter(churn==1)->datC5
  datC5$N<-unclass(telec%>%filter(area%in%datC5$levels)%>%count(area))[[2]]
  datC5$ChurnPerc<-datC5$n/datC5$N
  datC5$Varname<-rep("area",nrow(datC5))
  
  summary(telec$refurb_new)    # 1 NA
  telec%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC6
  datC6$N<-unclass(telec%>%filter(refurb_new%in%datC6$levels)%>%count(refurb_new))[[2]]
  datC6$ChurnPerc<-datC6$n/datC6$N
  datC6$Varname<-rep("refurb_new",nrow(datC6))
  
  
  # hnd_webcap,	marital,	ethnic,	age1,	age2,	models, 
  
  summary(telec$hnd_webcap)     # 6062 NA's
  telec%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC7
  datC7$N<-unclass(telec%>%filter(hnd_webcap%in%datC7$levels)%>%count(hnd_webcap))[[2]]
  datC7$ChurnPerc<-datC7$n/datC7$N
  datC7$Varname<-rep("hnd_webcap",nrow(datC7))
  
  summary(telec$marital)    #1152 NAs
  telec%>%count(churn,levels=marital)%>%filter(churn==1)->datC8
  datC8$N<-unclass(telec%>%filter(marital%in%datC8$levels)%>%count(marital))[[2]]
  datC8$ChurnPerc<-datC8$n/datC8$N
  datC8$Varname<-rep("marital",nrow(datC8))
  
  summary(telec$ethnic)     #1152 NAs
  telec%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC9
  datC9$N<-unclass(telec%>%filter(ethnic%in%datC9$levels)%>%count(ethnic))[[2]]
  datC9$ChurnPerc<-datC9$n/datC9$N
  datC9$Varname<-rep("ethnic",nrow(datC9))
  
  summary(telec$age1)   #1152 NAs
  # telec$age1<-as.factor(telec$age1)
  telec%>%count(churn,levels=age1)%>%filter(churn==1)->datC10
  datC10$N<-unclass(telec%>%filter(age1%in%datC10$levels)%>%count(age1))[[2]]
  datC10$ChurnPerc<-datC10$n/datC10$N
  datC10$Varname<-rep("age1",nrow(datC10))
  
  summary(telec$age2)   #1152 NAs
  #telec$age2<-as.factor(telec$age2)
  telec%>%count(churn,levels=age2)%>%filter(churn==1)->datC11
  datC11$N<-unclass(telec%>%filter(age2%in%datC11$levels)%>%count(age2))[[2]]
  datC11$ChurnPerc<-datC11$n/datC11$N
  datC11$Varname<-rep("age2",nrow(datC11))
  
  summary(telec$models)   #1 NAs
  #telec$models<-as.factor(telec$models)
  telec%>%count(churn,levels=models)%>%filter(churn==1)->datC12
  datC12$N<-unclass(telec%>%filter(models%in%datC12$levels)%>%count(models))[[2]]
  datC12$ChurnPerc<-datC12$n/datC12$N
  datC12$Varname<-rep("models",nrow(datC12))
  
  
  # hnd_price,	actvsubs,	uniqsubs,	   forgntvl,	"dwlltype"         "dwllsize"  
  
  summary(telec$hnd_price)   #636 NAs
  #telec$hnd_price<-as.factor(telec$hnd_price)
  telec%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC13
  datC13$N<-unclass(telec%>%filter(hnd_price%in%datC13$levels)%>%count(hnd_price))[[2]]
  datC13$ChurnPerc<-datC13$n/datC13$N
  datC13$Varname<-rep("hnd_price",nrow(datC13))
  
  summary(telec$actvsubs)
  #telec$actvsubs<-as.factor(telec$actvsubs)
  telec%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC14
  datC14$N<-unclass(telec%>%filter(actvsubs%in%datC14$levels)%>%count(actvsubs))[[2]]
  datC14$ChurnPerc<-datC14$n/datC14$N
  datC14$Varname<-rep("actvsubs",nrow(datC14))
  
  summary(telec$uniqsubs)
  #telec$uniqsubs<-as.factor(telec$uniqsubs)
  telec%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC15
  datC15$N<-unclass(telec%>%filter(uniqsubs%in%datC15$levels)%>%count(uniqsubs))[[2]]
  datC15$ChurnPerc<-datC15$n/datC15$N
  datC15$Varname<-rep("uniqsubs",nrow(datC15))
  
  summary(telec$forgntvl)     #1152 NAs
  #telec$forgntvl<-as.factor(telec$forgntvl)
  telec%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC16
  datC16$N<-unclass(telec%>%filter(forgntvl%in%datC16$levels)%>%count(forgntvl))[[2]]
  datC16$ChurnPerc<-datC16$n/datC16$N
  datC16$Varname<-rep("forgntvl",nrow(datC16))
  
  summary(telec$dwlltype)   #  20824  NA's
  telec%>%count(churn,levels=dwlltype)%>%filter(churn==1)->datC17
  datC17$N<-unclass(telec%>%filter(dwlltype%in%datC17$levels)%>%count(dwlltype))[[2]]
  datC17$ChurnPerc<-datC17$n/datC17$N
  datC17$Varname<-rep("dwlltype",nrow(datC17))
  
  summary(telec$dwllsize)   #  24991  NA's
  telec%>%count(churn,levels=dwllsize)%>%filter(churn==1)->datC18
  datC18$N<-unclass(telec%>%filter(dwllsize%in%datC18$levels)%>%count(dwllsize))[[2]]
  datC18$ChurnPerc<-datC18$n/datC18$N
  datC18$Varname<-rep("dwllsize",nrow(datC18))  
  
  
  #    mtrcycle,	truck,	car_buy,    "csa"
  
  summary(telec$mtrcycle)     #1152 NAs
  #telec$mtrcycle<-as.factor(telec$mtrcycle)
  telec%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC19
  datC19$N<-unclass(telec%>%filter(mtrcycle%in%datC19$levels)%>%count(mtrcycle))[[2]]
  datC19$ChurnPerc<-datC19$n/datC19$N
  datC19$Varname<-rep("mtrcycle",nrow(datC19))
  
  summary(telec$truck)     #1152 NAs
  #telec$truck<-as.factor(telec$truck)
  telec%>%count(churn,levels=truck)%>%filter(churn==1)->datC20
  datC20$N<-unclass(telec%>%filter(truck%in%datC20$levels)%>%count(truck))[[2]]
  datC20$ChurnPerc<-datC20$n/datC20$N
  datC20$Varname<-rep("truck",nrow(datC20))
  
  summary(telec$churn)
  telec%>%count(churn,levels=churn)%>%filter(churn==1)->datC21
  datC21$N<-unclass(telec%>%filter(churn%in%datC21$levels)%>%count(churn))[[2]]
  datC21$ChurnPerc<-datC21$n/datC21$N
  datC21$Varname<-rep("churn",nrow(datC21))
  
  summary(telec$car_buy)     #1152 NAs
  telec%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC22
  datC22$N<-unclass(telec%>%filter(car_buy%in%datC22$levels)%>%count(car_buy))[[2]]
  datC22$ChurnPerc<-datC22$n/datC22$N
  datC22$Varname<-rep("car_buy",nrow(datC22))
  
  summary(telec$csa)   #  18  NA's
  telec%>%count(churn,levels=csa)%>%filter(churn==1)->datC23
  datC23$N<-unclass(telec%>%filter(csa%in%datC23$levels)%>%count(csa))[[2]]
  datC23$ChurnPerc<-datC23$n/datC23$N
  datC23$Varname<-rep("csa",nrow(datC23))  
  
DataCat1<-rbind(datC1,  datC2,  datC3,	datC4,	datC5,	datC6,	datC7,  datC8,	datC9,	datC10,	
                datC11, datC12,	datC13,	datC14, datC15,	datC16,	datC17, datC18, datC19, datC20,
                datC21, datC22, datC23)	

#write.csv(DataCat1,"CategoricalProfiling28March.csv",row.names = F)
}

teleco<-telec

str(teleco)
names(teleco)
summary(teleco)

#  Before Treatement Boxplot  #
{
  names(teleco)
  summary(teleco)
  str(teleco)
  dim(teleco)
  list<-names(teleco)
  list<-list[-c(6,8,24:42,44:46,52)]
  list
  par(mfrow=c(3,10))
  par("mar")
  par(mar=c(1,1,1,1))
  for(i in 1:length(list))
  {
    boxplot(teleco[,list[i]],main=list[i])
  }
  rm(list)
  dev.off()
  rm(i)
}


#----------------------------------------------------------------------------------#
#                            Data Preparation                                      #
#----------------------------------------------------------------------------------#

  # which are outliers imputed with closest to 98th percentile values  
  # which has NA's imputed with median values

{
  # "mou_Mean"         "totmrc_Mean"     "change_mou"    "drop_blk_Mean"      "owylis_vce_Range" 
  
  summary(teleco$mou_Mean)
  boxplot(teleco$mou_Mean,horizontal = T)
  quantile(teleco$mou_Mean,p=(90:100)/100,na.rm = T)
  teleco$mou_Mean<-ifelse(teleco$mou_Mean>=2082,2082,teleco$mou_Mean)  #imputing with 98th percentile
  teleco$mou_Mean[is.na(teleco$mou_Mean)]<-median(teleco$mou_Mean,na.rm = T)
  summary(teleco$mou_Mean)
  boxplot(teleco$mou_Mean,horizontal = T)
  
  summary(teleco$totmrc_Mean)
  boxplot(teleco$totmrc_Mean,horizontal = T)
  # this shows a negative value. Which cannot be there in monthly total recurring charge.
  # changing negatives to 0
  teleco$totmrc_Mean<-ifelse(teleco$totmrc_Mean<=0,0,teleco$totmrc_Mean)  #imputing negatives with zero
  quantile(teleco$totmrc_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$totmrc_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$totmrc_Mean<-ifelse(teleco$totmrc_Mean>=170.,170,teleco$totmrc_Mean)  #imputing with 99.8th percentile
  teleco$totmrc_Mean[is.na(teleco$totmrc_Mean)]<-median(teleco$totmrc_Mean,na.rm = T)
  summary(teleco$totmrc_Mean)
  boxplot(teleco$totmrc_Mean,horizontal = T)
  
  summary(teleco$change_mou)
  boxplot(teleco$change_mou,horizontal = T)
  max<-quantile((teleco$change_mou),0.75,na.rm=TRUE)+(IQR(teleco$change_mou,na.rm = TRUE)*1.5)
  min<-quantile((teleco$change_mou),0.25,na.rm=TRUE)-(IQR(teleco$change_mou,na.rm = TRUE)*1.5)
  #index<-which(teleco$change_mou < min | teleco$change_mou > max)
  #teleco$change_mou<-teleco$change_mou[-index,]
  max
  min
  teleco$change_mou<-ifelse(teleco$change_mou>=287.875,287.875,teleco$change_mou)
  teleco$change_mou<-ifelse(teleco$change_mou<=-303.125,-303.125,teleco$change_mou)
  teleco$change_mou[is.na(teleco$change_mou)]<-median(teleco$change_mou,na.rm = T)
  summary(teleco$change_mou)
  boxplot(teleco$change_mou,horizontal = T)
  
  summary(teleco$drop_blk_Mean)
  boxplot(teleco$drop_blk_Mean,horizontal = T)
  quantile(teleco$drop_blk_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$drop_blk_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$drop_blk_Mean<-ifelse(teleco$drop_blk_Mean>=126.66667,126.66667,teleco$drop_blk_Mean)  # imputing with 99.8th percentile
  summary(teleco$drop_blk_Mean)
  boxplot(teleco$drop_blk_Mean,horizontal = T)
  
  summary(teleco$owylis_vce_Range)
  boxplot(teleco$owylis_vce_Range,horizontal = T)
  quantile(teleco$owylis_vce_Range,p=(90:100)/100,na.rm = T)
  quantile(teleco$owylis_vce_Range,p=(990:1000)/1000,na.rm = T)
  teleco$owylis_vce_Range<-ifelse(teleco$owylis_vce_Range>=165,165,teleco$owylis_vce_Range) # imputing with 99.7th percentile
  summary(teleco$owylis_vce_Range)
  boxplot(teleco$owylis_vce_Range,horizontal = T)
  
  #"months"     "totcalls"         "income"           "eqpdays"          "custcare_Mean"    "callwait_Mean" 
  
  summary(teleco$months)
  hist(teleco$months)
  summary(teleco$months)
  
  summary(teleco$totcalls)
  boxplot(teleco$totcalls,horizontal = T)
  quantile(teleco$totcalls,p=(990:1000)/1000,na.rm = T)
  teleco$totcalls<-ifelse(teleco$totcalls>=29476.57,29476.57,teleco$totcalls) # imputing with 99.7th percentile
  summary(teleco$totcalls)
  boxplot(teleco$totcalls,horizontal = T)
  
  summary(teleco$income)
  datC1
  teleco$income<-ifelse(is.na(teleco$income),7,teleco$income) # imputing NA's with income group 7
  teleco$income<-as.factor(ifelse(teleco$income == 7,"High_Churn","Low_Churn"))
  summary(teleco$income)
  
  summary(teleco$eqpdays)
  # eqpdays cannot be negative. changing those to zero
  teleco$eqpdays<-ifelse(teleco$eqpdays<=0,0,teleco$eqpdays)
  quantile(teleco$eqpdays,p=(90:100)/100,na.rm = T)
  teleco$eqpdays<-ifelse(is.na(teleco$eqpdays),326,teleco$eqpdays)
  summary(teleco$eqpdays)
  
  summary(teleco$custcare_Mean)
  boxplot(teleco$custcare_Mean,horizontal = T)
  quantile(teleco$custcare_Mean,p=(90:100)/100,na.rm = T)
  teleco$custcare_Mean<-ifelse(teleco$custcare_Mean>=21,21,teleco$custcare_Mean) # imputing with 99th pecrentile
  summary(teleco$custcare_Mean)
  boxplot(teleco$custcare_Mean,horizontal = T)
  
  summary(teleco$callwait_Mean)
  boxplot(teleco$callwait_Mean,horizontal = T)
  quantile(teleco$callwait_Mean,p=(90:100)/100,na.rm = T)
  teleco$callwait_Mean<-ifelse(teleco$callwait_Mean>=23.67,23.67,teleco$callwait_Mean) # imputing with 99th pecrentile
  summary(teleco$callwait_Mean)
  boxplot(teleco$callwait_Mean,horizontal = T)
  
  #"iwylis_vce_Mean"  "ccrndmou_Range"   "adjqty"       "ovrrev_Mean"   "rev_Mean"       "ovrmou_Mean"    
  
  summary(teleco$iwylis_vce_Mean)
  boxplot(teleco$iwylis_vce_Mean,horizontal = T)
  quantile(teleco$iwylis_vce_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$iwylis_vce_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$iwylis_vce_Mean<-ifelse(teleco$iwylis_vce_Mean>=135.67,135.67,teleco$iwylis_vce_Mean)# imputing with 99.8th percentile
  summary(teleco$iwylis_vce_Mean)
  boxplot(teleco$iwylis_vce_Mean,horizontal = T)
  
  summary(teleco$ccrndmou_Range)
  boxplot(teleco$ccrndmou_Range,horizontal = T)
  quantile(teleco$ccrndmou_Range,p=(990:1000)/1000,na.rm = T)
  teleco$ccrndmou_Range<-ifelse(teleco$ccrndmou_Range>=150.408,150.408,teleco$ccrndmou_Range) # imputing with 99.8th percentile
  summary(teleco$ccrndmou_Range)
  boxplot(teleco$ccrndmou_Range,horizontal = T)
  
  summary(teleco$adjqty)
  boxplot(teleco$adjqty,horizontal = T)
  quantile(teleco$adjqty,p=(990:1000)/1000,na.rm = T)
  teleco$adjqty<-ifelse(teleco$adjqty>=34875.34,34875.34,teleco$adjqty) # imputing with 99.8th percentile
  summary(teleco$adjqty)
  boxplot(teleco$adjqty,horizontal = T)
  
  summary(teleco$ovrrev_Mean)
  boxplot(teleco$ovrrev_Mean,horizontal = T)
  quantile(teleco$ovrrev_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$ovrrev_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$ovrrev_Mean<-ifelse(teleco$ovrrev_Mean>=252.8218,252.8218,teleco$ovrrev_Mean)  # imputing with 99.8th percentile
  teleco$ovrrev_Mean[is.na(teleco$ovrrev_Mean)]<-median(teleco$ovrrev_Mean,na.rm = T)
  summary(teleco$ovrrev_Mean)
  boxplot(teleco$ovrrev_Mean,horizontal = T)
  
  summary(teleco$rev_Mean)
  boxplot(teleco$rev_Mean,horizontal = T)
  # MOnthly revenue cannot be negative, changing it to zero
  teleco$rev_Mean<-ifelse(teleco$rev_Mean<=0,0,teleco$rev_Mean)
  quantile(teleco$rev_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$rev_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$rev_Mean<-ifelse(teleco$rev_Mean>=373.0171,373.0171,teleco$rev_Mean) # imputing with 99.8th percentile
  teleco$rev_Mean[is.na(teleco$rev_Mean)]<-median(teleco$rev_Mean,na.rm = T)
  summary(teleco$rev_Mean)
  boxplot(teleco$rev_Mean,horizontal = T)
  
  summary(teleco$ovrmou_Mean)
  boxplot(teleco$ovrmou_Mean,horizontal = T)
  quantile(teleco$ovrmou_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$ovrmou_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$ovrmou_Mean<-ifelse(teleco$ovrmou_Mean>=716.6462,716.6462,teleco$ovrmou_Mean) # imputing with 99.7th percentile
  teleco$ovrmou_Mean[is.na(teleco$ovrmou_Mean)]<-median(teleco$ovrmou_Mean,na.rm = T)
  summary(teleco$ovrmou_Mean)
  boxplot(teleco$ovrmou_Mean,horizontal = T)
  
  #   "avg3mou"      "avgmou"           "avg3qty"       "avgqty"        "avg6mou"        "avg6qty"
  
  summary(teleco$avg3mou)
  boxplot(teleco$avg3mou,horizontal = T)
  quantile(teleco$avg3mou,p=(90:100)/100,na.rm = T)
  quantile(teleco$avg3mou,p=(990:1000)/1000,na.rm = T)
  teleco$avg3mou<-ifelse(teleco$avg3mou>=3414.224,3414.224,teleco$avg3mou) # imputing with 99.8th percentile
  summary(teleco$avg3mou)
  boxplot(teleco$avg3mou,horizontal = T)
  
  summary(teleco$avgmou)
  boxplot(teleco$avgmou,horizontal = T)
  quantile(teleco$avgmou,p=(90:100)/100,na.rm = T)
  quantile(teleco$avgmou,p=(990:1000)/1000,na.rm = T)
  teleco$avgmou<-ifelse(teleco$avgmou>=3111.431,3111.431,teleco$avgmou) # imputing with 99.9th percentile
  summary(teleco$avgmou)
  boxplot(teleco$avgmou,horizontal = T)
  
  summary(teleco$avg3qty)
  boxplot(teleco$avg3qty,horizontal = T)
  quantile(teleco$avg3qty,p=(90:100)/100,na.rm = T)
  quantile(teleco$avg3qty,p=(990:1000)/1000,na.rm = T)
  teleco$avg3qty<-ifelse(teleco$avg3qty>=1287.224,1287.224,teleco$avg3qty) # imputing with 99.7th percentile
  summary(teleco$avg3qty)
  boxplot(teleco$avg3qty,horizontal = T)
  
  summary(teleco$avgqty)
  boxplot(teleco$avgqty,horizontal = T)
  quantile(teleco$avgqty,p=(90:100)/100,na.rm = T)
  quantile(teleco$avgqty,p=(990:1000)/1000,na.rm = T)
  teleco$avgqty<-ifelse(teleco$avgqty>=1277.04,1277.04,teleco$avgqty) # imputing with 99.8th percentile
  summary(teleco$avgqty)
  boxplot(teleco$avgqty,horizontal = T)
  
  summary(teleco$avg6mou)
  boxplot(teleco$avg6mou,horizontal = T)
  quantile(teleco$avg6mou,p=(90:100)/100,na.rm = T)
  quantile(teleco$avg6mou,p=(990:1000)/1000,na.rm = T)
  teleco$avg6mou<-ifelse(teleco$avg6mou>=3177.12,3177.12,teleco$avg6mou) # imputing with 99.8th percentile
  teleco$avg6mou[is.na(teleco$avg6mou)]<-median(teleco$avg6mou,na.rm = T)
  summary(teleco$avg6mou)
  boxplot(teleco$avg6mou,horizontal = T)
  
  summary(teleco$avg6qty)
  boxplot(teleco$avg6qty,horizontal = T)
  quantile(teleco$avg6qty,p=(90:100)/100,na.rm = T)
  quantile(teleco$avg6qty,p=(990:1000)/1000,na.rm = T)
  teleco$avg6qty<-ifelse(teleco$avg6qty>=1367,1367,teleco$avg6qty) # imputing with 99.8th percentile
  teleco$avg6qty[is.na(teleco$avg6qty)]<-median(teleco$avg6qty,na.rm = T)
  summary(teleco$avg6qty)
  boxplot(teleco$avg6qty,horizontal = T)
  
  # "crclscod"      "asl_flag"         "prizm_social_one" "area"        "refurb_new"     
  
  summary(teleco$crclscod)
  datC2
  cr<-unclass(datC2%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$crclscod<-as.factor(ifelse(teleco$crclscod %in% cr,"High_Churn","Low_Churn"))
  summary(teleco$crclscod)
  
  summary(teleco$asl_flag)
  
  summary(teleco$prizm_social_one)
  datC4
  teleco$prizm_social_one<-as.factor(ifelse(is.na(teleco$prizm_social_one),"6",teleco$prizm_social_one)) # new element for NA's. 6 means Missing
  teleco%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC4
  datC4$N<-unclass(teleco%>%filter(prizm_social_one%in%datC4$levels)%>%count(prizm_social_one))[[2]]
  datC4$ChurnPerc<-datC4$n/datC4$N
  datC4
  ps<-unclass(datC4%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$prizm_social_one<-as.factor(ifelse(teleco$prizm_social_one %in% ps,"High_Churn","Low_Churn"))
  summary(teleco$prizm_social_one)
  
  
  summary(teleco$area)
  datC5
  teleco$area<-as.factor(ifelse(is.na(teleco$area),"15",teleco$area)) # imputing NA's with OHIO AREA(15) as its  closest to NA's churn percentage.
  teleco%>%count(churn,levels=area)%>%filter(churn==1)->datC5
  datC5$N<-unclass(teleco%>%filter(area%in%datC5$levels)%>%count(area))[[2]]
  datC5$ChurnPerc<-datC5$n/datC5$N
  datC5
  ar<-unclass(datC5%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$area<-as.factor(ifelse(teleco$area %in% ar,"High_Churn","Low_Churn"))
  summary(teleco$area)
  
  summary(teleco$refurb_new)
  datC6
  teleco$refurb_new<-as.factor(ifelse(is.na(teleco$refurb_new),"2",teleco$refurb_new)) # imputing NA with R, here 2 means R
  teleco$refurb_new<-factor(teleco$refurb_new,labels=c("N","R"))
  summary(teleco$refurb_new)
  
  # "hnd_webcap"    "marital"          "ethnic"        "age1"           "age2"        
  
  summary(teleco$hnd_webcap)
  datC7
  teleco$hnd_webcap<-as.factor(ifelse(is.na(teleco$hnd_webcap),"4",teleco$hnd_webcap)) # new element for NA's. 4 means Missing
  teleco%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC7
  datC7$N<-unclass(teleco%>%filter(hnd_webcap%in%datC7$levels)%>%count(hnd_webcap))[[2]]
  datC7$ChurnPerc<-datC7$n/datC7$N
  datC7
  hw<-unclass(datC7%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$hnd_webcap<-as.factor(ifelse(teleco$hnd_webcap %in% hw,"High_Churn","Low_Churn"))
  summary(teleco$hnd_webcap)
  
  summary(teleco$marital)
  datC8
  teleco$marital<-as.factor(ifelse(is.na(teleco$marital),"4",teleco$marital)) # new element for NA's. 4 means Missing
  teleco%>%count(churn,levels=marital)%>%filter(churn==1)->datC8
  datC8$N<-unclass(teleco%>%filter(marital%in%datC8$levels)%>%count(marital))[[2]]
  datC8$ChurnPerc<-datC8$n/datC8$N
  datC8
  mr<-unclass(datC8%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$marital<-as.factor(ifelse(teleco$marital %in% mr,"High_Churn","Low_Churn"))
  summary(teleco$marital)
  
  summary(teleco$ethnic)
  datC9
  teleco$ethnic<-as.factor(ifelse(is.na(teleco$ethnic),"9",teleco$ethnic)) # imputing NA's with M. 9 means M
  teleco%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC9
  datC9$N<-unclass(teleco%>%filter(ethnic%in%datC9$levels)%>%count(ethnic))[[2]]
  datC9$ChurnPerc<-datC9$n/datC9$N
  datC9
  et<-unclass(datC9%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$ethnic<-as.factor(ifelse(teleco$ethnic %in% et,"High_Churn","Low_Churn"))
  summary(teleco$ethnic)
  
  summary(teleco$age1)
  teleco$age1<-as.factor(ifelse(teleco$age1==0,"Default", ifelse(teleco$age1<=30,"Young",
                                                                 ifelse(teleco$age1>55,"Old","Mid_Age"))))
  datC10
  teleco%>%count(churn,levels=age1)%>%filter(churn==1)->datC10
  datC10$N<-unclass(teleco%>%filter(age1%in%datC10$levels)%>%count(age1))[[2]]
  datC10$ChurnPerc<-datC10$n/datC10$N
  datC10$Varname<-rep("age1",nrow(datC10))
  datC10
  teleco$age1<-as.factor(ifelse(is.na(teleco$age1),"3",teleco$age1)) # imputing NA's with old. 3 means old
  teleco%>%count(churn,levels=age1)%>%filter(churn==1)->datC10
  datC10$N<-unclass(teleco%>%filter(age1%in%datC10$levels)%>%count(age1))[[2]]
  datC10$ChurnPerc<-datC10$n/datC10$N
  datC10$Varname<-rep("age1",nrow(datC10))
  datC10
  teleco$age1<-factor(teleco$age1,labels=c("Default","Mid_Age","Old","Young"))
  summary(teleco$age1)
  
  summary(teleco$age2)
  teleco$age2<-as.factor(ifelse(teleco$age2==0,"Default", ifelse(teleco$age2<=30,"Young",
                                                                 ifelse(teleco$age2>55,"Old","Mid_Age"))))
  datC11
  teleco%>%count(churn,levels=age2)%>%filter(churn==1)->datC11
  datC11$N<-unclass(teleco%>%filter(age2%in%datC11$levels)%>%count(age2))[[2]]
  datC11$ChurnPerc<-datC11$n/datC11$N
  datC11$Varname<-rep("age2",nrow(datC11))
  datC11
  teleco$age2<-as.factor(ifelse(is.na(teleco$age2),"3",teleco$age2)) # imputing NA's with old. 3 means old
  teleco%>%count(churn,levels=age2)%>%filter(churn==1)->datC11
  datC11$N<-unclass(teleco%>%filter(age2%in%datC11$levels)%>%count(age2))[[2]]
  datC11$ChurnPerc<-datC11$n/datC11$N
  datC11$Varname<-rep("age2",nrow(datC11))
  datC11
  teleco$age2<-factor(teleco$age2,labels=c("Default","Mid_Age","Old","Young"))
  summary(teleco$age2)
  
  # "models"       "hnd_price"        "actvsubs"      "uniqsubs"        "forgntvl"  
  
  summary(teleco$models)
  datC12
  md<-unclass(datC12%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$models<-as.factor(ifelse(teleco$models %in% md,"High_Churn","Low_Churn"))
  summary(teleco$models)
  
  summary(teleco$hnd_price)
  datC13
  teleco$hnd_price <-ifelse(is.na(teleco$hnd_price),299.9899902,teleco$hnd_price)
  teleco$hnd_price<-as.factor(ifelse(teleco$hnd_price>200,"Expensive","Affordable"))
  summary(teleco$hnd_price)
  
  summary(teleco$actvsubs)
  datC14
  ac<-unclass(datC14%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$actvsubs<-as.factor(ifelse(teleco$actvsubs %in% ac,"High_Churn","Low_Churn"))
  summary(teleco$actvsubs)
  
  summary(teleco$uniqsubs)
  datC15
  uc<-unclass(datC15%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$uniqsubs<-as.factor(ifelse(teleco$uniqsubs %in% uc,"High_Churn","Low_Churn"))
  summary(teleco$actvsubs)
  
  summary(teleco$forgntvl)
  datC16
  teleco$forgntvl<-as.factor(ifelse(is.na(teleco$forgntvl),1,teleco$forgntvl)) # imputing NA with 1.
  summary(teleco$forgntvl)
  
  #"dwlltype"      "dwllsize"         "mtrcycle"     "truck"  "roam_Mean"    "car_buy"    "csa"     "da_Mean"      
  
  summary(teleco$dwlltype)
  datC17
  teleco$dwlltype<-ifelse(is.na(teleco$dwlltype),1,teleco$dwlltype)  # imputing na with M. 1 means M here 
  teleco$dwlltype<-factor(teleco$dwlltype,labels=c("M","S"))
  #teleco$dwlltype<-as.factor(teleco$dwlltype)
  summary(teleco$dwlltype)
  
  summary(teleco$dwllsize)
  datC18
  teleco$dwllsize<-ifelse(is.na(teleco$dwllsize),13,teleco$dwllsize)  # imputing na with M. 13 means M here 
  teleco%>%count(churn,levels=dwllsize)%>%filter(churn==1)->datC18
  datC18$N<-unclass(teleco%>%filter(dwllsize%in%datC18$levels)%>%count(dwllsize))[[2]]
  datC18$ChurnPerc<-datC18$n/datC18$N
  ds<-unclass(datC18%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$dwllsize<-as.factor(ifelse(teleco$dwllsize %in% ds,"High_Churn","Low_Churn"))
  summary(teleco$dwllsize)
  
  summary(teleco$mtrcycle)
  datC19
  teleco$mtrcycle<-as.factor(ifelse(is.na(teleco$mtrcycle),0,teleco$mtrcycle))  # imputing NA with 0. 
  summary(teleco$mtrcycle)
  
  summary(teleco$truck)
  datC20
  teleco$truck<-as.factor(ifelse(is.na(teleco$truck),1,teleco$truck))  # imputing NA with 1. 
  summary(teleco$truck)
  
  summary(teleco$roam_Mean)
  boxplot(teleco$roam_Mean,horizontal = T)
  quantile(teleco$roam_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$roam_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$roam_Mean<-ifelse(teleco$roam_Mean>=59.05145,59.05145,teleco$roam_Mean) # imputing with 99.8th percentile
  teleco$roam_Mean[is.na(teleco$roam_Mean)]<-median(teleco$roam_Mean,na.rm = T)
  summary(teleco$roam_Mean)
  boxplot(teleco$roam_Mean,horizontal = T)
  
  summary(teleco$churn)
  teleco$churn<-as.factor(teleco$churn)
  summary(teleco$churn)
  
  summary(teleco$car_buy)
  datC22
  teleco$car_buy<-ifelse(is.na(teleco$car_buy),1,teleco$car_buy)  # imputing na with New. 1 means New here 
  teleco$car_buy<-factor(teleco$car_buy,labels=c("New","Unknown"))
  summary(teleco$car_buy)
  
  summary(teleco$csa)
  datC23
  cb<-unclass(datC23%>%filter(ChurnPerc>0.25)%>%select(levels))[[1]]
  teleco$csa<-as.factor(ifelse(teleco$csa %in% cb,"High_Churn","Low_Churn"))
  summary(teleco$csa)
  
  summary(teleco$da_Mean)
  boxplot(teleco$da_Mean,horizontal = T)
  quantile(teleco$da_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$da_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$da_Mean<-ifelse(teleco$da_Mean>=21.25654,21.25654,teleco$da_Mean) # imputing with 99.9th percentile.
  teleco$da_Mean[is.na(teleco$da_Mean)]<-median(teleco$da_Mean,na.rm = T)
  summary(teleco$da_Mean)
  boxplot(teleco$da_Mean,horizontal = T)
  
  #  "adjmou"           "totrev"    "adjrev"           "avgrev"      "compcalls_Mean"   "plcdcalls_Mean"
  
  summary(teleco$adjmou)
  boxplot(teleco$adjmou,horizontal = T)
  quantile(teleco$adjmou,p=(90:100)/100,na.rm = T)
  quantile(teleco$adjmou,p=(990:1000)/1000,na.rm = T)
  teleco$adjmou<-ifelse(teleco$adjmou>=73299.37,73299.37,teleco$adjmou) # imputing with 99.8th percentile.
  summary(teleco$adjmou)
  boxplot(teleco$adjmou,horizontal = T)
  
  summary(teleco$totrev)
  boxplot(teleco$totrev,horizontal = T)
  quantile(teleco$totrev,p=(90:100)/100,na.rm = T)
  quantile(teleco$totrev,p=(990:1000)/1000,na.rm = T)
  teleco$totrev<-ifelse(teleco$totrev>=6995.497,6995.497,teleco$totrev)  # imputing with 99.8th percentile.
  summary(teleco$totrev)
  boxplot(teleco$totrev,horizontal = T)
  
  summary(teleco$adjrev)
  boxplot(teleco$adjrev,horizontal = T)
  quantile(teleco$adjrev,p=(90:100)/100,na.rm = T)
  quantile(teleco$adjrev,p=(990:1000)/1000,na.rm = T)
  teleco$adjrev<-ifelse(teleco$adjrev>=7975.516,7975.516,teleco$adjrev) # imputing with 99.9th percentile.
  summary(teleco$adjrev)
  boxplot(teleco$adjrev,horizontal = T)
  
  summary(teleco$avgrev)
  boxplot(teleco$avgrev,horizontal = T)
  quantile(teleco$avgrev,p=(90:100)/100,na.rm = T)
  quantile(teleco$avgrev,p=(990:1000)/1000,na.rm = T)
  teleco$avgrev<-ifelse(teleco$avgrev>=332.9742,332.9742,teleco$avgrev) # imputing with 99.9th percentile.
  summary(teleco$avgrev)
  boxplot(teleco$avgrev,horizontal = T)
  
  summary(teleco$compcalls_Mean)
  boxplot(teleco$compcalls_Mean,horizontal = T)
  quantile(teleco$compcalls_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$compcalls_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$compcalls_Mean<-ifelse(teleco$compcalls_Mean>=958.2613,958.2613,teleco$compcalls_Mean) # imputing with 99.9th percentile.
  summary(teleco$compcalls_Mean)
  boxplot(teleco$compcalls_Mean,horizontal = T)
  
  summary(teleco$plcdcalls_Mean)
  boxplot(teleco$plcdcalls_Mean,horizontal = T)
  quantile(teleco$plcdcalls_Mean,p=(90:100)/100,na.rm = T)
  quantile(teleco$plcdcalls_Mean,p=(990:1000)/1000,na.rm = T)
  teleco$plcdcalls_Mean<-ifelse(teleco$plcdcalls_Mean>=1286.9013,1286.9013,teleco$plcdcalls_Mean) # imputing with 99.8th percentile.
  summary(teleco$plcdcalls_Mean)
  boxplot(teleco$plcdcalls_Mean,horizontal = T)
  
}

str(teleco)
summary(teleco)

#  After Treatement Boxplot  #
{
  names(teleco)
  summary(teleco)
  str(teleco)
  dim(teleco)
  list<-names(teleco)
  list<-list[-c(6,8,24:42,44:46,52)]
  list
  par(mfrow=c(3,10))
  par("mar")
  par(mar=c(1,1,1,1))
  for(i in 1:length(list))
  {
    boxplot(teleco[,list[i]],main=list[i])
  }
  rm(list)
  dev.off()
  rm(i)
}


#categorical & continuous data is not on a same/similar scale. we need to treat this.
categorical<-c("income","crclscod","asl_flag","prizm_social_one","area","refurb_new","hnd_webcap","marital",
               "ethnic","age1","age2","models","hnd_price","actvsubs","uniqsubs","forgntvl","dwlltype",
               "dwllsize","mtrcycle","truck","churn","car_buy","csa","Customer_ID")
teleco[,!(names(teleco) %in% categorical)] = scale(teleco[,!(names(teleco) %in% categorical)])
names(teleco)
summary(teleco)


#----------------------------------------------------------------------------------#
#------------------------- Splitting data into train/test  ------------------------#
#----------------------------------------------------------------------------------#

set.seed(250)
index<-sample(nrow(teleco),0.70*nrow(teleco),replace=F)
train<-teleco[index,]
test<-teleco[-index,]
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

#----------------------------------------------------------------------------------#
#--------------------------- Building a logistic MOdel  ---------------------------#
#----------------------------------------------------------------------------------#

names(teleco)
names(train)
model<-glm(churn~ ., data=train[,-52],family="binomial")
summary(model)
library(car)
vif(model)


# stepwise greater than 5 vif  reduction and then reduction of insignificant variables.
# models 1 to 9
{
model1<-glm(churn~ .-totcalls, data=train[,-52],family="binomial")
summary(model1)
vif(model1)
model2<-glm(churn~ .-totcalls-adjrev, data=train[,-52],family="binomial")
summary(model2)
vif(model2)
model3<-glm(churn~ .-totcalls-adjrev-avg3mou, data=train[,-52],family="binomial")
summary(model3)
vif(model3)
model4<-glm(churn~ .-totcalls-adjrev-avg3mou-avg6qty, data=train[,-52],family="binomial")
summary(model4)
vif(model4)
model5<-glm(churn~ .-totcalls-adjrev-avg3mou-avg6qty-plcdcalls_Mean, data=train[,-52],family="binomial")
summary(model5)
vif(model5)
model6<-glm(churn~ .-totcalls-adjrev-avg3mou-avg6qty-plcdcalls_Mean-adjmou, data=train[,-52],family="binomial")
summary(model6)
vif(model6)
model7<-glm(churn~ .-totcalls-adjrev-avg3mou-avg6qty-plcdcalls_Mean-adjmou-avgqty, data=train[,-52],family="binomial")
summary(model7)
vif(model7)
model8<-glm(churn~ .-totcalls-adjrev-avg3mou-avg6qty-plcdcalls_Mean-adjmou-avgqty-ovrrev_Mean, data=train[,-52],family="binomial")
summary(model8)
vif(model8)
model9<-glm(churn~ .-custcare_Mean-callwait_Mean-iwylis_vce_Mean-ccrndmou_Range-avg3qty
            -avg6mou-crclscod-area-marital-forgntvl-mtrcycle-car_buy-da_Mean-avgrev-totrev
            -totcalls-adjrev-avg3mou-avg6qty-plcdcalls_Mean-adjmou-avgqty-ovrrev_Mean
            , data=train[,-52],family="binomial")
summary(model9)
vif(model9)
}

model10<-glm(churn~ .-custcare_Mean-callwait_Mean-iwylis_vce_Mean-ccrndmou_Range-avg3qty
             -avg6mou-crclscod-area-marital-forgntvl-mtrcycle-car_buy-da_Mean-avgrev-totrev
             -totcalls-adjrev-avg3mou-avg6qty-plcdcalls_Mean-adjmou-avgqty-ovrrev_Mean-age2
             , data=train[,-52],family="binomial")
summary(model10)
vif(model10)


trpred<-predict(model10,type = "response",train)
auc(train$churn,trpred)                                # Area under the curve: 0.631

table(telec$churn)/nrow(telec)
predtr<-as.factor(ifelse(trpred>=0.2392114,1,0))

library(irr)
kappa2(data.frame(train$churn,predtr))                 #   0.147
library(caret) 
confusionMatrix(predtr,train$churn,positive="1")       #  0.5915 
library(ROCR)
trpredic = prediction(trpred, train$churn)
trperf<-performance(trpredic,"tpr","fpr")
plot(trperf,col="red",main = "Training ROC Curve")
abline(0,1,lty=8,col="orange")

trauc<-performance(trpredic,"auc")
trauc<-unlist(slot(trauc,"y.values"))
trauc<-round(trauc,4)                                  #   0.6310055
trauc

tspred<-predict(model10,type = "response",test)
auc(test$churn,tspred)                                 #   Area under the curve: 0.6334

table(telec$churn)/nrow(telec)
predts<-as.factor(ifelse(tspred>=0.2392114,1,0))
library(irr)
kappa2(data.frame(test$churn,predts))                  #  0.15
library(caret)
confusionMatrix(predts,test$churn,positive="1")        #   0.5941
library(ROCR)
tspredic = prediction(tspred, test$churn)
tsperf<-performance(tspredic,"tpr","fpr")
plot(tsperf,col="red",main = "Test ROC Curve")
abline(0,1,lty=8,col="orange")

tsauc<-performance(tspredic,"auc")
tsauc<-unlist(slot(tsauc,"y.values"))
tsauc <-round(tsauc,4)                                 #  0.6334006
tsauc


#------------------------------------------------------------------------------#

#    Q1
head(sort(abs(model10$coefficients),decreasing = T),10)

#    Q2
summary(mob)


#   Q3
tail(sort(abs(model10$coefficients),decreasing = T),10)


#   Q4
library(gains)
test$churn<-as.numeric(test$churn)
test$pred<-predict(model10,type = "response",test)
gains(test$churn,test$pred,groups = 10)

quantile(test$pred,prob=c(0.10,0.20,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

targeted<-test[test$pred>0.2763574 & test$pred<=0.7556875 & test$churn=="1","Customer_ID"]
targeted
targeted<-as.data.frame(targeted)
nrow(targeted)                                       #  3869
#write.csv(targeted,"targeted.csv",row.names = F)


#   Q5

test$pred<-predict(model10,type = "response",test)
quantile(test$pred,prob=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
test$pred_levels<-as.factor(ifelse(test$pred < 0.1894031,"Low_Score",
                                   ifelse(test$pred >= 0.1894031 & test$pred < 0.2763574,"Medium_Score","High_Score")))

table(test$pred_levels,test$churn)

str(test$totrev)

quantile(test$totrev, prob=c(0.10,0.20,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
test$Revenue_Levels<-as.factor(ifelse(test$totrev < -0.563237,"Low_Revenue",
                                      ifelse(test$totrev >= -0.563237 & test$totrev < 0.121406,"Medium_Revenue","High_Revenue")))
table(test$Revenue_Levels,test$churn)
targeted1<-test[test$pred_levels=="High_Score"& test$Revenue_Levels=="High_Revenue","Customer_ID"]
targeted2<-test[test$pred_levels=="High_Score"& test$Revenue_Levels=="Medium_Revenue","Customer_ID"]
targeted3<-test[test$pred_levels=="Medium_Score"& test$Revenue_Levels=="High_Revenue","Customer_ID"]
targeted1<-as.data.frame(targeted1)
targeted2<-as.data.frame(targeted2)
targeted3<-as.data.frame(targeted3)
nrow(targeted1)     #    1957
nrow(targeted2)     #    2521     
nrow(targeted3)     #    1913

#Score/Revenue	Low_Revenue	Medium_Revenue	High_Revenue
#Low_Score---------------------------------------------
#Medium_Score--------------------------------------1913
#High_Score---------------------------2521---------1957

#write.csv(targeted1,"High_Score_High_Revenue_Target.csv",row.names = F)
#write.csv(targeted2,"High_Score_Mid_Revenue_Target.csv",row.names = F)
#write.csv(targeted3,"Mid_score_High_Revenue_Target.csv",row.names = F)
