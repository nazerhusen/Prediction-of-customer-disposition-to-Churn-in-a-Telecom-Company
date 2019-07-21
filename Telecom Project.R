#----------------------------------------------------------------------------------#
#                          set memory, read file                                   #
#----------------------------------------------------------------------------------#

{
memory.limit()
memory.size(10000)

setwd("C:/Users/...../Desktop/Telecom Project")

mo<-read.csv("c:/Users/...../Desktop/Telecom Project/telecom.csv")

options(scipen=999)
}

str(mo)

#----------------------------------------------------------------------------------#
#                               Libraries                                          #
#----------------------------------------------------------------------------------#

{
  library(dplyr)
  library(ggplot2)
  library(pROC)
}

#----------------------------------------------------------------------------------#
#                      Creating Data Quality Report                                #
#----------------------------------------------------------------------------------#

{
VarName<-names(mo)                    #creating Variable Names for dataframe qlty
qlty<-as.data.frame(VarName)          #making dataframe qlty
qlty$ClassType<-sapply(mo, class)     #adding class type to dataframe qlty
qlty$NoofRecs<-nrow(mo)               #adding number of records to dataframe qlty

str(mo)

for(i in 1:ncol(mo))       # this is a loop for counting unique values to add to dataframe qlty
{
  qlty$UniqueRecs[i]              <-length(unique(mo[,i]))
  qlty$AvailbleRecs[i]            <-colSums(!is.na(mo[i]))
  qlty$AvailbleRecPercent[i]      <-round(qlty$AvailbleRecs[i] / qlty$NoofRecs[i],2)
  qlty$Missing[i]                 <-colSums(is.na(mo[i]))
  qlty$MissingPercent[i]          <-round(qlty$Missing[i] / qlty$NoofRecs[i],2)
  qlty$Minimum[i]                 <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',min(mo[,i],na.rm = T),0),2)
  qlty$Maximum[i]                 <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',max(mo[,i],na.rm=T),0),2)  
  qlty$Mean[i]                    <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',colMeans(mo[i], na.rm = T),0),2)
  qlty$FifthPercentile[i]         <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',quantile(mo[i],p=0.05,na.rm = T),0),2)
  qlty$TenthPercentile[i]         <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',quantile(mo[i],p=0.10,na.rm = T),0),2)
  qlty$TwentyFifthPercentile[i]   <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',quantile(mo[i],p=0.25,na.rm = T),0),2)
  qlty$FiftythPercentile[i]       <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',quantile(mo[i],p=0.5,na.rm = T),0),2)
  qlty$SeventyFifthPercentile[i]  <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',quantile(mo[i],p=0.75,na.rm = T),0),2)
  qlty$NinetythPercentile[i]      <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',quantile(mo[i],p=0.90,na.rm = T),0),2)
  qlty$NinetyFifthPercentile[i]   <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',quantile(mo[i],p=0.95,na.rm = T),0),2)
  qlty$NinetyEightthPercentile[i] <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',quantile(mo[i],p=0.98,na.rm = T),0),2)
  qlty$NinetyNinethPercentile[i]  <-round(ifelse(class(mo[,i])=='integer' | class(mo[,i])=='numeric',quantile(mo[i],p=0.99,na.rm = T),0),2)
}
# export qlty to excel file to check how the data is spread, how many missing values in how many variables.
write.csv(qlty, "DQR.csv",row.names = F)
}

str(mo)

#----------------------------------------------------------------------------------#
#             Merging variables to create new derived variable                     #
#       we can derive at new variables by clubbing certain existing variables      #
#----------------------------------------------------------------------------------#

mo$compcalls_Mean<-mo$comp_dat_Mean+mo$comp_vce_Mean
mo$plcdcalls_Mean<-mo$plcd_dat_Mean+mo$plcd_vce_Mean

names(mo)

#----------------------------------------------------------------------------------#
#               Omitting variables with > 40% missing values                       #
#----------------------------------------------------------------------------------#

mob<-mo[,colMeans(is.na(mo))<=0.40]
names(mob)

#--------------  dropping insignificant Variables/Duplicates  ---------------------#
#    data definitions file has variables clearly defined & merged variables.       #
#----------------------------------------------------------------------------------#

{#rev_Range Not Significant
#mou_Range We already have mou_mean
#callwait_Range as i already have callwait_Mean
#datovr_Mean,   datovr_Range is already included in ovrrev_Mean.
#mou_opkv_Range, mou_pead_Mean & opk_dat_Mean are all offpeak/peak calls. doesnt have signifance here.
#recv_sms_Mean not significant.
#roam_Mean not significant.
#comp_vce_Mean,comp_dat_Mean,plcd_dat_Mean,plcd_vce_Mean as these are now captured in compcalls_Mean & plcdcalls_Mean
#drop_vce_Mean,drop_dat_Mean  blck_dat_Mean, drop_vce_Range can be dropped as these are already captured in drop_blk_Mean
}

mobi<-mob[,-c(3:4,7,9,17,23:24,48,52:54,59:63,69:70)]
str(mobi)
names(mobi)
summary(mobi)

##---------------------------   Data Exploration    ------------------------------##
###------------------  Binning Numeric/Integer Variables  -----------------------###
#----------------------------------------------------------------------------------#

names(mobi)
str(mobi)
{
  # mou_Mean,	totmrc_Mean,	change_mou,	drop_blk_Mean,	  owylis_vce_Range,	
  {
  summary(mobi$mou_Mean) #180NAs
  mobi%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
  dat1$N<-unclass(mobi%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
  dat1$churn_perc<-dat1$n/dat1$N
  dat1$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
  dat1$LessThan<-unclass(mobi%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
  dat1$varname<-rep("mou_Mean",nrow(dat1))
  
  summary(mobi$totmrc_Mean)   #180 NAs
  mobi%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
  dat2$N<-unclass(mobi%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
  dat2$churn_perc<-dat2$n/dat2$N
  dat2$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
  dat2$LessThan<-unclass(mobi%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
  dat2$varname<-rep("totmrc_Mean",nrow(dat2))
  
  summary(mobi$change_mou)  #412 NAs
  mobi%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
  dat3$N<-unclass(mobi%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
  dat3$churn_perc<-dat3$n/dat3$N
  dat3$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
  dat3$LessThan<-unclass(mobi%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
  dat3$varname<-rep("change_mou",nrow(dat3))
  
  summary(mobi$drop_blk_Mean)   
  mobi%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
  dat4$N<-unclass(mobi%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
  dat4$churn_perc<-dat4$n/dat4$N
  dat4$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
  dat4$LessThan<-unclass(mobi%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
  dat4$varname<-rep("drop_blk_Mean",nrow(dat4)) 
  
  summary(mobi$owylis_vce_Range)  
  mobi%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
  dat5$N<-unclass(mobi%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
  dat5$churn_perc<-dat5$n/dat5$N
  dat5$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min( owylis_vce_Range)))[[2]]
  dat5$LessThan<-unclass(mobi%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
  dat5$varname<-rep("owylis_vce_Range",nrow(dat5))  
}
  
  #	months,  totcalls,	eqpdays,    custcare_Mean,	callwait_Mean,	
  {
    summary(mobi$months)
    mobi%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
    dat6$N<-unclass(mobi%>%mutate(dec=ntile(months,n=5))%>%count(dec)%>%unname())[[2]]
    dat6$churn_perc<-dat6$n/dat6$N
    dat6$GreaterThan<-unclass(mobi%>%mutate(dec=ntile( months,n=5))%>%group_by(dec)%>%summarise(min(months)))[[2]]
    dat6$LessThan<-unclass(mobi%>%mutate(dec=ntile(months,n=5))%>%group_by(dec)%>%summarise(max(months)))[[2]]
    dat6$varname<-rep("months",nrow(dat6))
    
    summary(mobi$totcalls)
    mobi%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
    dat7$N<-unclass(mobi%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
    dat7$churn_perc<-dat7$n/dat7$N
    dat7$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min( totcalls)))[[2]]
    dat7$LessThan<-unclass(mobi%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
    dat7$varname<-rep("totcalls",nrow(dat7))
    
    summary(mobi$eqpdays)   #   1 NA
    mobi%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
    dat8$N<-unclass(mobi%>%mutate(dec=ntile(eqpdays,n=9))%>%count(dec)%>%unname())[[2]]
    dat8$churn_perc<-dat8$n/dat8$N
    dat8$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(eqpdays,n=9))%>%group_by(dec)%>%summarise(min( eqpdays)))[[2]]
    dat8$LessThan<-unclass(mobi%>%mutate(dec=ntile(eqpdays,n=9))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
    dat8$varname<-rep("eqpdays",nrow(dat8))
    
    summary(mobi$custcare_Mean)
    mobi%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
    dat9$N<-unclass(mobi%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(dec)%>%unname())[[2]]
    dat9$churn_perc<-dat9$n/dat9$N
    dat9$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(min( custcare_Mean)))[[2]]
    dat9$LessThan<-unclass(mobi%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
    dat9$varname<-rep("custcare_Mean",nrow(dat9))    
    
    summary(mobi$callwait_Mean)
    mobi%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
    dat10$N<-unclass(mobi%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(dec)%>%unname())[[2]]
    dat10$churn_perc<-dat10$n/dat10$N
    dat10$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
    dat10$LessThan<-unclass(mobi%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
    dat10$varname<-rep("callwait_Mean",nrow(dat10))
  }
  
  #	iwylis_vce_Mean,ccrndmou_Range,	adjqty,   ovrrev_Mean,	rev_Mean,	
  {
    summary(mobi$iwylis_vce_Mean)
    mobi%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
    dat11$N<-unclass(mobi%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
    dat11$churn_perc<-dat11$n/dat11$N
    dat11$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
    dat11$LessThan<-unclass(mobi%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
    dat11$varname<-rep("iwylis_vce_Mean",nrow(dat11))
    
    summary(mobi$ccrndmou_Range)
    mobi%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
    dat12$N<-unclass(mobi%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(dec)%>%unname())[[2]]
    dat12$churn_perc<-dat12$n/dat12$N
    dat12$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(min( ccrndmou_Range)))[[2]]
    dat12$LessThan<-unclass(mobi%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
    dat12$varname<-rep("ccrndmou_Range",nrow(dat12))
    
    summary(mobi$adjqty)
    mobi%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat13
    dat13$N<-unclass(mobi%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
    dat13$churn_perc<-dat13$n/dat13$N
    dat13$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min( adjqty)))[[2]]
    dat13$LessThan<-unclass(mobi%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
    dat13$varname<-rep("adjqty",nrow(dat13))
    
    summary(mobi$ovrrev_Mean)   #180 NAs
    mobi%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat14
    dat14$N<-unclass(mobi%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
    dat14$churn_perc<-dat14$n/dat14$N
    dat14$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
    dat14$LessThan<-unclass(mobi%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
    dat14$varname<-rep("ovrrev_Mean",nrow(dat14)) 
    
    summary(mobi$rev_Mean)   #180 NAs
    mobi%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat15
    dat15$N<-unclass(mobi%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
    dat15$churn_perc<-dat15$n/dat15$N
    dat15$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
    dat15$LessThan<-unclass(mobi%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
    dat15$varname<-rep("rev_Mean",nrow(dat15))
  }
  
  #	ovrmou_Mean,avg3mou,  	avgmou,	avg3qty,	avgqty,	avg6mou,	avg6qty,
  {  
    summary(mobi$ovrmou_Mean)   #180 NAs
    mobi%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat16
    dat16$N<-unclass(mobi%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
    dat16$churn_perc<-dat16$n/dat16$N
    dat16$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
    dat16$LessThan<-unclass(mobi%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
    dat16$varname<-rep("ovrmou_Mean",nrow(dat16))
    
    summary(mobi$avg3mou)
    mobi%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat17
    dat17$N<-unclass(mobi%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
    dat17$churn_perc<-dat17$n/dat17$N
    dat17$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min( avg3mou)))[[2]]
    dat17$LessThan<-unclass(mobi%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
    dat17$varname<-rep("avg3mou",nrow(dat17))
    
    summary(mobi$avgmou)
    mobi%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
    dat18$N<-unclass(mobi%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
    dat18$churn_perc<-dat18$n/dat18$N
    dat18$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
    dat18$LessThan<-unclass(mobi%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
    dat18$varname<-rep("avgmou",nrow(dat18))
    
    summary(mobi$avg3qty)
    mobi%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat19
    dat19$N<-unclass(mobi%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
    dat19$churn_perc<-dat19$n/dat19$N
    dat19$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
    dat19$LessThan<-unclass(mobi%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
    dat19$varname<-rep("avg3qty",nrow(dat19))
    
    summary(mobi$avgqty)
    mobi%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
    dat20$N<-unclass(mobi%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
    dat20$churn_perc<-dat20$n/dat20$N
    dat20$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
    dat20$LessThan<-unclass(mobi%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
    dat20$varname<-rep("avgqty",nrow(dat20))
    
    summary(mobi$avg6mou)    # 2055 NA's
    mobi%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat21
    dat21$N<-unclass(mobi%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
    dat21$churn_perc<-dat21$n/dat21$N
    dat21$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
    dat21$LessThan<-unclass(mobi%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
    dat21$varname<-rep("avg6mou",nrow(dat21))
    
    summary(mobi$avg6qty)    # 2055 NA's
    mobi%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
    dat22$N<-unclass(mobi%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
    dat22$churn_perc<-dat22$n/dat22$N
    dat22$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
    dat22$LessThan<-unclass(mobi%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
    dat22$varname<-rep("avg6qty",nrow(dat22))
  }
  
  #		roam_Mean,	 da_Mean,   adjmou,	totrev,	adjrev,	avgrev, 
  {
    summary(mobi$roam_Mean)   #171 NAs
    mobi%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
    dat23$N<-unclass(mobi%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(dec)%>%unname())[[2]]
    dat23$churn_perc<-dat23$n/dat23$N
    dat23$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
    dat23$LessThan<-unclass(mobi%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
    dat23$varname<-rep("roam_Mean",nrow(dat23))
    
    summary(mobi$da_Mean)   #171 NAs
    mobi%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
    dat24$N<-unclass(mobi%>%mutate(dec=ntile(da_Mean,n=10))%>%count(dec)%>%unname())[[2]]
    dat24$churn_perc<-dat24$n/dat24$N
    dat24$GreaterThan<-unclass(mobi%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
    dat24$LessThan<-unclass(mobi%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
    dat24$varname<-rep("da_Mean",nrow(dat24))
    
    summary(mobi$adjmou)
    mobi%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
    dat25$N<-unclass(mobi%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
    dat25$churn_perc<-dat25$n/dat25$N
    dat25$GreaterThan<-unclass(mobi%>%mutate(dec=ntile( adjmou,n=10))%>%group_by(dec)%>%summarise(min( adjmou)))[[2]]
    dat25$LessThan<-unclass(mobi%>%mutate(dec=ntile( adjmou,n=10))%>%group_by(dec)%>%summarise(max( adjmou)))[[2]]
    dat25$varname<-rep("adjmou",nrow(dat25))
    
    summary(mobi$totrev)
    mobi%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
    dat26$N<-unclass(mobi%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
    dat26$churn_perc<-dat26$n/dat26$N
    dat26$GreaterThan<-unclass(mobi%>%mutate(dec=ntile( totrev,n=10))%>%group_by(dec)%>%summarise(min( totrev)))[[2]]
    dat26$LessThan<-unclass(mobi%>%mutate(dec=ntile( totrev,n=10))%>%group_by(dec)%>%summarise(max( totrev)))[[2]]
    dat26$varname<-rep("totrev",nrow(dat26))
    
    summary(mobi$adjrev)
    mobi%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
    dat27$N<-unclass(mobi%>%mutate(dec=ntile( adjrev,n=10))%>%count(dec)%>%unname())[[2]]
    dat27$churn_perc<-dat27$n/dat27$N
    dat27$GreaterThan<-unclass(mobi%>%mutate(dec=ntile( adjrev,n=10))%>%group_by(dec)%>%summarise(min( adjrev)))[[2]]
    dat27$LessThan<-unclass(mobi%>%mutate(dec=ntile( adjrev,n=10))%>%group_by(dec)%>%summarise(max( adjrev)))[[2]]
    dat27$varname<-rep("adjrev",nrow(dat27))
    
    summary(mobi$avgrev)
    mobi%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
    dat28$N<-unclass(mobi%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
    dat28$churn_perc<-dat28$n/dat28$N
    dat28$GreaterThan<-unclass(mobi%>%mutate(dec=ntile( avgrev,n=10))%>%group_by(dec)%>%summarise(min( avgrev)))[[2]]
    dat28$LessThan<-unclass(mobi%>%mutate(dec=ntile( avgrev,n=10))%>%group_by(dec)%>%summarise(max( avgrev)))[[2]]
    dat28$varname<-rep("avgrev",nrow(dat28))
  } 
  
  #	"compcalls_Mean"   "plcdcalls_Mean"  
  {
    summary(mobi$compcalls_Mean)
    mobi%>%mutate(dec=ntile(compcalls_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
    dat29$N<-unclass(mobi%>%mutate(dec=ntile(compcalls_Mean,n=5))%>%count(dec)%>%unname())[[2]]
    dat29$churn_perc<-dat29$n/dat29$N
    dat29$GreaterThan<-unclass(mobi%>%mutate(dec=ntile( compcalls_Mean,n=5))%>%group_by(dec)%>%summarise(min(compcalls_Mean)))[[2]]
    dat29$LessThan<-unclass(mobi%>%mutate(dec=ntile( compcalls_Mean,n=5))%>%group_by(dec)%>%summarise(max(compcalls_Mean)))[[2]]
    dat29$varname<-rep("compcalls_Mean",nrow(dat29))
    
    summary(mobi$plcdcalls_Mean)
    mobi%>%mutate(dec=ntile(plcdcalls_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat30
    dat30$N<-unclass(mobi%>%mutate(dec=ntile(plcdcalls_Mean,n=5))%>%count(dec)%>%unname())[[2]]
    dat30$churn_perc<-dat30$n/dat30$N
    dat30$GreaterThan<-unclass(mobi%>%mutate(dec=ntile( plcdcalls_Mean,n=5))%>%group_by(dec)%>%summarise(min(plcdcalls_Mean)))[[2]]
    dat30$LessThan<-unclass(mobi%>%mutate(dec=ntile(plcdcalls_Mean,n=5))%>%group_by(dec)%>%summarise(max(plcdcalls_Mean)))[[2]]
    dat30$varname<-rep("plcdcalls_Mean",nrow(dat30))
}


DataCon<-rbind(dat1,	dat2,	  dat3,	  dat4,	  dat5,	  dat6,	  dat7,	  dat8,	  dat9,	  dat10,	
               dat11,	dat12,	dat13,	dat14,  dat15,	dat16,	dat17,	dat18,	dat19,	dat20,	
               dat21,	dat22,	dat23,	dat24,	dat25,	dat26,	dat27,	dat28,	dat29,	dat30)

write.csv(DataCon,"ContinuousProfiling.csv",row.names = F)
}

##---------------------------   Data Exploration    ------------------------------##
### ----------------      Binning factor variables      -------------------------###
#----------------------------------------------------------------------------------#

names(mobi)
str(mobi)
{
# "income"   crclscod,	asl_flag,	prizm_social_one,	area,	refurb_new,
{
  summary(mobi$income)   #  16336  NA's
  mobi%>%count(churn,levels=income)%>%filter(churn==1)->datC1
  datC1$N<-unclass(mobi%>%filter(income%in%datC1$levels)%>%count(income))[[2]]
  datC1$ChurnPerc<-datC1$n/datC1$N
  datC1$Varname<-rep("income",nrow(datC1))
  
  summary(mobi$crclscod)
  mobi%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC2
  datC2$N<-unclass(mobi%>%filter(crclscod%in%datC2$levels)%>%count(crclscod))[[2]]
  datC2$ChurnPerc<-datC2$n/datC2$N
  datC2$Varname<-rep("crclscod",nrow(datC2))
  
  summary(mobi$asl_flag)
  mobi%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC3
  datC3$N<-unclass(mobi%>%filter(asl_flag%in%datC3$levels)%>%count(asl_flag))[[2]]
  datC3$ChurnPerc<-datC3$n/datC3$N
  datC3$Varname<-rep("asl_flag",nrow(datC3))
  
  summary(mobi$prizm_social_one) # 4698 NAs
  mobi%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC4
  datC4$N<-unclass(mobi%>%filter(prizm_social_one%in%datC4$levels)%>%count(prizm_social_one))[[2]]
  datC4$ChurnPerc<-datC4$n/datC4$N
  datC4$Varname<-rep("prizm_social_one",nrow(datC4))
  
  summary(mobi$area)  #  18 NA's
  mobi%>%count(churn,levels=area)%>%filter(churn==1)->datC5
  datC5$N<-unclass(mobi%>%filter(area%in%datC5$levels)%>%count(area))[[2]]
  datC5$ChurnPerc<-datC5$n/datC5$N
  datC5$Varname<-rep("area",nrow(datC5))
  
  summary(mobi$refurb_new)    # 1 NA
  mobi%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC6
  datC6$N<-unclass(mobi%>%filter(refurb_new%in%datC6$levels)%>%count(refurb_new))[[2]]
  datC6$ChurnPerc<-datC6$n/datC6$N
  datC6$Varname<-rep("refurb_new",nrow(datC6))
}

# hnd_webcap,	marital,	ethnic,	age1,	age2,	models, 
{
  summary(mobi$hnd_webcap)     # 5054 NA's
  mobi%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC7
  datC7$N<-unclass(mobi%>%filter(hnd_webcap%in%datC7$levels)%>%count(hnd_webcap))[[2]]
  datC7$ChurnPerc<-datC7$n/datC7$N
  datC7$Varname<-rep("hnd_webcap",nrow(datC7))
  
  summary(mobi$marital)    #543 NAs
  mobi%>%count(churn,levels=marital)%>%filter(churn==1)->datC8
  datC8$N<-unclass(mobi%>%filter(marital%in%datC8$levels)%>%count(marital))[[2]]
  datC8$ChurnPerc<-datC8$n/datC8$N
  datC8$Varname<-rep("marital",nrow(datC8))
  
  summary(mobi$ethnic)     #543 NAs
  mobi%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC9
  datC9$N<-unclass(mobi%>%filter(ethnic%in%datC9$levels)%>%count(ethnic))[[2]]
  datC9$ChurnPerc<-datC9$n/datC9$N
  datC9$Varname<-rep("ethnic",nrow(datC9))
  
  summary(mobi$age1)   #1673 NAs
  mobi%>%count(churn,levels=age1)%>%filter(churn==1)->datC10
  datC10$N<-unclass(mobi%>%filter(age1%in%datC10$levels)%>%count(age1))[[2]]
  datC10$ChurnPerc<-datC10$n/datC10$N
  datC10$Varname<-rep("age1",nrow(datC10))
  
  summary(mobi$age2)   #1673 NAs
  mobi%>%count(churn,levels=age2)%>%filter(churn==1)->datC11
  datC11$N<-unclass(mobi%>%filter(age2%in%datC11$levels)%>%count(age2))[[2]]
  datC11$ChurnPerc<-datC11$n/datC11$N
  datC11$Varname<-rep("age2",nrow(datC11))
  
  summary(mobi$models)   #1 NAs
  mobi%>%count(churn,levels=models)%>%filter(churn==1)->datC12
  datC12$N<-unclass(mobi%>%filter(models%in%datC12$levels)%>%count(models))[[2]]
  datC12$ChurnPerc<-datC12$n/datC12$N
  datC12$Varname<-rep("models",nrow(datC12))
}

# hnd_price,	actvsubs,	uniqsubs,	 forgntvl,	"dwlltype"   "dwllsize"  
{
  summary(mobi$hnd_price)   #633 NAs
  mobi%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC13
  datC13$N<-unclass(mobi%>%filter(hnd_price%in%datC13$levels)%>%count(hnd_price))[[2]]
  datC13$ChurnPerc<-datC13$n/datC13$N
  datC13$Varname<-rep("hnd_price",nrow(datC13))
  
  summary(mobi$actvsubs)
  mobi%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC14
  datC14$N<-unclass(mobi%>%filter(actvsubs%in%datC14$levels)%>%count(actvsubs))[[2]]
  datC14$ChurnPerc<-datC14$n/datC14$N
  datC14$Varname<-rep("actvsubs",nrow(datC14))
  
  summary(mobi$uniqsubs)
  mobi%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC15
  datC15$N<-unclass(mobi%>%filter(uniqsubs%in%datC15$levels)%>%count(uniqsubs))[[2]]
  datC15$ChurnPerc<-datC15$n/datC15$N
  datC15$Varname<-rep("uniqsubs",nrow(datC15))
  
  summary(mobi$forgntvl)     #1320 NAs
  mobi%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC16
  datC16$N<-unclass(mobi%>%filter(forgntvl%in%datC16$levels)%>%count(forgntvl))[[2]]
  datC16$ChurnPerc<-datC16$n/datC16$N
  datC16$Varname<-rep("forgntvl",nrow(datC16))
  
  summary(mobi$dwlltype)   #  7653  NA's
  mobi%>%count(churn,levels=dwlltype)%>%filter(churn==1)->datC17
  datC17$N<-unclass(mobi%>%filter(dwlltype%in%datC17$levels)%>%count(dwlltype))[[2]]
  datC17$ChurnPerc<-datC17$n/datC17$N
  datC17$Varname<-rep("dwlltype",nrow(datC17))
  
  summary(mobi$dwllsize)   #  8945  NA's
  mobi%>%count(churn,levels=dwllsize)%>%filter(churn==1)->datC18
  datC18$N<-unclass(mobi%>%filter(dwllsize%in%datC18$levels)%>%count(dwllsize))[[2]]
  datC18$ChurnPerc<-datC18$n/datC18$N
  datC18$Varname<-rep("dwllsize",nrow(datC18))  
}

#  mtrcycle,	truck,	churn  car_buy,    "csa"
{  
  summary(mobi$mtrcycle)     #1658 NAs
  mobi%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC19
  datC19$N<-unclass(mobi%>%filter(mtrcycle%in%datC19$levels)%>%count(mtrcycle))[[2]]
  datC19$ChurnPerc<-datC19$n/datC19$N
  datC19$Varname<-rep("mtrcycle",nrow(datC19))
  
  summary(mobi$truck)     #1320 NAs
  mobi%>%count(churn,levels=truck)%>%filter(churn==1)->datC20
  datC20$N<-unclass(mobi%>%filter(truck%in%datC20$levels)%>%count(truck))[[2]]
  datC20$ChurnPerc<-datC20$n/datC20$N
  datC20$Varname<-rep("truck",nrow(datC20))
  
  summary(mobi$churn)
  mobi$churn<-as.factor(mobi$churn)
  mobi%>%count(churn,levels=churn)%>%filter(churn==1)->datC21
  datC21$N<-unclass(mobi%>%filter(churn%in%datC21$levels)%>%count(churn))[[2]]
  datC21$ChurnPerc<-datC21$n/datC21$N
  datC21$Varname<-rep("churn",nrow(datC21))
  
  summary(mobi$car_buy)     #1665 NAs
  mobi%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC22
  datC22$N<-unclass(mobi%>%filter(car_buy%in%datC22$levels)%>%count(car_buy))[[2]]
  datC22$ChurnPerc<-datC22$n/datC22$N
  datC22$Varname<-rep("car_buy",nrow(datC22))
  
  summary(mobi$csa)   #  70  NA's
  mobi%>%count(churn,levels=csa)%>%filter(churn==1)->datC23
  datC23$N<-unclass(mobi%>%filter(csa%in%datC23$levels)%>%count(csa))[[2]]
  datC23$ChurnPerc<-datC23$n/datC23$N
  datC23$Varname<-rep("csa",nrow(datC23))  
}  
DataCat1<-rbind(datC1,  datC2,  datC3,	datC4,	datC5,	datC6,	datC7,  datC8,	datC9,	datC10,	
                datC11, datC12,	datC13,	datC14, datC15,	datC16,	datC17, datC18, datC19, datC20,
                datC21, datC22, datC23)	

write.csv(DataCat1,"CategoricalProfiling.csv",row.names = F)
}

mobic<-mobi

str(mobic)
names(mobic)
summary(mobic)

#  Before Treatement Boxplot  #
{
  names(mobic)
  summary(mobic)
  str(mobic)
  dim(mobic)
  list<-names(mobic)
  list<-list[-c(6,8,24:42,44:46,52)]
  list
  par(mfrow=c(3,10))
  par("mar")
  par(mar=c(1,1,1,1))
  for(i in 1:length(list))
  {
    boxplot(mobic[,list[i]],main=list[i])
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
{  
summary(mobic$mou_Mean)
boxplot(mobic$mou_Mean,horizontal = T)
quantile(mobic$mou_Mean,p=(90:100)/100,na.rm = T)
mobic$mou_Mean<-ifelse(mobic$mou_Mean>=2083.24,2083.24,mobic$mou_Mean)  #imputing with 98th percentile
mobic$mou_Mean[is.na(mobic$mou_Mean)]<-median(mobic$mou_Mean,na.rm = T)
summary(mobic$mou_Mean)
boxplot(mobic$mou_Mean,horizontal = T)

summary(mobic$totmrc_Mean)
boxplot(mobic$totmrc_Mean,horizontal = T)
# this shows a negative value. Which cannot be there in monthly total recurring charge.
# changing negatives to 0
mobic$totmrc_Mean<-ifelse(mobic$totmrc_Mean<=0,0,mobic$totmrc_Mean)  #imputing negatives with zero
quantile(mobic$totmrc_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$totmrc_Mean,p=(990:1000)/1000,na.rm = T)
mobic$totmrc_Mean<-ifelse(mobic$totmrc_Mean>=199.99,199.99,mobic$totmrc_Mean)  #imputing with 99.8th percentile
mobic$totmrc_Mean[is.na(mobic$totmrc_Mean)]<-median(mobic$totmrc_Mean,na.rm = T)
summary(mobic$totmrc_Mean)
boxplot(mobic$totmrc_Mean,horizontal = T)

summary(mobic$change_mou)
boxplot(mobic$change_mou,horizontal = T)
max<-quantile((mobic$change_mou),0.75,na.rm=TRUE)+(IQR(mobic$change_mou,na.rm = TRUE)*1.5)
min<-quantile((mobic$change_mou),0.25,na.rm=TRUE)-(IQR(mobic$change_mou,na.rm = TRUE)*1.5)
max
min
mobic$change_mou<-ifelse(mobic$change_mou>=286.875,286.875,mobic$change_mou)
mobic$change_mou<-ifelse(mobic$change_mou<=-302.125,-302.125,mobic$change_mou)
mobic$change_mou[is.na(mobic$change_mou)]<-median(mobic$change_mou,na.rm = T)
summary(mobic$change_mou)
boxplot(mobic$change_mou,horizontal = T)

summary(mobic$drop_blk_Mean)
boxplot(mobic$drop_blk_Mean,horizontal = T)
quantile(mobic$drop_blk_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$drop_blk_Mean,p=(990:1000)/1000,na.rm = T)
mobic$drop_blk_Mean<-ifelse(mobic$drop_blk_Mean>=126.66667,126.66667,mobic$drop_blk_Mean)  # imputing with 99.8th percentile
summary(mobic$drop_blk_Mean)
boxplot(mobic$drop_blk_Mean,horizontal = T)

summary(mobic$owylis_vce_Range)
boxplot(mobic$owylis_vce_Range,horizontal = T)
quantile(mobic$owylis_vce_Range,p=(90:100)/100,na.rm = T)
quantile(mobic$owylis_vce_Range,p=(990:1000)/1000,na.rm = T)
mobic$owylis_vce_Range<-ifelse(mobic$owylis_vce_Range>=165,165,mobic$owylis_vce_Range) # imputing with 99.7th percentile
summary(mobic$owylis_vce_Range)
boxplot(mobic$owylis_vce_Range,horizontal = T)
}
#"months"     "totcalls"         "income"           "eqpdays"          "custcare_Mean"    "callwait_Mean" 
{
summary(mobic$months)
hist(mobic$months)
summary(mobic$months)

summary(mobic$totcalls)
boxplot(mobic$totcalls,horizontal = T)
quantile(mobic$totcalls,p=(990:1000)/1000,na.rm = T)
mobic$totcalls<-ifelse(mobic$totcalls>=29540.29,29540.29,mobic$totcalls) # imputing with 99.7th percentile
summary(mobic$totcalls)
boxplot(mobic$totcalls,horizontal = T)

summary(mobic$income)
datC1
mobic$income<-ifelse(is.na(mobic$income),1,mobic$income) # imputing NA's with income group 1
inc<-unclass(datC1%>%filter(ChurnPerc>0.32)%>%select(levels))[[1]]
mobic$income<-as.factor(ifelse(mobic$income %in% inc,"High_Churn","Low_Churn"))
summary(mobic$income)

summary(mobic$eqpdays)
# eqpdays cannot be negative. changing those to zero
mobic$eqpdays<-ifelse(mobic$eqpdays<=0,0,mobic$eqpdays)
quantile(mobic$eqpdays,p=(90:100)/100,na.rm = T)
mobic$eqpdays<-ifelse(is.na(mobic$eqpdays),326,mobic$eqpdays)
summary(mobic$eqpdays)

summary(mobic$custcare_Mean)
boxplot(mobic$custcare_Mean,horizontal = T)
quantile(mobic$custcare_Mean,p=(90:100)/100,na.rm = T)
mobic$custcare_Mean<-ifelse(mobic$custcare_Mean>=21,21,mobic$custcare_Mean) # imputing with 99th pecrentile
summary(mobic$custcare_Mean)
boxplot(mobic$custcare_Mean,horizontal = T)

summary(mobic$callwait_Mean)
boxplot(mobic$callwait_Mean,horizontal = T)
quantile(mobic$callwait_Mean,p=(90:100)/100,na.rm = T)
mobic$callwait_Mean<-ifelse(mobic$callwait_Mean>=23.666667,23.666667,mobic$callwait_Mean) # imputing with 99th pecrentile
summary(mobic$callwait_Mean)
boxplot(mobic$callwait_Mean,horizontal = T)
}
#"iwylis_vce_Mean"  "ccrndmou_Range"   "adjqty"       "ovrrev_Mean"   "rev_Mean"       "ovrmou_Mean"    
{
summary(mobic$iwylis_vce_Mean)
boxplot(mobic$iwylis_vce_Mean,horizontal = T)
quantile(mobic$iwylis_vce_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$iwylis_vce_Mean,p=(990:1000)/1000,na.rm = T)
mobic$iwylis_vce_Mean<-ifelse(mobic$iwylis_vce_Mean>=135.66667,135.66667,mobic$iwylis_vce_Mean)# imputing with 99.8th percentile
summary(mobic$iwylis_vce_Mean)
boxplot(mobic$iwylis_vce_Mean,horizontal = T)

summary(mobic$ccrndmou_Range)
boxplot(mobic$ccrndmou_Range,horizontal = T)
quantile(mobic$ccrndmou_Range,p=(990:1000)/1000,na.rm = T)
mobic$ccrndmou_Range<-ifelse(mobic$ccrndmou_Range>=149.788,149.788,mobic$ccrndmou_Range) # imputing with 99.8th percentile
summary(mobic$ccrndmou_Range)
boxplot(mobic$ccrndmou_Range,horizontal = T)

summary(mobic$adjqty)
boxplot(mobic$adjqty,horizontal = T)
quantile(mobic$adjqty,p=(990:1000)/1000,na.rm = T)
mobic$adjqty<-ifelse(mobic$adjqty>=34963.94,34963.94,mobic$adjqty) # imputing with 99.8th percentile
summary(mobic$adjqty)
boxplot(mobic$adjqty,horizontal = T)

summary(mobic$ovrrev_Mean)
boxplot(mobic$ovrrev_Mean,horizontal = T)
quantile(mobic$ovrrev_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$ovrrev_Mean,p=(990:1000)/1000,na.rm = T)
mobic$ovrrev_Mean<-ifelse(mobic$ovrrev_Mean>=253.0204,253.0204,mobic$ovrrev_Mean)  # imputing with 99.8th percentile
mobic$ovrrev_Mean[is.na(mobic$ovrrev_Mean)]<-median(mobic$ovrrev_Mean,na.rm = T)
summary(mobic$ovrrev_Mean)
boxplot(mobic$ovrrev_Mean,horizontal = T)

summary(mobic$rev_Mean)
boxplot(mobic$rev_Mean,horizontal = T)
# MOnthly revenue cannot be negative, changing it to zero
mobic$rev_Mean<-ifelse(mobic$rev_Mean<=0,0,mobic$rev_Mean)
quantile(mobic$rev_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$rev_Mean,p=(990:1000)/1000,na.rm = T)
mobic$rev_Mean<-ifelse(mobic$rev_Mean>=372.5045,372.5045,mobic$rev_Mean) # imputing with 99.8th percentile
mobic$rev_Mean[is.na(mobic$rev_Mean)]<-median(mobic$rev_Mean,na.rm = T)
summary(mobic$rev_Mean)
boxplot(mobic$rev_Mean,horizontal = T)

summary(mobic$ovrmou_Mean)
boxplot(mobic$ovrmou_Mean,horizontal = T)
quantile(mobic$ovrmou_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$ovrmou_Mean,p=(990:1000)/1000,na.rm = T)
mobic$ovrmou_Mean<-ifelse(mobic$ovrmou_Mean>=716.7635,716.7635,mobic$ovrmou_Mean) # imputing with 99.7th percentile
mobic$ovrmou_Mean[is.na(mobic$ovrmou_Mean)]<-median(mobic$ovrmou_Mean,na.rm = T)
summary(mobic$ovrmou_Mean)
boxplot(mobic$ovrmou_Mean,horizontal = T)
}
#   "avg3mou"      "avgmou"           "avg3qty"       "avgqty"        "avg6mou"        "avg6qty"
{
summary(mobic$avg3mou)
boxplot(mobic$avg3mou,horizontal = T)
quantile(mobic$avg3mou,p=(90:100)/100,na.rm = T)
quantile(mobic$avg3mou,p=(990:1000)/1000,na.rm = T)
mobic$avg3mou<-ifelse(mobic$avg3mou>=3415.364,3415.364,mobic$avg3mou) # imputing with 99.8th percentile
summary(mobic$avg3mou)
boxplot(mobic$avg3mou,horizontal = T)

summary(mobic$avgmou)
boxplot(mobic$avgmou,horizontal = T)
quantile(mobic$avgmou,p=(90:100)/100,na.rm = T)
quantile(mobic$avgmou,p=(990:1000)/1000,na.rm = T)
mobic$avgmou<-ifelse(mobic$avgmou>=3112.539,3112.539,mobic$avgmou) # imputing with 99.9th percentile
summary(mobic$avgmou)
boxplot(mobic$avgmou,horizontal = T)

summary(mobic$avg3qty)
boxplot(mobic$avg3qty,horizontal = T)
quantile(mobic$avg3qty,p=(90:100)/100,na.rm = T)
quantile(mobic$avg3qty,p=(990:1000)/1000,na.rm = T)
mobic$avg3qty<-ifelse(mobic$avg3qty>=1289.546,1289.546,mobic$avg3qty) # imputing with 99.7th percentile
summary(mobic$avg3qty)
boxplot(mobic$avg3qty,horizontal = T)

summary(mobic$avgqty)
boxplot(mobic$avgqty,horizontal = T)
quantile(mobic$avgqty,p=(90:100)/100,na.rm = T)
quantile(mobic$avgqty,p=(990:1000)/1000,na.rm = T)
mobic$avgqty<-ifelse(mobic$avgqty>=1280.7407,1280.7407,mobic$avgqty) # imputing with 99.8th percentile
summary(mobic$avgqty)
boxplot(mobic$avgqty,horizontal = T)

summary(mobic$avg6mou)
boxplot(mobic$avg6mou,horizontal = T)
quantile(mobic$avg6mou,p=(90:100)/100,na.rm = T)
quantile(mobic$avg6mou,p=(990:1000)/1000,na.rm = T)
mobic$avg6mou<-ifelse(mobic$avg6mou>=3179.388,3179.388,mobic$avg6mou) # imputing with 99.8th percentile
mobic$avg6mou[is.na(mobic$avg6mou)]<-median(mobic$avg6mou,na.rm = T)
summary(mobic$avg6mou)
boxplot(mobic$avg6mou,horizontal = T)

summary(mobic$avg6qty)
boxplot(mobic$avg6qty,horizontal = T)
quantile(mobic$avg6qty,p=(90:100)/100,na.rm = T)
quantile(mobic$avg6qty,p=(990:1000)/1000,na.rm = T)
mobic$avg6qty<-ifelse(mobic$avg6qty>=1367,1367,mobic$avg6qty) # imputing with 99.8th percentile
mobic$avg6qty[is.na(mobic$avg6qty)]<-median(mobic$avg6qty,na.rm = T)
summary(mobic$avg6qty)
boxplot(mobic$avg6qty,horizontal = T)
}
# "crclscod"      "asl_flag"         "prizm_social_one" "area"        "refurb_new"     
{
summary(mobic$crclscod)
datC2
cr<-unclass(datC2%>%filter(ChurnPerc>0.32)%>%select(levels))[[1]]
mobic$crclscod<-as.factor(ifelse(mobic$crclscod %in% cr,"High_Churn","Low_Churn"))
summary(mobic$crclscod)

summary(mobic$asl_flag)

summary(mobic$prizm_social_one)
datC4
mobic$prizm_social_one<-as.factor(ifelse(is.na(mobic$prizm_social_one),"6",mobic$prizm_social_one)) # new element for NA's. 6 means Missing
  mobic%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC4
  datC4$N<-unclass(mobic%>%filter(prizm_social_one%in%datC4$levels)%>%count(prizm_social_one))[[2]]
  datC4$ChurnPerc<-datC4$n/datC4$N
datC4
ps<-unclass(datC4%>%filter(ChurnPerc>0.32)%>%select(levels))[[1]]
mobic$prizm_social_one<-as.factor(ifelse(mobic$prizm_social_one %in% ps,"High_Churn","Low_Churn"))
summary(mobic$prizm_social_one)

summary(mobic$area)
datC5
mobic$area<-as.factor(ifelse(is.na(mobic$area),"9",mobic$area)) # imputing NA's with LOS ANGELES AREA(9) as its  closest to NA's churn percentage.
  mobic%>%count(churn,levels=area)%>%filter(churn==1)->datC5
  datC5$N<-unclass(mobic%>%filter(area%in%datC5$levels)%>%count(area))[[2]]
  datC5$ChurnPerc<-datC5$n/datC5$N
datC5
ar<-unclass(datC5%>%filter(ChurnPerc>0.32)%>%select(levels))[[1]]
mobic$area<-as.factor(ifelse(mobic$area %in% ar,"High_Churn","Low_Churn"))
summary(mobic$area)

summary(mobic$refurb_new)
datC6
mobic$refurb_new<-as.factor(ifelse(is.na(mobic$refurb_new),"1",mobic$refurb_new)) # imputing NA with R, here 2 means R
mobic$refurb_new<-factor(mobic$refurb_new,labels=c("N","R"))
summary(mobic$refurb_new)
}
# "hnd_webcap"    "marital"          "ethnic"        "age1"           "age2"        
{
summary(mobic$hnd_webcap)
datC7
mobic$hnd_webcap<-as.factor(ifelse(is.na(mobic$hnd_webcap),"4",mobic$hnd_webcap)) # new element for NA's. 4 means Missing
  mobic%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC7
  datC7$N<-unclass(mobic%>%filter(hnd_webcap%in%datC7$levels)%>%count(hnd_webcap))[[2]]
  datC7$ChurnPerc<-datC7$n/datC7$N
datC7
hw<-unclass(datC7%>%filter(ChurnPerc>0.32)%>%select(levels))[[1]]
mobic$hnd_webcap<-as.factor(ifelse(mobic$hnd_webcap %in% hw,"High_Churn","Low_Churn"))
summary(mobic$hnd_webcap)
  
summary(mobic$marital)
datC8
mobic$marital<-as.factor(ifelse(is.na(mobic$marital),"6",mobic$marital)) # new element for NA's. 6 means Missing
  mobic%>%count(churn,levels=marital)%>%filter(churn==1)->datC8
  datC8$N<-unclass(mobic%>%filter(marital%in%datC8$levels)%>%count(marital))[[2]]
  datC8$ChurnPerc<-datC8$n/datC8$N
datC8
mr<-unclass(datC8%>%filter(ChurnPerc>0.33)%>%select(levels))[[1]]
mobic$marital<-as.factor(ifelse(mobic$marital %in% mr,"High_Churn","Low_Churn"))
summary(mobic$marital)

summary(mobic$ethnic)
datC9
mobic$ethnic<-as.factor(ifelse(is.na(mobic$ethnic),"11",mobic$ethnic)) # imputing NA's with O. 11 means O
  mobic%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC9
  datC9$N<-unclass(mobic%>%filter(ethnic%in%datC9$levels)%>%count(ethnic))[[2]]
  datC9$ChurnPerc<-datC9$n/datC9$N
datC9
et<-unclass(datC9%>%filter(ChurnPerc>0.32)%>%select(levels))[[1]]
mobic$ethnic<-as.factor(ifelse(mobic$ethnic %in% et,"High_Churn","Low_Churn"))
summary(mobic$ethnic)

summary(mobic$age1)
mobic$age1<-as.factor(ifelse(mobic$age1==0,"Default", ifelse(mobic$age1<=30,"Young",
              ifelse(mobic$age1>55,"Old","Mid_Age"))))
datC10
  mobic%>%count(churn,levels=age1)%>%filter(churn==1)->datC10
  datC10$N<-unclass(mobic%>%filter(age1%in%datC10$levels)%>%count(age1))[[2]]
  datC10$ChurnPerc<-datC10$n/datC10$N
  datC10$Varname<-rep("age1",nrow(datC10))
datC10
mobic$age1<-as.factor(ifelse(is.na(mobic$age1),"2",mobic$age1)) # imputing NA's with old. 3 means old
  mobic%>%count(churn,levels=age1)%>%filter(churn==1)->datC10
  datC10$N<-unclass(mobic%>%filter(age1%in%datC10$levels)%>%count(age1))[[2]]
  datC10$ChurnPerc<-datC10$n/datC10$N
  datC10$Varname<-rep("age1",nrow(datC10))
datC10
mobic$age1<-factor(mobic$age1,labels=c("Default","Mid_Age","Old","Young"))
summary(mobic$age1)

summary(mobic$age2)
mobic$age2<-as.factor(ifelse(mobic$age2==0,"Default", ifelse(mobic$age2<=30,"Young",
              ifelse(mobic$age2>55,"Old","Mid_Age"))))
datC11
  mobic%>%count(churn,levels=age2)%>%filter(churn==1)->datC11
  datC11$N<-unclass(mobic%>%filter(age2%in%datC11$levels)%>%count(age2))[[2]]
  datC11$ChurnPerc<-datC11$n/datC11$N
  datC11$Varname<-rep("age2",nrow(datC11))
datC11
mobic$age2<-as.factor(ifelse(is.na(mobic$age2),"2",mobic$age2)) # imputing NA's with old. 3 means old
  mobic%>%count(churn,levels=age2)%>%filter(churn==1)->datC11
  datC11$N<-unclass(mobic%>%filter(age2%in%datC11$levels)%>%count(age2))[[2]]
  datC11$ChurnPerc<-datC11$n/datC11$N
  datC11$Varname<-rep("age2",nrow(datC11))
datC11
mobic$age2<-factor(mobic$age2,labels=c("Default","Mid_Age","Old","Young"))
summary(mobic$age2)
}
# "models"       "hnd_price"        "actvsubs"      "uniqsubs"        "forgntvl"  
{
summary(mobic$models)
datC12
mobic$models<-ifelse(is.na(mobic$models),1,mobic$models)
md<-unclass(datC12%>%filter(ChurnPerc>0.35)%>%select(levels))[[1]]
mobic$models<-as.factor(ifelse(mobic$models %in% md,"High_Churn","Low_Churn"))
summary(mobic$models)

summary(mobic$hnd_price)
datC13
mobic$hnd_price <-ifelse(is.na(mobic$hnd_price),499.99,mobic$hnd_price)
mobic$hnd_price<-as.factor(ifelse(mobic$hnd_price>139.99,"Expensive","Affordable"))
summary(mobic$hnd_price)

summary(mobic$actvsubs)
datC14
ac<-unclass(datC14%>%filter(ChurnPerc>0.32)%>%select(levels))[[1]]
mobic$actvsubs<-as.factor(ifelse(mobic$actvsubs %in% ac,"High_Churn","Low_Churn"))
summary(mobic$actvsubs)

summary(mobic$uniqsubs)
datC15
uc<-unclass(datC15%>%filter(ChurnPerc>0.32)%>%select(levels))[[1]]
mobic$uniqsubs<-as.factor(ifelse(mobic$uniqsubs %in% uc,"High_Churn","Low_Churn"))
summary(mobic$actvsubs)

summary(mobic$forgntvl)
datC16
mobic$forgntvl<-as.factor(ifelse(is.na(mobic$forgntvl),0,mobic$forgntvl)) # imputing NA with 0.
summary(mobic$forgntvl)
}
#"dwlltype"      "dwllsize"         "mtrcycle"     "truck"  "roam_Mean"    "car_buy"    "csa"     "da_Mean"      
{
summary(mobic$dwlltype)
datC17
mobic$dwlltype<-ifelse(is.na(mobic$dwlltype),1,mobic$dwlltype)  # imputing na with M. 1 means M here 
mobic$dwlltype<-factor(mobic$dwlltype,labels=c("M","S"))
summary(mobic$dwlltype)

summary(mobic$dwllsize)
datC18
mobic$dwllsize<-ifelse(is.na(mobic$dwllsize),2,mobic$dwllsize)  # imputing na with M. 13 means M here 
  mobic%>%count(churn,levels=dwllsize)%>%filter(churn==1)->datC18
  datC18$N<-unclass(mobic%>%filter(dwllsize%in%datC18$levels)%>%count(dwllsize))[[2]]
  datC18$ChurnPerc<-datC18$n/datC18$N
ds<-unclass(datC18%>%filter(ChurnPerc>0.32)%>%select(levels))[[1]]
mobic$dwllsize<-as.factor(ifelse(mobic$dwllsize %in% ds,"High_Churn","Low_Churn"))
summary(mobic$dwllsize)

summary(mobic$mtrcycle)
datC19
mobic$mtrcycle<-as.factor(ifelse(is.na(mobic$mtrcycle),1,mobic$mtrcycle))  # imputing NA with 0. 
summary(mobic$mtrcycle)

summary(mobic$truck)
datC20
mobic$truck<-as.factor(ifelse(is.na(mobic$truck),0,mobic$truck))  # imputing NA with 1. 
summary(mobic$truck)

summary(mobic$roam_Mean)
boxplot(mobic$roam_Mean,horizontal = T)
quantile(mobic$roam_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$roam_Mean,p=(990:1000)/1000,na.rm = T)
mobic$roam_Mean<-ifelse(mobic$roam_Mean>=60.94820,60.94820,mobic$roam_Mean) # imputing with 99.8th percentile
mobic$roam_Mean[is.na(mobic$roam_Mean)]<-median(mobic$roam_Mean,na.rm = T)
summary(mobic$roam_Mean)
boxplot(mobic$roam_Mean,horizontal = T)

summary(mobic$churn)
mobic$churn<-as.factor(mobic$churn)
summary(mobic$churn)

summary(mobic$car_buy)
datC22
mobic$car_buy<-ifelse(is.na(mobic$car_buy),1,mobic$car_buy)  # imputing na with New. 1 means New here 
mobic$car_buy<-factor(mobic$car_buy,labels=c("New","Unknown"))
summary(mobic$car_buy)

summary(mobic$csa)
datC23
cb<-unclass(datC23%>%filter(ChurnPerc>0.35)%>%select(levels))[[1]]
mobic$csa<-as.factor(ifelse(mobic$csa %in% cb,"High_Churn","Low_Churn"))
summary(mobic$csa)

summary(mobic$da_Mean)
boxplot(mobic$da_Mean,horizontal = T)
quantile(mobic$da_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$da_Mean,p=(990:1000)/1000,na.rm = T)
mobic$da_Mean<-ifelse(mobic$da_Mean>=20.92984,20.92984,mobic$da_Mean) # imputing with 99.9th percentile.
mobic$da_Mean[is.na(mobic$da_Mean)]<-median(mobic$da_Mean,na.rm = T)
summary(mobic$da_Mean)
boxplot(mobic$da_Mean,horizontal = T)
}
#  "adjmou"           "totrev"    "adjrev"           "avgrev"      "compcalls_Mean"   "plcdcalls_Mean"
{
summary(mobic$adjmou)
boxplot(mobic$adjmou,horizontal = T)
quantile(mobic$adjmou,p=(90:100)/100,na.rm = T)
quantile(mobic$adjmou,p=(990:1000)/1000,na.rm = T)
mobic$adjmou<-ifelse(mobic$adjmou>=75129.36,75129.36,mobic$adjmou) # imputing with 99.8th percentile.
summary(mobic$adjmou)
boxplot(mobic$adjmou,horizontal = T)

summary(mobic$totrev)
boxplot(mobic$totrev,horizontal = T)
quantile(mobic$totrev,p=(90:100)/100,na.rm = T)
quantile(mobic$totrev,p=(990:1000)/1000,na.rm = T)
mobic$totrev<-ifelse(mobic$totrev>=7213.739,7213.739,mobic$totrev)  # imputing with 99.8th percentile.
summary(mobic$totrev)
boxplot(mobic$totrev,horizontal = T)

summary(mobic$adjrev)
boxplot(mobic$adjrev,horizontal = T)
quantile(mobic$adjrev,p=(90:100)/100,na.rm = T)
quantile(mobic$adjrev,p=(990:1000)/1000,na.rm = T)
mobic$adjrev<-ifelse(mobic$adjrev>=7069.156,7069.156,mobic$adjrev) # imputing with 99.9th percentile.
summary(mobic$adjrev)
boxplot(mobic$adjrev,horizontal = T)

summary(mobic$avgrev)
boxplot(mobic$avgrev,horizontal = T)
quantile(mobic$avgrev,p=(90:100)/100,na.rm = T)
quantile(mobic$avgrev,p=(990:1000)/1000,na.rm = T)
mobic$avgrev<-ifelse(mobic$avgrev>=320.7788,320.7788,mobic$avgrev) # imputing with 99.9th percentile.
summary(mobic$avgrev)
boxplot(mobic$avgrev,horizontal = T)

summary(mobic$compcalls_Mean)
boxplot(mobic$compcalls_Mean,horizontal = T)
quantile(mobic$compcalls_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$compcalls_Mean,p=(990:1000)/1000,na.rm = T)
mobic$compcalls_Mean<-ifelse(mobic$compcalls_Mean>=957.2627,957.2627,mobic$compcalls_Mean) # imputing with 99.9th percentile.
summary(mobic$compcalls_Mean)
boxplot(mobic$compcalls_Mean,horizontal = T)

summary(mobic$plcdcalls_Mean)
boxplot(mobic$plcdcalls_Mean,horizontal = T)
quantile(mobic$plcdcalls_Mean,p=(90:100)/100,na.rm = T)
quantile(mobic$plcdcalls_Mean,p=(990:1000)/1000,na.rm = T)
mobic$plcdcalls_Mean<-ifelse(mobic$plcdcalls_Mean>=1288.3940,1288.3940,mobic$plcdcalls_Mean) # imputing with 99.8th percentile.
summary(mobic$plcdcalls_Mean)
boxplot(mobic$plcdcalls_Mean,horizontal = T)
}
}

str(mobic)
summary(mobic)

#  After Treatement Boxplot  #
{
  names(mobic)
  summary(mobic)
  str(mobic)
  dim(mobic)
  list<-names(mobic)
  list<-list[-c(6,8,24:42,44:46,52)]
  list
  par(mfrow=c(3,10))
  par("mar")
  par(mar=c(1,1,1,1))
  for(i in 1:length(list))
  {
    boxplot(mobic[,list[i]],main=list[i])
  }
  rm(list)
  dev.off()
  rm(i)
}

#categorical & continuous data is not on a same/similar scale. we need to treat this.
categorical<-c("income","crclscod","asl_flag","prizm_social_one","area","refurb_new","hnd_webcap","marital",
               "ethnic","age1","age2","models","hnd_price","actvsubs","uniqsubs","forgntvl","dwlltype",
               "dwllsize","mtrcycle","truck","churn","car_buy","csa","Customer_ID")
mobic[,!(names(mobic) %in% categorical)] = scale(mobic[,!(names(mobic) %in% categorical)])
names(mobic)
summary(mobic)

#----------------------------------------------------------------------------------#
#------------------------- Splitting data into train/test  ------------------------#
#----------------------------------------------------------------------------------#

set.seed(270)
index<-sample(nrow(mobic),0.70*nrow(mobic),replace=F)
train<-mobic[index,]
test<-mobic[-index,]
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

#----------------------------------------------------------------------------------#
#--------------------------- Building a logistic MOdel  ---------------------------#
#----------------------------------------------------------------------------------#

names(mobic)
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
}

# excluding variables based on insignificance

model6<-glm(churn~ .-totcalls-adjrev-avg3mou-avg6qty-plcdcalls_Mean-custcare_Mean-callwait_Mean
            -iwylis_vce_Mean-ovrmou_Mean-mtrcycle-truck-da_Mean-forgntvl-avgqty
            -age1-age2-avg6mou-income-car_buy, data=train[,-52],family="binomial")
summary(model6)
vif(model6)


# Model validations.

# on Train data
  
trpred<-predict(model6,type = "response",train)
auc(train$churn,trpred)                                # Area under the curve: 0.7008

table(mobi$churn)/nrow(mobi)
predtr<-as.factor(ifelse(trpred>=0.3287911,1,0))

library(irr)
kappa2(data.frame(train$churn,predtr))                 #   0.273

library(caret)
install.packages('e1071', dependencies=TRUE)
confusionMatrix(predtr,train$churn,positive="1")       #  0.6484

library(ROCR)
trpredic = prediction(trpred, train$churn)
trperf<-performance(trpredic,"tpr","fpr")
plot(trperf,col="red",main = "Training ROC Curve")
abline(0,1,lty=8,col="orange")

trauc<-performance(trpredic,"auc")
trauc<-unlist(slot(trauc,"y.values"))
trauc<-round(trauc,4)                                  #   0.7008
trauc

# on Test data

tspred<-predict(model6,type = "response",test)
auc(test$churn,tspred)                                 #   Area under the curve: 0.7053

table(mobi$churn)/nrow(mobi)
predts<-as.factor(ifelse(tspred>=0.3287911,1,0))

library(irr)
kappa2(data.frame(test$churn,predts))                  #  0.281

library(caret)
confusionMatrix(predts,test$churn,positive="1")        #   0.6533

library(ROCR)
tspredic = prediction(tspred, test$churn)
tsperf<-performance(tspredic,"tpr","fpr")
plot(tsperf,col="red",main = "Test ROC Curve")
abline(0,1,lty=8,col="orange")

tsauc<-performance(tspredic,"auc")
tsauc<-unlist(slot(tsauc,"y.values"))
tsauc <-round(tsauc,4)                                 #  0.7053
tsauc


#------------------------------------------------------------------------------#

# question  What are the top five factors driving likelihood of churn at telecom?
head(sort(abs(model6$coefficients),decreasing = T),6)

# question  Validation of survey findings. 
        
        #a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behavior. 
        #b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn?

summary(mob)

# Ans : Mean total monthly recurring charge and Mean monthly revenue (charge amount) both defines the cost to customer and are forming important factors to churn. 
      # Mean number of completed voice & data calls and Mean number of dropped or blocked calls are factors determining network issues and are constituting important factors for churn.



#   
tail(sort(abs(model6$coefficients),decreasing = T),10)


# question  Prioritization of customers for proactive retention strategy

library(gains)
test$churn<-as.numeric(test$churn)
test$pred<-predict(model6,type = "response",test)
gains(test$churn,test$pred,groups = 10)

# We can use Gains chart, the best tool to prioritize customers for a proactive retention campaigns.
# Below is the Gains(lift) chart that is produced from the model.
# Here we can see about 30% of the customers fall under top 30%

# We can cross verify this using quantiles. Dividing the data into 10 equal parts forming 10% buckets.
# We can see nearly 30% of customers fall under top 30% of data. (70% to 100%)

quantile(test$pred,prob=c(0.10,0.20,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

targeted<-test[test$pred>0.2326652 & test$pred<=0.8848846 & test$churn=="1","Customer_ID"]
targeted
targeted<-as.data.frame(targeted)
nrow(targeted)                                       #  7987
write.csv(targeted,"targeted.csv",row.names = F)



# question   target segments for proactive retention campaigns

test$pred<-predict(model6,type = "response",test)
quantile(test$pred,prob=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
test$pred_levels<-as.factor(ifelse(test$pred < 0.1932718,"Low_Score",
                                   ifelse(test$pred >= 0.1932718 & test$pred < 0.3975819,"Medium_Score","High_Score")))

table(test$pred_levels,test$churn)
summary(test$churn)
str(test$totrev)

quantile(test$totrev, prob=c(0.10,0.20,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
test$Revenue_Levels<-as.factor(ifelse(test$totrev < -0.5598603,"Low_Revenue",
                                      ifelse(test$totrev >= -0.5598603 & test$totrev < 0.1332409,"Medium_Revenue","High_Revenue")))
table(test$Revenue_Levels,test$churn)
targeted1<-test[test$pred_levels=="High_Score"& test$Revenue_Levels=="High_Revenue","Customer_ID"]
targeted2<-test[test$pred_levels=="High_Score"& test$Revenue_Levels=="Medium_Revenue","Customer_ID"]
targeted3<-test[test$pred_levels=="Medium_Score"& test$Revenue_Levels=="High_Revenue","Customer_ID"]
targeted1<-as.data.frame(targeted1)
targeted2<-as.data.frame(targeted2)
targeted3<-as.data.frame(targeted3)
nrow(targeted1)     #    2969
nrow(targeted2)     #    2057     
nrow(targeted3)     #    2434


# Below table shows the targeted customer sections that Mobicom need to focus on retaining them from losing heavy revenues.

# It describes the probability of churn: 
#Medium Churn score & High Revenues has 1913 customers predicted
#High Churn score & Medium Revenues has 2521 customers predicted
#High Churn score & Medium Revenues has 1957 customers predicted

#Having retention campaigns targeted at these customers will help Mibicom maintain their customer base.


#Score/Revenue	Low_Revenue	Medium_Revenue	High_Revenue
#Low_Score---------------------------------------------
#Medium_Score--------------------------------------2434
#High_Score---------------------------2057---------2969

write.csv(targeted1,"High_Score_High_Revenue_Target.csv",row.names = F)
write.csv(targeted2,"High_Score_Mid_Revenue_Target.csv",row.names = F)
write.csv(targeted3,"Mid_score_High_Revenue_Target.csv",row.names = F)
