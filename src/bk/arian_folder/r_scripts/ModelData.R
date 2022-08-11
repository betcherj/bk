library(tools)
library(utils)
library(stargazer)
library(ggpubr)
library(nnet)
library(MASS)
library(mlogit)
library(foreign)
library(ggplot2)
library(MLmetrics)
library(caret)
library(leaps)
library(MPV)
library(zoo)
library(dplyr)
library(tidyverse)
library(data.table)

#rm(list = ls())
###


ModelData<-merge(ScheduleData,GameDataFinal,by=c("game_id","home_team","away_team","season","week"),all.x = TRUE)

ModelData$SpreadClose<-ifelse(is.na(ModelData$SpreadClose),ModelData$SpreadOpen,ModelData$SpreadClose)
ftable(is.na(ModelData$SpreadClose))
ftable(is.na(ModelData$SpreadOpen))

ModelData$SpreadMove<-ModelData$SpreadOpen-ModelData$SpreadClose
ModelData$SpreadMove<-ifelse(is.na(ModelData$SpreadMove),0,ModelData$SpreadMove)

ModelData$TotalOpen<-ifelse(is.na(ModelData$TotalOpen),ModelData$total,ModelData$TotalOpen)
ModelData$TotalMove<-ModelData$TotalOpen-ModelData$TotalClose
ModelData$TotalMove<-ifelse(is.na(ModelData$TotalMove),0,ModelData$TotalMove)

ModelData$ActualSpread<-ModelData$away_score-ModelData$home_score
ModelData$ActualTotal<-ModelData$away_score+ModelData$home_score
ModelData$Win<-ifelse(ModelData$away_score<ModelData$home_score,1,0)
ModelData$Win<-ifelse(ModelData$home_score==ModelData$away_score & ModelData$Home==0,1,ModelData$Win)

### New Terms

ModelData$ADJ_SOS_RunEPA<-ModelData$Roll_run_epa-(ModelData$CumRun_epa)
ModelData$ADJ_SOS_PassEPA<-ModelData$Roll_pass_epa-(ModelData$CumPass_epa)
ModelData$ADJ_SOS_DRunEPA<-ModelData$Roll_Drun_epa-(ModelData$CumDRun_epa)
ModelData$ADJ_SOS_DPassEPA<-ModelData$Roll_Dpass_epa-(ModelData$CumDPass_epa)

ModelData$Int_ADJ_SOS_RunEPA<-ModelData$ADJ_SOS_RunEPA*(1-ModelData$Roll_PassRate)
ModelData$Int_ADJ_SOS_PassEPA<-ModelData$ADJ_SOS_PassEPA*ModelData$Roll_PassRate
ModelData$Int_ADJ_SOS_DRunEPA<-ModelData$ADJ_SOS_DRunEPA-(1-ModelData$Roll_DPassRate)
ModelData$Int_ADJ_SOS_DPassEPA<-ModelData$ADJ_SOS_DPassEPA*ModelData$Roll_DPassRate

ModelData$Net_OEPA<-ModelData$Int_ADJ_SOS_RunEPA+ModelData$Int_ADJ_SOS_PassEPA
ModelData$Net_DEPA<-ModelData$Int_ADJ_SOS_DRunEPA+ModelData$Int_ADJ_SOS_DPassEPA

ModelData$O_ADJ_SOS_RunEPA<-ModelData$Roll_run_epa-(ModelData$ACumRun_epa)
ModelData$O_ADJ_SOS_PassEPA<-ModelData$Roll_pass_epa-(ModelData$ACumPass_epa)
ModelData$O_ADJ_SOS_DRunEPA<-ModelData$Roll_Drun_epa-(ModelData$ACumDRun_epa)
ModelData$O_ADJ_SOS_DPassEPA<-ModelData$Roll_Dpass_epa-(ModelData$ACumDPass_epa)

ModelData$O_Int_ADJ_SOS_RunEPA<-ModelData$O_ADJ_SOS_RunEPA*(1-ModelData$O_Roll_PassRate)
ModelData$O_Int_ADJ_SOS_PassEPA<-ModelData$O_ADJ_SOS_PassEPA*ModelData$O_Roll_PassRate
ModelData$O_Int_ADJ_SOS_DRunEPA<-ModelData$O_ADJ_SOS_DRunEPA-(1-ModelData$O_Roll_DPassRate)
ModelData$O_Int_ADJ_SOS_DPassEPA<-ModelData$O_ADJ_SOS_DPassEPA*ModelData$O_Roll_DPassRate

ModelData$O_Net_OEPA<-ModelData$O_Int_ADJ_SOS_RunEPA+ModelData$O_Int_ADJ_SOS_PassEPA
ModelData$O_Net_DEPA<-ModelData$O_Int_ADJ_SOS_DRunEPA+ModelData$O_Int_ADJ_SOS_DPassEPA

###
ModelData$ADJ_SOS_Runsuccess<-ModelData$Roll_Run_success-(ModelData$CumRun_success)
ModelData$ADJ_SOS_Passsuccess<-ModelData$Roll_Pass_success-(ModelData$CumPass_success)
ModelData$ADJ_SOS_DRunsuccess<-ModelData$Roll_DRun_success-(ModelData$CumDRun_success)
ModelData$ADJ_SOS_DPasssuccess<-ModelData$Roll_DPass_success-(ModelData$CumDPass_success)

ModelData$Int_ADJ_SOS_Runsuccess<-ModelData$ADJ_SOS_Runsuccess*(1-ModelData$Roll_PassRate)
ModelData$Int_ADJ_SOS_Passsuccess<-ModelData$ADJ_SOS_Passsuccess*ModelData$Roll_PassRate
ModelData$Int_ADJ_SOS_DRunsuccess<-ModelData$ADJ_SOS_DRunsuccess-(1-ModelData$Roll_DPassRate)
ModelData$Int_ADJ_SOS_DPasssuccess<-ModelData$ADJ_SOS_DPasssuccess*ModelData$Roll_DPassRate

ModelData$Net_OSuccess<-ModelData$Int_ADJ_SOS_Runsuccess+ModelData$Int_ADJ_SOS_Passsuccess
ModelData$Net_DSuccess<-ModelData$Int_ADJ_SOS_DRunsuccess+ModelData$Int_ADJ_SOS_DPasssuccess

#

ModelData$O_ADJ_SOS_Runsuccess<-ModelData$O_Roll_Run_success-(ModelData$ACumRun_success)
ModelData$O_ADJ_SOS_Passsuccess<-ModelData$O_Roll_Pass_success-(ModelData$ACumPass_success)
ModelData$O_ADJ_SOS_DRunsuccess<-ModelData$O_Roll_DRun_success-(ModelData$ACumDRun_success)
ModelData$O_ADJ_SOS_DPasssuccess<-ModelData$O_Roll_DPass_success-(ModelData$ACumDPass_success)

ModelData$O_Int_ADJ_SOS_Runsuccess<-ModelData$O_ADJ_SOS_Runsuccess*(1-ModelData$O_Roll_PassRate)
ModelData$O_Int_ADJ_SOS_Passsuccess<-ModelData$O_ADJ_SOS_Passsuccess*ModelData$O_Roll_PassRate
ModelData$O_Int_ADJ_SOS_DRunsuccess<-ModelData$O_ADJ_SOS_DRunsuccess-(1-ModelData$O_Roll_DPassRate)
ModelData$O_Int_ADJ_SOS_DPasssuccess<-ModelData$O_ADJ_SOS_DPasssuccess*ModelData$O_Roll_DPassRate

ModelData$O_Net_OSuccess<-ModelData$O_Int_ADJ_SOS_Runsuccess+ModelData$O_Int_ADJ_SOS_Passsuccess
ModelData$O_Net_DSuccess<-ModelData$O_Int_ADJ_SOS_DRunsuccess+ModelData$O_Int_ADJ_SOS_DPasssuccess


# 

#
## Finalize
#

drops <- c("btbR", "btbtbR", "Opp_btbH", "Opp_btbtbH")
ModelData<-as.data.frame(ModelData)
ModelData<-ModelData[ , !(names(ModelData) %in% drops)]

ModelData$spread_line<-ifelse(is.na(ModelData$spread_line),ModelData$SpreadClose,ModelData$spread_line)
ModelData$SpreadOpen<-ifelse(is.na(ModelData$SpreadOpen),ModelData$spread_line,ModelData$SpreadOpen)
ModelData$SpreadClose<-ifelse(is.na(ModelData$SpreadClose),ModelData$spread_line,ModelData$SpreadClose)
ModelData$SpreadMove<-ifelse(is.na(ModelData$SpreadMove),0,ModelData$SpreadMove)
quantile(abs(ModelData$SpreadOpen-ModelData$SpreadClose),0.995,na.rm = T)
ModelData<-subset(ModelData,abs(ModelData$SpreadMove)<7)
#ModelData<-subset(ModelData, !is.na(ModelData$spread_line))

### Factors
ModelData$Win<-as.factor(ModelData$Win)
ModelData$TrueHome<-as.factor(ModelData$TrueHome)
ModelData$btbH<-as.factor(ModelData$btbH)
ModelData$btbtbH<-as.factor(ModelData$btbtbH)
ModelData$Opp_btbR<-as.factor(ModelData$Opp_btbR)
ModelData$Opp_btbtbR<-as.factor(ModelData$Opp_btbtbR)
ModelData$PredHomeAdv<-ifelse(ModelData$game_type=="SB",0,ModelData$PredHomeAdv)
ModelData$PredHomeAdv<-ifelse(ModelData$TrueHome==0,0,ModelData$PredHomeAdv)

ModelData$WP<-ifelse(ModelData$ML<0,(abs(ModelData$ML)/(abs(ModelData$ML)+100))-(abs(ModelData$ML)/(abs(ModelData$ML)+100))*0.037879,100/(ModelData$ML+100)-100/(ModelData$ML+100)*0.037879)
#ModelData$WP<-ifelse(ModelData$SpreadClose>0 & ModelData$WP>0.5, 0.5-(ModelData$WP-0.5), ModelData$WP)
summary(ModelData$WP)
hist(ModelData$WP)


ggplot(ModelData, aes((SpreadOpen), (WP))) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE)

library(mgcv)
Mdl2<-bam(WP~s(SpreadClose, bs = 'cr', k =40)
          , method = "REML",family=("gaussian"(link="identity")), data=ModelData)
summary(Mdl2)

ModelData$WPClose<-round(predict(Mdl2,ModelData,type="response"),3)

library(mgcv)
Mdl<-bam(WP~s(SpreadOpen, bs = 'cr', k =40)
           , method = "REML",family=("gaussian"(link="identity")), data=ModelData)
summary(Mdl)

ModelData$WP<-round(predict(Mdl,ModelData,type="response"),3)

#ModelData$WP<-ifelse(ModelData$SpreadClose< -20,0.93,ModelData$WP)
#ModelData$WP<-ifelse(ModelData$SpreadClose>20,0.07,ModelData$WP)

#ModelData$WPClose<-ifelse(ModelData$SpreadClose< -20, 0.93,ModelData$WPClose)
#ModelData$WPClose<-ifelse(ModelData$SpreadClose>20,0.07,ModelData$WPClose)

ModelData[is.na(ModelData)] <- 0
ModelData[ModelData == -Inf] <- 0
ModelData[ModelData == Inf] <- 0

drops <- c("Roll_EXPPass","Roll_DEXPFourth","Roll_DEXPPass","O_Roll_EXPFourth"
           ,"O_Roll_DEXPFourth","Roll_run_xYPP","Roll_EXPFourth","Roll_Pass_EXPSucc",
           "O_Roll_DRun_EXPSucc","Roll_Run_EXPSucc","O_Roll_Run_EXPSucc","O_Roll_run_xYPP"
           ,"O_Roll_Drun_xYPP","Roll_Drun_xYPP","Roll_Dxyac","O_Roll_EXPPass","O_DRoll_EXPPass",
           "Roll_Dxyac_epa","Roll_DRun_EXPSucc","O_Roll_Dxyac","Roll_xyac","O_Roll_xyac","O_Roll_Dxyac_epa","O_Roll_xyac_epa",
           "O_Roll_GoRate","O_Roll_DGoRate","O_Roll_DEXPPass",
           "O_Roll_DPass_EXPSucc","O_Roll_Pass_EXPSucc","Roll_DPass_EXPSucc","Roll_xyac_epa")

ModelData<-ModelData[ , !(names(ModelData) %in% drops)]

###
write.csv(ModelData,'/Users/am/Desktop/NFLSets/ModelData.csv')


#
## TotalData
#

ModelData$total_line<-ifelse((ModelData$total_line=="OFF"),ModelData$PredTotal,ModelData$total_line)

ModelData$total_line<-ifelse((ModelData$total_line==0),ModelData$PredTotal,ModelData$total_line)
ModelData$TotalOpen<-ifelse((ModelData$TotalOpen==0),ModelData$total_line,ModelData$TotalOpen)
ModelData$TotalClose<-ifelse((ModelData$TotalClose==0),ModelData$total_line,ModelData$TotalClose)
ModelData$TotalClose<-ifelse((ModelData$TotalClose==0),ModelData$TotalOpen,ModelData$TotalClose)
ModelData$TotalMove<-ifelse(is.na(ModelData$TotalMove),0,ModelData$TotalMove)
ModelData$total_line<-as.numeric(ModelData$total_line)
ModelData$TotalOpen<-as.numeric(ModelData$TotalOpen)
ModelData$TotalClose<-as.numeric(ModelData$TotalClose)
ModelData$weekday<-as.factor(ModelData$weekday)
ModelData$i_wind<-as.factor(ModelData$i_wind)
ModelData$Rain<-as.factor(ModelData$Rain)
ModelData$Snow<-as.factor(ModelData$Snow)
ModelData$surface<-as.factor(ModelData$i_surface)


### Factors
ModelData$btbH<-as.factor(ModelData$btbH)
ModelData$btbtbH<-as.factor(ModelData$btbtbH)
ModelData$Opp_btbR<-as.factor(ModelData$Opp_btbR)
ModelData$Opp_btbtbR<-as.factor(ModelData$Opp_btbtbR)
ModelData$PrimeTime<-as.factor(ModelData$PrimeTime)
ModelData$weekday<-relevel(ModelData$weekday,"Sunday")
ModelData$weekday<-as.factor(ModelData$weekday)
ModelData$PrimeTime<-relevel(ModelData$PrimeTime,"Regular")
quantile(abs(ModelData$TotalOpen-ModelData$TotalClose),0.995,na.rm = T)
ModelData<-subset(ModelData,abs(ModelData$TotalOpen-ModelData$TotalClose)<7)

write.csv(ModelData,'/Users/am/Desktop/NFLSets/TotalData.csv')

