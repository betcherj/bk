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

#
## 
### Game Level
## 
#

GameData<- read.csv('/Users/am/Desktop/NFLSets/PBP/Schedule.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
GameData<-GameData[,-c(3,6,5,7,13:17)]


GameData$home_team<-ifelse(GameData$home_team=="STL","LA",GameData$home_team)
GameData$home_team<-ifelse(GameData$home_team=="OAK","LV",GameData$home_team)
GameData$home_team<-ifelse(GameData$home_team=="SD","LAC",GameData$home_team)
GameData$away_team<-ifelse(GameData$away_team=="STL","LA",GameData$away_team)
GameData$away_team<-ifelse(GameData$away_team=="OAK","LV",GameData$away_team)
GameData$away_team<-ifelse(GameData$away_team=="SD","LAC",GameData$away_team)

GameData2<-GameData
GameData2$home_team<-GameData$away_team
GameData2$away_team<-GameData$home_team

GameData2$away_score<-GameData$home_score
GameData2$home_score<-GameData$away_score
GameData2$home_result<-GameData2$home_result*-1

GameData<-rbind(GameData,GameData2)

#
## Pass YPP
#

PassYPP<- read.csv('/Users/am/Desktop/NFLSets/GBGData/PassYPP.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
PassYPP$home_team<-PassYPP$posteam
PassYPP<-PassYPP[,c(2,5:12)]

GameData<-merge(GameData,PassYPP,by=c("home_team","season","week"),all.x = TRUE)

PassYPP<- read.csv('/Users/am/Desktop/NFLSets/GBGData/PassYPP.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
PassYPP$home_team<-PassYPP$defteam
PassYPP<-PassYPP[,c(2,5:12)]
colnames(PassYPP)[c(3:8)] <- paste("D", colnames(PassYPP)[c(3:8)], sep = "")

GameData<-merge(GameData,PassYPP,by=c("home_team","season","week"),all.x = TRUE)


#
## Pass Succ
#

PassSucc<- read.csv('/Users/am/Desktop/NFLSets/GBGData/PassSucc.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
PassSucc$home_team<-PassSucc$posteam
PassSucc$Pass_EXPSucc<-PassSucc$EXPsucc
PassSucc$Pass_success<-PassSucc$success
PassSucc<-PassSucc[,c(4,7:10)]
PassSucc<-PassSucc[,-2]
GameData<-merge(GameData,PassSucc,by=c("home_team","game_id"),all.x = TRUE)

PassSucc<- read.csv('/Users/am/Desktop/NFLSets/GBGData/PassSucc.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
PassSucc$away_team<-PassSucc$posteam
PassSucc$Pass_EXPSucc<-PassSucc$EXPsucc
PassSucc$Pass_success<-PassSucc$success
PassSucc<-PassSucc[,c(4,7:10)]
colnames(PassSucc)[c(2,4,5)] <- paste("D", colnames(PassSucc)[c(2,4,5)], sep = "")
PassSucc<-PassSucc[,-2]
GameData<-merge(GameData,PassSucc,by=c("away_team","game_id"),all.x = TRUE)

#
## Run YPP
#

RunYPP<- read.csv('/Users/am/Desktop/NFLSets/GBGData/RunYPP.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
RunYPP$home_team<-RunYPP$posteam
RunYPP<-RunYPP[,c(2,5:9)]
GameData<-merge(GameData,RunYPP,by=c("home_team","season","week"),all.x = TRUE)

RunYPP<- read.csv('/Users/am/Desktop/NFLSets/GBGData/RunYPP.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
RunYPP$home_team<-RunYPP$defteam
RunYPP<-RunYPP[,c(2,5:9)]
colnames(RunYPP)[c(3:5)] <- paste("D", colnames(RunYPP)[c(3:5)], sep = "")
GameData<-merge(GameData,RunYPP,by=c("home_team","season","week"),all.x = TRUE)

#
## Run Succ
#

RunSucc<- read.csv('/Users/am/Desktop/NFLSets/GBGData/RunSucc.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
RunSucc$home_team<-RunSucc$posteam
RunSucc$Run_EXPSucc<-RunSucc$EXPsucc
RunSucc$Run_success<-RunSucc$success
RunSucc<-RunSucc[,c(4,7:10)]
RunSucc<-RunSucc[,-2]

GameData<-merge(GameData,RunSucc,by=c("home_team","game_id"),all.x = TRUE)

RunSucc<- read.csv('/Users/am/Desktop/NFLSets/GBGData/RunSucc.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
RunSucc$away_team<-RunSucc$posteam
RunSucc$Run_EXPSucc<-RunSucc$EXPsucc
RunSucc$Run_success<-RunSucc$success
RunSucc<-RunSucc[,c(4,7:10)]
colnames(RunSucc)[c(2,4,5)] <- paste("D", colnames(RunSucc)[c(2,4,5)], sep = "")
RunSucc<-RunSucc[,-2]

GameData<-merge(GameData,RunSucc,by=c("away_team","game_id"),all.x = TRUE)

#
## EPA
#

EPA<- read.csv('/Users/am/Desktop/NFLSets/GBGData/EPA.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)

kick<-subset(EPA,EPA$play_type=="kick")
special<-subset(EPA,EPA$play_type=="special")
fumble<-subset(EPA,EPA$play_type=="fumble")
pass<-subset(EPA,EPA$play_type=="pass")
run<-subset(EPA,EPA$play_type=="run")

names(kick)[names(kick) == 'playcount'] <- 'kick_count'
names(special)[names(special) == 'playcount'] <- 'special_count'
names(fumble)[names(fumble) == 'playcount'] <- 'fumble_count'

names(kick)[names(kick) == 'posteam'] <- 'home_team'
names(special)[names(special) == 'posteam'] <- 'home_team'
names(fumble)[names(fumble) == 'posteam'] <- 'home_team'
names(pass)[names(pass) == 'posteam'] <- 'home_team'
names(run)[names(run) == 'posteam'] <- 'home_team'

names(kick)[names(kick) == 'epa'] <- 'kick_epa'
names(special)[names(special) == 'epa'] <- 'special_epa'
names(fumble)[names(fumble) == 'epa'] <- 'fumble_epa'
names(pass)[names(pass) == 'epa'] <- 'pass_epa'
names(run)[names(run) == 'epa'] <- 'run_epa'

kick<-kick[,c(2,3,5,7,8)]
special<-special[,c(2,3,5,7,8)]
fumble<-fumble[,c(2,3,5,7,8)]
pass<-pass[,c(2,3,5,7)]
run<-run[,c(2,3,5,7)]

GameData<-merge(GameData,pass,by=c("home_team","season","week"),all.x = TRUE)
GameData<-merge(GameData,run,by=c("home_team","season","week"),all.x = TRUE)
GameData<-merge(GameData,kick,by=c("home_team","season","week"),all.x = TRUE)
GameData<-merge(GameData,special,by=c("home_team","season","week"),all.x = TRUE)
GameData<-merge(GameData,fumble,by=c("home_team","season","week"),all.x = TRUE)


#
## Defensive EPA
#

EPA<- read.csv('/Users/am/Desktop/NFLSets/GBGData/EPA.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)

kick<-subset(EPA,EPA$play_type=="kick")
special<-subset(EPA,EPA$play_type=="special")
fumble<-subset(EPA,EPA$play_type=="fumble")
pass<-subset(EPA,EPA$play_type=="pass")
run<-subset(EPA,EPA$play_type=="run")

names(kick)[names(kick) == 'playcount'] <- 'Dkick_count'
names(special)[names(special) == 'playcount'] <- 'Dspecial_count'
names(fumble)[names(fumble) == 'playcount'] <- 'Dfumble_count'

names(kick)[names(kick) == 'posteam'] <- 'away_team'
names(special)[names(special) == 'posteam'] <- 'away_team'
names(fumble)[names(fumble) == 'posteam'] <- 'away_team'
names(pass)[names(pass) == 'posteam'] <- 'away_team'
names(run)[names(run) == 'posteam'] <- 'away_team'

names(kick)[names(kick) == 'epa'] <- 'Dkick_epa'
names(special)[names(special) == 'epa'] <- 'Dspecial_epa'
names(fumble)[names(fumble) == 'epa'] <- 'Dfumble_epa'
names(pass)[names(pass) == 'epa'] <- 'Dpass_epa'
names(run)[names(run) == 'epa'] <- 'Drun_epa'

kick<-kick[,c(2,3,5,7,8)]
special<-special[,c(2,3,5,7,8)]
fumble<-fumble[,c(2,3,5,7,8)]
pass<-pass[,c(2,3,5,7)]
run<-run[,c(2,3,5,7)]

GameData<-merge(GameData,pass,by=c("away_team","season","week"),all.x = TRUE)
GameData<-merge(GameData,run,by=c("away_team","season","week"),all.x = TRUE)
GameData<-merge(GameData,kick,by=c("away_team","season","week"),all.x = TRUE)
GameData<-merge(GameData,special,by=c("away_team","season","week"),all.x = TRUE)
GameData<-merge(GameData,fumble,by=c("away_team","season","week"),all.x = TRUE)

GameData[is.na(GameData)] <- 0

#
## Play Types
#

PlayTypes<- read.csv('/Users/am/Desktop/NFLSets/GBGData/GamePlayType.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
PlayTypes$home_team<-PlayTypes$posteam
PlayTypes<-PlayTypes[,c(2,5:8,10)]

GameData<-merge(GameData,PlayTypes,by=c("home_team","season","week"),all.x = TRUE)

PlayTypes<- read.csv('/Users/am/Desktop/NFLSets/GBGData/GamePlayType.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
PlayTypes$away_team<-PlayTypes$posteam
PlayTypes<-PlayTypes[,c(2,5:8,10)]
colnames(PlayTypes)[c(3:5)] <- paste("D", colnames(PlayTypes)[c(3:5)], sep = "")

GameData<-merge(GameData,PlayTypes,by=c("away_team","season","week"),all.x = TRUE)

#
## Aggressive
#

Agressive<- read.csv('/Users/am/Desktop/NFLSets/GBGData/GameAggressive.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
Agressive$home_team<-Agressive$posteam
Agressive<-Agressive[,c(2,5:8,10)]

GameData<-merge(GameData,Agressive,by=c("home_team","season","week"),all.x = TRUE)


Agressive<- read.csv('/Users/am/Desktop/NFLSets/GBGData/GameAggressive.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
Agressive$away_team<-Agressive$posteam
Agressive<-Agressive[,c(2,5:8,10)]
colnames(Agressive)[c(3:5)] <- paste("D", colnames(Agressive)[c(3:5)], sep = "")

GameData<-merge(GameData,Agressive,by=c("away_team","season","week"),all.x = TRUE)


#
## GameData Rolling Average
# 

GameData0<-arrange(GameData,season,home_team,week)

GameData2<-arrange(GameData0,season,home_team,week) %>%
  group_by(season,home_team)%>%
  select(c(2,4,9:62)) %>%
  mutate_at(vars(-group_cols()), 
            ~ rollapplyr(.x, list(-seq(22)), sum, partial = TRUE, fill = NA, na.rm = T)) %>%
  as.data.frame()

GameData2<-GameData2[,-c(1,2)]
colnames(GameData2)<- paste("Roll", colnames(GameData2), sep = "_")

GameData0[,63:116]<-NA
colnames(GameData0)[63:116]<-colnames(GameData2)[1:54]
GameData0[,63:116]<-GameData2[,1:54]


#
## Opponent
#

GameData0<-arrange(GameData0,season,away_team,week)

GameData3<-arrange(GameData0,season,away_team,week) %>%
  group_by(season,away_team)%>%
  select(c(1,2,9:62)) %>%
  mutate_at(vars(-group_cols()), 
            ~ rollapplyr(.x, list(-seq(22)), sum, partial = TRUE, fill = NA, na.rm = T)) %>%
  as.data.frame()

GameData3<-GameData3[,-c(1,2)]
colnames(GameData3)<- paste("O_Roll", colnames(GameData3), sep = "_")

GameData0[,117:170]<-NA
colnames(GameData0)[117:170]<-colnames(GameData3)[1:54]
GameData0[,117:170]<-GameData3[1:54]

#
## Offensive
#

GameData<-GameData0

GameData$Roll_pass_yards<-GameData$Roll_pass_yards/GameData$Roll_passCount
GameData$Roll_Pass_success<-GameData$Roll_Pass_success/GameData$Roll_passCount
GameData$Roll_Pass_EXPSucc<-GameData$Roll_Pass_EXPSucc/GameData$Roll_passCount

GameData$Roll_xyac<-GameData$Roll_xyac/GameData$Roll_passCount
GameData$Roll_yac<-GameData$Roll_yac/GameData$Roll_passCount
GameData$Roll_yac_epa<-GameData$Roll_yac_epa/GameData$Roll_passCount
GameData$Roll_xyac_epa<-GameData$Roll_xyac_epa/GameData$Roll_passCount

GameData$Roll_run_yards<-GameData$Roll_run_yards/GameData$Roll_RunCount
GameData$Roll_Run_success<-GameData$Roll_Run_success/GameData$Roll_RunCount
GameData$Roll_Run_EXPSucc<-GameData$Roll_Run_EXPSucc/GameData$Roll_RunCount
GameData$Roll_run_xYPP<-GameData$Roll_run_xYPP/GameData$Roll_RunCount

GameData$Roll_pass_epa<-GameData$Roll_pass_epa/GameData$Roll_passCount
GameData$Roll_run_epa<-GameData$Roll_pass_epa/GameData$Roll_RunCount

GameData$Roll_kick_epa<-GameData$Roll_kick_epa/GameData$Roll_kick_count
GameData$Roll_special_epa<-GameData$Roll_special_epa/GameData$Roll_special_count
GameData$Roll_fumble_epa<-GameData$Roll_fumble_epa/GameData$Roll_fumble_count

GameData$Roll_EXPPass<-GameData$Roll_EXPPass/GameData$Roll_playCount
GameData$Roll_PassRate<-GameData$Roll_PassRate/GameData$Roll_playCount

GameData$Roll_EXPFourth<-GameData$Roll_EXPFourth/GameData$Roll_fourthCount
GameData$Roll_GoRate<-GameData$Roll_GoRate/GameData$Roll_fourthCount

GameData$Roll_Run_Succ_AEX<-(GameData$Roll_Run_success-GameData$Roll_Run_EXPSucc)
GameData$Roll_Run_yards_AEX<-(GameData$Roll_run_yards-GameData$Roll_run_xYPP)
GameData$Roll_Pass_Succ_AEX<-(GameData$Roll_Pass_success-GameData$Roll_Pass_EXPSucc)
GameData$Roll_PassRate_AEX<-GameData$Roll_PassRate-GameData$Roll_EXPPass
GameData$Roll_GoRate_AEX<-GameData$Roll_GoRate-GameData$Roll_EXPFourth
GameData$Roll_yac_epa_AEX<-GameData$Roll_yac_epa-GameData$Roll_xyac_epa
GameData$Roll_yac_AEX<-GameData$Roll_yac/GameData$Roll_xyac


#
## Defensive
#

GameData$Roll_Dpass_yards<-GameData$Roll_Dpass_yards/GameData$Roll_DpassCount
GameData$Roll_DPass_success<-GameData$Roll_DPass_success/GameData$Roll_DpassCount
GameData$Roll_DPass_EXPSucc<-GameData$Roll_DPass_EXPSucc/GameData$Roll_DpassCount
GameData$Roll_Dxyac<-GameData$Roll_Dxyac/GameData$Roll_DpassCount
GameData$Roll_Dyac<-GameData$Roll_Dyac/GameData$Roll_DpassCount
GameData$Roll_Dyac_epa<-GameData$Roll_Dyac_epa/GameData$Roll_DpassCount
GameData$Roll_Dxyac_epa<-GameData$Roll_Dxyac_epa/GameData$Roll_DpassCount

GameData$Roll_Drun_yards<-GameData$Roll_Drun_yards/GameData$Roll_DRunCount
GameData$Roll_DRun_success<-GameData$Roll_DRun_success/GameData$Roll_DRunCount
GameData$Roll_DRun_EXPSucc<-GameData$Roll_DRun_EXPSucc/GameData$Roll_DRunCount
GameData$Roll_Drun_xYPP<-GameData$Roll_Drun_xYPP/GameData$Roll_DRunCount

GameData$Roll_Dpass_epa<-GameData$Roll_Dpass_epa/GameData$Roll_DpassCount
GameData$Roll_Drun_epa<-GameData$Roll_Dpass_epa/GameData$Roll_DRunCount

GameData$Roll_Dkick_epa<-GameData$Roll_Dkick_epa/GameData$Roll_Dkick_count
GameData$Roll_Dspecial_epa<-GameData$Roll_Dspecial_epa/GameData$Roll_Dspecial_count
GameData$Roll_Dfumble_epa<-GameData$Roll_Dfumble_epa/GameData$Roll_Dfumble_count

GameData$Roll_DEXPPass<-GameData$Roll_DEXPPass/GameData$Roll_DplayCount
GameData$Roll_DPassRate<-GameData$Roll_DPassRate/GameData$Roll_DplayCount

GameData$Roll_DEXPFourth<-GameData$Roll_DEXPFourth/GameData$Roll_DfourthCount
GameData$Roll_DGoRate<-GameData$Roll_DGoRate/GameData$Roll_DfourthCount

GameData$Roll_DRun_Succ_AEX<-(GameData$Roll_DRun_success-GameData$Roll_DRun_EXPSucc)
GameData$Roll_DRun_yards_AEX<-(GameData$Roll_Drun_yards-GameData$Roll_Drun_xYPP)
GameData$Roll_DPass_Succ_AEX<-(GameData$Roll_DPass_success-GameData$Roll_DPass_EXPSucc)
GameData$Roll_DPassRate_AEX<-GameData$Roll_DPassRate-GameData$Roll_DEXPPass
GameData$Roll_DGoRate_AEX<-GameData$Roll_DGoRate-GameData$Roll_DEXPFourth
GameData$Roll_Dyac_epa_AEX<-GameData$Roll_Dyac_epa-GameData$Roll_Dxyac_epa
GameData$Roll_Dyac_AEX<-GameData$Roll_Dyac/GameData$Roll_Dxyac

#
## Opponent Offense
#
GameData2<-GameData


GameData2$O_Roll_pass_yards<-GameData2$O_Roll_pass_yards/GameData2$O_Roll_DpassCount
GameData2$O_Roll_Pass_success<-GameData2$O_Roll_Pass_success/GameData2$O_Roll_DpassCount
GameData2$O_Roll_Pass_EXPSucc<-GameData2$O_Roll_Pass_EXPSucc/GameData2$O_Roll_DpassCount

GameData2$O_Roll_xyac<-GameData2$O_Roll_xyac/GameData2$O_Roll_DpassCount
GameData2$O_Roll_yac<-GameData2$O_Roll_yac/GameData2$O_Roll_DpassCount
GameData2$O_Roll_yac_epa<-GameData2$O_Roll_yac_epa/GameData2$O_Roll_DpassCount
GameData2$O_Roll_xyac_epa<-GameData2$O_Roll_xyac_epa/GameData2$O_Roll_DpassCount

GameData2$O_Roll_run_yards<-GameData2$O_Roll_run_yards/GameData2$O_Roll_DRunCount
GameData2$O_Roll_Run_success<-GameData2$O_Roll_Run_success/GameData2$O_Roll_DRunCount
GameData2$O_Roll_Run_EXPSucc<-GameData2$O_Roll_Run_EXPSucc/GameData2$O_Roll_DRunCount
GameData2$O_Roll_run_xYPP<-GameData2$O_Roll_run_xYPP/GameData2$O_Roll_DRunCount

GameData2$O_Roll_pass_epa<-GameData2$O_Roll_pass_epa/GameData2$O_Roll_DpassCount
GameData2$O_Roll_run_epa<-GameData2$O_Roll_pass_epa/GameData2$O_Roll_DRunCount

GameData2$O_Roll_kick_epa<-GameData2$O_Roll_kick_epa/GameData2$O_Roll_Dkick_count
GameData2$O_Roll_special_epa<-GameData2$O_Roll_special_epa/GameData2$O_Roll_Dspecial_count
GameData2$O_Roll_fumble_epa<-GameData2$O_Roll_fumble_epa/GameData2$O_Roll_Dfumble_count

GameData2$O_Roll_EXPPass<-GameData2$O_Roll_EXPPass/GameData2$O_Roll_DplayCount
GameData2$O_Roll_PassRate<-GameData2$O_Roll_PassRate/GameData2$O_Roll_DplayCount

GameData2$O_Roll_EXPFourth<-GameData2$O_Roll_EXPFourth/GameData2$O_Roll_DfourthCount
GameData2$O_Roll_GoRate<-GameData2$O_Roll_GoRate/GameData2$O_Roll_DfourthCount

GameData2$O_Roll_Run_Succ_AEX<-(GameData2$O_Roll_Run_success-GameData2$O_Roll_Run_EXPSucc)
GameData2$O_Roll_Run_yards_AEX<-(GameData2$O_Roll_run_yards-GameData2$O_Roll_run_xYPP)
GameData2$O_Roll_Pass_Succ_AEX<-(GameData2$O_Roll_Pass_success-GameData2$O_Roll_Pass_EXPSucc)
GameData2$O_Roll_PassRate_AEX<-GameData2$O_Roll_PassRate-GameData2$O_Roll_EXPPass
GameData2$O_Roll_GoRate_AEX<-GameData2$O_Roll_GoRate-GameData2$O_Roll_EXPFourth
GameData2$O_Roll_yac_epa_AEX<-GameData2$O_Roll_yac_epa-GameData2$O_Roll_xyac_epa
GameData2$O_Roll_yac_AEX<-GameData2$O_Roll_yac/GameData2$O_Roll_xyac


#
## Opponent Defense
#


GameData2$O_Roll_Dpass_yards<-GameData2$O_Roll_Dpass_yards/GameData2$O_Roll_passCount
GameData2$O_Roll_DPass_success<-GameData2$O_Roll_DPass_success/GameData2$O_Roll_passCount
GameData2$O_Roll_DPass_EXPSucc<-GameData2$O_Roll_DPass_EXPSucc/GameData2$O_Roll_passCount

GameData2$O_Roll_Dxyac<-GameData2$O_Roll_Dxyac/GameData2$O_Roll_passCount
GameData2$O_Roll_Dyac<-GameData2$O_Roll_Dyac/GameData2$O_Roll_passCount
GameData2$O_Roll_Dyac_epa<-GameData2$O_Roll_Dyac_epa/GameData2$O_Roll_passCount
GameData2$O_Roll_Dxyac_epa<-GameData2$O_Roll_Dxyac_epa/GameData2$O_Roll_passCount

GameData2$O_Roll_Drun_yards<-GameData2$O_Roll_Drun_yards/GameData2$O_Roll_RunCount
GameData2$O_Roll_DRun_success<-GameData2$O_Roll_DRun_success/GameData2$O_Roll_RunCount
GameData2$O_Roll_DRun_EXPSucc<-GameData2$O_Roll_DRun_EXPSucc/GameData2$O_Roll_RunCount
GameData2$O_Roll_Drun_xYPP<-GameData2$O_Roll_Drun_xYPP/GameData2$O_Roll_RunCount

GameData2$O_Roll_Dpass_epa<-GameData2$O_Roll_Dpass_epa/GameData2$O_Roll_passCount
GameData2$O_Roll_Drun_epa<-GameData2$O_Roll_Dpass_epa/GameData2$O_Roll_RunCount

GameData2$O_Roll_Dkick_epa<-GameData2$O_Roll_Dkick_epa/GameData2$O_Roll_kick_count
GameData2$O_Roll_Dspecial_epa<-GameData2$O_Roll_Dspecial_epa/GameData2$O_Roll_special_count
GameData2$O_Roll_Dfumble_epa<-GameData2$O_Roll_Dfumble_epa/GameData2$O_Roll_fumble_count

GameData2$O_Roll_DEXPPass<-GameData2$O_Roll_DEXPPass/GameData2$O_Roll_playCount
GameData2$O_Roll_DPassRate<-GameData2$O_Roll_DPassRate/GameData2$O_Roll_playCount

GameData2$O_Roll_DEXPFourth<-GameData2$O_Roll_DEXPFourth/GameData2$O_Roll_fourthCount
GameData2$O_Roll_DGoRate<-GameData2$O_Roll_DGoRate/GameData2$O_Roll_fourthCount

GameData2$O_Roll_DRun_Succ_AEX<-(GameData2$O_Roll_DRun_success-GameData2$O_Roll_DRun_EXPSucc)
GameData2$O_Roll_DRun_yards_AEX<-(GameData2$O_Roll_Drun_yards-GameData2$O_Roll_Drun_xYPP)
GameData2$O_Roll_DPass_Succ_AEX<-(GameData2$O_Roll_DPass_success-GameData2$O_Roll_DPass_EXPSucc)
GameData2$O_Roll_DPassRate_AEX<-GameData2$O_Roll_DPassRate-GameData2$O_Roll_DEXPPass
GameData2$O_Roll_DGoRate_AEX<-GameData2$O_Roll_DGoRate-GameData2$O_Roll_DEXPFourth
GameData2$O_Roll_Dyac_epa_AEX<-GameData2$O_Roll_Dyac_epa-GameData2$O_Roll_Dxyac_epa
GameData2$O_Roll_Dyac_AEX<-GameData2$O_Roll_Dyac/GameData2$O_Roll_Dxyac


GameDataFinal<-GameData2[,-c(6:62)]

drops <- c("O_Roll_fourthCount","O_Roll_playCount","O_Roll_fumble_count","O_Roll_special_count"
           ,"O_Roll_kick_count","O_Roll_RunCount","O_Roll_passCount",
           "O_Roll_DplayCount","O_Roll_Dfumble_count","O_Roll_Dspecial_count","O_Roll_Dkick_count"
           ,"O_Roll_DfourthCount","O_Roll_DRunCount","O_Roll_DpassCount",
           "Roll_DfourthCount","Roll_DplayCount","Roll_Dfumble_count","Roll_Dspecial_count"
           ,"Roll_Dkick_count","Roll_DRunCount","Roll_DpassCount","Roll_fourthCount","Roll_playCount","Roll_fumble_count","Roll_special_count"
           ,"Roll_kick_count","Roll_RunCount","Roll_passCount")

GameDataFinal<-GameDataFinal[ , !(names(GameDataFinal) %in% drops)]

