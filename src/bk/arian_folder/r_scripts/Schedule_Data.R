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

##
###
##
#

#rm(list = ls())

# pbp21<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
# pbpAll<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean21.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
# pbp<-rbind(pbp21,pbpAll)

pbpnew<-pbp[!duplicated(pbp$game_id),]


drops <- c("season","week","game_id","Snow"
           ,"Rain","wind","i_wind","i_roof",
           "i_surface","LightRain","temp"
           )
ScheduleKeeps<-subset(pbpnew,select=drops)

#
##
### Home Field and Total
##
#

library(dplyr)
Schedule<- read.csv('/Users/am/Desktop/NFLSets/PBP/Schedule.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
Schedule<-Schedule[,-16]
Schedule<-merge(Schedule,ScheduleKeeps,by=c("season","week","game_id"),all.x = TRUE)
Schedule$Home<-1
Schedule$TrueHome<-ifelse(Schedule$location=="Home",1,0)

Schedule2<-Schedule
Schedule2$home_team<-Schedule$away_team
Schedule2$away_team<-Schedule$home_team

Schedule2$away_score<-Schedule$home_score
Schedule2$home_score<-Schedule$away_score
Schedule2$home_result<-Schedule2$home_result*-1
Schedule2$Home<-0
Schedule2$TrueHome<-ifelse(Schedule2$location=="Home"|Schedule2$location=="Netural",0,1)

ScheduleAll<-rbind(Schedule,Schedule2)

HomeTest<-subset(ScheduleAll,!is.na(ScheduleAll$home_score)&ScheduleAll$TrueHome==1 &ScheduleAll$week<18)
quantile(HomeTest$home_result,0.975)
HomeTest<-subset(HomeTest,HomeTest$home_result<33& HomeTest$home_result>-29&HomeTest$week<18)
HomeTest$count<-1

detach("package:plyr", unload = TRUE)
HomeTest$Win<-ifelse(HomeTest$home_score>HomeTest$away_score,1,0)
HomeTest$Win<-ifelse(HomeTest$home_score==HomeTest$away_score,0.5,HomeTest$Win)

Bins<- HomeTest %>%
  group_by(season) %>%
  summarize(home_score=sum(home_score)/sum(count),count=sum(count),away_score=sum(away_score)/sum(count),
            home_winP=sum(Win)/sum(count))
Bins$HomeAdv<-Bins$home_score-Bins$away_score

model<-loess(Bins$HomeAdv~Bins$season)
summary(model)
Bins$PredHomeAdv<-predict(model,Bins)
Bins<-Bins[,c(1,7)]
ScheduleAll<-merge(ScheduleAll,Bins,by="season")
ScheduleAll$PredHomeAdv<-ifelse(ScheduleAll$location!="Home",-1*ScheduleAll$PredHomeAdv,ScheduleAll$PredHomeAdv)
ScheduleAll$PredHomeAdv<-ifelse(ScheduleAll$location=="Neutral",0,ScheduleAll$PredHomeAdv)

TotalTest<-subset(ScheduleAll,!is.na(ScheduleAll$home_score))
TotalTest$Total<-TotalTest$home_score+TotalTest$away_score
quantile(TotalTest$Total,0.975)
#TotalTest<-subset(TotalTest,TotalTest$Total<70& TotalTest$Total>21&TotalTest$week<18)
TotalTest$count<-1

detach("package:plyr", unload = TRUE)
Bins<- TotalTest %>%
  group_by(season) %>%
  summarize(PredTotal=median(Total))

model<-loess(Bins$PredTotal~Bins$season)
summary(model)
Bins$PredTotal2<-predict(model,Bins)
Bins$PredTotal<-(Bins$PredTotal+Bins$PredTotal2)/2
Bins<-Bins[,c(1,2)]

ScheduleAll<-merge(ScheduleAll,Bins,by="season")

ScheduleAll$home_team<-ifelse(ScheduleAll$home_team=="STL","LA",ScheduleAll$home_team)
ScheduleAll$home_team<-ifelse(ScheduleAll$home_team=="OAK","LV",ScheduleAll$home_team)
ScheduleAll$home_team<-ifelse(ScheduleAll$home_team=="SD","LAC",ScheduleAll$home_team)
ScheduleAll$away_team<-ifelse(ScheduleAll$away_team=="STL","LA",ScheduleAll$away_team)
ScheduleAll$away_team<-ifelse(ScheduleAll$away_team=="OAK","LV",ScheduleAll$away_team)
ScheduleAll$away_team<-ifelse(ScheduleAll$away_team=="SD","LAC",ScheduleAll$away_team)


#
## Odds from PBP data
#

Odds<- pbpnew[,c("game_id","season","week","home_team","away_team","spread_line","total_line")]

Odds2<-Odds
Odds2$home_team<-Odds$away_team
Odds2$away_team<-Odds$home_team
Odds2$spread_line<-Odds2$spread_line*-1
Odds<-rbind(Odds,Odds2)

ScheduleAll<-merge(ScheduleAll,Odds,by=c("home_team","away_team","week","season","game_id"),all.x=TRUE)

#
## Odds from Historical with Open and Close
#

OddsMine<- read.csv('/Users/am/Desktop/NFLSets/Odds/HistoricNFLOdds.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
OddsMine$home_team<-OddsMine$Team
OddsMine$away_team<-OddsMine$Opp
OddsMine$SpreadOpen<-OddsMine$Spread
OddsMine$TotalOpen<-OddsMine$Total
OddsMine$gameday<-OddsMine$Date
OddsMine<-OddsMine[,c(10,11,14,12,13,4,5,8)]

OddsMine$home_team<-ifelse(OddsMine$home_team=="STL","LA",OddsMine$home_team)
OddsMine$home_team<-ifelse(OddsMine$home_team=="OAK","LV",OddsMine$home_team)
OddsMine$home_team<-ifelse(OddsMine$home_team=="SD","LAC",OddsMine$home_team)
OddsMine$home_team<-ifelse(OddsMine$home_team=="JAC","JAX",OddsMine$home_team)
OddsMine$away_team<-ifelse(OddsMine$away_team=="JAC","JAX",OddsMine$away_team)
OddsMine$away_team<-ifelse(OddsMine$away_team=="STL","LA",OddsMine$away_team)
OddsMine$away_team<-ifelse(OddsMine$away_team=="OAK","LV",OddsMine$away_team)
OddsMine$away_team<-ifelse(OddsMine$away_team=="SD","LAC",OddsMine$away_team)
OddsMine$TotalOpen<-ifelse(OddsMine$TotalOpen>100,(OddsMine$TotalClose),OddsMine$TotalOpen)

ScheduleAll<-merge(ScheduleAll,OddsMine,by=c("home_team","away_team","gameday"),all.x=TRUE)
ScheduleAll$spread_line<-ScheduleAll$spread_line*-1

ScheduleAll$TotalClose<-ifelse(is.na(ScheduleAll$TotalClose),ScheduleAll$total_line,ScheduleAll$TotalClose)
ScheduleAll$TotalOpen<-ifelse(is.na(ScheduleAll$TotalOpen),ScheduleAll$total_line,ScheduleAll$TotalOpen)
ScheduleAll$SpreadClose<-ifelse(is.na(ScheduleAll$SpreadClose),ScheduleAll$spread_line,ScheduleAll$SpreadClose)
ScheduleAll$SpreadOpen<-ifelse(is.na(ScheduleAll$SpreadOpen),ScheduleAll$spread_line,ScheduleAll$SpreadOpen)

#ScheduleAll$Trash<-ifelse(is.na(ScheduleAll$spread_line) & ScheduleAll$season<2020,1,0)
#ScheduleAll<-subset(ScheduleAll,ScheduleAll$Trash==0)

#
## Odds from 2021 (new season)
#


Odds21<- read.csv('/Users/am/Desktop/NFLSets/GBGData/NFLOdds21.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
Odds21$home_team<-Odds21$Team
Odds21$away_team<-Odds21$Opp
Odds21$Spread21<-Odds21$Spread
Odds21$Total21<-Odds21$Total
Odds21$ML21<-Odds21$ML
Odds21$gameday<-Odds21$Date
Odds21<-Odds21[,-c(1:7)]

ScheduleAll<-merge(ScheduleAll,Odds21,by=c("home_team","away_team","gameday"),all.x=TRUE)
ScheduleAll$spread_line<-ifelse(ScheduleAll$season>2020 & is.na(ScheduleAll$spread_line),ScheduleAll$Spread21,ScheduleAll$spread_line)
ScheduleAll$total_line<-ifelse(ScheduleAll$season>2020& is.na(ScheduleAll$total_line),ScheduleAll$Total21,ScheduleAll$total_line)
ScheduleAll<-ScheduleAll[,-c(13:16)]

ScheduleAll$PrimeTime<-ifelse(ScheduleAll$weekday!="Sunday","PrimeTime","Regular")
ScheduleAll$PrimeTime<-ifelse(ScheduleAll$gametime>18,"PrimeTime",ScheduleAll$PrimeTime)
ScheduleAll$PrimeTime<-ifelse(is.na(ScheduleAll$PrimeTime),"Regular",ScheduleAll$PrimeTime)

#
##
### Read in CSVs
##
#

#
## Travel and SRS
#

Travel<- read.csv('/Users/am/Desktop/NFLSets/GBGData/Travel.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
Travel<-Travel[,c(2,3,4,7,33:45)]
Travel$TotalRest<-Travel$Rest+Travel$Opp_Rest

ScheduleData<-merge(ScheduleAll,Travel,by=c("away_team","home_team","game_id","week"),all.x = TRUE)

ScheduleData$weekday<-ifelse(ScheduleData$weekday=="Saturday","Sunday",ScheduleData$weekday)
ScheduleData$weekday<-ifelse(ScheduleData$weekday=="Friday","Thursday",ScheduleData$weekday)
ScheduleData$weekday<-ifelse(ScheduleData$weekday=="Tuesday","Monday",ScheduleData$weekday)
ScheduleData$weekday<-ifelse(ScheduleData$weekday=="Wednesday","Thursday",ScheduleData$weekday)
ftable(ScheduleData$weekday)

SRS<- read.csv('/Users/am/Desktop/NFLSets/GBGData/SRS.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
SRS$home_team<-SRS$Team
SRS<-SRS[,c(3:17)]

ScheduleData<-merge(ScheduleData,SRS,by=c("home_team","season","week"),all.x = TRUE)

SRS<- read.csv('/Users/am/Desktop/NFLSets/GBGData/SRS.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
SRS$away_team<-SRS$Team
SRS<-SRS[,c(3:17)]
colnames(SRS)[3:14] <- paste("A", colnames(SRS)[3:14], sep = "")

ScheduleData<-merge(ScheduleData,SRS,by=c("away_team","season","week"),all.x = TRUE)


