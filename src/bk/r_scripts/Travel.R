# library(tools)
# library(utils)
# library(stargazer)
# library(ggpubr)
# library(nnet)
# library(MASS)
# library(mlogit)
# library(foreign)
# library(ggplot2)
# library(MLmetrics)
# library(caret)
# library(leaps)
# library(MPV)
# library(zoo)
# library(tidyverse)
# library(data.table)


#rm(list = ls())

#
##
### Team/Game Data Clean
##
#


Schedule<- read.csv('/Users/am/Desktop/NFLSets/PBP/Schedule.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
library(dplyr)
Schedule$Home<-1

Schedule2<-Schedule
Schedule2$home_team<-Schedule$away_team
Schedule2$away_team<-Schedule$home_team

Schedule2$away_score<-Schedule$home_score
Schedule2$home_score<-Schedule$away_score
Schedule2$home_result<-Schedule2$home_result*-1
Schedule2$Home<-0

ScheduleAll<-rbind(Schedule,Schedule2)

ScheduleAll$home_team<-ifelse(ScheduleAll$home_team=="STL","LA",ScheduleAll$home_team)
ScheduleAll$home_team<-ifelse(ScheduleAll$home_team=="OAK","LV",ScheduleAll$home_team)
ScheduleAll$home_team<-ifelse(ScheduleAll$home_team=="SD","LAC",ScheduleAll$home_team)
ScheduleAll$away_team<-ifelse(ScheduleAll$away_team=="STL","LA",ScheduleAll$away_team)
ScheduleAll$away_team<-ifelse(ScheduleAll$away_team=="OAK","LV",ScheduleAll$away_team)
ScheduleAll$away_team<-ifelse(ScheduleAll$away_team=="SD","LAC",ScheduleAll$away_team)


Stadiums<- read.csv('/Users/am/Desktop/NFLSets/stadiums/stadiums.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
Stadiums$TimeZone<-as.numeric(Stadiums$TimeZone)
Stadiums[37,]<-c(37,"LV","AFC",36.0909,115.1833,3)
Stadiums$Team<-ifelse(Stadiums$Team=="JAC","JAX",Stadiums$Team)
Stadiums$longitude<-as.numeric(Stadiums$longitude)
Stadiums$latitude<-as.numeric(Stadiums$latitude)

ScheduleAll$stadium<-ifelse(ScheduleAll$stadium=="Wembley Stadium", "LON", ScheduleAll$stadium)
ScheduleAll$stadium<-ifelse(ScheduleAll$stadium=="Azteca Stadium", "MEX", ScheduleAll$stadium)
ScheduleAll$stadium<-ifelse(ScheduleAll$stadium=="Estadio Azteca", "MEX", ScheduleAll$stadium)
ScheduleAll$stadium<-ifelse(ScheduleAll$stadium=="Tottenham Hotspur Stadium", "LON", ScheduleAll$stadium)
ScheduleAll$stadium<-ifelse(ScheduleAll$stadium=="Twickenham Stadium", "LON", ScheduleAll$stadium)
ScheduleAll$stadium<-ifelse(ScheduleAll$stadium=="Twickenham", "LON", ScheduleAll$stadium)
ScheduleAll$stadium<-ifelse(ScheduleAll$stadium=="Tottenham Hotspur", "LON", ScheduleAll$stadium)
ScheduleAll$stadium<-ifelse(ScheduleAll$stadium=="Tottenham Stadium", "LON", ScheduleAll$stadium)
ftable(ScheduleAll$stadium,ScheduleAll$location=="Neutral")
Stadiums$home_team<-Stadiums$Team
Schedule<-merge(ScheduleAll,Stadiums, by="home_team",all.x=TRUE)

Stadiums<- read.csv('/Users/am/Desktop/NFLSets/stadiums/stadiums.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
Stadiums$TimeZone<-as.numeric(Stadiums$TimeZone)
Stadiums[37,]<-c(37,"LV","AFC",36.0909,115.1833,3)
Stadiums$Team<-ifelse(Stadiums$Team=="JAC","JAX",Stadiums$Team)
Stadiums$longitude<-as.numeric(Stadiums$longitude)
Stadiums$latitude<-as.numeric(Stadiums$latitude)
Stadiums$away_team<-Stadiums$Team
Schedule3<-merge(Schedule,Stadiums, by="away_team",all.x=TRUE)

Schedule3$longitude.y<-round(Schedule3$longitude.y,2)
Schedule3$longitude.x<-round(Schedule3$longitude.x,2)
Schedule3$latitude.y<-round(Schedule3$latitude.y,2)
Schedule3$latitude.x<-round(Schedule3$latitude.x,2)

Schedule3<-as.data.frame(Schedule3)
library(geosphere)
Schedule3$Distance<-distm(cbind(Schedule3$longitude.y, Schedule3$latitude.y), cbind(Schedule3$longitude.x, Schedule3$latitude.x), fun = distHaversine)

Schedule3<-Schedule3 %>%  
  mutate(Distance = distHaversine(cbind(longitude.y, latitude.y), cbind(longitude.x, latitude.x)))
Schedule3$Distance<-Schedule3$Distance/1000
Schedule3$Distance<-ifelse(Schedule3$location=="Neutral" ,0, Schedule3$Distance)

Schedule4<-Schedule3

library(plyr)
Schedule4<-arrange(Schedule4,home_team,game_id,season) %>%
  group_by(game_id,season,home_team)%>%
  mutate(Rest= (as.Date(as.character(gameday), format="%Y-%m-%d")-
           as.Date(as.character(lag(gameday)), format="%Y-%m-%d")))

Schedule4$Rest<-as.numeric(Schedule4$Rest)
summary(Schedule4$Rest)

Schedule4$Rest<-ifelse(lag(Schedule4$season)!=Schedule4$season,7,Schedule4$Rest)
summary(Schedule4$Rest)
ftable(is.na(Schedule4$Rest))
Schedule4$Rest<-ifelse(is.na(Schedule4$Rest),7,Schedule4$Rest)

Schedule4$Home<-ifelse(Schedule4$location=="Neutral",0.5,Schedule4$Home)
Schedule4$btbR<-ifelse(lag(Schedule4$Home)==0&Schedule4$Home==0&lag(Schedule4$home_team)==Schedule4$home_team &lag(Schedule4$season)==Schedule4$season&lag(Schedule4$week)==Schedule4$week-1 ,1,0)
Schedule4$btbH<-ifelse(lag(Schedule4$Home)==1&Schedule4$Home==1&lag(Schedule4$home_team)==Schedule4$home_team &lag(Schedule4$season)==Schedule4$season&lag(Schedule4$week)==Schedule4$week-1,1,0)
ftable(Schedule4$btbR)
ftable(Schedule4$btbH)

Schedule4$btbtbH<-ifelse(Schedule4$btbH==1 & lag(lag(Schedule4$Home))==1&lag(Schedule4$Home)==1 & lag(lag(Schedule4$home_team))==lag(Schedule4$home_team) &lag(lag(Schedule4$season))==lag(Schedule4$season) &lag(lag(Schedule4$week))==lag(Schedule4$week)-1,1,0)
Schedule4$btbtbR<-ifelse(Schedule4$btbR==1 & lag(lag(Schedule4$Home))==0&lag(Schedule4$Home)==0 & lag(lag(Schedule4$home_team))==lag(Schedule4$home_team) &lag(lag(Schedule4$season))==lag(Schedule4$season) &lag(lag(Schedule4$week))==lag(Schedule4$week)-1,1,0)
ftable(Schedule4$btbtbH)
ftable(Schedule4$btbtbR)


Schedule2<-Schedule4


Schedule2<-arrange(Schedule2,away_team,game_id,season) %>%
  group_by(game_id,season,away_team)%>%
  mutate(Opp_Rest= (as.Date(as.character(gameday), format="%Y-%m-%d")-
                  as.Date(as.character(lag(gameday)), format="%Y-%m-%d")))

Schedule2$Opp_Rest<-as.numeric(Schedule2$Opp_Rest)
summary(Schedule2$Opp_Rest)
ftable(is.na(Schedule2$Opp_Rest))

Schedule2$Opp_Rest<-ifelse(lag(Schedule2$season)!=Schedule2$season,7,Schedule2$Opp_Rest)
Schedule2$Opp_Rest<-ifelse(is.na(Schedule2$Opp_Rest),7,Schedule2$Opp_Rest)

Schedule2$Opp_btbH<-ifelse(lag(Schedule2$Home)==0&Schedule2$Home==0 & lag(Schedule2$away_team)==Schedule2$away_team &lag(Schedule2$season)==Schedule2$season &lag(Schedule2$week)==Schedule2$week-1,1,0)
Schedule2$Opp_btbR<-ifelse(lag(Schedule2$Home)==1&Schedule2$Home==1 & lag(Schedule2$away_team)==Schedule2$away_team &lag(Schedule2$season)==Schedule2$season &lag(Schedule2$week)==Schedule2$week-1,1,0)
ftable(Schedule2$Opp_btbH)
ftable(Schedule2$Opp_btbR)

Schedule2$Opp_btbtbH<-ifelse(Schedule2$Opp_btbH==1 & lag(lag(Schedule2$Home))==0&lag(Schedule2$Home)==0 & lag(lag(Schedule2$away_team))==lag(Schedule2$away_team) &lag(lag(Schedule2$season))==lag(Schedule2$season) &lag(lag(Schedule2$week))==lag(Schedule2$week)-1,1,0)
Schedule2$Opp_btbtbR<-ifelse(Schedule2$Opp_btbR==1 & lag(lag(Schedule2$Home))==1&lag(Schedule2$Home)==1 & lag(lag(Schedule2$away_team))==lag(Schedule2$away_team) &lag(lag(Schedule2$season))==lag(Schedule2$season) &lag(lag(Schedule2$week))==lag(Schedule2$week)-1,1,0)
ftable(Schedule2$Opp_btbtbH)
ftable(Schedule2$Opp_btbtbR)


#Bins<- Schedule3 %>%
 # group_by(game_id) %>%
 #summarize(lagOpp_btbH= sum(Opp_btbH),lagOpp_btbR= sum(Opp_btbR),btbR=sum(btbR),btbH=sum(btbH))
#Bins[1,2:5]<-0
#Bins$rowsum<-(Bins$lagOpp_btbH+Bins$lagOpp_btbR+Bins$btbR+Bins$btbH)
#Bins2<-subset(Bins,Bins$rowsum%%2!=0)

###


Schedule2$Opp_btbH<-ifelse(is.na(Schedule2$Opp_btbH),0,Schedule2$Opp_btbH)
Schedule2$Opp_btbR<-ifelse(is.na(Schedule2$Opp_btbR),0,Schedule2$Opp_btbR)
Schedule2$btbH<-ifelse(is.na(Schedule2$btbH),0,Schedule2$btbH)
Schedule2$btbR<-ifelse(is.na(Schedule2$btbR),0,Schedule2$btbR)

Schedule2$Opp_btbtbH<-ifelse(is.na(Schedule2$Opp_btbtbH),0,Schedule2$Opp_btbtbH)
Schedule2$Opp_btbtbR<-ifelse(is.na(Schedule2$Opp_btbtbR),0,Schedule2$Opp_btbtbR)
Schedule2$btbtbH<-ifelse(is.na(Schedule2$btbtbH),0,Schedule2$btbtbH)
Schedule2$btbtbR<-ifelse(is.na(Schedule2$btbtbR),0,Schedule2$btbtbR)

Schedule2$Rest<-ifelse(is.na(Schedule2$Rest),7,Schedule2$Rest)
Schedule2$Opp_Rest<-ifelse(is.na(Schedule2$Opp_Rest),7,Schedule2$Opp_Rest)

Schedule2$Rest<-as.numeric(Schedule2$Rest)
Schedule2$Opp_Rest<-as.numeric(Schedule2$Opp_Rest)
Schedule2$DRest<-Schedule2$Rest-Schedule2$Opp_Rest

Schedule2$DistanceD<-ifelse(Schedule2$Home==1,-Schedule2$Distance,Schedule2$Distance)

### Time Zone Shift
Schedule2$TimeZone.y<-as.numeric(Schedule2$TimeZone.y)
Schedule2$TimeZone.x<-as.numeric(Schedule2$TimeZone.x)
Schedule2$early_visitor<-ifelse((Schedule2$gametime=="13:00"|Schedule2$gametime=="13:05"|Schedule2$gametime=="13:30"|Schedule2$gametime=="12:00"|Schedule2$gametime=="12:30"),
                                  Schedule2$TimeZone.y-Schedule2$TimeZone.x,0)
ftable(Schedule2$gametime)
write.csv(Schedule2,'/Users/am/Desktop/NFLSets/GBGData/Travel.csv')

