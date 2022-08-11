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
library(tidyverse)
library(data.table)

#rm(list = ls())

pbp21<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
pbpAll<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean21.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
pbp<-rbind(pbp21,pbpAll)

###
epa_data<-subset(pbp, (pbp$play_type!="extra_point")  & (pbp$play_type!="kickoff")
                 & (pbp$play_type!="field_goal")  & (pbp$play_type!="punt") & !is.na(pbp$epa))

# quick anal
# pbp21<-subset(epa_data,epa_data$season>2016)
# #hist(pbp21$air_yards)
# pbp21$BadWind<-ifelse(pbp21$wind>10,"Yes","No")
# pbp21$count<-1
# quantile(pbp21$air_yards,seq(0, 1, by = .1),na.rm=T)
# 
# library(Hmisc)
# pbp21$air_yards_bin<-as.numeric(cut2((pbp21$air_yards),g=10))
# detach("package:Hmisc", unload = TRUE)
# 
# Bins<- pbp21 %>%
#   group_by(BadWind,air_yards_bin) %>%
#   summarize(Points_Added=sum(epa)/sum(count),playcount=sum(count))
# 
# ggplot(Bins, aes(x = air_yards_bin, y = Points_Added,colour = BadWind)) +
#   geom_point()+
#   geom_smooth(method = "gam", se = FALSE)+
#   ggtitle("Points added by Air Yards Decile per QB Dropback, Wind > 10 MPH, ")
# 
# pbp21$QuarterBack<-ifelse(pbp21$passer_player_name=="J.Allen","J.Allen","No")
# pbp21$QuarterBack<-ifelse(pbp21$passer_player_name=="M.Jones","M.Jones",pbp21$QuarterBack)
# Test<-subset(pbp21,pbp21$passer_player_name=="M.Jones"|pbp21$passer_player_name=="J.Allen" & pbp21$season==2021)
# 
# Bins2<- Test %>%
#   group_by(QuarterBack,air_yards_bin,BadWind) %>%
#   summarize(Points_Added=sum(epa)/sum(count),dropbacks=sum(count))
# 
# Test<-subset(pbp21,pbp21$passer_player_name=="J.Allen" & pbp21$season>2015)
# 
# Bins2<- Test %>%
#   group_by(air_yards_bin,BadWind) %>%
#   summarize(Points_Added=sum(epa)/sum(count),dropbacks=sum(count))
# 
# ggplot(Bins2, aes(x = air_yards_bin, y = Points_Added, colour =BadWind)) +
#   geom_point()+
#   geom_smooth(method = "loess", se = FALSE)+
#   ggtitle("Points added by Air Yards Decile, Josh Allen, Career")
# 
# ###


#
## Game
#


epa_data<-subset(pbp, (pbp$play_type!="extra_point")  & (pbp$play_type!="kickoff")
                 & (pbp$play_type!="field_goal")  & (pbp$play_type!="punt"))

epa_data$start_wp<-ifelse(epa_data$drive_play_id_started==epa_data$play_id,epa_data$wp,0)
epa_data$epa<-ifelse(is.na(epa_data$epa),0,epa_data$epa)
library(dplyr)

epa_data<-subset(epa_data,!is.na(epa_data$posteam)&!is.na(epa_data$defteam))
epa_data<-subset(epa_data,(epa_data$posteam!="")&(epa_data$defteam!=""))
epa_data$count<-1

Bins<- epa_data %>%
  group_by(season,posteam,defteam,week,fixed_drive) %>%
  summarize(epa=sum(epa),playcount=sum(count))

Bins$count<-1
Drive_Bins<- Bins %>%
  group_by(season,posteam,defteam,week) %>%
  summarize(epa=sum(epa),playcount=sum(playcount),drives=sum(count))

write.csv(Drive_Bins,'/Users/am/Desktop/NFLSets/GBGData/DriveEPA.csv')

##

Drive_Bins$count<-1
Team_Bins<- Drive_Bins %>%
  group_by(season,week,posteam) %>%
  summarize(epa=sum(epa),playcount=sum(playcount),drives=sum(drives))

EarlyWeeks<-subset(Team_Bins,Team_Bins$week<9&Team_Bins$season<2021)
LateWeeks<-subset(Team_Bins,Team_Bins$week>8&Team_Bins$season<2021)

EarlyWeeks<- EarlyWeeks %>%
  group_by(season,posteam) %>%
  summarize(epa=sum(epa),playcount=sum(playcount),drives=sum(drives))

LateWeeks<- LateWeeks %>%
  group_by(season,posteam) %>%
  summarize(epa=sum(epa),playcount=sum(playcount),drives=sum(drives))

EarlyWeeks$EPA_Drive<-EarlyWeeks$epa/EarlyWeeks$drives
EarlyWeeks$EPA_Play<-EarlyWeeks$epa/EarlyWeeks$playcount
LateWeeks$EPA_Drive_L<-LateWeeks$epa/LateWeeks$drives
LateWeeks$EPA_Play_L<-LateWeeks$epa/LateWeeks$playcount

Off_Cor<-merge(EarlyWeeks,LateWeeks,by=c("season","posteam"))
plot(Off_Cor$EPA_Drive,Off_Cor$EPA_Drive_L)
plot(Off_Cor$EPA_Play,Off_Cor$EPA_Play_L)

Def_Bins<- Drive_Bins %>%
  group_by(season,week,defteam) %>%
  summarize(epa=sum(epa),playcount=sum(playcount),drives=sum(drives))

EarlyWeeks<-subset(Def_Bins,Def_Bins$week<9&Def_Bins$season<2021)
LateWeeks<-subset(Def_Bins,Def_Bins$week>8&Def_Bins$season<2021)

EarlyWeeks<- EarlyWeeks %>%
  group_by(season,defteam) %>%
  summarize(epa=sum(epa),playcount=sum(playcount),drives=sum(drives))

LateWeeks<- LateWeeks %>%
  group_by(season,defteam) %>%
  summarize(epa=sum(epa),playcount=sum(playcount),drives=sum(drives))

EarlyWeeks$EPA_Drive<-EarlyWeeks$epa/EarlyWeeks$drives
EarlyWeeks$EPA_Play<-EarlyWeeks$epa/EarlyWeeks$playcount
LateWeeks$EPA_Drive_L<-LateWeeks$epa/LateWeeks$drives
LateWeeks$EPA_Play_L<-LateWeeks$epa/LateWeeks$playcount

Def_Cor<-merge(EarlyWeeks,LateWeeks,by=c("season","defteam"))
cor(Def_Cor$EPA_Drive,Def_Cor$EPA_Drive_L)
cor(Def_Cor$EPA_Play,Def_Cor$EPA_Play_L)

#
## Game
#

epa_data<-subset(pbp,pbp$wp>0.005&pbp$wp<0.995 & !is.na(pbp$epa) & pbp$play_type!="no_play")
epa_data$trash<-ifelse((epa_data$interception==1|epa_data$fumble_lost==1) & epa_data$half_seconds_remaining<20,1,0)
ftable(epa_data$trash)
epa_data<-subset(epa_data,epa_data$trash==0)
epa_data$trash<-ifelse(epa_data$down==4& (epa_data$play_type=="run"|epa_data$play_type=="pass"),1,0)
ftable(epa_data$trash)
epa_data<-subset(epa_data,epa_data$trash==0)
epa_data$count<-1

epa_data$play_type<-ifelse(epa_data$play_type=="punt"|epa_data$play_type=="kickoff","special",epa_data$play_type)
epa_data$play_type<-ifelse(epa_data$play_type=="extra_point"|epa_data$play_type=="field_goal","kick",epa_data$play_type)
epa_data$play_type<-ifelse(epa_data$fumble_lost==1,"fumble",epa_data$play_type)
#epa_data$play_type<-ifelse(epa_data$interception==1,"interception",epa_data$play_type)
epa_data$epa<-ifelse(epa_data$interception==1,-4.34266023,epa_data$epa)
ftable(epa_data$play_type)

library(dplyr)
epa_data$count<-1
Bins<- epa_data %>%
  group_by(season,posteam,defteam,week,play_type) %>%
  summarize(epa=sum(epa),playcount=sum(count))

write.csv(Bins,'/Users/am/Desktop/NFLSets/GBGData/EPA.csv')