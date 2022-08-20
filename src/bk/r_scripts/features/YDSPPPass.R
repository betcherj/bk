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

#pbp21<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
#pbpAll<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean21.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
#pbp<-rbind(pbp21,pbpAll)

pass_data<-subset(pbp,pbp$play_type=="pass"& !is.na(pbp$down))
pass_data$count<-1

Bins<- pass_data %>%
  group_by(yardline_100) %>%
  summarize(yards_gained=sum(yards_gained)/sum(count),count=sum(count))
plot(Bins$yardline_100,Bins$yards_gained)

pass_data$xyac_mean_yardage<-ifelse(is.na(pass_data$xyac_mean_yardage),0,pass_data$xyac_mean_yardage)
pass_data$xyac_mean_yardage<-ifelse(pass_data$complete_pass==0,0,pass_data$xyac_mean_yardage)

pass_data$xyac_epa<-ifelse(is.na(pass_data$xyac_epa),0,pass_data$xyac_epa)
pass_data$xyac_epa<-ifelse(pass_data$complete_pass==0,0,pass_data$xyac_epa)

pass_data$yac_epa<-ifelse(is.na(pass_data$yac_epa),0,pass_data$yac_epa)
pass_data$yac_epa<-ifelse(pass_data$complete_pass==0,0,pass_data$yac_epa)

pass_data$yards_after_catch<-ifelse(is.na(pass_data$yards_after_catch),0,pass_data$yards_after_catch)

#
## Game
#
pass_data_filter<-subset(pass_data,pass_data$wp>0.005&pass_data$wp<0.995)

library(dplyr)
pass_data_filter$count<-1
Bins<- pass_data_filter %>%
  group_by(season,posteam,defteam,week) %>%
  summarize(pass_yards=sum(yards_gained),passCount=sum(count)
            ,xyac=sum(xyac_mean_yardage),yac=sum(yards_after_catch),
            xyac_epa=sum(xyac_epa),yac_epa=sum(yac_epa))

write.csv(Bins,'/Users/am/Desktop/NFLSets/GBGData/passYPP.csv')