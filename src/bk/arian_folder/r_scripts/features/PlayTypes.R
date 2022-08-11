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

pass_data<-subset(pbp,(pbp$play_type=="pass"|pbp$play_type=="run") & !is.na(pbp$down))
pass_data$count<-1
pass_data$PassPlay<-ifelse(pass_data$play_type=="pass",1,0)
pass_data$half_seconds_remaining2<-pass_data$half_seconds_remaining^2
pass_data$wp<-ifelse(is.na(pass_data$wp),0.5,pass_data$wp)
pass_data<-subset(pass_data,pass_data$wp>0.01&pass_data$wp<0.99)

#
## 5 Fit Model
#

library(mgcv)
Gibbs<-bam(PassPlay~s(yardline_100, bs = 'cr', k =20)
           +wind+Rain+LightRain+Snow+i_roof+ydstogo
           +s(ydstogo, by=down,bs = 'cr')
           +s(score_differential, by=qtr, bs = 'cr')
           +s(half_seconds_remaining2, by=score_differential, bs = 'cr')+wp 
           , method = "REML",family=("binomial"(link="logit")), data=pass_data)
summary(Gibbs)

pass_data$EXPPass<-predict(Gibbs,pass_data,type="response")


calibration_plot <- pass_data %>% 
  mutate(
    pred_rnd = round(EXPPass, 2)
  ) %>% 
  group_by(pred_rnd) %>% 
  summarize(
    mean_pred = mean(EXPPass),
    mean_obs = mean(PassPlay),
    n = n()
  ) %>% 
  ggplot(aes(x = mean_pred, y = mean_obs)) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(size = n), alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Predicted Pass Play", 
    y = "Observed Pass Play"
  ) +
  coord_cartesian(
    xlim = c(0,1), ylim = c(0, 1)
  )
calibration_plot


##
### Exports
##
#

#
## Coach
#
pass_data$posteam_coach<-ifelse(pass_data$home_team==pass_data$posteam,pass_data$home_coach,pass_data$away_coach)
pass_data$count<-1
Bins<- pass_data %>%
  group_by(posteam_coach,season) %>%
  summarize(EXPPass=sum(EXPPass)/sum(count),PassRate=sum(PassPlay)/sum(count),playCount=sum(count))

Bins$PassAEX<-round((Bins$PassRate-Bins$EXPPass),4)

#write.csv(Bins,'/Users/am/Desktop/NFLSets/Coaches/CoachPlayType.csv')

#
## Game
#

library(dplyr)
pass_data$count<-1
Bins<- pass_data %>%
  group_by(season,posteam,defteam,week) %>%
  summarize(EXPPass=sum(EXPPass),PassRate=sum(PassPlay),playCount=sum(count))

Bins$PassAEX<-round((Bins$PassRate-Bins$EXPPass),4)

write.csv(Bins,'/Users/am/Desktop/NFLSets/GBGData/GamePlayType.csv')
