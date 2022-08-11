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
pass_data<-subset(pbp, !is.na(pbp$down))
go_data<-subset(pass_data,(pass_data$down==4))
go_data$GoOnFourth<-ifelse(go_data$play_type=="pass"|go_data$play_type=="run",1,0)
go_data<-subset(go_data,go_data$play_type!="is.na")

ftable(go_data$GoOnFourth)
go_data$GoOnFourth<-as.numeric(go_data$GoOnFourth)

go_data$count<-1
go_data$half_seconds_remaining2<-go_data$half_seconds_remaining^2
go_data$wp<-ifelse(is.na(go_data$wp),0.5,go_data$wp)
go_data<-subset(go_data,go_data$wp>0.005&go_data$wp<0.995)

#
## 5 Fit Model
#

library(mgcv)
Gibbs<-bam(GoOnFourth~s(yardline_100, bs = 'cr', k =20)
           +wind+Rain+LightRain+Snow+i_roof+ydstogo
           +s(score_differential, by=qtr, bs = 'cr')
           +s(half_seconds_remaining2, by=score_differential, bs = 'cr') 
           , method = "REML",family=("binomial"(link="logit")), data=go_data)
summary(Gibbs)

go_data$EXPFourth<-predict(Gibbs,go_data,type="response")


calibration_plot <- go_data %>% 
  mutate(
    pred_rnd = round(EXPFourth, 2)
  ) %>% 
  group_by(pred_rnd) %>% 
  summarize(
    mean_pred = mean(EXPFourth),
    mean_obs = mean(GoOnFourth),
    n = n()
  ) %>% 
  ggplot(aes(x = mean_pred, y = mean_obs)) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(size = n), alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Predicted GO Play", 
    y = "Observed GO Play"
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
go_data$posteam_coach<-ifelse(go_data$home_team==go_data$posteam,go_data$home_coach,go_data$away_coach)
go_data$count<-1
Bins<- go_data %>%
  group_by(posteam_coach,season) %>%
  summarize(EXPFourth=sum(EXPFourth)/sum(count),GoRate=sum(GoOnFourth)/sum(count),fourthCount=sum(count))

Bins$GoAEX<-round((Bins$GoRate-Bins$EXPFourth),4)

#write.csv(Bins,'/Users/am/Desktop/NFLSets/Coaches/CoachPlayType.csv')

#
## Game
#

library(dplyr)
go_data$count<-1
Bins<- go_data %>%
  group_by(season,posteam,defteam,week) %>%
  summarize(EXPFourth=sum(EXPFourth),GoRate=sum(GoOnFourth),fourthCount=sum(count))

Bins$GoAEX<-round((Bins$GoRate-Bins$EXPFourth),4)

write.csv(Bins,'/Users/am/Desktop/NFLSets/GBGData/GameAggressive.csv')
