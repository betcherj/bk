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

# Modeling
pass_data<-subset(pass_data,pass_data$wp>0.005&pass_data$wp<0.995)
pass_data$success<-as.numeric(pass_data$success)
pass_data1<-subset(pass_data,pass_data$down==1)
pass_data2<-subset(pass_data,pass_data$down==2)
pass_data3<-subset(pass_data,pass_data$down==3 | pass_data$down==4)

#
## 5 Fit Models
#

library(mgcv)
Gibbs<-bam(success~s(yardline_100, bs = 'cr', k =10)
           +wind+Rain+LightRain+Snow+i_roof+
             +s(ydstogo,bs = 'cr')
           , method = "REML",family=("binomial"(link="logit")), data=pass_data1)
summary(Gibbs)

pass_data1$EXPsucc<-predict(Gibbs,pass_data1,type="response")

Gibbs<-bam(success~s(yardline_100, bs = 'cr', k =10)
           +wind+Rain+LightRain+Snow+i_roof+
             +s(ydstogo,bs = 'cr')
           , method = "REML",family=("binomial"(link="logit")), data=pass_data2)
summary(Gibbs)

pass_data2$EXPsucc<-predict(Gibbs,pass_data2,type="response")

Gibbs<-bam(success~s(yardline_100, bs = 'cr', k =10)
           +wind+Rain+LightRain+Snow+i_roof+
             +s(ydstogo,bs = 'cr')
           , method = "REML",family=("binomial"(link="logit")), data=pass_data3)
summary(Gibbs)

pass_data3$EXPsucc<-predict(Gibbs,pass_data3,type="response")



#
## Calibration
#
pass_data_all<-rbind(pass_data1,pass_data2,pass_data3)

calibration_plot <- pass_data_all %>% 
  mutate(
    pred_rnd = round(EXPsucc, 2)
  ) %>% 
  group_by(pred_rnd) %>% 
  summarize(
    mean_pred = mean(EXPsucc),
    mean_obs = mean(success),
    n = n()
  ) %>% 
  ggplot(aes(x = mean_pred, y = mean_obs)) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(size = n), alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Predicted pass Success", 
    y = "Observed pass Success"
  ) +
  coord_cartesian(
    xlim = c(0,1), ylim = c(0, 1)
  )
calibration_plot

#
## Create Table
#

require(dplyr)
Bins<- pass_data_all %>%
  group_by(posteam,defteam,game_id) %>%
  summarize(EXPsucc=sum(EXPsucc),success=sum(success),passCount=sum(count))

write.csv(Bins,'/Users/am/Desktop/NFLSets/GBGData/PassSucc.csv')

