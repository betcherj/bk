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

pbpAll<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
pbp21<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean21.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
pbp<-rbind(pbp21,pbpAll)


run_data<-subset(pbp,pbp$play_type=="run"& !is.na(pbp$down))
run_data$count<-1

# Modeling
run_data<-subset(run_data,run_data$wp>0.005&run_data$wp<0.995)
run_data$success<-as.numeric(run_data$success)
run_data1<-subset(run_data,run_data$down==1)
run_data2<-subset(run_data,run_data$down==2)
run_data3<-subset(run_data,run_data$down==3 | run_data$down==4)

#
## 5 Fit Models
#

library(mgcv)
Gibbs<-bam(success~s(yardline_100, bs = 'cr', k =20)
           +Rain+LightRain+Snow+i_surface+
           +s(ydstogo,bs = 'cr')
           , method = "REML",family=("binomial"(link="logit")), data=run_data1)
summary(Gibbs)

run_data1$EXPsucc<-predict(Gibbs,run_data1,type="response")

Gibbs<-bam(success~s(yardline_100, bs = 'cr', k =20)
           +Rain+LightRain+Snow+i_surface+
             +s(ydstogo,bs = 'cr')
           , method = "REML",family=("binomial"(link="logit")), data=run_data2)
summary(Gibbs)

run_data2$EXPsucc<-predict(Gibbs,run_data2,type="response")

Gibbs<-bam(success~s(yardline_100, bs = 'cr', k =20)
           +Rain+LightRain+Snow+i_surface+
             +s(ydstogo,bs = 'cr')
           , method = "REML",family=("binomial"(link="logit")), data=run_data3)
summary(Gibbs)

run_data3$EXPsucc<-predict(Gibbs,run_data3,type="response")



#
## Calibration
#
run_data_all<-rbind(run_data1,run_data2,run_data3)

calibration_plot <- run_data_all %>% 
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
    x = "Predicted Run Success", 
    y = "Observed Run Success"
  ) +
  coord_cartesian(
    xlim = c(0,1), ylim = c(0, 1)
  )
calibration_plot

#
## Create Table
#

require(dplyr)
Bins<- run_data_all %>%
  group_by(posteam,defteam,game_id) %>%
  summarize(EXPsucc=sum(EXPsucc),success=sum(success),RunCount=sum(count))

write.csv(Bins,'/Users/am/Desktop/NFLSets/GBGData/RunSucc.csv')

