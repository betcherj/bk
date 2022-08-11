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

run_data<-subset(pbp,pbp$play_type=="run"& !is.na(pbp$down))
run_data$count<-1

Bins<- run_data %>%
  group_by(yardline_100) %>%
  summarize(yards_gained=sum(yards_gained)/sum(count),count=sum(count))
plot(Bins$yardline_100,Bins$yards_gained)


#
##
### Modeling
##
#

#
## 1 Y Density Plot for Normality
#

run_data<-subset(run_data,run_data$wp>0.005&run_data$wp<0.995)
quantile(run_data$yards_gained,0.005)
run_data$yards_gained<-ifelse(run_data$yards_gained<=-6,-6,run_data$yards_gained)
run_data$yards_gained2<-log(run_data$yards_gained-min(run_data$yards_gained)+1)

library(e1071)
par(mfrow=c(1, 1))  # divide graph area in 2 columns

plot(density(run_data$yards_gained2), main="Density Plot: yds", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(run_data$yards_gained2), 2)))
polygon(density(run_data$yards_gained2), col="red")


#
## 5 Fit Model
#

library(mgcv)
Gibbs<-bam(yards_gained2~s(yardline_100, bs = 'cr', k =20)
           +Rain+LightRain+Snow+i_surface+
           +s(ydstogo, by=down,bs = 'cr')
           , method = "REML",family=("gaussian"(link="identity")), data=run_data)
summary(Gibbs)

run_data$xYPP<-exp(predict(Gibbs,run_data,type = "response"))-7

###

run_data2<-subset(run_data, run_data$yards_gained > quantile(run_data$yards_gained,0.05) & run_data$yards_gained < quantile(run_data$yards_gained, 0.95))
calibration_plot <- run_data2 %>% 
  mutate(
    pred_rnd = round(xYPP, 1)
  ) %>% 
  group_by(pred_rnd) %>% 
  summarize(
    mean_pred = mean(xYPP),
    mean_obs = mean(yards_gained),
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
    xlim = c(0,10), ylim = c(0, 10)
  )
calibration_plot


##
### Exports
##
#


#
## Game
#

library(dplyr)
Bins<- run_data %>%
  group_by(season,posteam,defteam,week) %>%
  summarize(run_xYPP=sum(xYPP),run_yards=sum(yards_gained),RunCount=sum(count))

write.csv(Bins,'/Users/am/Desktop/NFLSets/GBGData/RunYPP.csv')
