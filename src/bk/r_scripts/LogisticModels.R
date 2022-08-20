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

#
##
###
##
#
#rm(list = ls())


ModelData<- fread('/Users/am/Desktop/NFLSets/ModelData.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
ModelData<-ModelData[,-1]
ModelData<-as.data.frame(ModelData)

#
##
### Linear Modeling
##
#

ModelData$ActualSpread<-ifelse(ModelData$season==2021 & ModelData$week>20 & ModelData$Home==1,NA,ModelData$ActualSpread)
PredictData<-subset(ModelData,ModelData$season==2021 & ModelData$week>20 & ModelData$Home==1)
RegData<-subset(ModelData,!is.na(ModelData$ActualSpread)& ModelData$Home==1)
RegData<-subset(RegData,RegData$week>8 & RegData$week !=16 & RegData$week !=17)
RegData$Trash<-ifelse(RegData$week==18 & RegData$season==2021,1,0)
RegData<-subset(RegData,RegData$Trash==0)
RegData<-RegData[,-199]
PredictData<-rbind(PredictData,RegData)
which(colnames(RegData)=="Win")

#
##  split into test and train
#

set.seed(1124)
RegData<-as.data.frame(RegData)
PredictData<-as.data.frame(PredictData)
dt = sort(sample(nrow(RegData), nrow(RegData)*0.75))
trainingData<-RegData[dt,]
testData<-RegData[-dt,]
trainingDataNew<-trainingData
testDataNew<-testData

Corr<-RegData[,c(150,22,23,24,36:151,157:196)]
Corr<-dplyr::select_if(Corr, is.numeric)
Corr <- round(abs(cor(Corr)),3)
Corr<-as.data.frame(Corr)

cols = c(43:151,157:196)
pre_proc_val <- preProcess(trainingDataNew[,cols], method = c("center", "scale"))
trainingDataNew[,cols] = predict(pre_proc_val, trainingDataNew[,cols])
testDataNew[,cols] = predict(pre_proc_val,testDataNew[,cols])

###
pre_proc_val <- preProcess(RegData[,cols], method = c("center", "scale"))
RegData[,cols] = predict(pre_proc_val,RegData[,cols])
PredictData[,cols] = predict(pre_proc_val,PredictData[,cols])

cols_log = c(156,22,23,24,36:151,157:196)

x <- model.matrix(Win~., trainingDataNew[,cols_log])[,-1]
y_train = trainingDataNew$Win

x_test <- model.matrix(Win~., testDataNew[,cols_log])[,-1]
y_test = testDataNew$Win

x_true <- model.matrix(Win~., RegData[,cols_log])[,-1]
y_true = RegData$Win

PredictData$Win<-0
x_pred <- model.matrix(Win~.,PredictData[,cols_log])[,-1]

library(glmnet)

#
## Ridge
#

lambdas <- round(10^seq(2, -2, by = -.01),4)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'binomial', lambda = lambdas)
summary(ridge_reg)

cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas, family = 'binomial')
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

ridge_model <- glmnet(x, y_train, alpha = 0, lambda = optimal_lambda, standardize = TRUE, family = 'binomial')
coef(ridge_model)

#
## Lasso
#

lambdas <- round(10^seq(2, -2, by = -.01),4)
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5, family = 'binomial')

lambda_best <- lasso_reg$lambda.min
lambda_best

lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE, family = 'binomial')
coef(lasso_model)

#
## Elastic Net
#
trainingDataNew$Win<-ifelse(trainingDataNew$Win==1,"Win","Loss")

train_cont <- trainControl(method = "repeatedcv",
                           number = 2,
                           repeats = 2,
                           search = "random",
                           verboseIter = TRUE,
                           savePredictions = "all", classProbs=TRUE, summaryFunction=mnLogLoss)

elastic_reg <- train(Win ~ .,
                     data=trainingDataNew[,cols_log],
                     method = "glmnet",
                     tuneLength = 5, metric="logLoss",
                     trControl = train_cont, family = 'binomial')


elastic_reg$results
elastic_reg$bestTune
bestTune<-elastic_reg$bestTune

elastic_model <- glmnet(x, y_train, alpha = bestTune[,1], lambda = bestTune[,2], standardize = TRUE, family = 'binomial')
coef(elastic_model)

trainingDataNew$Win<-ifelse(trainingDataNew$Win=="Win",1,0)
testDataNew$Win<-as.numeric(testDataNew$Win)

#
## Evaluation
#

trainingDataNew$Probs <- (predict(elastic_model, x,type="response")
          +predict(ridge_model, s = optimal_lambda, newx = x,type="response")
          +predict(lasso_model, s = lambda_best, newx = x,type="response"))/3


plot(trainingDataNew$Probs,trainingDataNew$ActualSpread)
abline(lm(trainingDataNew$ActualSpread~trainingDataNew$Probs))
Mdl<-(lm(trainingDataNew$ActualSpread~trainingDataNew$Probs))
coef(Mdl)


library(Hmisc)
trainingDataNew$ProbsBin<-as.numeric(cut2((trainingDataNew$Probs-0.5),g=20))
detach("package:Hmisc", unload = TRUE)

trainingDataNew$count<-1
Bins<- trainingDataNew %>%
  group_by((SpreadClose)) %>%
  summarize(AvProbs=median((Probs-0.5)+0.5),MeanProbs=sum((Probs-0.5)+0.5)/sum(count),AvWP=median((WPClose)),MeanWP=mean((WPClose)),count=sum(count))

# Ridge
predictions_trainingDataNew <- predict(ridge_model, s = optimal_lambda, newx = x,type="response")
LogLoss(predictions_trainingDataNew,trainingDataNew$Win)
RidgePredictions <- predict(ridge_model, s = optimal_lambda, newx = x_test,type="response")
LogLoss(RidgePredictions,testDataNew$Win)

# Lasso 

predictions_trainingDataNew <- predict(lasso_model, s = lambda_best, newx = x,type="response")
LogLoss(predictions_trainingDataNew,trainingDataNew$Win)
LassoPredictions <- predict(lasso_model, s = lambda_best, newx = x_test,type="response")
LogLoss(LassoPredictions,testDataNew$Win)

# Elastic Train
predictions_trainingDataNew <- predict(elastic_model, x,type="response")
LogLoss(predictions_trainingDataNew,trainingDataNew$Win)
ElasticPredictions <- predict(elastic_model, x_test,type="response")
LogLoss(ElasticPredictions,testDataNew$Win)

### True
LogLoss(testDataNew$WP,testDataNew$Win)

### All
testDataNew$Elastic_WP<-round(ElasticPredictions,3)
testDataNew$Lasso_WP<-round(LassoPredictions,3)
testDataNew$Ridge_WP<-round(RidgePredictions,3)
testDataNew$My_WP<-(round(RidgePredictions,3)+round(LassoPredictions,3)+round(ElasticPredictions,3))/3
testDataNew$WP<-round(testDataNew$WP,3)
LogLoss(testDataNew$My_WP,testDataNew$Win)

### Regression to market by WP

testData<-testDataNew
testData<-subset(testData,abs(testData$My_WP-testData$WP)<quantile(abs(testData$My_WP-testData$WP),0.99))

# testData$Elastic_WP<-testData$Elastic_WP*(1-(abs(testData$Elastic_WP-testData$WP)^1)/max(abs(testData$Elastic_WP-testData$WP)^1))+
#   testData$WP*((abs(testData$Elastic_WP-testData$WP))/max(abs(testData$Elastic_WP-testData$WP)))
# testData$Ridge_WP<-testData$Ridge_WP*(1-(abs(testData$Ridge_WP-testData$WP)^1)/max(abs(testData$Ridge_WP-testData$WP)^1))+
#   testData$WP*((abs(testData$Ridge_WP-testData$WP))/max(abs(testData$Ridge_WP-testData$WP)))
# testData$Lasso_WP<-testData$Lasso_WP*(1-(abs(testData$Lasso_WP-testData$WP)^1)/max(abs(testData$Lasso_WP-testData$WP)^1))+
#   testData$WP*((abs(testData$Lasso_WP-testData$WP))/max(abs(testData$Lasso_WP-testData$WP)^1))
# 
# testData$My_WP<-(round(testData$Ridge_WP,3)+round(testData$Lasso_WP,3)+round(testData$Elastic_WP,3))/3

Weights<- data.frame(matrix(ncol = 4, nrow = nrow(testData)))
x <- c("Weight", "MSE", "MAE","LogLoss")
colnames(Weights) <- x
max(abs(testData$My_WP-testData$WP))/nrow(testData)

for(i in 1:nrow(testData)){
  Weights[i,1]<-i/nrow(testData)
  preds<-testData$My_WP*(i/nrow(testData))+testData$WP*(1-(i/nrow(testData)))
  Weights[i,2]<-sqrt(sum((preds-testData$Win)^2)/nrow(testData))
  Weights[i,3]<-mean(abs(preds-testData$Win))
  Weights[i,4]<-(LogLoss(preds,testData$Win))
  i=i+1
}

Weights<-Weights[complete.cases(Weights),]
plot(Weights$Weight,Weights$LogLoss)
  
for(i in 1:nrow(testData)){
  Weights[i,1]<-i/nrow(testData)
  preds<-testData$My_WP*(1-(i/nrow(testData)*abs(testData$My_WP-testData$WP)^1)/max(abs(testData$My_WP-testData$WP)^1))+
  testData$WP*((i/nrow(testData)*abs(testData$My_WP-testData$WP))/max(abs(testData$My_WP-testData$WP)^1))
  Weights[i,4]<-(LogLoss(preds,testData$Win))
  i=i+1
}

Weights<-Weights[complete.cases(Weights),]
plot(Weights$Weight,Weights$LogLoss)

#
##
#

testData2<-testData


testData2$Cover<-ifelse(testData2$ActualSpread<testData2$SpreadOpen,1,0)
testData2$Cover<-ifelse(testData2$ActualSpread==testData2$SpreadOpen,NA,testData2$Cover)
ftable(testData2$Cover)

testData2$ElasticHit<-ifelse(testData2$Elastic_WP>testData2$WP & testData2$Cover==1,1,0)
testData2$ElasticHit<-ifelse(testData2$Elastic_WP<testData2$WP & testData2$Cover==0,1,testData2$ElasticHit)
testData2$ElasticHit<-ifelse(testData2$Elastic_WP==testData2$WP,NA,testData2$ElasticHit)
testData2$ElasticHit<-ifelse(is.na(testData2$Cover),NA,testData2$ElasticHit)
ftable(testData2$ElasticHit)

testData2$LassoHit<-ifelse(testData2$Lasso_WP>testData2$WP & testData2$Cover==1,1,0)
testData2$LassoHit<-ifelse(testData2$Lasso_WP<testData2$WP & testData2$Cover==0,1,testData2$LassoHit)
testData2$LassoHit<-ifelse(testData2$Lasso_WP==testData2$WP,1,testData2$LassoHit)
testData2$LassoHit<-ifelse(is.na(testData2$Cover),NA,testData2$LassoHit)
ftable(testData2$LassoHit)

testData2$RidgeHit<-ifelse(testData2$Ridge_WP>testData2$WP & testData2$Cover==1,1,0)
testData2$RidgeHit<-ifelse(testData2$Ridge_WP<testData2$WP & testData2$Cover==0,1,testData2$RidgeHit)
testData2$RidgeHit<-ifelse(testData2$Ridge_WP==testData2$WP,NA,testData2$RidgeHit)
testData2$RidgeHit<-ifelse(is.na(testData2$Cover),NA,testData2$RidgeHit)
ftable(testData2$RidgeHit)

testData2$AllHit<-ifelse(testData2$My_WP>testData2$WP & testData2$Cover==1,1,0)
testData2$AllHit<-ifelse(testData2$My_WP<testData2$WP & testData2$Cover==0,1,testData2$AllHit)
testData2$AllHit<-ifelse(testData2$My_WP==testData2$WP,NA,testData2$AllHit)
testData2$AllHit<-ifelse(is.na(testData2$Cover),NA,testData2$AllHit)
ftable(testData2$AllHit)

### Spread Conversions
testData2$My_WP<-testData2$My_WP*0.5+testData2$WP*0.5

library(mgcv)
Mdl<-bam(SpreadClose~s(WPClose, bs = 'cr', k =20)
         , method = "REML",family=("gaussian"(link="identity")), data=testData2)
summary(Mdl)

testData2$WPClose2<-testData2$WPClose
testData2$WPClose<-testData2$My_WP
testData2$My_Spread<-round(predict(Mdl,testData2,type="response"),2)

testData2$WPClose<-testData2$WPClose2
plot(testData2$SpreadClose,testData2$My_WP)
plot(testData2$My_WP,testData2$My_Spread)

#testData2$My_Spread<-testData2$My_WP*coef(Mdl)[2]+coef(Mdl)[1]
#testData2$My_Spread<-round(testData2$My_Spread,1)
testData2$My_Spread_diff<-testData2$SpreadOpen-testData2$My_Spread
testData2$My_WP_diff<-testData2$WP-testData2$My_WP

testData2$SpreadHit<-ifelse(testData2$My_Spread<testData2$SpreadOpen & testData2$Cover==1,1,0)
testData2$SpreadHit<-ifelse(testData2$My_Spread>testData2$SpreadOpen & testData2$Cover==0,1,testData2$SpreadHit)
testData2$SpreadHit<-ifelse(testData2$My_Spread==testData2$SpreadOpen,NA,testData2$SpreadHit)
testData2$SpreadHit<-ifelse(is.na(testData2$Cover),NA,testData2$SpreadHit)
ftable(testData2$SpreadHit)

### Calculating CLV 

testData2$Abs_My_Spread_diff<-abs(testData2$SpreadOpen-testData2$My_Spread)
testData2$Abs_My_WP_diff<-abs(testData2$WP-testData2$My_WP)

library(Hmisc)
testData2$Diff_Bin<-as.numeric(cut2((testData2$Abs_My_WP_diff),g=5))
detach("package:Hmisc", unload = TRUE)

TestSpreads<-subset(testData2,!is.na(testData2$SpreadHit))
TestSpreads$count<-1

TestSpreads$CLV<-ifelse(TestSpreads$My_Spread_diff>0 & (TestSpreads$SpreadOpen-TestSpreads$SpreadClose)<0
                        ,(TestSpreads$SpreadMove),0)
TestSpreads$CLV<-ifelse(TestSpreads$My_Spread_diff<0 & (TestSpreads$SpreadOpen-TestSpreads$SpreadClose)<0
                        ,abs(TestSpreads$SpreadMove),TestSpreads$CLV)
TestSpreads$CLV<-ifelse(TestSpreads$My_Spread_diff<0 & (TestSpreads$SpreadOpen-TestSpreads$SpreadClose)>0
                        ,-(TestSpreads$SpreadMove),TestSpreads$CLV)
TestSpreads$CLV<-ifelse(TestSpreads$My_Spread_diff>0 & (TestSpreads$SpreadOpen-TestSpreads$SpreadClose)>0
                        ,(TestSpreads$SpreadMove),TestSpreads$CLV)

TestSpreads$CLV_WP<-ifelse(TestSpreads$CLV>0 , abs(TestSpreads$WP-TestSpreads$WPClose),0)
TestSpreads$CLV_WP<-ifelse(TestSpreads$CLV<0 , -1* abs(TestSpreads$WP-TestSpreads$WPClose),TestSpreads$CLV_WP)

Bins<- TestSpreads %>%
  group_by(Diff_Bin) %>%
  summarize(SpreadHitRate=sum(SpreadHit)/sum(count),count=sum(count),MY_mse=MSE(ActualSpread,My_Spread),Veg_mse=MSE(ActualSpread,SpreadOpen)
            ,MY_mae=MAE(ActualSpread,My_Spread),Veg_mae=MAE(ActualSpread,SpreadOpen),Abs_My_Spread_diff=sum(Abs_My_Spread_diff)/sum(count)
            ,Abs_My_WP_diff=sum(Abs_My_WP_diff)/sum(count),CLV=sum(CLV)/sum(count),CLV_WP=sum(CLV_WP)/sum(count))
Bins$MSEDiff<-round((Bins$Veg_mse-Bins$MY_mse)/(Bins$Veg_mse),6)
Bins$MAEDiff<-round((Bins$Veg_mae-Bins$MY_mae)/(Bins$Veg_mae),6)
ggplot(Bins, aes(x = Diff_Bin, y = SpreadHitRate)) +
  geom_point()

#
## New vs old
#

library(Hmisc)
TestSpreads$Diff_Bin<-as.numeric(cut2((TestSpreads$Abs_My_WP_diff),g=5))
detach("package:Hmisc", unload = TRUE)

TestSpreads$NewSeasons<-ifelse(TestSpreads$season>2007,1,0)
TestSpreads$NewWeek<-ifelse(floor(TestSpreads$week<12),1,2)

Bins<- TestSpreads %>%
  group_by(Diff_Bin,NewSeasons) %>%
  summarize(SpreadHitRate=sum(SpreadHit)/sum(count),count=sum(count),MY_mse=MSE(ActualSpread,My_Spread),Veg_mse=MSE(ActualSpread,SpreadOpen)
            ,MY_mae=MAE(ActualSpread,My_Spread),Veg_mae=MAE(ActualSpread,SpreadOpen),Abs_My_Spread_diff=sum(Abs_My_Spread_diff)/sum(count)
            ,Abs_My_WP_diff=sum(Abs_My_WP_diff)/sum(count),CLV=sum(CLV)/sum(count),CLV_WP=sum(CLV_WP)/sum(count))
Bins$MSEDiff<-round((Bins$Veg_mse-Bins$MY_mse)/(Bins$Veg_mse),6)
Bins$MAEDiff<-round((Bins$Veg_mae-Bins$MY_mae)/(Bins$Veg_mae),6)
ggplot(Bins, aes(x = Diff_Bin, y = SpreadHitRate, colour = factor(NewSeasons))) +
  geom_point()


#
##
### Full Model
##
#

elastic_model2 <- glmnet(x_true, y_true, alpha = bestTune[,1], lambda = bestTune[,2], standardize = TRUE, family = 'binomial')
ridge_model2 <- glmnet(x_true, y_true, alpha = 0, lambda = optimal_lambda, standardize = TRUE, family = 'binomial')
lasso_model2 <- glmnet(x_true, y_true, alpha = 1, lambda = lambda_best, standardize = TRUE, family = 'binomial')

ElasticPredictions <- predict(elastic_model2, x_pred,type="response")
LassoPredictions <- predict(lasso_model2, s = lambda_best, newx = x_pred,type="response")
RidgePredictions <- predict(ridge_model2, s = optimal_lambda, newx = x_pred,type="response")

PredictData$Elastic_WP<-ElasticPredictions
PredictData$Lasso_WP<-LassoPredictions
PredictData$Ridge_WP<-RidgePredictions
PredictData$My_WP<-(PredictData$Elastic_WP+PredictData$Lasso_WP+PredictData$Ridge_WP)/3

# Market Weighting
PredictData2<-PredictData
#PredictData2<-subset(PredictData,abs(PredictData$My_WP-PredictData$WP)<quantile(abs(PredictData$My_WP-PredictData$WP),0.99))

PredictData2$Elastic_WP<-PredictData2$Elastic_WP*(1-(abs(PredictData2$Elastic_WP-PredictData2$WP))/max(abs(PredictData2$Elastic_WP-PredictData2$WP)))+
  PredictData2$WP*((abs(PredictData2$Elastic_WP-PredictData2$WP))/max(abs(PredictData2$Elastic_WP-PredictData2$WP)))

PredictData2$Lasso_WP<-PredictData2$Lasso_WP*(1-(abs(PredictData2$Lasso_WP-PredictData2$WP))/max(abs(PredictData2$Lasso_WP-PredictData2$WP)))+
  PredictData2$WP*((abs(PredictData2$Lasso_WP-PredictData2$WP))/max(abs(PredictData2$Lasso_WP-PredictData2$WP)))

PredictData2$Ridge_WP<-PredictData2$Ridge_WP*(1-(abs(PredictData2$Ridge_WP-PredictData2$WP))/max(abs(PredictData2$Ridge_WP-PredictData2$WP)))+
  PredictData2$WP*((abs(PredictData2$Ridge_WP-PredictData2$WP))/max(abs(PredictData2$Ridge_WP-PredictData2$WP)))

PredictData2$My_WP_reg<-(PredictData2$Elastic_WP+PredictData2$Lasso_WP+PredictData2$Ridge_WP)/3

PredictData2$Cover<-ifelse(PredictData2$ActualSpread<PredictData2$SpreadOpen,1,0)
PredictData2$Cover<-ifelse(PredictData2$ActualSpread==PredictData2$SpreadOpen,NA,PredictData2$Cover)
ftable(PredictData2$Cover)

PredictData2$Hit<-ifelse(PredictData2$My_WP>PredictData2$WP & PredictData2$Cover==1,1,0)
PredictData2$Hit<-ifelse(PredictData2$My_WP<PredictData2$WP & PredictData2$Cover==0,1,PredictData2$Hit)
PredictData2$Hit<-ifelse(PredictData2$My_WP==PredictData2$WP,NA,PredictData2$Hit)
PredictData2$Hit<-ifelse(is.na(PredictData2$Cover),NA,PredictData2$Hit)
ftable(PredictData2$Hit)

PredictData2$My_WP<-PredictData2$My_WP*0.5+PredictData2$WP*0.5

### Fix The WP-> Spread Conversion ###
library(mgcv)
Mdl<-bam(SpreadClose~s(WPClose, bs = 'cr', k =20)
         , method = "REML",family=("gaussian"(link="identity")), data=PredictData2)
summary(Mdl)

PredictData2$WPClose2<-PredictData2$WPClose
PredictData2$WPClose<-PredictData2$My_WP
PredictData2$PredSpread<-round(predict(Mdl,PredictData2,type="response"),2)
PredictData2<-as.data.frame(PredictData2)
PredictData2$WPClose<-PredictData2$WPClose2
PredictData2$My_Spread_reg<-PredictData2$PredSpread

### Full Check

PredictData2$My_Spread_diff<-PredictData2$SpreadOpen-PredictData2$PredSpread
PredictData2$My_WP_diff<-PredictData2$WP-PredictData2$My_WP

PredictData2$Abs_My_Spread_diff<-abs(PredictData2$SpreadOpen-PredictData2$PredSpread)
PredictData2$Abs_My_Spread_diff_reg<-abs(PredictData2$SpreadOpen-PredictData2$My_Spread_reg)
PredictData2$Abs_My_WP_diff<-abs(PredictData2$WP-PredictData2$My_WP)
PredictData2$Abs_My_WP_diff_reg<-abs(PredictData2$WP-PredictData2$My_WP_reg)


PredictData2$CLV<-ifelse(PredictData2$My_Spread_diff>0 & (PredictData2$SpreadOpen-PredictData2$SpreadClose)<0
                        ,(PredictData2$SpreadMove),0)
PredictData2$CLV<-ifelse(PredictData2$My_Spread_diff<0 & (PredictData2$SpreadOpen-PredictData2$SpreadClose)<0
                        ,abs(PredictData2$SpreadMove),PredictData2$CLV)
PredictData2$CLV<-ifelse(PredictData2$My_Spread_diff<0 & (PredictData2$SpreadOpen-PredictData2$SpreadClose)>0
                        ,-(PredictData2$SpreadMove),PredictData2$CLV)
PredictData2$CLV<-ifelse(PredictData2$My_Spread_diff>0 & (PredictData2$SpreadOpen-PredictData2$SpreadClose)>0
                        ,(PredictData2$SpreadMove),PredictData2$CLV)

PredictData2$CLV_WP<-ifelse(PredictData2$CLV>0 , abs(PredictData2$WP-PredictData2$WPClose),0)
PredictData2$CLV_WP<-ifelse(PredictData2$CLV<0 , -1* abs(PredictData2$WP-PredictData2$WPClose),PredictData2$CLV_WP)


PredictData2$SpreadHit<-ifelse(PredictData2$PredSpread<PredictData2$SpreadOpen & PredictData2$Cover==1,1,0)
PredictData2$SpreadHit<-ifelse(PredictData2$PredSpread>PredictData2$SpreadOpen & PredictData2$Cover==0,1,PredictData2$SpreadHit)
PredictData2$SpreadHit<-ifelse(PredictData2$PredSpread==PredictData2$SpreadOpen,NA,PredictData2$SpreadHit)
PredictData2$SpreadHit<-ifelse(is.na(PredictData2$Cover),NA,PredictData2$SpreadHit)
ftable(PredictData2$SpreadHit)

library(Hmisc)
PredictData2$Diff_Bin<-as.numeric(cut2((PredictData2$Abs_My_WP_diff),g=5))
detach("package:Hmisc", unload = TRUE)

PredictData2$NewSeasons<-ifelse(PredictData2$season>2007,1,0)
PredictData2$NewWeek<-ifelse(floor(PredictData2$week<12),1,2)
PredictData2$count<-1

PredictSpreads<-subset(PredictData2,!is.na(PredictData2$SpreadHit))
Bins<- PredictSpreads %>%
  group_by(Diff_Bin,NewSeasons) %>%
  summarize(SpreadHitRate=sum(SpreadHit)/sum(count),count=sum(count),MY_mse=MSE(ActualSpread,PredSpread),Veg_mse=MSE(ActualSpread,SpreadOpen)
            ,MY_mae=MAE(ActualSpread,PredSpread),Veg_mae=MAE(ActualSpread,SpreadOpen),Abs_My_Spread_diff=sum(Abs_My_Spread_diff)/sum(count)
            ,Abs_My_WP_diff=sum(Abs_My_WP_diff)/sum(count),CLV=sum(CLV)/sum(count),CLV_WP=sum(CLV_WP)/sum(count))
Bins$MSEDiff<-round((Bins$Veg_mse-Bins$MY_mse)/(Bins$Veg_mse),6)
Bins$MAEDiff<-round((Bins$Veg_mae-Bins$MY_mae)/(Bins$Veg_mae),6)
ggplot(Bins, aes(x = Diff_Bin, y = SpreadHitRate, colour = factor(NewSeasons))) +
  geom_point()


###

Bet_Data<-subset(PredictData2,PredictData2$season==2021&PredictData2$week>19)

keeps <- c("home_team","away_team","week","Pick","spread_line","WP","PredSpread","My_WP","My_WP_reg","My_Spread_reg",
           "Abs_My_WP_diff","Abs_My_WP_diff_reg","Abs_My_Spread_diff","Abs_My_Spread_diff_reg","Cover")
SpreadPredicts<-Bet_Data[ , (names(Bet_Data) %in% keeps)]


