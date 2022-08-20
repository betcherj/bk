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
ModelData$season2<-ModelData$season

#
##
### Linear Modeling
##
#

ModelData$ActualSpread<-ifelse(ModelData$season==2021 &ModelData$week>17,NA,ModelData$ActualSpread)
PredictData<-subset(ModelData,ModelData$season==2021 &ModelData$week==18)
RegData<-subset(ModelData,!is.na(ModelData$ActualSpread))
RegData<-subset(RegData,RegData$week>10 & RegData$week!=16 & RegData$week!=17)
RegData$Trash<-ifelse(RegData$week==18 & RegData$season==2021,1,0)
RegData<-subset(RegData,RegData$Trash==0)
RegData<-RegData[,-200]
PredictData<-rbind(PredictData,RegData)
which(colnames(RegData)=="Win")

#
##  split into test and train
#

set.seed(1153)
RegData<-as.data.frame(RegData)
PredictData<-as.data.frame(PredictData)
dt = sort(sample(nrow(RegData), nrow(RegData)*0.99))
trainingData<-RegData[dt,]
testData<-RegData[-dt,]
trainingDataNew<-trainingData
testDataNew<-testData


Corr<-RegData[,c(156,46:57,70:97,124:137,157:166,177:186,199)]
Corr<-dplyr::select_if(Corr, is.numeric)
Corr <- round(abs(cor(Corr)),3)
Corr<-as.data.frame(Corr)

cols = c(46:57,70:97,124:137,157:166,177:186)
pre_proc_val <- preProcess(trainingDataNew[,cols], method = c("center", "scale"))
trainingDataNew[,cols] = predict(pre_proc_val, trainingDataNew[,cols])
testDataNew[,cols] = predict(pre_proc_val,testDataNew[,cols])

###
pre_proc_val <- preProcess(RegData[,cols], method = c("center", "scale"))
RegData[,cols] = predict(pre_proc_val,RegData[,cols])
PredictData[,cols] = predict(pre_proc_val,PredictData[,cols])

cols_log = c(156,46:57,70:97,124:137,157:166,177:186)

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
                     tuneLength = 10, metric="logLoss",
                     trControl = train_cont, family = 'binomial')

# Best tuning parameter
# elastic_reg$results
# elastic_reg$bestTune
bestTune<-elastic_reg$bestTune

elastic_model <- glmnet(x, y_train, alpha = bestTune[,1], lambda = bestTune[,2], standardize = TRUE, family = 'binomial')
coef(elastic_model)

trainingDataNew$Win<-ifelse(trainingDataNew$Win=="Win",1,0)
testDataNew$Win<-as.numeric(testDataNew$Win)

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

PredictData$ElasticPredictions<-ElasticPredictions
PredictData$LassoPredictions<-LassoPredictions
PredictData$RidgePredictions<-RidgePredictions
PredictData$AllPredictions<-(PredictData$ElasticPredictions+PredictData$LassoPredictions+PredictData$RidgePredictions)/3

Gibbs<-glm(Win~PredHomeAdv+DRest+early_visitor+TrueHome,data=RegData, family=binomial(link = "logit"))
summary(Gibbs)
PredictData$Home_Adv<-predict(Gibbs,PredictData, type="response")
hist(PredictData$Home_Adv)

PredictData2<-subset(PredictData,PredictData$ActualSpread>-32 & PredictData$ActualSpread<28)
Mdl<-(lm(PredictData2$ActualSpread~PredictData2$AllPredictions))
coef(Mdl)

PredictData2<-subset(PredictData,PredictData$week==18)

keeps <- c("home_team","season","AllPredictions","away_team")
Predicts<-PredictData2[ , (names(PredictData2) %in% keeps)]

Predicts$PredSpread<-Predicts$AllPredictions*coef(Mdl)[2]+coef(Mdl)[1]

PowerRanks<-subset(Predicts,Predicts$season==2021)


