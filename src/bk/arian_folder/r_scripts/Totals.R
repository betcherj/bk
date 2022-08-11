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


TotalData<- fread('/Users/am/Desktop/NFLSets/TotalData.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
TotalData<-TotalData[,-1]

TotalData$ActualSpread<-ifelse(TotalData$season==2021 & TotalData$week>20 & TotalData$Home==1,NA,TotalData$ActualSpread)
PredictData<-subset(TotalData,TotalData$season==2021 & TotalData$week>20 & TotalData$Home==1)
RegData<-subset(TotalData,!is.na(TotalData$ActualSpread)& TotalData$Home==1)
RegData<-subset(RegData,RegData$week>8 & RegData$week !=16 & RegData$week !=17)
RegData$Trash<-ifelse(RegData$week==18 & RegData$season==2021,1,0)
RegData<-subset(RegData,RegData$Trash==0)
RegData<-RegData[,-200]
PredictData<-rbind(PredictData,RegData)
which(colnames(RegData)=="ActualTotal")

TotalData$PrimeTime <- ordered(TotalData$PrimeTime, levels = c("Regular", "PrimeTime"))
TotalData$weekday <- ordered(TotalData$weekday, levels = c("Sunday", "Monday","Thursday"))

#
##
### Linear Modeling
##
#

Corr<-RegData[,c(151,24,37:147,153:192)]
Corr<-dplyr::select_if(Corr, is.numeric)
Corr <- round(abs(cor(Corr)),3)
Corr<-as.data.frame(Corr)
trash<-subset(RegData,(is.na(RegData$ActualTotal)))

#
##  split into test and train
#

RegData<-as.data.frame(RegData)
PredictData<-as.data.frame(PredictData)
set.seed(1137)
dt = sort(sample(nrow(RegData), nrow(RegData)*0.75))
trainingData<-RegData[dt,]
testData<-RegData[-dt,]
trainingDataNew<-trainingData
testDataNew<-testData

cols = c(24,37:151,157:196)

pre_proc_val <- preProcess(trainingData[,cols], method = c("center", "scale"))
trainingData[,cols] = predict(pre_proc_val, trainingData[,cols])
testData[,cols] = predict(pre_proc_val,testData[,cols])

###
pre_proc_val <- preProcess(RegData[,cols], method = c("center", "scale"))
RegData[,cols] = predict(pre_proc_val,RegData[,cols])
PredictData[,cols] = predict(pre_proc_val,PredictData[,cols])

cols_reg = c(155,8,35,17,18,24,37:151,157:196,199)

dummies <- dummyVars(ActualTotal ~ ., data = trainingData[,cols_reg],fullRank=TRUE)
train_dummies = predict(dummies, newdata = trainingData[,cols_reg])
test_dummies = predict(dummies, newdata = testData[,cols_reg])

###
dummies <- dummyVars(ActualTotal ~ ., data = RegData[,cols_reg],fullRank=TRUE)
real_dummies = predict(dummies, newdata = RegData[,cols_reg])
pred_dummies = predict(dummies, newdata = PredictData[,cols_reg])

library(glmnet)

x = as.matrix(train_dummies)
y_train = trainingData$ActualTotal

x_test = as.matrix(test_dummies)
y_test = testData$ActualTotal

x_true = as.matrix(real_dummies)
y_true = RegData$ActualTotal

x_pred = as.matrix(pred_dummies)

#
## Ridge
#

lambdas <- round(10^seq(2, -2, by = -.01),4)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)
summary(ridge_reg)

cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

ridge_model <- glmnet(x, y_train, alpha = 0, lambda = optimal_lambda, standardize = TRUE)
coef(ridge_model)
#
## Lasso
#

lambdas <- round(10^seq(2, -2, by = -.01),4)
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

lambda_best <- lasso_reg$lambda.min 
lambda_best

lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)
coef(lasso_model)


# Elastic Net

train_cont <- trainControl(method = "repeatedcv",
                           number = 2,
                           repeats = 2,
                           search = "random",
                           verboseIter = TRUE,
                           savePredictions = "all")

# Train the model
elastic_reg <- train(ActualTotal ~ .,
                     data=trainingData[,cols_reg],
                     method = "glmnet",
                     tuneLength = 5,
                     trControl = train_cont,
                     metric="RMSE")


# Best tuning parameter
bestTune<-elastic_reg$bestTune

elastic_model <- glmnet(x, y_train, alpha = bestTune[,1], lambda = bestTune[,2], standardize = TRUE)
coef(elastic_model)


#
## Evaluations
#

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

#
## Evaluation
#

RegData$predictions <- (predict(ridge_model, s = optimal_lambda, newx = x_true)+predict(lasso_model, s = lambda_best, newx = x_true)+predict(elastic_model,newx=x_true))/3


# Ridge
predictions_trainingData <- predict(ridge_model, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_trainingData, trainingData)
RidgePredictions <- predict(ridge_model, s = optimal_lambda, newx = x_test)
eval_results(y_test, RidgePredictions, testData)

# Lasso

predictions_trainingData <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_trainingData, trainingData)

LassoPredictions <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, LassoPredictions, testData)


# Elastic 
predictions_trainingData <- predict(elastic_model,newx=x)
eval_results(y_train, predictions_trainingData, trainingData)

ElasticPredictions <- predict(elastic_model,newx=x_test)
eval_results(y_test, ElasticPredictions, testData)


### True
eval_results(RegData$ActualTotal,RegData$total_line,RegData)
MAE(RegData$ActualTotal,RegData$total_line)

###
testData$ElasticPredictions<-round(ElasticPredictions,3)
testData$LassoPredictions<-round(LassoPredictions,3)
testData$RidgePredictions<-round(RidgePredictions,3)
testData$My_Total<-(round(RidgePredictions,3)+round(LassoPredictions,3)+round(ElasticPredictions,3))/3

### All True
eval_results(y_test,testData$My_Total,testData)
MAE(y_test,testData$My_Total)

###
testData$ElasticPick<-ifelse(testData$ElasticPredictions<testData$TotalOpen,1,0)
testData$RidgePick<-ifelse(testData$RidgePredictions<testData$TotalOpen,1,0)
testData$LassoPick<-ifelse(testData$LassoPredictions<testData$TotalOpen,1,0)

#
## Regression to market
#

testDataNew<-subset(testData,abs(testData$My_Total-testData$total_line)<quantile(abs(testData$My_Total-testData$total_line),0.99))

# testDataNew$ElasticPredictions<-testDataNew$ElasticPredictions*(1-(abs(testDataNew$ElasticPredictions-testDataNew$total_line))/max(abs(testDataNew$ElasticPredictions-testDataNew$total_line)))+
#   testDataNew$total_line*((abs(testDataNew$ElasticPredictions-testDataNew$total_line))/max(abs(testDataNew$ElasticPredictions-testDataNew$total_line)))
# testDataNew$RidgePredictions<-testDataNew$RidgePredictions*(1-(abs(testDataNew$RidgePredictions-testDataNew$total_line))/max(abs(testDataNew$RidgePredictions-testDataNew$total_line)))+
#   testDataNew$total_line*((abs(testDataNew$RidgePredictions-testDataNew$total_line))/max(abs(testDataNew$RidgePredictions-testDataNew$total_line)))
# testDataNew$LassoPredictions<-testDataNew$LassoPredictions*(1-(abs(testDataNew$LassoPredictions-testDataNew$total_line))/max(abs(testDataNew$LassoPredictions-testDataNew$total_line)))+
#   testDataNew$total_line*((abs(testDataNew$LassoPredictions-testDataNew$total_line))/max(abs(testDataNew$LassoPredictions-testDataNew$total_line)))


testDataNew$My_Total2<-(round(testDataNew$LassoPredictions,3)+round(testDataNew$RidgePredictions,3)+round(testDataNew$ElasticPredictions,3))/3

library(mgcv)
Mdl<-bam(total_line~s(My_Total2, bs = 'cr', k =10)
         , method = "REML",family=("quasipoisson"(link="log")), data=testDataNew)
summary(Mdl)

testDataNew$My_Total<-round(predict(Mdl,testDataNew,type="response"),2)
ggplot(testDataNew, aes(My_Total2,My_Total)) +
  geom_point()+
  scale_x_continuous(breaks = round(seq(min(testDataNew$My_Total2), max(testDataNew$My_Total2), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(testDataNew$My_Total), max(testDataNew$My_Total), by = 1),1))

Weights<- data.frame(matrix(ncol = 3, nrow = nrow(testDataNew)))
x <- c("Weight", "MSE", "MAE")
colnames(Weights) <- x

for(i in 1:nrow(testDataNew)){
  Weights[i,1]<-i/nrow(testDataNew)
  preds<-testDataNew$My_Total*(i/nrow(testDataNew))+testDataNew$total_line*(1-(i/nrow(testDataNew)))
  Weights[i,2]<-sqrt(sum((preds-testDataNew$ActualTotal)^2)/nrow(testDataNew))
  Weights[i,3]<-mean(abs(preds-testDataNew$ActualTotal))
  i=i+1
}

Weights<-Weights[complete.cases(Weights),]

plot(Weights$Weight,Weights$MSE)
plot(Weights$Weight,Weights$MAE)

for(i in 1:nrow(testDataNew)){
  Weights[i,1]<-i/nrow(testDataNew)
  preds<-testDataNew$My_Total*(1-(i/nrow(testDataNew)*abs(testDataNew$My_Total-testDataNew$total_line)^1)/max(abs(testDataNew$My_Total-testDataNew$total_line)^1))+
    testDataNew$total_line*((i/nrow(testDataNew)*abs(testDataNew$My_Total-testDataNew$total_line))/max(abs(testDataNew$My_Total-testDataNew$total_line)^1))
  Weights[i,3]<-mean(abs(preds-testDataNew$ActualTotal))
  i=i+1
}

Weights<-Weights[complete.cases(Weights),]
plot(Weights$Weight,Weights$MAE)

#
## Hit Rates
#
testDataNew$My_Total<-testDataNew$My_Total*0.5+testDataNew$total_line*0.5

testDataNew2<-testDataNew
testDataNew2$Cover<-ifelse(testDataNew2$ActualTotal<testDataNew2$TotalOpen,1,0)
testDataNew2$Cover<-ifelse(testDataNew2$ActualTotal==testDataNew2$TotalOpen,NA,testDataNew2$Cover)
ftable(testDataNew2$Cover)

testDataNew2$ElasticHit<-ifelse(testDataNew2$ElasticPredictions<testDataNew2$TotalOpen & testDataNew2$Cover==1,1,0)
testDataNew2$ElasticHit<-ifelse(testDataNew2$ElasticPredictions>testDataNew2$TotalOpen & testDataNew2$Cover==0,1,testDataNew2$ElasticHit)
testDataNew2$ElasticHit<-ifelse(testDataNew2$ElasticPredictions==testDataNew2$TotalOpen,NA,testDataNew2$ElasticHit)
testDataNew2$ElasticHit<-ifelse(is.na(testDataNew2$Cover),NA,testDataNew2$ElasticHit)
ftable(testDataNew2$ElasticHit)

testDataNew2$LassoHit<-ifelse(testDataNew2$LassoPredictions<testDataNew2$TotalOpen & testDataNew2$Cover==1,1,0)
testDataNew2$LassoHit<-ifelse(testDataNew2$LassoPredictions>testDataNew2$TotalOpen & testDataNew2$Cover==0,1,testDataNew2$LassoHit)
testDataNew2$LassoHit<-ifelse(testDataNew2$LassoPredictions==testDataNew2$TotalOpen,NA,testDataNew2$LassoHit)
testDataNew2$LassoHit<-ifelse(is.na(testDataNew2$Cover),NA,testDataNew2$LassoHit)
ftable(testDataNew2$LassoHit)

testDataNew2$RidgeHit<-ifelse(testDataNew2$RidgePredictions<testDataNew2$TotalOpen & testDataNew2$Cover==1,1,0)
testDataNew2$RidgeHit<-ifelse(testDataNew2$RidgePredictions>testDataNew2$TotalOpen & testDataNew2$Cover==0,1,testDataNew2$RidgeHit)
testDataNew2$RidgeHit<-ifelse(testDataNew2$RidgePredictions==testDataNew2$TotalOpen,NA,testDataNew2$RidgeHit)
testDataNew2$RidgeHit<-ifelse(is.na(testDataNew2$Cover),NA,testDataNew2$RidgeHit)
ftable(testDataNew2$RidgeHit)

testDataNew2$AllHit<-ifelse(testDataNew2$My_Total<testDataNew2$TotalOpen & testDataNew2$Cover==1,1,0)
testDataNew2$AllHit<-ifelse(testDataNew2$My_Total>testDataNew2$TotalOpen & testDataNew2$Cover==0,1,testDataNew2$AllHit)
testDataNew2$AllHit<-ifelse(testDataNew2$My_Total==testDataNew2$TotalOpen,NA,testDataNew2$AllHit)
testDataNew2$AllHit<-ifelse(is.na(testDataNew2$Cover),NA,testDataNew2$AllHit)
ftable(testDataNew2$AllHit)

#
## CLV
#

testDataNew2$My_Total_diff<-testDataNew2$TotalOpen-testDataNew2$My_Total
testDataNew2$ABS_My_Total_diff<-abs(testDataNew2$My_Total_diff)

library(Hmisc)
testDataNew2$Diff_Bin<-as.numeric(cut2((testDataNew2$ABS_My_Total_diff),g=5))
detach("package:Hmisc", unload = TRUE)

TestTotals<-subset(testDataNew2,!is.na(testDataNew2$AllHit))
TestTotals$count<-1

TestTotals$CLV<-ifelse(TestTotals$My_Total_diff>0 & (TestTotals$TotalOpen-TestTotals$TotalClose)<0
                        ,(TestTotals$TotalMove),0)
TestTotals$CLV<-ifelse(TestTotals$My_Total_diff<0 & (TestTotals$TotalOpen-TestTotals$TotalClose)<0
                        ,abs(TestTotals$TotalMove),TestTotals$CLV)
TestTotals$CLV<-ifelse(TestTotals$My_Total_diff<0 & (TestTotals$TotalOpen-TestTotals$TotalClose)>0
                        ,-(TestTotals$TotalMove),TestTotals$CLV)
TestTotals$CLV<-ifelse(TestTotals$My_Total_diff>0 & (TestTotals$TotalOpen-TestTotals$TotalClose)>0
                        ,(TestTotals$TotalMove),TestTotals$CLV)

Bins<- TestTotals %>%
  group_by(Diff_Bin) %>%
  summarize(AllHitRate=sum(AllHit)/sum(count),count=sum(count),MY_mse=MSE(ActualTotal,My_Total),Veg_mse=MSE(ActualTotal,TotalOpen)
            ,MY_mae=MAE(ActualTotal,My_Total),Veg_mae=MAE(ActualTotal,TotalOpen),ABS_My_Total_diff=sum(ABS_My_Total_diff)/sum(count),
            CLV=sum(CLV)/sum(count))
Bins$MSEDiff<-round((Bins$Veg_mse-Bins$MY_mse)/(Bins$Veg_mse),6)
Bins$MAEDiff<-round((Bins$Veg_mae-Bins$MY_mae)/(Bins$Veg_mae),6)
ggplot(Bins, aes(x = Diff_Bin, y = AllHitRate)) +
  geom_point()

TestTotals$NewSeasons<-ifelse(TestTotals$season>2007,1,0)
TestTotals$NewWeek<-ifelse(floor(TestTotals$week<12),1,2)

library(Hmisc)
TestTotals$Diff_Bin<-as.numeric(cut2((TestTotals$ABS_My_Total_diff),g=5))
detach("package:Hmisc", unload = TRUE)

Bins<- TestTotals %>%
  group_by(Diff_Bin,NewSeasons) %>%
  summarize(AllHitRate=sum(AllHit)/sum(count),count=sum(count),MY_mse=MSE(ActualTotal,My_Total),Veg_mse=MSE(ActualTotal,TotalOpen)
            ,MY_mae=MAE(ActualTotal,My_Total),Veg_mae=MAE(ActualTotal,TotalOpen),ABS_My_Total_diff=sum(ABS_My_Total_diff)/sum(count),
            CLV=sum(CLV)/sum(count))
Bins$MSEDiff<-round((Bins$Veg_mse-Bins$MY_mse)/(Bins$Veg_mse),6)
Bins$MAEDiff<-round((Bins$Veg_mae-Bins$MY_mae)/(Bins$Veg_mae),6)
ggplot(Bins, aes(x = Diff_Bin, y = AllHitRate,colour = factor(NewSeasons))) +
  geom_point()


#
##
### Full Model
##
#

elastic_model2 <- glmnet(x_true, y_true,  alpha = bestTune[,1], lambda = bestTune[,2], standardize = TRUE)

ridge_model2 <- glmnet(x_true, y_true, alpha = 0, lambda = optimal_lambda, standardize = TRUE)

lasso_model2 <- glmnet(x_true, y_true, alpha = 1, lambda = lambda_best, standardize = TRUE)


ElasticPredictions <- predict(elastic_model2, x_pred)
LassoPredictions <- predict(lasso_model2, s = lambda_best, newx = x_pred)
RidgePredictions <- predict(ridge_model2, s = optimal_lambda, newx = x_pred)
PredictData$ElasticPredictions<-ElasticPredictions
PredictData$LassoPredictions<-LassoPredictions
PredictData$RidgePredictions<-RidgePredictions
PredictData$My_Total2<-(PredictData$ElasticPredictions+PredictData$LassoPredictions+PredictData$RidgePredictions)/3

# Market Weighting

PredictData2<-subset(PredictData,abs(PredictData$My_Total-PredictData$total_line)<quantile(abs(PredictData$My_Total-PredictData$total_line),0.99))

library(mgcv)
Mdl<-bam(total_line~s(My_Total2, bs = 'cr', k =10)
         , method = "REML",family=("quasipoisson"(link="log")), data=PredictData2)
summary(Mdl)

PredictData2$My_Total<-round(predict(Mdl,PredictData2,type="response"),2)

ggplot(PredictData2, aes(My_Total2,My_Total)) +
  geom_point()+
  scale_x_continuous(breaks = round(seq(min(PredictData2$My_Total2), max(PredictData2$My_Total2), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(PredictData2$My_Total), max(PredictData2$My_Total), by = 1),1))

PredictData2$My_Total<-PredictData2$My_Total*0.5+PredictData2$total_line*0.5

PredictData2$ElasticPredictions<-PredictData2$ElasticPredictions*(1-(abs(PredictData2$ElasticPredictions-PredictData2$total_line))/max(abs(PredictData2$ElasticPredictions-PredictData2$total_line)))+
  PredictData2$total_line*((abs(PredictData2$ElasticPredictions-PredictData2$total_line))/max(abs(PredictData2$ElasticPredictions-PredictData2$total_line)))
PredictData2$RidgePredictions<-PredictData2$RidgePredictions*(1-(abs(PredictData2$RidgePredictions-PredictData2$total_line))/max(abs(PredictData2$RidgePredictions-PredictData2$total_line)))+
  PredictData2$total_line*((abs(PredictData2$RidgePredictions-PredictData2$total_line))/max(abs(PredictData2$RidgePredictions-PredictData2$total_line)))
PredictData2$LassoPredictions<-PredictData2$LassoPredictions*(1-(abs(PredictData2$LassoPredictions-PredictData2$total_line))/max(abs(PredictData2$LassoPredictions-PredictData2$total_line)))+
  PredictData2$total_line*((abs(PredictData2$LassoPredictions-PredictData2$total_line))/max(abs(PredictData2$LassoPredictions-PredictData2$total_line)))

PredictData2$My_Total_reg<-(PredictData2$ElasticPredictions+PredictData2$LassoPredictions+PredictData2$RidgePredictions)/3

PredictData2$Cover<-ifelse(PredictData2$ActualTotal<PredictData2$TotalOpen,1,0)
PredictData2$Cover<-ifelse(PredictData2$ActualTotal==PredictData2$TotalOpen,NA,PredictData2$Cover)
ftable(PredictData2$Cover)

PredictData2$AllHit<-ifelse(PredictData2$My_Total<PredictData2$TotalOpen & PredictData2$Cover==1,1,0)
PredictData2$AllHit<-ifelse(PredictData2$My_Total>PredictData2$TotalOpen & PredictData2$Cover==0,1,PredictData2$AllHit)
PredictData2$AllHit<-ifelse(PredictData2$My_Total==PredictData2$TotalOpen,NA,PredictData2$AllHit)
PredictData2$AllHit<-ifelse(is.na(PredictData2$Cover),NA,PredictData2$AllHit)
ftable(PredictData2$AllHit)

PredictData2$My_Total_diff<-PredictData2$TotalOpen-PredictData2$My_Total
PredictData2$ABS_My_Total_diff<-abs(PredictData2$My_Total_diff)

PredictData2$My_Total_diff_reg<-PredictData2$TotalOpen-PredictData2$My_Total_reg
PredictData2$ABS_My_Total_diff_reg<-abs(PredictData2$My_Total_diff_reg)

Total_Data<-subset(PredictData2,PredictData2$season==2021&PredictData2$week>19)
keeps <- c("home_team","away_team","week","Pick","total_line","My_Total","My_Total_reg","Hit","Cover","My_Total_diff","My_Total_diff_reg","ABS_My_Total_diff","ABS_My_Total_diff_reg")
TotalPredicts<-Total_Data[ , (names(Total_Data) %in% keeps)]
