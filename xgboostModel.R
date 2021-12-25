
# Title: "XGBoost Model"
# Author: "Vaishak Naik"


## Imported Library

require(xgboost)
require(Matrix)
library(caTools)
library(stringr)
library(car)
library(faraway)
library(ggplot2)
library(corrgram)
library(stringr)
library(leaps)
library(corrgram)
library(gridExtra)
library(corrplot)
library(rpart); 
library(rpart.plot)
library(ranger)
library(caret)



## Importing Data Wrangling And Variable Selection File

source("datawranglingVariableSelection.R")


## Splitting the data into training and test set

set.seed(617)
airbnb <- data[,-(1:2),drop=FALSE]
airbnb = na.omit(airbnb)
split <- sample.split(data, SplitRatio = .8)
train <- data[split,]
test <- data[!split,]
trainMatrix <- train[,-(1:2),drop=FALSE]
testMatrix <- test[,-(1:2),drop=FALSE]



## XGBoost Model Building

xgboost = xgboost(data=as.matrix(trainMatrix), 
                  label = train$price,
                  nrounds=1000,
                  verbose = 0,
                  subsample = 0.5,
                  eta = 0.1,
                  early_stopping_rounds = 150
)
xgboost$best_iteration


## Model Analysis

pred_test <- predict(xgboost, newdata=as.matrix(testMatrix))
length(pred_test)
nrow(test)
pred_data_frame <- data.frame(price=pred_test, actual=test$price)
RMSE(pred_data_frame$price, test$price)
importance_matrix <- xgb.importance(model = xgboost)
xgb.plot.importance(importance_matrix = importance_matrix)


## Scoring Data

scoringDataModified <- scoringData[,-(1:2),drop=FALSE]
pred =  predict(xgboost, newdata=as.matrix(scoringDataModified))
length(pred)
nrow(scoringData)
submissionFile = data.frame(id=scoringData$id, price=pred)
write.csv(submissionFile, 'xgboostScoring.csv', row.names=FALSE)


