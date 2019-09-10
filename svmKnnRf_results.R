library(caret)
library(lattice)
library(ggplot2)
library(doParallel)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
set.seed(123)
trainingData <- read.csv("trainingData.csv")
View(trainingData)
Originaltrainingdata <- trainingData
#removing what I don't want currently to test
trainingData$LONGITUDE <- NULL
trainingData$LATITUDE <- NULL
trainingData$TIMESTAMP <- NULL
trainingData$PHONEID <- NULL
names(trainingData)
trainingData$RELATIVEPOSITION <- NULL
names(trainingData)
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)

{
  ## using only 2 users   
  User2and16 <- filter(trainingData, USERID == 16|USERID == 2)
  #MadeV2 cause I removed BuildingID and Floor
  User2and16V2 <- User2and16
  #Removing BuildingID and FLoor from my V2 to remove attributes
  User2and16V2$BUILDINGID <- NULL
  User2and16V2$FLOOR <- NULL
  #MadeV2 cause I removed BuildingID and Floor
  User2and16V2$BuildingandFLoorandSpace <- paste(User2and16$BUILDINGID,"-",User2and16$FLOOR,"-",User2and16$SPACEID)
  names(User2and16V2)
  inTraining <- createDataPartition(User2and16V2$USERID,p=.70, list = FALSE)
  training <- User2and16V2[inTraining,]
  testing <-  User2and16V2[-inTraining,]
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  svmFituser216 <- train(BuildingandFLoorandSpace~., data = training, method = "svmLinear",trControl=fitControl, tuneLength = 1)
  svmFituser216
  svmpreds <- predict(svmFituser216,testing)
  svmFituser216
  svmFituser216 <- train(BuildingandFLoorandSpace~., data = training, method = "svmLinear",trControl=fitControl, tuneLength = 1)
  User2and16V2$SPACEID <- NULL
  User2and16V2$BuildingandFloorandSpace <- NULL
  #MadeV2 cause I removed BuildingID and Floor
  User2and16V2$BuildingandFloorandSpace <- paste(User2and16$BUILDINGID,"-",User2and16$FLOOR,"-",User2and16$SPACEID)
  names(User2and16V2)
  User2and16V2$BuildingandFLoorandSpace <- NULL
  names(User2and16V2)
  inTraining <- createDataPartition(User2and16V2$USERID,p=.70, list = FALSE)
  training <- User2and16V2[inTraining,]
  testing <-  User2and16V2[-inTraining,]
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  svmFituser216 <- train(BuildingandFLoorandSpace~., data = training, method = "svmLinear",trControl=fitControl, tuneLength = 1)
  svmFituser216 <- train(BuildingandFloorandSpace~., data = training, method = "svmLinear",trControl=fitControl, tuneLength = 1)
  inTraining <- createDataPartition(User2and16V2$USERID,p=.80, list = FALSE)
  training <- User2and16V2[inTraining,]
  testing <-  User2and16V2[-inTraining,]
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  svmFituser216 <- train(BuildingandFloorandSpace~., data = training, method = "svmLinear",trControl=fitControl, tuneLength = 1)
  inTraining <- createDataPartition(User2and16V2$USERID,p=.50, list = FALSE)
  training <- User2and16V2[inTraining,]
  testing <-  User2and16V2[-inTraining,]
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  User2and16V2$BuildingandFloorandSpace
  #MadeV2 cause I removed BuildingID and Floor
  User2and16V2 <- User2and16
  names(User2and16V2)
  #MadeV2 cause I removed BuildingID and Floor
  User2and16V2$BuildingandFloorandSpace <- paste(User2and16$BUILDINGID,"-",User2and16$FLOOR)
  names(User2and16V2)
  #MadeV2 cause I removed BuildingID and Floor
  User2and16V2$Building <- paste(User2and16$BUILDINGID,"-",User2and16$FLOOR)
  User2and16V2$BuildingandFLoorandSpace <- NULL
  names(User2and16V2)
  #MadeV2 cause I removed BuildingID and Floor
  User2and16V2$BuildingandFloor <- paste(User2and16$BUILDINGID,"-",User2and16$FLOOR)
  User2and16V2$Building <- NULL
  User2and16V2$BUILDINGID <- NULL
  User2and16V2$FLOOR <- NULL
  names(User2and16V2)
  User2and16V2$BuildingandFloorandSpace <- NULL
  names(User2and16V2)
  inTraining <- createDataPartition(User2and16V2$USERID,p=.80, list = FALSE)
  training <- User2and16V2[inTraining,]
  testing <-  User2and16V2[-inTraining,]
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  svmFituser216 <- train(BuildingandFloor~., data = training, method = "svmLinear",trControl=fitControl, tuneLength = 1)
  svmFituser216
  svmpreds <- predict(svmFituser216,testing)
  postResample(svmpreds,as.factor(testing$BuildingandFLoor))
  postResample(svmpreds,as.factor(testing$BuildingandFloor))
  confusionMatrix(svmpreds,as.factor(testing$BuildingandFloor))
}

{
  User123456 <- filter(trainingData, USERID == 1|USERID == 2|USERID == 3|USERID == 4|USERID == 5|USERID == 6)
  names(User123456)
  User123456$BuildingandFloor <- paste(User123456$BUILDINGID,"-", User123456$FLOOR)
  names(User123456)
  User123456$BUILDINGID <- NULL
  User123456$FLOOR <- NULL
  names(User123456)
  inTraining_123456 <- createDataPartition(User123456$USERID,p=.70, list = FALSE)
  training123456 <- User123456[inTraining_123456,]
  testing123456 <-  User123456[-inTraining_123456,]
  fitControl_123456 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  svmFituser123456 <- train(BuildingandFloor~., data = training123456, method = "svmLinear",trControl=fitControl_123456, tuneLength = 1)
  svmpreds <- predict(svmFituser123456,testing)
  svmpreds
  postResample(svmpreds,as.factor(testing$BuildingandFloor))
  svmpreds_123456 <- predict(svmFituser123456,testing123456)
  postResample(svmpreds,as.factor(testing$BuildingandFloor))
  confusionMatrix(svmpreds,as.factor(testing$BuildingandFloor))
}

{
  #users 7 - 12
  User789101112 <- filter(trainingData, USERID == 7|USERID == 8|USERID == 9|USERID == 10|USERID == 11|USERID == 12)
  names(User789101112)
  User789101112$BuildingandFloor <- paste(User789101112$BUILDINGID,"-", User789101112$FLOOR)
  names(User123456)
  names(User789101112)
  names(User789101112)
  User789101112$BUILDINGID <- NULL
  User789101112$FLOOR <- NULL
  names(User789101112)
  inTraining7_12 <- createDataPartition(User789101112$USERID,p=.70, list = FALSE)
  training7_12 <- User789101112[inTraining7_12,]
  testing7_12 <-  User789101112[-inTraining7_12,]
  fitControl7_12 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  svmFituser7_12 <- train(BuildingandFloor~., data = training7_12, method = "svmLinear",trControl=fitControl7_12, tuneLength = 1)
  svmFituser7_12
  svmpreds7_12 <- predict(svmFituser7_12,testing7_12)
  postResample(svmpreds7_12,as.factor(testing7_12$BuildingandFloor))
  confusionMatrix(svmpreds7_12,as.factor(testing7_12$BuildingandFloor))
  
}
{
  ## Users 13 - 18
  User13_18 <- filter(trainingData, USERID == 13|USERID == 14|USERID == 15|USERID == 16|USERID == 17|USERID == 18)
  names(User13_18)
  User13_18$BuildingandFloor <- paste(User13_18$BUILDINGID,"-", User13_18$FLOOR)
  names(User789101112)
  names(User13_18)
  User13_18$BUILDINGID <- NULL
  User13_18$FLOOR <- NULL
  names(User789101112)
  inTraining13_18 <- createDataPartition(User13_18$USERID,p=.70, list = FALSE)
  training13_18 <- User13_18[inTraining13_18,]
  testing13_18 <-  User13_18[-inTraining13_18,]
  fitControl13_18 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  svmFituser13_18 <- train(BuildingandFloor~., data = training13_18, method = "svmLinear",trControl=fitControl13_18, tuneLength = 1)
  svmpreds13_18 <- predict(svmFituser13_18,testing13_18)
  postResample(svmpreds13_18,as.factor(testing13_18$BuildingandFloor))
  confusionMatrix(svmpreds13_18,as.factor(testing13_18$BuildingandFloor))
}
{
  #knn 1_6
  inTraining_123456 <- createDataPartition(User123456$USERID,p=.70, list = FALSE)
  training123456 <- User123456[inTraining_123456,]
  testing123456 <-  User123456[-inTraining_123456,]
  knnControl_123456 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  knnFituser123456 <- train(BuildingandFloor~., data = training123456, method = "knn",trControl=knnControl_123456, tuneLength = 1)
  knnpreds_123456 <- predict(knnFituser123456,testing123456)
  postResample(knnpreds_123456,as.factor(testing123456$BuildingandFloor))
  confusionMatrix(knnpreds_123456,as.factor(testing123456$BuildingandFloor))
}

{
  #knn 7_12
  inTraining7_12 <- createDataPartition(User789101112$USERID,p=.70, list = FALSE)
  training7_12 <- User789101112[inTraining7_12,]
  testing7_12 <-  User789101112[-inTraining7_12,]
  
  knnfitControl7_12 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  knnFituser7_12 <- train(BuildingandFloor~., data = training7_12, method = "knn",trControl=knnfitControl7_12, tuneLength = 1)
  knnpreds7_12 <- predict(knnFituser7_12,testing7_12)
  postResample(knnpreds7_12,as.factor(testing7_12$BuildingandFloor))
  confusionMatrix(knnpreds7_12,as.factor(testing7_12$BuildingandFloor))
}

{
  #knn 13_18  
  User13_18 <- filter(trainingData, USERID == 13|USERID == 14|USERID == 15|USERID == 16|USERID == 17|USERID == 18)
  names(User13_18)
  User13_18$BuildingandFloor <- paste(User13_18$BUILDINGID,"-", User13_18$FLOOR)
  names(User13_18)
  User13_18$BUILDINGID <- NULL
  User13_18$FLOOR <- NULL
  inTraining13_18 <- createDataPartition(User13_18$USERID,p=.70, list = FALSE)
  training13_18 <- User13_18[inTraining13_18,]
  testing13_18 <-  User13_18[-inTraining13_18,]  
  knnfitControl13_18 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  knnFituser13_18 <- train(BuildingandFloor~., data = training13_18, method = "knn",trControl=knnfitControl13_18, tuneLength = 1)
  knnpreds13_18 <- predict(knnFituser13_18,testing13_18)
  postResample(knnpreds13_18,as.factor(testing13_18$BuildingandFloor))
  confusionMatrix(svmpreds13_18,as.factor(testing13_18$BuildingandFloor))
}

{
  #random forest 1_6
  
  #User123456 <- filter(trainingData, USERID == 1|USERID == 2|USERID == 3|USERID == 4|USERID == 5|USERID == 6)
  #names(User123456)
  #User123456$BuildingandFloor <- paste(User123456$BUILDINGID,"-", User123456$FLOOR)
  #names(User123456)
  #User123456$BUILDINGID <- NULL
  #User123456$FLOOR <- NULL
  #names(User123456)  
  inTraining1_6rf <- createDataPartition(User123456$USERID,p=.70, list = FALSE)
  training1_6rf <- User123456[inTraining1_6rf,]
  testing1_6rf <-  User123456[-inTraining1_6rf,]  
  rfControl_123456 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  rfFituser123456 <- train(BuildingandFloor~., data = training1_6rf, method = "rf",trControl=rfControl_123456, tuneLength = 1)
  rfpreds_123456 <- predict(rfFituser123456,testing1_6rf)
  postResample(rfpreds_123456,as.factor(testing1_6rf$BuildingandFloor))
  confusionMatrix(rfpreds_123456,as.factor(testing1_6rf$BuildingandFloor))
}  

{
  #User7_12 <- filter(trainingData, USERID == 7|USERID == 8|USERID == 9|USERID == 10|USERID == 11|USERID == 12)
  #names(User7_12)
  #User7_12$BuildingandFloor <- paste(User13_18$BUILDINGID,"-", User13_18$FLOOR)
  #names(User7_12)
  #User7_12$BUILDINGID <- NULL
  #User7_12$FLOOR <- NULL
  #ames(User789101112)
  #inTraining7_12 <- createDataPartition(User789101112$USERID,p=.70, list = FALSE)
  #training7_12 <- User789101112[inTraining7_12,]
  #testing7_12 <-  User789101112[-inTraining7_12,]
  
  rffitControl7_12 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  rfFituser7_12 <- train(BuildingandFloor~., data = training7_12, method = "rf",trControl=rffitControl7_12, tuneLength = 1)
  rfpreds7_12 <- predict(rfFituser7_12,testing7_12)
  postResample(rfpreds7_12,as.factor(testing7_12$BuildingandFloor))
  confusionMatrix(rfpreds7_12,as.factor(testing7_12$BuildingandFloor))
  
}

{
  #User13_18 <- filter(trainingData, USERID == 13|USERID == 14|USERID == 15|USERID == 16|USERID == 17|USERID == 18)
  #names(User13_18)
  #User13_18$BuildingandFloor <- paste(User13_18$BUILDINGID,"-", User13_18$FLOOR)
  
  inTraining13_18 <- createDataPartition(User13_18$USERID,p=.70, list = FALSE)
  training13_18 <- User13_18[inTraining13_18,]
  testing13_18 <-  User13_18[-inTraining13_18,]  
  rffitControl13_18 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  rfFituser13_18 <- train(BuildingandFloor~., data = training13_18, method = "rf",trControl=rffitControl13_18, tuneLength = 1)
  rfpreds13_18 <- predict(rfFituser13_18,testing13_18)
  postResample(rfpreds13_18,as.factor(testing13_18$BuildingandFloor))
  confusionMatrix(rfpreds13_18,as.factor(testing13_18$BuildingandFloor))
  
}

plotDatarf <- resamples(list(rf_1318=rfFituser13_18,rf_712=rfFituser7_12,rf_16=rfFituser123456))
summary(plotDatarf)
bwplot(plotDatarf)

plotDataknn <- resamples(list(knn_1318=knnFituser13_18,knn_712=knnFituser7_12,knn_16=knnFituser123456))
summary(plotDataknn)
bwplot(plotDataknn)


plotDataSMV <- resamples(list(SVM_1318=svmFituser13_18,SVM_712=svmFituser7_12,SVM_16=svmFituser123456))
summary(plotDataSMV)
bwplot(plotDataSMV) 
