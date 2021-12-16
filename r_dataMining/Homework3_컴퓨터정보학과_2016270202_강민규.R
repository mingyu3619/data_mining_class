#################################################
##  2021-06-04
#################################################
library(tidyverse)
library(datarium)
library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(kknn)
library(ROCR)
library(kernlab)
library(MASS)
library(pubh)
library(huxtable)
library(fastAdaboost)
install.packages("keras")
library(keras)
install.packages("mgcv")
library(mgcv)
install.packages("party")
library(party)
install.packages("RSNNS")
library(RSNNS)
install.packages("deepnet")
library(deepnet)
install.packages("naivebayes")
library(naivebayes)

setRepositories(ind= 1:8)
WORK_DIR <- "C:\\r_dataMining"
setwd(WORK_DIR)
getwd()


################################################################################# segmentationData 데이터 사용시!

#############################################################     function


k_fold_cv_function <- function(){
#data loading
data("GermanCredit")

GermanCredit <- GermanCredit[,-nearZeroVar(GermanCredit)]
##raw data shuffling
randomIdx <- sample(1:nrow(GermanCredit))
GermanCredit <- GermanCredit[randomIdx,]
na.omit(GermanCredit)

### k fold 함수 사용하여 데이터 쪼개기
foldData_10 <- createFolds(GermanCredit$Class,k=10)
result=list()
for(i in 1:length(foldData_10)){
  if(i==2){break}
  valid_index  <-foldData_10[[i]]
  
  foldData_10$testSet <- GermanCredit[valid_index,]
  foldData_10$trainSet <- GermanCredit[-valid_index,]

##train test 비율 적절한지는 createFolds에서 관리
  ##modeling
  #model  <- ksvm(Class ~., data = foldData_10$trainSet,kernel = "rbf",type="C-svc")
  #model <- kknn(Class ~ ., train=foldData_10$trainSet ,test = foldData_10$testSet , k=20)
  #adaboostModel <- adaboost(Class ~., data = foldData_10$trainSet,10)  
  adaboostModel <- rpart(Class~., data = foldData_10$trainSet, method = "class")
  
##prediction
  #predictResult <- predict(model , newdata = foldData_10$testSet)
  prediction_ada  <- predict(adaboostModel , newdata = foldData_10$testSet,type="class")
  
##model confusion matrix
  #confusion <-  table(actual=foldData_10$testSet$Class,Predictions=predictResult)
  confusion <- table(Actual = foldData_10$testSet$Class,Predicted = prediction_ada)
  
##confusion matrix 수치 
  Accuracy <- sum(diag(confusion))/sum(confusion)*100
  Sensitivity <- confusion[2,2]/sum(confusion)
  Specificity <-confusion[1,1] /sum(confusion[1,])
  result <- rbind(result,data.frame(Accuracy,Sensitivity,Specificity))
  
}
print(result)
output <- apply(result,2,mean)

return (output)
}


k_fold_cv_function()


######################################################################################### segmentationData 사용시!

data(segmentationData)

segmentationData <- segmentationData[,c(-1,-2)]
randomIdx <- sample(1:nrow(segmentationData))
segmentationData   <- segmentationData[randomIdx,]

### k fold 함수 사용하여 데이터 쪼개기
foldData_10 <- createFolds(segmentationData$Class,k=10)
result=list()
for(i in 1:length(foldData_10)){

  #if(i==2){break}
  valid_index  <-foldData_10[[i]]
  
  foldData_10$testSet <- segmentationData[valid_index,]
  foldData_10$trainSet <- segmentationData[-valid_index,]
  
  ##train test 비율 적절한지는 createFolds에서 관리
  
##modeling
  
  #rpart_Model <- rpart(Class~., data = foldData_10$trainSet, method = "class")
  #model_svm  <- ksvm(Class ~., data = foldData_10$trainSet,kernel = "rbf",type="C-svc")
  #model_cforest <- cforest(Class ~., data=foldData_10$trainSet)
  model_nb <- naive_bayes(Class ~., data=foldData_10$trainSet)
  
##prediction
  
  #prediction_rpart  <- predict(rpart_Model , newdata = foldData_10$testSet,type="class")
  #prediction_svm  <- predict(model_svm , newdata = foldData_10$testSet)
  #prediction_cforest  <- predict(model_cforest , newdata = foldData_10$testSet)
  prediction_nb  <- predict(model_nb , newdata = foldData_10$testSet)
##model confusion matrix
  
  #confusion <- table(Actual = foldData_10$testSet$Class, Predicted = prediction_rpart)
  #confusion <- table(Actual = foldData_10$testSet$Class, Predicted = prediction_svm)
  #confusion <- table(Actual = foldData_10$testSet$Class, Predicted = prediction_cforest)
  confusion <- table(Actual = foldData_10$testSet$Class, Predicted = prediction_nb)
##confusion matrix 수치 
  Accuracy <- sum(diag(confusion))/sum(confusion)*100
  Sensitivity <- confusion[2,2]/sum(confusion)
  Specificity <-confusion[1,1] /sum(confusion[1,])
  result <- rbind(result,data.frame(Accuracy,Sensitivity,Specificity))
  
}
print(result)
output <- apply(result,2,mean)

print(output)


