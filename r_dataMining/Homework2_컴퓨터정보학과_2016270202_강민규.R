library(tidyverse)
install.packages("datarium")
library(datarium)
install.packages('caret')
library(caret)
install.packages('rpart')
library(rpart)
library(rpart.plot)
library(dplyr)
install.packages('kknn')
library(knnn)
install.packages('ROCR')
library(ROCR)
install.packages('kernlab')
library(kernlab)
library(MASS)
library(pubh)
library(huxtable)
library(tidyverse)
##2016270202-강민규data <- swiss
######################################################### homework2 cv
data <- swiss

testMethodKfoldCV <- function(data,k){
  length <- nrow(data)
  gap <- ceiling(nrow(data)/k)
  output <- list()
  
  for (i in seq(1,length,gap)){
    testSet <- na.omit(data[i:(i+gap),])
    trainSet <- data[-(i:(i+gap)),]
    model <- lm(Fertility ~., data = trainSet)
    prediction  <- model  %>% predict(testSet)
    result  <- data.frame(R2=R2(prediction,testSet$Fertility),
                          RMSE =RMSE(prediction,testSet$Fertility),
                          MAE=MAE(prediction,testSet$Fertility))
    
    output <- rbind(output,result)
  }
  
  
  output <- apply(output,2,mean)
  return(output)
}
result  <- testMethodKfoldCV(data,10)
print("result")
print(result)


for (i in c(1:10:2)){
  print(i)
}
seq(1,10,3)
for ( i in seq(1,10,3)){
  print(i)
}
i=1;gap=3
trainData <- data[i:(i+gap),]        ##파이썬 인덱싱으로  
testData <- data[-(i:(i+gap)),]
testData
dim(data)
dim(trainData)
dim(testData)
intersect(rownames(trainData),rownames(testData))
########################################################################

trainControl <- trainControl(method ="cv",number=10)
model <- train(Fertility ~., data,trControl=trainControl , method = "lm")

######################################################################## testMethodKfoldCVRepeat

testMethodKfoldCVRepeat <- function(data,k,repeat_num){
  
  ## k-foldCV
  testMethodKfoldCV_in <- function(data,k){
    length <- nrow(data)
    gap <- ceiling(nrow(data) /k)
    output <- list()
    
    for (i in seq(1,length,gap)){
      testSet <- na.omit(data[i:(i+gap),])
      trainSet <- data[-(i:(i+gap)),]
      model <- lm(Fertility ~., data = trainSet)
      prediction  <- model  %>% predict(testSet)
      result  <- data.frame(R2=R2(prediction,testSet$Fertility),
                            RMSE =RMSE(prediction,testSet$Fertility),
                            MAE=MAE(prediction,testSet$Fertility))
      
      output <- rbind(output,result)
    }
    
    output <- apply(output,2,mean)
    
    return(output)
  }
  ##
  repeated_arr <- list()
  for (i in 1:repeat_num){
    rows <- sample(nrow(data))
    data <- data[rows, ]
    repeated_arr <- rbind(repeated_arr,testMethodKfoldCV_in(data,k))
    
  }
  #repeated_arr <- data.frame(repeated_arr)
  repeated_arr <- data.frame(R2 = unlist(result$R2),RMSE=unlist(result$RMSE),MAE=unlist(result$MAE))
  repeated_arr <- apply(repeated_arr,2,mean) 
  return (repeated_arr) 
}


result  <- testMethodKfoldCVRepeat(data,10,5)
result
glimpse(result)
#output <- as.numeric(unlist(result))
#output <- data.frame(R2 = unlist(result$R2),RMSE=unlist(result$RMSE),MAE=unlist(result$MAE))
#glimpse(output)
#output <- apply(output,2,mean)


######################################################################## Q3 -1
data <- swiss
model <- lm(Fertility ~.,data=data)
summary(model)

model2 <- step(model,direction = "forward")
model3 <- step(model,direction = "backward")
model4 <- step(model,direction = "both")
min_AIC <- min(AIC(model2),AIC(model3),AIC(model3))
if (AIC(model2)==min_AIC){best_model <- model2}
if (AIC(model3)==min_AIC){best_model <- model3}
if (AIC(model4)==min_AIC){best_model <- model4}
prediction <- best_model  %>% predict(data)
best_model
result_M1 <- data.frame(R2=R2(prediction,data$Fertility),
                        RMSE =RMSE(prediction,data$Fertility),
                        MAE=MAE(prediction,data$Fertility))
result_M1
######################################################################## Q3 -2

predeicted_Fertility <- c()
for (i in 1:nrow(data)){
  testSet <- data[i,]
  trainSet <- data[-i,]
  model <- lm(Fertility ~.,data=trainData)
  model2 <- step(model,direction = "forward")
  model3 <- step(model,direction = "backward")
  model4 <- step(model,direction = "both")
  min_AIC <- min(AIC(model2),AIC(model3),AIC(model3))
  if (AIC(model2)==min_AIC){best_model <- model2}
  if (AIC(model3)==min_AIC){best_model <- model3}
  if (AIC(model4)==min_AIC){best_model <- model4}
  predeicted_Fertility[i] <- best_model %>% predict(testSet)
}
result <- data.frame(R2=R2(predeicted_Fertility,data$Fertility),
                     RMSE =RMSE(predeicted_Fertility,data$Fertility),
                     MAE=MAE(predeicted_Fertility,data$Fertility))
result
######################################################################## Q3 -3

testMethodKfoldCV <- function(data,k){
  length <- nrow(data)
  gap <- ceiling(nrow(data)/k)
  output <- list()
  
  for (i in seq(1,length,gap)){
    testSet <- na.omit(data[i:(i+gap),])
    trainSet <- data[-(i:(i+gap)),]
    model <- lm(Fertility ~., data = trainSet)
    ##
    model2 <- step(model,direction = "forward")
    model3 <- step(model,direction = "backward")
    model4 <- step(model,direction = "both")
    min_AIC <- min(AIC(model2),AIC(model3),AIC(model3))
    if (AIC(model2)==min_AIC){best_model <- model2}
    if (AIC(model3)==min_AIC){best_model <- model3}
    if (AIC(model4)==min_AIC){best_model <- model4}
    ##
    prediction  <- best_model  %>% predict(testSet)
    result  <- data.frame(R2=R2(prediction,testSet$Fertility),
                          RMSE =RMSE(prediction,testSet$Fertility),
                          MAE=MAE(prediction,testSet$Fertility))
    
    output <- rbind(output,result)
  }
  
  
  output <- apply(output,2,mean)
  return(output)
}
result  <- testMethodKfoldCV(data,10)
print("result")
print(result)