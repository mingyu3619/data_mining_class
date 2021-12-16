##############################################################
## 기말고사    2016270202 - 강민규
##############################################################
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
library(fastAdaboost)install.packages("keras")
library(keras)
#install.packages("mgcv")
library(mgcv)
#install.packages("party")
library(party)
#install.packages("RSNNS")
library(RSNNS)
#install.packages("deepnet")
library(deepnet)
#install.packages("naivebayes")
library(naivebayes)
library(tidyverse)
########################################################## 1번 
setRepositories(ind= 1:8)
WORK_DIR <- "C:\\r_dataMining\\FinalExam_DataMining"
setwd(WORK_DIR)
getwd()

data <- read_tsv("Q1_data.tsv")


data_cleaned <-data %>% mutate_if(is.character,as.numeric) %>%
  mutate(Disease= data$Disease)%>%na.omit()


trainSampleIndex <- data_cleaned$Disease %>%      ##나누는 부분 0.5비율로
  createDataPartition(p=0.5,list=F)


trainData <- data_cleaned[trainSampleIndex,]        ##파이썬 인덱싱으로  
testData <- data_cleaned[-trainSampleIndex,]

testData
dim(data_cleaned)
dim(trainData)
dim(testData)

##modeling
model <- kknn(Disease ~ ., train=trainData,test = testData, k=7)





#######################################################################2번



#############################################3번
data3 <- read_tsv("Q3_data.tsv")
View(data3)

data3 <- data3[,c(-1)]
trainSampleIndex <- data3$Grade %>%      ##나누는 부분 0.5비율로
  createDataPartition(p=0.5,list=F)

trainData <- data3[trainSampleIndex,]        ##파이썬 인덱싱으로  
testData <- data3[-trainSampleIndex,]

dim(data3)
dim(trainData)
dim(testData)

prop.table(table(data3$Grade))
prop.table(table(testData$Grade))
prop.table(table(trainData$Grade))

testData<- data.frame(testData)
trainData<- data.frame(trainData)
##modeling
model <- rpart(Grade ~ ., data =trainData, method = "class")
rpart.plot(model,extra="auto" ,box.palette = "green")

##predictino & Error estimation
predictResult <- predict(model,trainData,type='class')
confusion <-  table(artual=trainData$Grade,predictions=predictResult)

