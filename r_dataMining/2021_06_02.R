######################################################
## 2021-06-02  
##############################################

library(tidyverse)
library(datarium)
library(caret)
library(dplyr)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(kknn)
library(ROCR)
library(kernlab)
library(MASS)
install.packages("titanic")
library(titanic)
install.packages("fastAdaboost")
library(fastAdaboost)

setRepositories(ind= 1:8)
WORK_DIR <- "C:\\r_dataMining"
setwd(WORK_DIR)
getwd()

data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv")
View(data)

##raw data suffling
randomIdx <- sample(1:nrow(data))
data <- data[randomIdx,]

data$age <- as.numeric(data$age)
data$fare <- as.numeric(data$fare)
sum(is.na(data$age))
str(data)   ##보면서 필요한 열 생각해보기(이름 같은거 필요없으니)
cleanData <- data %>%                                     
  dplyr:::select(-c(home.dest,cabin,name,x,ticket))%>%
  mutate(pclass = factor(pclass,levels = c(1,2,3) , labels = c("Upper","Middle","Lower")),
         survived = factor(survived, levels = c(0,1), labels = c("No","Yes"))) %>% 
  na.omit()

##generating cross - validataion data(Train and Test set)
createTrainTestSet <- function(data,propTrain=0.8){
  propTest = 1- propTrain
  total_row <- propTrain * nrow(data)
  testSetIdx <- 1:total_row
  
  output <- list(testSet = data.frame(data[testSetIdx,]),
                trainSet = data.frame(data[-testSetIdx,])   )
  return(output)
}
foldData <- createTrainTestSet(cleanData,0.5)
dim(cleanData)
dim(foldData$testSet)
dim(foldData$trainSet)
## 죽은사람 vs 살아남은 사람 비율이 적적하게 나눠졌는지?
prop.table(table(foldData$trainSet$survived))
prop.table(table(foldData$testSet$survived))
prop.table(table(cleanData$survived))    


##modeling
model <- rpart(survived~., data = foldData$trainSet, method = "class")
rpart.plot(model,extra= 106)

##predictino & Error estimation
predictResult <- predict(model,foldData$testSet,type='class')
confusion <-  table(foldData$testSet$survived,predictResult)

Accuracy <- sum(diag(confusion))/sum(confusion)*100
Sensitivity <- confusion[2,2]/sum(confusion[,2])
Specificity <-confusion[1,1] /sum(confusion[,1])


#여기까지가 머신러닝의 과정
###########################################################################

##data loding
WebAddress <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
Data <- read.csv(WebAddress,head=F,stringsAsFactors =T)
#View(Data)

##data cleansing
feature <- c("radius","texture","perimeter"," area","smoothness",
             "compactness","convavity","concave_points","symmetry","fractal_dimentison")
colnames(Data) <- c("id","diagnosis",paste0(feature,"_mean"),paste0(feature,"_se"),paste0(feature,"_worst"))

cleanData <- Data[,3:ncol(Data)]
#View(cleanData)
rownames(cleanData) <-Data$id
cleanData <-cbind(cleanData,Data$diagnosis)

##data suffling
randomIdx <- sample(1:nrow(cleanData))
cleanData <- cleanData[randomIdx,]

##generate Train and Test set
foldData <- createTrainTestSet(cleanData,0.5)
dim(cleanData)
dim(foldData$testSet)
dim(foldData$trainSet)

##적절하게 나눠졌는지?
prop.table(table(foldData$trainSet$Data.diagnosis))
prop.table(table(foldData$testSet$Data.diagnosis))
prop.table(table(cleanData$`Data$diagnosis`))    

##modeling   lda/qda
ldaModel <- lda(Data.diagnosis ~ .,data = foldData$trainSet)
qdaModel <- qda(Data.diagnosis ~ .,data = foldData$trainSet)
knnModel_7 <- kknn(Data.diagnosis ~ ., train=foldData$trainSet ,test = foldData$testSet , k=7)
svmModel <- ksvm(Data.diagnosis ~., data = foldData$trainSet,kernel = "rbf",type="C-svc")
adaboostModel <- adaboost(Data.diagnosis ~., data = foldData$trainSet,10)

##prediction
prediction_lda  <- predict(ldaModel , newdata = foldData$testSet)
prediction_qda  <- predict(qdaModel , newdata = foldData$testSet)
prediction_svm  <- predict(svmModel , newdata = foldData$testSet)
prediction_ada  <- predict(adaboostModel , newdata = foldData$testSet)

##model performance check
confusion_lad <- table(Predicted = prediction_lda$class,
                   Diagnosis = foldData$testSet$Data.diagnosis)
confusion_qad <- table(Predicted = prediction_qda$class,
                       Diagnosis = foldData$testSet$Data.diagnosis)
copnfusion_7NN <- table(predicted=fitted(knnModel_7),Diagnosis = foldData$testSet$Data.diagnosis)
confusion_svm <- table(Predicted = prediction_svm,Diagnosis = foldData$testSet$Data.diagnosis)
confusion_ada <- table(Predicted = prediction_ada$class,Diagnosis = foldData$testSet$Data.diagnosis)

confusion_lad
confusion_qad
copnfusion_7NN
confusion_svm
confusion_ada

Accuracy <- sum(diag(confusion))/sum(confusion)*100
Sensitivity <- confusion[2,2]/sum(confusion[2,])
Specificity <-confusion[1,1] /sum(confusion[1,])
