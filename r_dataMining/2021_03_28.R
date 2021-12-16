#########################
##  Week 5
##  2021/03/17
#########################
library(tidyverse)
library(magrittr)
library(lubridate)
setRepositories(ind=1:8)
getwd()
setwd("C:\\r_dataMining")
getwd()
data <- data.frame(names=c("minseok",'kimchi','chulsoo'),rank=
1:3,age=c(34,35,36),city=c("sejong","seoul","suwon")           )
data
dataTibble <- tibble(names=c("minseok",'kimchi','chulsoo'),rank=1:3,
               age=c(34,35,36),city=c("sejong","seoul","suwon")           )
data
mydata_df <- read.csv("data/small_data.csv")
mydata_df
glimpse(mydata_df)
View(mydata_df)
summary(mydata_df)

untidy_data <- tibble(names=c("Ana","Bob","Cara"),meds=c("advil 600mg 2xday","tylenol 650mg 4xday","advil 200mg 3xday"))
untidy_data
untidy_data%>% 
  separate(col=meds,into=c("med_name","dose_mg","time_per_day"),sep = " ")%>% 
  mutate(time_per_day=as.numeric(str_remove(time_per_day,"xday")))%>% 
  mutate(dose_mg=as.numeric(str_remove(dose_mg,"mg")))
  dim(untidy_data)
  
untidy_data2 <- tibble(
    name = c( "Ana" , "Bob" , "Cara" ),
    wt_07_01_2018 = c( 100 , 150 , 140 ),
    wt_08_01_2018 = c( 104 , 155 , 138 ),
    wt_09_01_2018 = c( NA , 160 , 142 )
  )
untidy_data2
untidy_data2 %>% 
  gather(key = "date",value = "weight",-name)
untidy_data2 %>% 
  gather(key = "date",value = "weight",-name)%>%
  mutate(date=str_remove(date,"wt_"),date=dmy(date)) 

demo_data <-read_csv("C:\\r_dataMining\\data\\yrbss_demo.csv")
demo_data
View(demo_data)  
glimpse(demo_data)

bmi20 <- demo_data %>% filter(bmi>20)    ##filter ?Լ?( ??�� ?Ÿ?)

demo_data %>% 
  select(record,grade,sex)
lames(demo_data)
demo_data %>% rename(id=record)
colnames(demo_data)
