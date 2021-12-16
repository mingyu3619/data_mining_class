####################
## week 5
## 2021-03-29
####################

setRepositories(ind = 1:8)
install.packages("janitor")
library(tidyverse)
library(lubridate)
library(janitor)
library(glue)

setwd("C:\\r_dataMining")
getwd()

deomo_data<- read.csv("data/yrbss_demo.csv")
demo_data
View(demo_data)
new_data <-demo_data %>% 
  mutate(height =sqrt(stweight / bmi))
new_data
########################mutate practice
demo_data %>% mutate(bmi_high = (bmi > 30 ))
demo_data %>% mutate(male = (sex == "Male" ))
demo_data %>% mutate(male = 1 * (sex == "Male" ))
demo_data %>% mutate(grade = as.numeric(str_remove(grade, "th" )))

demo_data %>%
  mutate(bmi_group = case_when(
    bmi < 18.5 ~ "underweight",
    bmi >= 18.5 &bmi<=24.9 ~ "normal",
    bmi>24.9 & bmi <= 29.9 ~ "overweight",
    bmi>29.9 ~"obese"
  ))
########################## seperate ????
demo_data

demo_data %>% 
  separate(age,c("A","B","C","D","E"),sep=' ')
demo_data %>% separate(age, c( "agenum" , "yrs" ), sep = " " )
demo_data %>% separate(age, c( "agenum" , "yrs" ), sep = " " , remove = FALSE )
demo_data %>% separate(grade, c( "grade_n" ), sep = "h" )
demo_data %>% separate(grade, c( "grade_n" ), sep = "th" )
demo_data %>% separate(race4, c( "race4_1" , "race4_2" ), sep = "/" )
demo_data %>% unite( "sex_grade" , sex, grade, sep = "::::" )
demo_data %>% unite( "sex_grade" , sex, grade)
demo_data %>% unite( "race" , race4, race7) 
View(demo)
demo_data %>%
  unite("NewUniteVariablle",sex,grade,sep = " ")


data_dups <- tibble(
  name = c( "Ana" , "Bob" , "Cara" , "Ana" ),
  race = c( "Hispanic" , "Other" , "White" , "Hispanic" )
)
data_dups %>%
  distinct()
demo_data%>%
  arrange(desc(bmi),stweight)
demo_data %>% arrange(bmi, stweight) 
demo_data
demo_data %>% mutate_if(is.numeric, as.character) 
demo_data %>% mutate_if(is.character , tolower)
demo_data %>% mutate_if(is.double, round, digits= 0 )

demo_data %>% mutate_at(vars(contains( "race" )), str_detect, pattern = "White" )
#############################tabyl ????
demo_data %>% tabyl(grade)%>%select(-n)
demo_data %>% tabyl(grade)%>% adorn_totals("row")%>%adorn_pct_formatting(digits=1)
demo_data %>% tabyl(grade,sex)
demo_data%>%summarise(bmi_eman=mean(bmi,na.rm=T))
demo_data %>%
  group_by(grade) %>%
  summarize(n_per_group = n(),
            bmi_mean =mean(bmi,na.rm= TRUE ),
            bmi_sd = sd(bmi,na.rm= TRUE ))
demo_data %>% group_by(grade)
View(demo)

data1 <- tibble(id=1:2, name=c("Mina","Yi"),height=c(2,1),age=c(4,2))
data2 <- tibble(id=7:9,name=c("Bo","AI","Juan"),height=c(2,1.7,1.8),years=c(3,1,2))
data1
data2
bind_rows(data1,data2)
bind_cols(data1,data2)   ##????
