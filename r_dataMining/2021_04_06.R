############################  
##    week6 
## 2021-04-06
###########################


setRepositories(ind=c(1:8))
install.packages("janitor")
library(tidyverse)
library(lubridate)
library(janitor)
library(glue)


setwd("C:\\r_dataMining")
getwd()

df1 <- tibble(a = c(1,2),b=2:1)
df2 <- tibble (a=c(1,3),c=10:11)

left_join(df1,df2)   ##유니크 키로 a를 사용하며, 그과정에서 2는  df2에 없으니 NA,3은 df1없서 삭제
inner_join(df1,df2)  ##공통으로 갖고있는 1만 남게 됨
full_join(df1,df2)   ##모든 데이터 포함,없는건 Na처리
##################################################right_join , semi_join 거의 안씀

demo_data <- read_csv("data/yrbss_demo.csv")  
qn_data <- read_csv("data/yrbss_qn.csv")

full_join(demo_data,qn_data)

merged_data <- full_join(demo_data,qn_data,by='record')
intersect(colnames(demo_data),colnames(qn_data))  ##공통되는 열
dim(demo_data);dim(qn_data);dim(merged_data)  ##(20000,8) (20000,5)   -->(20000,12) 
BP_wide <- tibble(id = letters[ 1 : 4 ],
                  sex = c( "F" , "M" , "M" , "F" ),
                  SBP_v1 = c( 130 , 120 , 130 , 119 ),
                  SBP_v2 = c( 110 , 116 , 136 , 106 ),
                  SBP_v3 = c( 112 , 122 , 138 , 118 ))

BP_wide %>% gather(key="visit",value = "SBP",SBP_v1:SBP_v3)%>%
  mutate(visit=str_replace(visit,"SBP_v",""))                       ###gather은 wide 에서 long
                                                                    ## spreade은 long에서 wide
BP_long <- BP_wide %>%
  gather(key ="visit",value="SBP",SBP_v1:SBP_v3)
BP_long2 <- BP_long %>%
  mutate(visit =
           str_replace(visit, "SBP_v" , "" ))
BP_long2
BP_wide3 <- BP_long2 %>% spread(key="visit",value = "SBP")
BP_wide3
BP_wide
BP_long




#####여기서 부터 중간 practice
DBP_wide <- tibble(id = letters[ 1 : 4 ],
                   sex = c( "F" , "M" , "M" , "F" ),
                   v1.DBP = c( 88 , 84 , 102 , 70 ),
                   v2.DBP = c( 78 , 78 , 96 , 76 ),
                   v3.DBP = c( 94 , 82 , 94 , 74 ),
                   age=c( 23 , 56 , 41 , 38 )
)
DBP_wide
##1번,2번
DBP_long <- DBP_wide %>% gather(key = "visit",value = "DBP",v1.DBP:v3.DBP)%>%
  mutate(visit=str_remove(visit,".DBP"))%>%
  mutate(visit=as.numeric(str_remove(visit,"v")))
##3번
DBP_wide2 <- DBP_long %>% spread(key = DBP_long,value = DBP)%>%
  rename("visit.1"=`1`,"visit.2"=`2`,"visit.3"=`3`)
names(DBP_wide2)
DBP_wide
DBP_long
DBP_wide2

##########for data cleansing
mydata <- tibble (id=7:9,
                  name=c("Bo","Al","Juan"),
                  height=c(2,NA,1.80),
                  years=c(51,35,NA))
mydata %>% drop_na(height)
mydata %>% mutate(height = replace_na(height,"Unknown"),
                  years=replace_na(years,0))

qn_data%>%mutate_at(vars(starts_with("q")),
                  .funs = list(~replace_na(.,'No answer')))%>%
  tabyl(q8,q31)

qn_data2 <- qn_data%>% add_column(qn_yes=1)                 
all_data <- left_join(demo_data,qn_data2)
all_data%>%  tabyl(qn_yes)
all_data%>%tabyl(race4)
all_data%>% mutate(race4=na_if(race4,   "All other races"))%>%tabyl(race4)  ##all other race를 NA로 취급함

#########################stringr
mydata <- tibble(name=c("J.M","Ella","Jay"),state=c("New Mexico","New York","Oregon"))
mydata
mydata %>% filter(str_detect(name,"J"))
mydata %>% mutate(new_state=str_detect(state,"New"))
mydata%>%  mutate(state_old = str_replace_all(state,"New","Old"))

mydata %>% mutate(
  name2 = str_replace(name, "l" , "-" ), # first instance
  name3 = str_replace_all(name, "l" , "-" ), # all instances
  name4 = str_replace_all(name,  "\\." , "" )) #

mydata %>% mutate(
  short_name = str_sub(name, start = 1 , end = 2 ),
  short_name2 = str_sub(name, end = 2 ),
  short_state = str_sub(state, end = - 3 )
)

demo_data %>%
  group_by(sex) %>%
  summarize(n_sex = n(),
            bmi_mean = mean(bmi,na.rm= TRUE ),
            bmi_sd = sd(bmi,na.rm= TRUE )) %>%
  mutate(bmi_mean_se = glue( "{round(bmi_mean,1)} ({signif(bmi_sd/sqrt(n_sex),2)})" ))


timedata <-
  tibble(name = c( "Yi" , "Bo" , "DJ" ),
         dob=c( "10/31/1952" , "1/12/1984" , "2/02/2002" ))
timedata %>%
  mutate(dob_date = mdy(dob),
         dob_wrong = dmy(dob))
timedata %>% mutate(
  dob = mdy(dob),
  dob_year = year(dob),
  time_since_birth = dob %--% today(),
  age = time_since_birth %/% years( 1 ),
  dobplus = dob + days( 10 )
)
mydata <- tibble( "First Name" = c( "Yi" , "DJ" ), "last init" = c( "C" , "R" ),
                  "% in" = c( 0.1 , 0.5 ), "ñ$$$" = 1 : 2 , " " = 3 : 2 , " hi" =c( "a" , "b" ),
                  "null" =c( NA , NA ))
mydata

mydata%>%clean_names()
mydata%>%remove_empty(c("rows","cols"))                       ##열 깨끗하게 하기

library(readxl)
read_excel("data/messy_names.xlsx",.name_repair = janitor::make_clean_names)
