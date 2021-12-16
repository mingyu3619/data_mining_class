#######################################
##  week3 data mining 
##
##  2021-03-15
#######################################
install.packages("tidyverse")
install.packages("rvest")
install.packages("robotstxt")
install.packages("stringr")
install.packages("ggplot2")
library(stringr)
library(tidyverse)
library(rvest)
library(robotstxt)
library(ggplot2)
# set directory
setRepositories(ind =1:8)
setwd("C:\\r_dataMining")
getwd()



Height <- rnorm(100,mean=173.2,sd =5)
Gender <- c(rep("male",50),rep("Female",50))

dataFrameData <- data.frame(Height,Gender)
View(data)

##tibble example
tibleData <-tibble(Height,Gender)
tibleData

data$Height
data$Gender 

data[0][0]
data[1,1]
data[0,0]

glimpse(dataFrameData)
attributes(dataFrameData)
rownames(dataFrameData)


########## crawling part

##HTML document example

paths_allowed("http://www.imdb.com")
paths_allowed("http://www.facebook.com")

#main > div > span > div > div > div.lister > table > tbody > tr:nth-child(1) > td.titleColumn > a

page <- read_html("https://www.imdb.com/chart/top")     #��???б?
titles <- page %>%
  html_nodes(".titleColumn a")%>%
  html_text()
titlesr <- page %>%                                        #?⵵ ?б?
  html_nodes(".secondaryInfo") %>%
  html_text() %>%
  str_replace("\\(","") %>%
  str_replace("\\)","") %>%
  as.numeric()

scoyearsre <- page%>%                                        # ??�� ?б?
  html_nodes(".imdbRating")%>%
  html_text()%>%
  as.numeric()
score

data<-data.frame(title=titles,year=year,score=score)
View(data)
data<-tibble(title=title,year=year,score=score)
View(data)

glimpse(data)

data<- data%>%
  mutate(rank=1:nrow(data))
View(data)

#Q1
data %>%
  filter(year==1995) ==
#Q2
data%>%
data%>%
  group_by(year) %>%
  summarise(total=n())%>%
  arrange((desc(total))) %>%
  head(5)es(daView(data)
names(data)

ggplot(data,aes(x=year,y=score))+
  geom_point() +
  geom_smooth(method=lm) +
  theme_minimal()
   