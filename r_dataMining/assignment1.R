#######################################
##homwork1 crawling and data cleansing 
##2021-04-05  
#######################################

install.packages("rvest")
install.packages("tidyverse")
library(rvest)
library(tidyverse)
library(stringr)
setRepositories(ind =1:8)
setwd("C:\\r_dataMining")
getwd()

page <- read_html("https://ko.wikipedia.org/wiki/%ED%94%84%EB%A6%AC%EB%AF%B8%EC%96%B4%EB%A6%AC%EA%B7%B8")

html <- page %>% html_nodes(xpath='/html/body/div/div/div[1]/div[2]/main/div[3]/div[4]/div[1]/table[5]/tbody') %>%
  html_table(header=TRUE,convert = FALSE)
html <- html[[1]]  ## html[[1]]에 Table이 들어있음
html <- html[-1,]   ##2행의 불필요한 정보 제거
View(html)

colnames(html) <- c('시즌','우승','준우승','3위','최다득점자')                     ##열 이름 바뀌기
html <- html %>%separate(col='시즌',into='시즌',sep="-")%>%                     ##시즌 시작 년도만 기록
  separate(col='우승',into='우승',sep="\\(")%>%                                 ##불필요한 우승횟수 제거
  separate(col='우승',into='우승',sep="\\[")


html[1,5] <- '테디 셰링엄 (노팅엄 포리스트,22골)'                                     ##예외데이터처리


html <- html%>% mutate('최다득점자'=str_replace(최다득점자,"\\(",","))%>%           ##최다 득점자 -->득점자,소속팀,골수
  separate(col='최다득점자',into=c('득점왕','득점왕소속팀','골수'),sep=",")%>%
  separate(col='골수',into='골수',sep="골") %>%
  mutate('골수'=as.numeric(골수))

html <- html %>% mutate(우승=str_trim(우승))%>%                                 ##득점왕이 우승했는지 여부
  mutate(html,'득점왕and우승'=ifelse(html$우승==html$득점왕소속팀,1,0))
options(digits = 5)
html %>% summarise(mean(골수))
html %>% summarise(sum(골수))
html %>% summarise(sd(골수))
html %>% summarise(mean(득점왕and우승))
summary(html)
View(html)
