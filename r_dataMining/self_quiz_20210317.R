#########################
##  SELF_QUIZ 2??
##  2021/03/17
#########################

setRepositories(ind=1:8)
setwd("C:\\r_dataMining")
getwd()

install.packages("rvest")
library(stringr)
library(tidyverse)
library(rvest)
library(robotstxt)
library(ggplot2)

root_path="https://www.imdb.com"


pages <- read_html("https://www.imdb.com/chart/tvmeter")

links <- pages %>%
  html_nodes(".titleColumn a")%>%
  html_attr("href")
i=1
episodes <- NULL
genre <- NULL
title <- NULL
for (link in links){
  
  if(i==5)
    break
  print(i)
  print(link)
  link<-paste0(root_path,link)

  episodes <-rbind.data.frame(episodes, read_html(link)%>%
  html_nodes("div.button_panel.navigation_panel a div div span")%>%
  html_text())
  episodes
  
  title <-rbind(title, read_html(link)%>%
                 html_nodes("div.title_wrapper h1")%>%
                 html_text())
  title1 <-tibble( read_html(link)%>%
              html_nodes("div.title_wrapper h1")%>%
              html_text())
  
  genre <- rbind(genre,read_html(link)%>%
  html_nodes("div div.titleBar div.title_wrapper div a:nth-child(n)")%>%
  html_text()%>%str_replace_all("[\r\n]",""))
  genre
  i=i+1
  
  }
episodes
genre
View(genre)
title
View(title)
title1
ne <- NULL
ne <- cbind(ne,title,episodes,genre)
View(ne)
colnames(ne)
ne <- tibble(ne)
ne <- tibble(title=title, genre=genre[],episodes=episodes)
View(ne)
print("hi")
news
j=1
for (link in links){
  if(j==3)
    break
  print(j)
  j=j+1
  }

a <- c(a,"hi")
a <- c("js")
a
