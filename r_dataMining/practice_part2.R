

#install.packages("rvest")
library(stringr)
library(tidyverse)
library(rvest)
library(robotstxt)
library(ggplot2)

setRepositories(ind=1:8)
setwd("C:\\r_dataMining")
getwd()

pb <-"https://www.imdb.com/title/tt2442560/"
st <- read_html("https://www.imdb.com/title/tt4574334/")
fr <- read_html("https://www.imdb.com/title/tt0108778/")

scrape_episode <- function(x){
  x %>%
    html_nodes(".bp_sub_heading") %>%
    html_text() %>%
    str_replace(" episodes", "") %>%
    as.numeric()
}


scrape_episode(pb)
scrape_episode(fr)

scrape_show_info <- function(x){
  y <- read_html(x)
  title <- y %>%
    html_node("h1") %>%
    html_text(trim = TRUE)
  print(title)
  runtime <- y %>%
    html_node("time") %>%
    html_text() %>% # could use trim = TRUE instead of str_ functions
    str_trim()
  genres <- y %>%
    html_nodes(".see-more.canwrap~ .canwrap a") %>%
    html_text() %>%
    str_c(collapse = ",") %>%
    str_trim()
  tibble(title = title, runtime = runtime, genres = genres)
}
scrape_show_info(pb)
scrape_show_info(st)

urls <- read_html("http://www.imdb.com/chart/tvmeter") %>%
  html_nodes(".titleColumn a") %>%
  html_attr("href") %>%
  paste("http://www.imdb.com", ., sep = "")
urls

n <- 1
top_n_shows <- tibble( title = rep(NA, n),
                       runtime = rep(NA, n),
                       genres = rep(NA, n)
)
top_n_shows
for(i in 1:n){
  top_n_shows[i, ] = scrape_show_info(urls[i])
}
top_n_shows

