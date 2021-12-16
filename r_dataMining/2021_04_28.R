#########################################################
## 2021_04_28 logistric regressin
########################################################


library(MASS)
library(car)
library(broom)
library(tidyverse)
library(mosaic)
library(ggfortify)
library(huxtable)
library(jtools)
library(latex2exp)
library(pubh)
library(sjlabelled)
library(sjPlot)
library(sjmisc)
library(Epi)
###  Logistric Regression Example  ######################

data(diet,package = "Epi")
View(diet)
 
diet <- diet %>% 
  mutate(chd = factor(chd,labels = c("No CHD","CHD")))%>%
  var_labels(
    chd="Coronary heart disease",
    fibre="Fibre intake(10 g/day)"
      )
diet %>% 
  estat(~fibre|chd) %>%
  as_hux()%>%
  theme_pubh(1)
diet %>%
  na.omit() %>%
  copy_labels(diet)%>%
  box_plot(fibre ~chd)

lm(chd ~ fibre,data=diet)

model_binom<-glm(chd ~fibre,data = diet, family = binomial)
model_binom2<-glm(chd ~fibre +height + weight ,data = diet, family = binomial)

model_binom1
model_binom2

model_binom %>%
  summ( confint =T ,model.info=T,exp=T)

model_binom %>%
  glm_coef(labels=model_labels(model_binom))%>%
  as_hux()%>%
  set_align(everywhere,2:3,'right')%>%
  theme_pubh(1)%>%
  add_footnote(get_r2(model_binom))

 