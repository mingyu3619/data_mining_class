#####################################
##   2021-05-04 
##   week 10
#####################################

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


########################
setRepositories(ind= 1:8)
data(quine)

View(quine)
head(quine)
levels(quine$Eth) <- c("Aborighinal","white")
levels(quine$Sex) <- c("Female","Male")
quine$Sex

quine %>%
  group_by(Eth,Sex,Age) %>%
  summarise(
    n=n(),
    Mean=mean(Days,na.rm=T),
    Medain=median(Days,na.rm =T),
    CV=rel_dis(Days)
  )%>% as_hux()%>%
  theme_pubh(1)

quine %>% box_plot(Days~ Age|Sex,fill = ~Eth)%>%axis_labs() + theme_classic()

model_pois <- glm(Days ~ Eth + Sex+Age,family=poisson,data=quine)
model_pois %>% 
  glm_coef(labels = model_labels(model_pois),se_rob =T)%>%
  as_hux()%>%
  set_align(everywhere , 2:3,"right") %>%
  theme_pubh(1) %>% add_footnote(get_r2(model_pois))
