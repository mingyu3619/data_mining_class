##############################
## Week 7 2021-04-12
###########################

setRepositories(ind= 1:8)

library(MASS)
library(car)
install.packages("car")
library(broom)
library(tidyverse)
library(mosaic)
install.packages("mosaic")
install.packages("ggfortify")
library(ggfortify)
install.packages("huxtable")
library(huxtable)
install.packages("jtools")
library(jtools)
install.packages("latex2exp")
library(latex2exp)
install.packages("pubh")
library(pubh)
install.packages("sjlabelled")
library(sjlabelled)
install.packages("sjPlot")
library(sjPlot)
library(sjmisc)

data(birthwt)
View(birthwt)
str(birthwt)

birthwt <- birthwt %>% 
  mutate(smoke = factor(smoke,labels = c("Non-smoker","Smoker")),
         race = factor(race, labels = c("White","african American","Others")))%>% 
  var_labels(bwt="Birth weight(g)",smoke="Smoking status",race="Race",)

View(birthwt)
get_labels(birthwt)

birthwt %>%
  group_by(race,smoke) %>%
  summarise(
    n=n(),
    Mean=mean(bwt),
    SD=sd(bwt),
    Median=median(bwt),
    CV=rel_dis(bwt)
    )%>%
  as_hux()%>%
  theme_pubh(1)

birthwt %>%
  box_plot(bwt ~ smoke,fill = ~ race) + theme_classic()

birthwt%>%
  gen_bst_df(bwt ~race|smoke)%>%               ##gen_bst_df 
  as_hux()%>%
  theme_pubh(1)

model_norm <- lm(bwt ~smoke + race,data=birthwt)
model_norm

model_norm %>% Anova()
model_norm %>% 
  summ(confint=T,model.info=T)

model_norm %>% 
  glm_coef(labels=model_labels(model_norm))%>%
  as_hux()%>%
  set_align(everywhere,2:3,"right")%>%
  theme_pubh(1)%>%
  add_footnote(get_r2(model_norm),font_size=12)

model_norm%>%
  glance()

model_norm%>%
  plot_model("pred",terms= ~race|smoke)+theme_classic()
