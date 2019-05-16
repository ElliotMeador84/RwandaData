attach(df_base.line)
library(tidyr)
library(dplyr)
library(tidyverse)
library(igraph)
library(naniar)
library(statnet)
library(ggplot2)
library(ggraph)
library(fun)
library(tidygraph)
library(stringr)
library(haven)
library(ggpubr)

#ANOVA regression - two way ANOVA used

Cooperative <- as.factor(Qi.Coopname) 
Gender <- as.factor(Qvi.Sex)
Trust <- as.numeric(Coop_Union_Communication_TrustScale)
Type <- as.factor(Qii.Cooptype)
anovareg <- cbind.data.frame(Cooperative, Gender, Trust, Type)


#interaction effect = effect of one factor is dependent on the level of another factor

model1 <- aov(Trust~Type)
summary(model1)

model2<-lm(Trust~Gender, data = anovareg)

summary(model2)
#display trust in a graph

ggplot(anovareg, aes(x = Cooperative, y = Trust, fill = Type ))+
  stat_summary(fun.y = "mean", geom = "bar")


WomanTrust <- df_base.line %>% 
  filter(Qvi.Sex == 1)
mean(WomanTrust$Coop_Union_Communication_TrustScale)

MenTrust <- df_base.line %>% 
  filter(Qvi.Sex == 0)

mean(MenTrust$Coop_Union_Communication_TrustScale)

#display gender differences in a graph

levels (anovareg$Gender) <- c("Male", "Female")

ggplot(anovareg, aes(x=Gender, fill = Gender)) +
  geom_bar()+
  facet_wrap(Cooperative ~ Type)




Gender3<- df_base.line %>% 
  filter(Qi.Coopname==5) %>% 
  select(Qvi.Sex)

sum(Gender3$Qvi.Sex==1)
sum(Gender3$Qvi.Sex==0)
