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
library(colorspace)
library(RColorBrewer)

#ANOVA regression - two way ANOVA used --------------------

Cooperative <- as.factor(Qi.Coopname) 
Gender <- as.factor(Qvi.Sex)
Trust <- as.numeric(Coop_Union_Communication_TrustScale)
Type <- as.factor(Qii.Cooptype)

df<-df_base.line %>% 
  unite(YearsinCoop, Qiv.b.Membership_years, Qiv.d.Member_in_leadership_years, sep = '') %>% 
  mutate(YearsinCoop = str_replace_all(YearsinCoop, 'NA', ''))
Years <- as.numeric(df$YearsinCoop)

anovareg <- cbind.data.frame(Cooperative, Years, Gender, Trust, Type)


#interaction effect = effect of one factor is dependent on the level of another factor

model1 <- aov(Trust~Type)
summary(model1)

model2<-lm(Trust~Gender, data = anovareg)

summary(model2)

model3 <- aov(Trust~Years*Cooperative)
summary(model3)


#display trust in a graph----------------


ggplot(anovareg, aes(x = Cooperative, y = Trust, fill = Type))+
  stat_summary(fun.y = "mean", geom = "bar")+
  theme_minimal()+
  scale_fill_manual(values = pal(n=2))

pal <- choose_palette()


##box plots for years experience and age ------------------

years_g <- df_base.line %>% 
  unite(YearsinCoop, Qiv.b.Membership_years, Qiv.d.Member_in_leadership_years, sep = '') %>% 
  select(YearsinCoop, Qi.Coopname, Qii.Cooptype, Qv.Age) %>% 
  mutate(YearsinCoop = str_replace_all(YearsinCoop, 'NA', ''))

years_g$Qi.Coopname <- factor(years_g$Qi.Coopname, labels = c("1", "2", "3", "4", "5", "6"))

years_g$YearsinCoop <- as.numeric(years_g$YearsinCoop)
years_g$Qv.Age <- as.numeric(years_g$Qv.Age)

bp_yearsexperience <- ggplot(years_g, aes(x = Qi.Coopname, y = YearsinCoop, fill = Qii.Cooptype)) +
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values = pal(n=2))+
  coord_flip()+
  xlab("Cooperative")+
  ylab("No. of Years in Cooperative")

bp_age <- ggplot(years_g, aes(x = Qi.Coopname, y = Qv.Age, fill = Qii.Cooptype)) +
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values = pal(n=2))+
  coord_flip()+
  xlab("Cooperative")+
  ylab("Age of Members")




#womantrust analysis 

WomanTrust <- df_base.line %>% 
  filter(Qvi.Sex == 1)
mean(WomanTrust$Coop_Union_Communication_TrustScale)

MenTrust <- df_base.line %>% 
  filter(Qvi.Sex == 0)

mean

Trust <- df_base.line %>% 
  filter(Qi.Coopname == 6)

mean(Trust$Coop_Union_Communication_TrustScale)

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
