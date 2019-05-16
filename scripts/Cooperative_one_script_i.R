

library(tidyverse)
library(igraph)
library(naniar)
library(ggraph)
library(tidygraph)
library(ggpubr)
library(CINNA)
load('/Users/johne.meador/R/Rwanda_Paper/data/df_base.lineRData')

## Don't filter for Coop 1 yet as we want to apply the analysis to all coops eventually
df_base.line %>% names()
df_ls <- df_base.line %>%
  unite(Membertype, Qiv.a.Member, Qiv.c.Position, sep = '') %>%
  select(Qi.Coopname,
    Qvi.Sex,
    Coop_Union_Communication_TrustScale,
    Q3.b.Secondperson,
    Q3.a.Firstperson,
    Q3.c.thirdperson,
    Membertype
  ) %>%
  split(.$Qi.Coopname)

mutate(Membertype = ifelse(
  Membertype == 'MEMBER', 
  make.unique(Membertype), 
  Membertype))


### before we bin the Trust Scale question, let's take
# a look to see it's distribution. 


# There ARE NO real test for normality -- even computers get it wrong. The best way is to look at the data. A Shapiro Wilkes test can be run to help make or support the final decision. 

shapiro.test( #shaprio test of normality
  df_base.line$Coop_Union_Communication_TrustScale
  )

df_base.line %>% 
  select(Coop_Union_Communication_TrustScale) %>% 
  ggplot(aes(Coop_Union_Communication_TrustScale))+
  geom_density(fill = 'grey')+
  theme_pubr()+
  labs(title = 'Is the Trust scale normally distributed?', 
       subtitle = paste('Shapiro Test, p <= 0.001 (Likely not normal)'))


## I think for now your categories make sense. It might be worth coming back to as the way in which they are binned will effect model outputs. We can use case_when and dplyr to add

Coop_1$Trust <-
  cut(
    Coop_1$Coop_Union_Communication_TrustScale,
    breaks = c(0, 2, 4, 5),
    labels = c("Low", "Medium", "High")
  )

Coop_1 %>% 
  mutate(trust = case_when(
    Coop_1$Coop_Union_Communication_TrustScale <= 2 ~ 'Low', 
    between(Coop_1$Coop_Union_Communication_TrustScale, 2.01, 3.9) ~ 'Medium', 
    Coop_1$Coop_Union_Communication_TrustScale > 4 ~ 'High', T~''
    
  )) %>% 
  count(trust)










