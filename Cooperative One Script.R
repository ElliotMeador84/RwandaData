load("Rwanda_Project_2_Round_1.RData")
attach(df_base.line)
library(tidyr)
library(dplyr)
library(tidyverse)
library(igraph)
library(naniar)
library(statnet)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(stringr)
library(haven)
library(ggpubr)
library(CINNA)


Coop_1 <- df_base.line %>% 
  filter(Qi.Coopname==1) %>% 
  unite(Membertype, Qiv.a.Member, Qiv.c.Position, sep = '') %>% 
  select(Qvi.Sex, Coop_Union_Communication_TrustScale, Q3.b.Secondperson, Q3.a.Firstperson, Q3.c.thirdperson, Membertype) %>% 
  mutate(Membertype = ifelse(Membertype == 'MEMBER', make.unique(Membertype) , Membertype)) 

Coop_1$Trust <- cut(Coop_1$Coop_Union_Communication_TrustScale, breaks=c(0, 2, 4, 5), labels=c("Low", "Medium", "High"))

detach(package:plyr)

Nodes1 <- Coop_1 %>% 
  select(Q3.a.Firstperson) %>% 
  rename(Nodes = Q3.a.Firstperson) 
Nodes2<- Coop_1 %>% 
  select(Q3.b.Secondperson) %>% 
  rename(Nodes = Q3.b.Secondperson)
Nodes3 <- Coop_1 %>% 
  select(Q3.c.thirdperson) %>% 
  rename(Nodes = Q3.c.thirdperson)
Nodes4 <- Coop_1 %>% 
  select(Membertype, Trust, Qvi.Sex) %>% 
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  rename(Nodes = Membertype)


library(plyr)
Nodes <- rbind.fill(Nodes1, Nodes2, Nodes3, Nodes4) %>% 
  distinct() %>% 
  mutate(type = ifelse(str_detect(Nodes, 'MEMBER'), '0', '1'))


Nodes$Gender <- cut(Nodes$Qvi.Sex, breaks=c(-1, 0, 1), labels=c("Male","Female"), include.lowest = T)



detach(package:plyr)
Edges1 <- Coop_1 %>%
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.a.Firstperson) %>% 
  cbind('Weight' = 3) %>% 
  rename(From = Membertype, To=Q3.a.Firstperson)

Edges2 <- Coop_1 %>%
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.b.Secondperson) %>% 
  cbind('Weight' = 2) %>% 
  rename(From = Membertype, To=Q3.b.Secondperson)

Edges3 <- Coop_1 %>% 
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.c.thirdperson) %>% 
  cbind('Weight' = 1) %>% 
  rename(From = Membertype, To=Q3.c.thirdperson)

Edges <- rbind(Edges1, Edges2, Edges3)

g<- graph_from_data_frame(d=Edges, vertices = Nodes)

plot(g)


ggraph(g) + 
  geom_edge_link( arrow = arrow(length = unit(0.25, "cm")))+
  geom_node_point(aes(color = Trust, shape = Gender), size = 5) +
  geom_node_text(aes(label = NA), size = 3)



#centrality measures
calculate_centralities(g)

detach(package:statnet)
detach(package:sna)

dv<-degree(g)
sort(dv, decreasing = T)

members <- df_base.line %>% 
  filter(Qi.Coopname==1) %>% 
  filter(Qiv.a.Member == 'MEMBER') %>% 
  nrow()



V(g)$type <- bipartite_mapping(g)$type


ggraph(g,layout = "bipartite") + 
  geom_edge_link( arrow = arrow(length = unit(0.25, "cm")))+
  geom_node_point(aes(color = Gender), size = 5) +
  geom_node_text(aes(label = NA), size = 3)

#21 members

att_influence(g, 'PRESIDENT', 'Trust', mode = 'in')


#women

Coop_1woman<- df_base.line %>% 
  filter(Qi.Coopname==1) %>% 
  filter(Qvi.Sex==1) %>% 
  unite(Membertype, Qiv.a.Member, Qiv.c.Position, sep = '') %>% 
  select(Qvi.Sex, Coop_Union_Communication_TrustScale, Q3.b.Secondperson, Q3.a.Firstperson, Q3.c.thirdperson, Membertype) %>% 
  mutate(Membertype = ifelse(Membertype == 'MEMBER', make.unique(Membertype) , Membertype))


Edges1Woman <- Coop_1woman %>%
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.a.Firstperson) %>% 
  cbind('Weight' = 3) %>% 
  rename(From = Membertype, To=Q3.a.Firstperson)

Edges2Woman <- Coop_1woman %>%
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.b.Secondperson) %>% 
  cbind('Weight' = 2) %>% 
  rename(From = Membertype, To=Q3.b.Secondperson)

Edges3Woman <- Coop_1woman %>% 
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.c.thirdperson) %>% 
  cbind('Weight' = 1) %>% 
  rename(From = Membertype, To=Q3.c.thirdperson)

EdgesWoman <- rbind(Edges1Woman, Edges2Woman, Edges3Woman)

g<- graph_from_data_frame(d=EdgesWoman, vertices = Nodes)

plot(g)





att_influence(g, 'PRESIDENT', 'Trust', mode = 'in')
