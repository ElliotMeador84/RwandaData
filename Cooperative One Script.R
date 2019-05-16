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


#Creating Nodes----------------------------------------
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


#Creating Edges -------------------------
detach(package:plyr)
Edges1 <- Coop_1 %>%
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.a.Firstperson) %>% 
  cbind('Weight' = 1) %>% 
  rename(From = Membertype, To=Q3.a.Firstperson)

Edges2 <- Coop_1 %>%
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.b.Secondperson) %>% 
  cbind('Weight' = 2) %>% 
  rename(From = Membertype, To=Q3.b.Secondperson)

Edges3 <- Coop_1 %>% 
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.c.thirdperson) %>% 
  cbind('Weight' = 3) %>% 
  rename(From = Membertype, To=Q3.c.thirdperson)

Edges <- rbind(Edges1, Edges2, Edges3)


#Creating Facet Graphs -----------------
g<- graph_from_data_frame(d=Edges, vertices = Nodes)

plot(g)

ggraph(g) + 
  geom_edge_link( arrow = arrow(length = unit(0.25, "cm")))+
  geom_node_point(aes(color = Gender), size = 3) +
  geom_node_text(aes(label = NA), size = 3)+
  facet_wrap(~Weight)



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




#########################################



#Looking at the impact of years of experience on position in network 


df<-df_base.line %>% 
  unite(YearsinCoop, Qiv.b.Membership_years, Qiv.d.Member_in_leadership_years, sep = '') %>% 
  mutate(YearsinCoop = str_replace_all(YearsinCoop, 'NA', ''))

df$YearsinCoop <- as.numeric(df$YearsinCoop)

Experience_Coop <- df %>% 
  filter(Qi.Coopname==1) %>% 
  unite(Membertype, Qiv.a.Member, Qiv.c.Position, sep = '') %>% 
  select(Qvi.Sex, YearsinCoop, Q3.b.Secondperson, Q3.a.Firstperson, Q3.c.thirdperson, Membertype) %>% 
  mutate(Membertype = ifelse(Membertype == 'MEMBER', make.unique(Membertype) , Membertype)) 

detach(package:plyr)
EdgesEx_1 <- Experience_Coop %>%
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.a.Firstperson) %>% 
  cbind('Weight' = 1) %>% 
  rename(From = Membertype, To=Q3.a.Firstperson)

EdgesEx_2 <- Experience_Coop %>%
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.b.Secondperson) %>% 
  cbind('Weight' = 2) %>% 
  rename(From = Membertype, To=Q3.b.Secondperson)

EdgesEx_3 <- Experience_Coop %>% 
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  select(Membertype, Q3.c.thirdperson) %>% 
  cbind('Weight' = 3) %>% 
  rename(From = Membertype, To=Q3.c.thirdperson)
EdgesEx <- rbind(EdgesEx_1, EdgesEx_2, EdgesEx_3)

Nodes1 <- Experience_Coop %>% 
  select(Q3.a.Firstperson) %>% 
  rename(Nodes = Q3.a.Firstperson) 

Nodes2<- Experience_Coop %>% 
  select(Q3.b.Secondperson) %>% 
  rename(Nodes = Q3.b.Secondperson)

Nodes3 <- Experience_Coop %>% 
  select(Q3.c.thirdperson) %>% 
  rename(Nodes = Q3.c.thirdperson)

Nodes4 <- Experience_Coop %>% 
  select(Membertype, YearsinCoop, Qvi.Sex) %>% 
  filter(str_detect(Membertype, 'MEMBER')) %>% 
  rename(Nodes = Membertype)

library(plyr)
Nodes_Ex <- rbind.fill(Nodes1, Nodes2, Nodes3, Nodes4) %>% 
  distinct() %>% 
  mutate(type = ifelse(str_detect(Nodes, 'MEMBER'), '0', '1'))


Ex_g<- graph_from_data_frame(d=EdgesEx_1, vertices = Nodes_Ex, directed = T)

ggraph(Ex_g) + 
  geom_edge_link( arrow = arrow(length = unit(0.25, "cm")))+
  geom_node_point(aes(color = YearsinCoop ), size = 3) +
  geom_node_text(aes(label = NA), size = 3 )

dv<-degree(Ex_g)
sort(dv, decreasing = T)



##### Created code below which brings up the attributes of Nodes which are connected to a chosen Node. Is it possible to create this into a function?

Nodes_inc_on <- subgraph.edges(Ex_g, E(Ex_g)[inc(V(Ex_g)['PRESIDENT'])]) %>% 
  get.data.frame() %>% 
  select(1)

df2<- Reduce(intersect, list(Nodes_inc_on[, 1], Nodes_Ex[, 1])) %>% 
  as.data.frame()

Nodes_inc_on <- Nodes_Ex %>% 
  filter(Nodes_Ex$Nodes %in% df2$.)

#### Using this formula to compare average years of experience connected to nodes

Nodes_inc_on$YearsinCoop <- as.numeric(Nodes_inc_on$YearsinCoop)
  
YearsExp_Pres <- mean(Nodes_inc_on$YearsinCoop)

Nodes_Ex$YearsinCoop <- as.numeric(Nodes_Ex$YearsinCoop)

Avg_Years <- Nodes_Ex %>% 
  select (YearsinCoop) %>% 
  na.omit %>% 
  as.numeric()