


library(tidyverse)
library(igraph)
library(naniar)
library(ggraph)
library(tidygraph)
library(graphlayouts)
library(ggpubr)
library(CINNA)
load('/Users/johne.meador/R/Rwanda_Paper/data/df_base.lineRData')

## Don't filter for Coop 1 yet as we want to apply the analysis to all coops eventually
df_ls <- df_base.line %>%
  unite(Membertype, Qiv.a.Member, Qiv.c.Position, sep = '') %>%
  select(
    Qi.Coopname,
    Qvi.Sex,
    Coop_Union_Communication_TrustScale,
    Q3.b.Secondperson,
    Q3.a.Firstperson,
    Q3.c.thirdperson,
    Membertype
  ) %>%
  split(.$Qi.Coopname) # splits the dataframe into a list of dataframes



# The map function from the purrr package is used for iterative programme.

df_ls <- map(df_ls, function(x) {
  x %>%   mutate(Membertype = ifelse(Membertype == 'MEMBER', make.unique(Membertype) , Membertype))
})


### before we bin the Trust Scale question, let's take
# a look to see it's distribution.


# There ARE NO real test for normality -- even computers get it wrong. The best way is to look at the data. A Shapiro Wilkes test can be run to help make or support the final decision.

shapiro.test(#shaprio test of normality
  df_base.line$Coop_Union_Communication_TrustScale)

df_base.line %>%
  select(Coop_Union_Communication_TrustScale) %>%
  ggplot(aes(Coop_Union_Communication_TrustScale)) +
  geom_density(fill = 'grey') +
  theme_classic(base_size = 15) +
  labs(title = 'Is the Trust scale normally distributed?',
       subtitle = paste('Shapiro Test, p <= 0.001 (Likely NOT normal)'))


## I think your categories make sense. It might be worth coming back to as the way in which they are binned will effect model outputs. We can use case_when and map (from purrr) to iterate across.

df_ls <- map(df_ls, function(x) {
  x %>%
    mutate(
      trust = case_when(
        Coop_Union_Communication_TrustScale <= 2 ~ 'Low',
        between(Coop_Union_Communication_TrustScale, 2.01, 3.9) ~ 'Medium',
        Coop_Union_Communication_TrustScale > 4 ~ 'High',
        T ~ ''
        
      )
    )
})


## Add edge weight
## [I won't use these for graph presentations]

# keeps <- df_ls$`1` %>% names()
#
# keep_o <- keeps[c(1, 2, 3, 7, 8)]
#
# df_ls <- map(df_ls, function(x){
#   x %>%
#     gather(key, value, -keep_o) %>%
#     select(Membertype, key, value, everything()) %>%
#     mutate(weight = case_when(
#       str_detect(key, 'First') ~ 1,
#       str_detect(key, 'Second') ~ 2,
#       T ~ 3)) %>%
#     select(Membertype, value, weight, everything())
# })



# Let's see if we can make a graph
edges <- df_ls %>%
  bind_rows() %>%
  select(Membertype, contains('First'), Qi.Coopname) %>%
  unite(Membertype, c(Membertype, Qi.Coopname), remove = F)



g_tble <- edges %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  mutate(coop = str_sub(name, -1))
g_tble %>% 
  acquire(nodes) %>% 
  View()


g_tble %>%
  ggraph(. , layout = 'stress') +
  geom_edge_arc(curvature = .1,
                aes()) +
  geom_node_point(aes(color = name),
                  size = 3.5,
                  show.legend = F) +
  geom_node_text(
    aes(label = name),
    size = 2.5,
    nudge_x = .05,
    nudge_y = .05
  ) +
  facet_edges( ~  Qi.Coopname) +
  scale_edge_width(range = c(.5, 1.5),
                   breaks = 1:3,
                   guide = F) +
  scale_color_discrete() +
  theme_graph(background = 'white',
              foreground = 'black',
              border = T)
