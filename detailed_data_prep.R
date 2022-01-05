library(plyr)
library(readr)
library(tidyr)
library(dplyr)
library(networkD3)
library(htmlwidgets)
library(webshot)

# upload data
nodes_c <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/sa-nodes5.csv")
edges <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/sa-edges5.csv")

edges <- merge(edges, nodes_c, by.x = "Source", by.y = "ID", all.x = TRUE)
colnames(edges)[5] <- "Source_Cluster"
table(edges$Source_Cluster)
edges <- merge(edges, nodes_c, by.x = "Target", by.y = "ID", all.x = TRUE)
colnames(edges)[7] <- "Target_Cluster"
table(edges$Target_Cluster)

by_target <- edges %>% 
  group_by(Target, Year) %>% 
  add_count(Source_Cluster, name = "Count_types") %>%
  mutate(cit_other = sum(is.na(Source_Cluster))) %>%
  mutate(cit_bio = sum(Source_Cluster == "Biomedical")) %>%
  mutate(cit_beh = sum(Source_Cluster == "Behavioral Theory")) %>%
  mutate(cit_ti = sum(Source_Cluster == "Techno-Informatics")) %>%
  mutate(cit_na = sum(is.na(Source_Cluster)))

by_target <- by_target[,c(1,3,9,10,11,12,13)]
by_target <- unique(by_target)

by_source <- edges %>% 
  group_by(Source, Year) %>% 
  add_count(Target_Cluster, name = "Count_types") %>%
  mutate(ref_other = sum(is.na(Target_Cluster))) %>%
  mutate(ref_bio = sum(Target_Cluster == "Biomedical")) %>%
  mutate(ref_beh = sum(Target_Cluster == "Behavioral Theory")) %>%
  mutate(ref_ti = sum(Target_Cluster == "Techno-Informatics")) %>%
  mutate(ref_na = sum(Target_Cluster == "Non affective"))
by_source <- by_source[,c(2,3,9,10,11,12,13)]
by_source <- unique(by_source)

colnames(by_source)[2] <- "Year_source"
colnames(by_target)[2] <- "Year_target"

nodes_c <- merge(nodes_c, by_target, by.x = "ID", by.y = "Target", all.x = T)
nodes_c <- merge(nodes_c, by_source, by.x = "ID", by.y = "Source", all.x = T)

write.csv(nodes_c,"C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details2.csv")





