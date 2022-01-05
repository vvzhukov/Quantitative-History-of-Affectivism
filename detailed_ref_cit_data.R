library(plyr)
library(readr)
library(tidyr)
library(dplyr)
library(networkD3)

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
              group_by(Target) %>% 
              add_count(Source_Cluster, name = "Count_types") %>%
              mutate(cit_other = sum(is.na(Source_Cluster))) %>%
              mutate(cit_bio = sum(Source_Cluster == "Biomedical")) %>%
              mutate(cit_beh = sum(Source_Cluster == "Behavioral Theory")) %>%
              mutate(cit_ti = sum(Source_Cluster == "Techno-Informatics")) %>%
              mutate(cit_na = sum(Source_Cluster == "Non affective"))

by_target <- by_target[,c(1,9,10,11,12,13)]
by_target <- unique(by_target)

by_source <- edges %>% 
              group_by(Source) %>% 
              add_count(Target_Cluster, name = "Count_types") %>%
              mutate(ref_other = sum(is.na(Target_Cluster))) %>%
              mutate(ref_bio = sum(Target_Cluster == "Biomedical")) %>%
              mutate(ref_beh = sum(Target_Cluster == "Behavioral Theory")) %>%
              mutate(ref_ti = sum(Target_Cluster == "Techno-Informatics")) %>%
              mutate(ref_na = sum(Target_Cluster == "Non affective"))
by_source <- by_source[,c(2,3,9,10,11,12,13)]
by_source <- unique(by_source)


nodes_c <- merge(nodes_c, by_target, by.x = "ID", by.y = "Target", all.x = T)
nodes_c <- merge(nodes_c, by_source, by.x = "ID", by.y = "Source", all.x = T)

summary(nodes_c)
#write.csv(nodes_c,"C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details.csv")
nodes_c <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details.csv")
nodes_d <- nodes_c


nodes_c <- nodes_d
#nodes_c <- subset(nodes_d, Year >= 1963 & Year <= 1991)
#nodes_c <- subset(nodes_d, Year >= 1992 & Year <= 2002)
#nodes_c <- subset(nodes_d, Year >= 2003 & Year <= 2013)
#nodes_c <- subset(nodes_d, Year >= 2014 & Year <= 2020)

### 1. REF FULL

## create a dataframe with 8 nodes
nodes = data.frame("name" = c("RefBio.", "RefBeh.", "RefTech.", "RefNonAff.", 
                              "Bio.", "Beh.", "Tech.", "NonAff."))

## create edges with weights
sum_ref = sum(nodes_c$ref_other + nodes_c$ref_bio + nodes_c$ref_beh + nodes_c$ref_ti + nodes_c$ref_na, na.rm = T)
links = as.data.frame(matrix(c(0, 4, sum(subset(nodes_c, Cluster == "Biomedical")$ref_bio, na.rm = T) / sum_ref, # Cluster - Bio, Ref - Bio
                               1, 4, sum(subset(nodes_c, Cluster == "Biomedical")$ref_beh, na.rm = T) / sum_ref, # ... Ref - Beh
                               2, 4, sum(subset(nodes_c, Cluster == "Biomedical")$ref_ti, na.rm = T) / sum_ref, # ... Ref - Tec
                               3, 4, sum(subset(nodes_c, Cluster == "Biomedical")$ref_na, na.rm = T) / sum_ref, # ... Ref - NA
                               0, 5, sum(subset(nodes_c, Cluster == "Behavioral Theory")$ref_bio, na.rm = T) / sum_ref, # Cluster - Behavioral Theory, Ref - Bio
                               1, 5, sum(subset(nodes_c, Cluster == "Behavioral Theory")$ref_beh, na.rm = T) / sum_ref, # ... Ref - Beh
                               2, 5, sum(subset(nodes_c, Cluster == "Behavioral Theory")$ref_ti, na.rm = T) / sum_ref, # ... Ref - Tec
                               3, 5, sum(subset(nodes_c, Cluster == "Behavioral Theory")$ref_na, na.rm = T) / sum_ref, # ... Ref - NA
                               0, 6, sum(subset(nodes_c, Cluster == "Techno-Informatics")$ref_bio, na.rm = T) / sum_ref, # Cluster - Techno-Informatics, Ref - Bio
                               1, 6, sum(subset(nodes_c, Cluster == "Techno-Informatics")$ref_beh, na.rm = T) / sum_ref, # ... Ref - Beh
                               2, 6, sum(subset(nodes_c, Cluster == "Techno-Informatics")$ref_ti, na.rm = T) / sum_ref, # ... Ref - Tec
                               3, 6, sum(subset(nodes_c, Cluster == "Techno-Informatics")$ref_na, na.rm = T) / sum_ref, # ... Ref - NA
                               0, 7, sum(subset(nodes_c, Cluster == "Non affective")$ref_bio, na.rm = T) / sum_ref, # Cluster - Bio, Ref - Bio
                               1, 7, sum(subset(nodes_c, Cluster == "Non affective")$ref_beh, na.rm = T) / sum_ref, # ... Ref - Beh
                               2, 7, sum(subset(nodes_c, Cluster == "Non affective")$ref_ti, na.rm = T) / sum_ref, # ... Ref - Tec
                               3, 7, sum(subset(nodes_c, Cluster == "Non affective")$ref_na, na.rm = T) / sum_ref # ... Ref - NA
), byrow = TRUE, ncol = 3))

## set column names for links
names(links) = c("source", "target", "value")

## add edge types for coloring purpose
links$group = c("type_0", 
                "type_1", 
                "type_2",
                "type_3", 
                "type_0", 
                "type_1", 
                "type_2",
                "type_3",
                "type_0", 
                "type_1", 
                "type_2",
                "type_3",
                "type_0", 
                "type_1", 
                "type_2",
                "type_3")

## Create custom color list using d3 for each node
node_color <- 'd3.scaleOrdinal() .domain([
"RefBio.", "RefBeh.", "RefTech.", "RefNonAff.", 
"Bio.", "Beh.", "Tech.", "NonAff.", 
"type_0", "type_1", "type_2", "type_3"]) .range([
"brown", "magenta", "green" , "blue", 
"brown", "magenta", "green" , "blue", 
"brown", "magenta", "green" , "blue",])'

## Draw Sankey Diagram
p = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize = 0, nodeWidth = 40,
                  colourScale = node_color,
                  LinkGroup="group")
p


### 2. REF -NA

## create a dataframe with 8 nodes
nodes = data.frame("name" = c("RefBio.", "RefBeh.", "RefTech.", 
                              "Bio.", "Beh.", "Tech."))

## create edges with weights
sum_ref = sum(nodes_c$ref_other + nodes_c$ref_bio + nodes_c$ref_beh + nodes_c$ref_ti, na.rm = T)
links = as.data.frame(matrix(c(0, 3, sum(subset(nodes_c, Cluster == "Biomedical")$ref_bio, na.rm = T) / sum_ref, # Cluster - Bio, Ref - Bio
                               1, 3, sum(subset(nodes_c, Cluster == "Biomedical")$ref_beh, na.rm = T) / sum_ref, # ... Ref - Beh
                               2, 3, sum(subset(nodes_c, Cluster == "Biomedical")$ref_ti, na.rm = T) / sum_ref, # ... Ref - Tec
                               0, 4, sum(subset(nodes_c, Cluster == "Behavioral Theory")$ref_bio, na.rm = T) / sum_ref, # Cluster - Behavioral Theory, Ref - Bio
                               1, 4, sum(subset(nodes_c, Cluster == "Behavioral Theory")$ref_beh, na.rm = T) / sum_ref, # ... Ref - Beh
                               2, 4, sum(subset(nodes_c, Cluster == "Behavioral Theory")$ref_ti, na.rm = T) / sum_ref, # ... Ref - Tec
                               0, 5, sum(subset(nodes_c, Cluster == "Techno-Informatics")$ref_bio, na.rm = T) / sum_ref, # Cluster - Techno-Informatics, Ref - Bio
                               1, 5, sum(subset(nodes_c, Cluster == "Techno-Informatics")$ref_beh, na.rm = T) / sum_ref, # ... Ref - Beh
                               2, 5, sum(subset(nodes_c, Cluster == "Techno-Informatics")$ref_ti, na.rm = T) / sum_ref # ... Ref - Tec
), byrow = TRUE, ncol = 3))

## set column names for links
names(links) = c("source", "target", "value")

## add edge types for coloring purpose
links$group = c("type_0", 
                "type_1", 
                "type_2",
                "type_0", 
                "type_1", 
                "type_2",
                "type_0", 
                "type_1", 
                "type_2"
               )

## Create custom color list using d3 for each node
node_color <- 'd3.scaleOrdinal() .domain([
"RefBio.", "RefBeh.", "RefTech.", 
"Bio.", "Beh.", "Tech.",
"type_0", "type_1", "type_2"]) .range([
"brown", "magenta", "green" , 
"brown", "magenta", "green" ,
"brown", "magenta", "green" ])'

## Draw Sankey Diagram
q = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize = 0, nodeWidth = 40,
                  colourScale = node_color,
                  LinkGroup="group")
q


### 3. CIT FULL (r)

## create a dataframe with 8 nodes
nodes = data.frame("name" = c("Bio.", "Beh.", "Tech.", "NonAff.",
                              "CitBio.", "CitBeh.", "CitTech.", "CitNonAff."))

## create edges with weights
sum_cit = sum(nodes_c$cit_other + nodes_c$cit_bio + nodes_c$cit_beh + nodes_c$cit_ti + nodes_c$cit_na, na.rm = T)
links = as.data.frame(matrix(c(0, 4, sum(subset(nodes_c, Cluster == "Biomedical")$cit_bio, na.rm = T) / sum_cit, # Cluster - Bio, Ref - Bio
                               1, 4, sum(subset(nodes_c, Cluster == "Biomedical")$cit_beh, na.rm = T) / sum_cit, # ... Ref - Beh
                               2, 4, sum(subset(nodes_c, Cluster == "Biomedical")$cit_ti, na.rm = T) / sum_cit, # ... Ref - Tec
                               3, 4, sum(subset(nodes_c, Cluster == "Biomedical")$cit_na, na.rm = T) / sum_cit, # ... Ref - NA
                               0, 5, sum(subset(nodes_c, Cluster == "Behavioral Theory")$cit_bio, na.rm = T) / sum_cit, # Cluster - Behavioral Theory, Ref - Bio
                               1, 5, sum(subset(nodes_c, Cluster == "Behavioral Theory")$cit_beh, na.rm = T) / sum_cit, # ... Ref - Beh
                               2, 5, sum(subset(nodes_c, Cluster == "Behavioral Theory")$cit_ti, na.rm = T) / sum_cit, # ... Ref - Tec
                               3, 5, sum(subset(nodes_c, Cluster == "Behavioral Theory")$cit_na, na.rm = T) / sum_cit, # ... Ref - NA
                               0, 6, sum(subset(nodes_c, Cluster == "Techno-Informatics")$cit_bio, na.rm = T) / sum_cit, # Cluster - Techno-Informatics, Ref - Bio
                               1, 6, sum(subset(nodes_c, Cluster == "Techno-Informatics")$cit_beh, na.rm = T) / sum_cit, # ... Ref - Beh
                               2, 6, sum(subset(nodes_c, Cluster == "Techno-Informatics")$cit_ti, na.rm = T) / sum_cit, # ... Ref - Tec
                               3, 6, sum(subset(nodes_c, Cluster == "Techno-Informatics")$cit_na, na.rm = T) / sum_cit, # ... Ref - NA
                               0, 7, sum(subset(nodes_c, Cluster == "Non affective")$cit_bio, na.rm = T) / sum_cit, # Cluster - Bio, Ref - Bio
                               1, 7, sum(subset(nodes_c, Cluster == "Non affective")$cit_beh, na.rm = T) / sum_cit, # ... Ref - Beh
                               2, 7, sum(subset(nodes_c, Cluster == "Non affective")$cit_ti, na.rm = T) / sum_cit, # ... Ref - Tec
                               3, 7, sum(subset(nodes_c, Cluster == "Non affective")$cit_na, na.rm = T) / sum_cit # ... Ref - NA
), byrow = TRUE, ncol = 3))

## set column names for links
names(links) = c("source", "target", "value")

## add edge types for coloring purpose
links$group = c("type_0", 
                "type_1", 
                "type_2",
                "type_3", 
                "type_0", 
                "type_1", 
                "type_2",
                "type_3",
                "type_0", 
                "type_1", 
                "type_2",
                "type_3",
                "type_0", 
                "type_1", 
                "type_2",
                "type_3")

## Create custom color list using d3 for each node
node_color <- 'd3.scaleOrdinal() .domain([
"RefBio.", "RefBeh.", "RefTech.", "RefNonAff.", 
"Bio.", "Beh.", "Tech.", "NonAff.", 
"type_0", "type_1", "type_2", "type_3"]) .range([
"brown", "magenta", "green" , "blue", 
"brown", "magenta", "green" , "blue", 
"brown", "magenta", "green" , "blue",])'

## Draw Sankey Diagram
r = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize = 0, nodeWidth = 40,
                  colourScale = node_color,
                  LinkGroup="group")
r


### 4. CIT -NA
nodes = data.frame("name" = c("Bio.", "Beh.", "Tech.",
                              "CitBio.", "CitBeh.", "CitTech."))

## create edges with weights
sum_cit = sum(nodes_c$cit_other + nodes_c$cit_bio + nodes_c$cit_beh + nodes_c$cit_ti + nodes_c$cit_na, na.rm = T)
links = as.data.frame(matrix(c(0, 3, sum(subset(nodes_c, Cluster == "Biomedical")$cit_bio, na.rm = T) / sum_cit, # Cluster - Bio, Ref - Bio
                               1, 3, sum(subset(nodes_c, Cluster == "Biomedical")$cit_beh, na.rm = T) / sum_cit, # ... Ref - Beh
                               2, 3, sum(subset(nodes_c, Cluster == "Biomedical")$cit_ti, na.rm = T) / sum_cit, # ... Ref - Tec
                               0, 4, sum(subset(nodes_c, Cluster == "Behavioral Theory")$cit_bio, na.rm = T) / sum_cit, # Cluster - Behavioral Theory, Ref - Bio
                               1, 4, sum(subset(nodes_c, Cluster == "Behavioral Theory")$cit_beh, na.rm = T) / sum_cit, # ... Ref - Beh
                               2, 4, sum(subset(nodes_c, Cluster == "Behavioral Theory")$cit_ti, na.rm = T) / sum_cit, # ... Ref - Tec
                               0, 5, sum(subset(nodes_c, Cluster == "Techno-Informatics")$cit_bio, na.rm = T) / sum_cit, # Cluster - Techno-Informatics, Ref - Bio
                               1, 5, sum(subset(nodes_c, Cluster == "Techno-Informatics")$cit_beh, na.rm = T) / sum_cit, # ... Ref - Beh
                               2, 5, sum(subset(nodes_c, Cluster == "Techno-Informatics")$cit_ti, na.rm = T) / sum_cit # ... Ref - Tec
), byrow = TRUE, ncol = 3))

## set column names for links
names(links) = c("source", "target", "value")

## add edge types for coloring purpose
links$group = c("type_0", 
                "type_1", 
                "type_2",
                "type_0", 
                "type_1", 
                "type_2",
                "type_0", 
                "type_1", 
                "type_2"
                )

## Create custom color list using d3 for each node
node_color <- 'd3.scaleOrdinal() .domain([
"RefBio.", "RefBeh.", "RefTech.",  
"Bio.", "Beh.", "Tech.", 
"type_0", "type_1", "type_2"]) .range([
"brown", "magenta", "green" ,  
"brown", "magenta", "green" ,  
"brown", "magenta", "green" ])'

## Draw Sankey Diagram
s = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize = 0, nodeWidth = 40,
                  colourScale = node_color,
                  LinkGroup="group")
s




p
r
q
s
