library(plyr)
library(readr)
library(tidyr)
library(dplyr)
library(networkD3)
library(htmlwidgets)
library(webshot)

nodes_d <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details2.csv")


sankey <- function(nodes_c, png_file_prefix){
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
  "brown", "pink", "darkgreen" , "blue", 
  "brown", "pink", "darkgreen" , "blue", 
  "brown", "pink", "darkgreen" , "blue",])'
  
  ## Draw Sankey Diagram
  p = sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    fontSize = 0, nodeWidth = 40,
                    colourScale = node_color,
                    LinkGroup="group")
  
  
  ### 2. CIT FULL (r)
  
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
                                 0, 7, 0, #sum(subset(nodes_c, Cluster == "Non affective")$cit_bio, na.rm = T) / sum_cit, # Cluster - Bio, Ref - Bio
                                 1, 7, 0, #sum(subset(nodes_c, Cluster == "Non affective")$cit_beh, na.rm = T) / sum_cit, # ... Ref - Beh
                                 2, 7, 0, #sum(subset(nodes_c, Cluster == "Non affective")$cit_ti, na.rm = T) / sum_cit, # ... Ref - Tec
                                 3, 7, 0 #sum(subset(nodes_c, Cluster == "Non affective")$cit_na, na.rm = T) / sum_cit # ... Ref - NA
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
  "brown", "pink", "darkgreen" , "blue", 
  "brown", "pink", "darkgreen" , "blue", 
  "brown", "pink", "darkgreen" , "blue",])'
  
  ## Draw Sankey Diagram
  r = sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    fontSize = 0, nodeWidth = 40,
                    colourScale = node_color,
                    LinkGroup="group")
  
  
  ## Save both p and r
  saveWidget(p, "temp.html")
  webshot("temp.html", file = paste("ref_", png_file_prefix, ".png", sep =""))
  file.remove("temp.html")
  saveWidget(r, "temp.html")
  webshot("temp.html", file = paste("cit_", png_file_prefix, ".png", sep =""))
  file.remove("temp.html")

  
}


setwd("C:/Users/Rubinzone/Desktop/Network_visualisations/net_v8/sankey/sankey_v3/")
sankey(nodes_d, "full")
sankey(subset(nodes_d, Year_target <= 1965), "1950_1965")
sankey(subset(nodes_d, Year_target > 1965 & Year_target <= 1990), "1966_1990")
sankey(subset(nodes_d, Year_target >= 1991 & Year_target <= 2000), "1991_2000")
sankey(subset(nodes_d, Year_target >= 2001 & Year_target <= 2010), "2001_2010")
sankey(subset(nodes_d, Year_target >= 2011 & Year_target <= 2020), "2011_2020")































# Below we are reviewing only 3 categories (excluding Non-Affective)

sankey_na <- function(nodes_c, png_file_prefix){
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
  
  
  ## Save both q and s
  saveWidget(q, "temp.html")
  webshot("temp.html", file = paste("excl_na_ref_", png_file_prefix, ".png", sep =""))
  file.remove("temp.html")
  saveWidget(s, "temp.html")
  webshot("temp.html", file = paste("excl_na_cit_", png_file_prefix, ".png", sep =""))
  file.remove("temp.html")
}


setwd("C:/Users/Rubinzone/Desktop/Network_visualisations/net_v8/sankey/sankey_v2/")
sankey_na(nodes_d, "full")
sankey_na(subset(nodes_d, Year_target <= 1990), "1950_1990")
sankey_na(subset(nodes_d, Year_target >= 1991 & Year_target <= 2000), "1991_2000")
sankey_na(subset(nodes_d, Year_target >= 2001 & Year_target <= 2010), "2001_2010")
sankey_na(subset(nodes_d, Year_target >= 2011 & Year_target <= 2020), "2011_2020")