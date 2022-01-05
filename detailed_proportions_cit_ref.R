library(ggplot2)
library(dplyr)
library(reshape2)

nodes_d <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details2.csv")

# create a dataset
nodes_e <- nodes_d[,c(5,6,7,8,9,10,12,13,14,15,16)]
nodes_e %>% group_by(Year_target) %>% summarise(cit_other = sum(cit_other, na.rm = T),
                                                cit_bio = sum(cit_bio, na.rm = T),
                                                cit_beh = sum(cit_beh, na.rm = T),
                                                cit_ti = sum(cit_ti, na.rm = T),
                                                cit_na = sum(cit_na, na.rm = T),
                                                ref_other = sum(ref_other, na.rm = T),
                                                ref_bio = sum(ref_bio, na.rm = T),
                                                ref_beh = sum(ref_beh, na.rm = T),
                                                ref_ti = sum(ref_ti, na.rm = T),
                                                ref_na = sum(ref_na, na.rm = T)) -> nodes_e

nodes_ref <- melt(nodes_e[,c(1,8,9,10,11)], id.vars = "Year_target")
nodes_cit <- melt(nodes_e[,c(1,3,4,5,6)], id.vars = "Year_target")
nodes_ref[218,3] <- 0

# Stacked + percent
ggplot(nodes_ref, aes(fill=variable, y=value, x=Year_target)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual("legend", values = c("ref_bio" = "brown", "ref_beh" = "pink", "ref_ti" = "darkgreen", "ref_na" = "blue")) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggplot(nodes_cit, aes(fill=variable, y=value, x=Year_target)) + 
  geom_bar(position="fill", stat="identity")  + 
  scale_fill_manual("legend", values = c("cit_bio" = "brown", "cit_beh" = "pink", "cit_ti" = "darkgreen", "cit_na" = "blue")) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

nodes_ref <- melt(nodes_e[,c(1,8,9,10)], id.vars = "Year_target")
nodes_cit <- melt(nodes_e[,c(1,3,4,5)], id.vars = "Year_target")


ggplot(nodes_ref, aes(fill=variable, y=value, x=Year_target)) + 
  geom_bar(position="fill", stat="identity") + 
    scale_fill_manual("legend", values = c("ref_bio" = "brown", "ref_beh" = "pink", "ref_ti" = "darkgreen")) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())

ggplot(nodes_cit, aes(fill=variable, y=value, x=Year_target)) + 
  geom_bar(position="fill", stat="identity")  + 
    scale_fill_manual("legend", values = c("cit_bio" = "brown", "cit_beh" = "pink", "cit_ti" = "darkgreen")) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
