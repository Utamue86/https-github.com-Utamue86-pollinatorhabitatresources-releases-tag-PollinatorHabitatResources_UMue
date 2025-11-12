library(bipartite)
library(Rmisc)
library(tidyverse)
library(viridis)
library(lemon)
library(lme4)
library(MASS)
library(car)
library(nlme)
library(dplyr)
library(DHARMa)
library(ggplot2)
library(gridGraphics)
library(vegan)
library(gplots)
library(lattice)
library(writexl)



#upload data
#Abundrich_Central_upperQuant_allseasons
early<-filter(Abundrich_Central_upperQuant_allseasons, season=="early")
mid<-filter(Abundrich_Central_upperQuant_allseasons, season=="mid")
late<-filter(Abundrich_Central_upperQuant_allseasons, season=="late")

#filter out non-student farm plants:
table(early$plant)
early <- filter(early, plant != "ascfas")
early <- filter(early, plant != "antcor")
early <- filter(early, plant != "amsint")
early <- filter(early, plant != "calcil")
early <- filter(early, plant != "clapur")
early <- filter(early, plant != "lasgla")
early <- filter(early, plant != "limalb")
early <- filter(early, plant != "lotsco")
early <- filter(early, plant != "lupsuc")
early <- filter(early, plant != "monvil")
early <- filter(early, plant != "penhet")
early <- filter(early, plant != "scrcal")

table(early$plant)

#plot 
ggplot(early, aes(y=plant, x=mean_BC))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices early season")+ 
  labs(x='Betweenness centrality', y = 'Plant species')+
  geom_vline(xintercept=0.048, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

ggplot(early, aes(y=plant, x=mean_CC))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices early season")+ 
  labs(x='Closeness centrality', y = 'Plant species')+
  geom_vline(xintercept=0.031, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

#now also add figures for abundance and richness
#quantiles calculated in R file quantile_abund_rich_10.08.23
ggplot(early, aes(y=plant, x=mean_abund))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices early season")+ 
  labs(x='Abundance', y = 'Plant species')+
  geom_vline(xintercept=12, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))
  
  
ggplot(early, aes(y=plant, x=mean_rich))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices early season")+ 
  labs(x='Richness', y = 'Plant species')+
  geom_vline(xintercept=5.187500, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

#mid
table(mid$plant)
mid <- filter(mid, plant != "ascfas")
mid <- filter(mid, plant != "asceri")
mid <- filter(mid, plant != "antcor")
mid <- filter(mid, plant != "clapur")
mid <- filter(mid, plant != "lasgla")
mid <- filter(mid, plant != "lotsco")
mid<- filter(mid, plant != "lupsuc")
mid<- filter(mid, plant != "monvil")
mid<- filter(mid, plant != "penhet")
mid <- filter(mid, plant != "scrcal")
mid <- filter(mid, plant != "symchi")
mid <- filter(mid, plant != "oenela")
table(mid$plant)
#plot 
ggplot(mid, aes(y=plant, x=mean_BC))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices mid season")+ 
  labs(x='Betweenness centrality', y = 'Plant species')+
  geom_vline(xintercept=0.043, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))


ggplot(mid, aes(y=plant, x=mean_CC))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices mid season")+ 
  labs(x='Closeness centrality', y = 'Plant species')+
  geom_vline(xintercept=0.032, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))


ggplot(mid, aes(y=plant, x=mean_abund))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices mid season")+ 
  labs(x='Abundance', y = 'Plant species')+
  geom_vline(xintercept=36.43750, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

ggplot(mid, aes(y=plant, x=mean_rich))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices mid season")+ 
  labs(x='Richness', y = 'Plant species')+
  geom_vline(xintercept=8, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

#late
table(late$plant)
late <- filter(late, plant != "antcor")
late <- filter(late, plant != "ascfas")
late <- filter(late, plant != "lotsco")
late <- filter(late, plant != "monvil")
late <- filter(late, plant != "oenela")
late <- filter(late, plant != "penhet")
late <- filter(late, plant != "scrcal")
late <- filter(late, plant != "solvel")
late <- filter(late, plant != "symchi")
table(late$plant)

#plot 
ggplot(late, aes(y=plant, x=mean_BC))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices late season")+ 
  labs(x='Betweenness centrality', y = 'Plant species')+
  geom_vline(xintercept=0.065, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

#
ggplot(late, aes(y=plant, x=mean_CC))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices late season")+ 
  labs(x='Closeness centrality', y = 'Plant species')+
  geom_vline(xintercept=0.026, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

ggplot(late, aes(y=plant, x=mean_abund))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices late season")+ 
  labs(x='Abundance', y = 'Plant species')+
  geom_vline(xintercept=40.5, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

ggplot(late, aes(y=plant, x=mean_rich))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices late season")+ 
  labs(x='Richness', y = 'Plant species')+
  geom_vline(xintercept=7.25, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))



#d bees by plant:
#csv ballseasons
bearly<-filter(ballseasons, season=="early")
bmid<-filter(ballseasons, season=="mid")
blate<-filter(ballseasons, season=="late")

#use the quantiles as calculated in file d_bees_by_season_no_mean_18.10.22
#filter out non-student farm plants:
table(bearly$plant)
bearly <- filter(bearly, plant != "ascfas")
bearly <- filter(bearly, plant != "antcor")
bearly <- filter(bearly, plant != "amsint")
bearly <- filter(bearly, plant != "calcil")
bearly <- filter(bearly, plant != "clapur")
bearly <- filter(bearly, plant != "lasgla")
bearly <- filter(bearly, plant != "limalb")
bearly <- filter(bearly, plant != "lotsco")
bearly <- filter(bearly, plant != "lupsuc")
bearly <- filter(bearly, plant != "monvil")
bearly <- filter(bearly, plant != "penhet")
bearly <- filter(bearly, plant != "scrcal")
bearly <- filter(bearly, plant != "trifuc")
table(bearly$plant)

#plot 
ggplot(bearly, aes(y=plant, x=sum_d))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices early season")+ 
  labs(x='d visiting bee', y = 'Plant species')+
  geom_vline(xintercept=0.42, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

#mid:
#filter out non-student farm plants:
table(bmid$plant)
bmid <- filter(bmid, plant != "ascfas")
bmid <- filter(bmid, plant != "asceri")
bmid <- filter(bmid, plant != "antcor")
bmid <- filter(bmid, plant != "clapur")
bmid <- filter(bmid, plant != "lasgla")
bmid <- filter(bmid, plant != "lotsco")
bmid<- filter(bmid, plant != "lupsuc")
bmid<- filter(bmid, plant != "monvil")
bmid<- filter(bmid, plant != "penhet")
bmid <- filter(bmid, plant != "scrcal")
bmid <- filter(bmid, plant != "symchi")
bmid <- filter(bmid, plant != "oenela")
table(bmid$plant)

#plot 
ggplot(bmid, aes(y=plant, x=sum_d))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices mid season")+ 
  labs(x='d visiting bee', y = 'Plant species')+
  geom_vline(xintercept=0.42, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))


#late
table(blate$plant)
blate <- filter(blate, plant != "antcor")
blate <- filter(blate, plant != "ascfas")
blate <- filter(blate, plant != "lotsco")
blate <- filter(blate, plant != "monvil")
blate <- filter(blate, plant != "oenela")
blate <- filter(blate, plant != "penhet")
blate <- filter(blate, plant != "scrcal")
blate <- filter(blate, plant != "solvel")
blate <- filter(blate, plant != "symchi")
table(blate$plant)

ggplot(blate, aes(y=plant, x=sum_d))+
  geom_point(aes(size=6))+
  ggtitle("Species level indices late season")+ 
  labs(x='d visiting bee', y = 'Plant species')+
  geom_vline(xintercept=0.35, color='black', linetype='dashed', alpha=.8)+
  theme_minimal()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(panel.spacing = unit(2, "cm")) +
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=12))+
  theme(axis.text.y = element_text(face = "italic"))

