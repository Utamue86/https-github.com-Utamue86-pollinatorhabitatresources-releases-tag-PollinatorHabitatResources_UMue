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
library(vegan)

#Background:

#load table prepared in R file 20.01.22 
#####
#can start here for the figures
fl_phen_update<-read.csv("/Users/utamueller/Desktop/02.06.2021 Postdoc/PostDoc/FFAR/Analysis/Analysis/2020/First analysis SF/SF/csv files 2021/fl_phen_update.csv")
#
fl_phen_prop<-fl_phen_update
table(fl_phen_prop$plant)
#########

str(fl_phen_update)
factor.vble <- c("plant_y", "year")
fl_phen_prop[factor.vble] <- map(fl_phen_prop[factor.vble], as.factor)

#now filter fl_phen_prop per year 
#merge wirth abunddiv
#make the plots per year to see differences
flphen1 <- filter(fl_phen_prop, year == "1")
flphen2 <- filter(fl_phen_prop, year == "2")
flphen3 <- filter(fl_phen_prop, year == "3")



#max floral area and phenology 

#per year

#plot without species labels and legend and median values as dotted lines
#dots black, add names of best performing species in power point
#first: figure with names to see them!
ggplot(flphen1, aes(mean_prop_over_thres_plant_y,max_propflarea))+
  geom_point(size=3.0)+
  geom_text(aes(label = plant), size=6)+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Proportion sample rounds over seasonal threshold")+
  ylab("Max proportion sown area covered by flowers of species")+
  ggtitle("Year 1")+ 
  coord_cartesian(xlim =c(0, 56), ylim = c(0, 15))+
  geom_vline(xintercept=15.31, color='black', linetype='dashed', alpha=.8)+
  geom_hline(yintercept=3.54, color='black', linetype='dashed', alpha=.8)+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position="none")+
  theme(title = element_text(hjust=.5, size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=16), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size=16),
        axis.text.x.bottom = element_text(size=16),axis.text.y = element_text(size=16))

#then without names:
ggplot(flphen1, aes(mean_prop_over_thres_plant_y,max_propflarea))+
  geom_point(size=3.0)+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Proportion sample rounds over seasonal threshold")+
  ylab("Max proportion sown area covered by flowers of species")+
  ggtitle("Year 1")+ 
  coord_cartesian(xlim =c(0, 56), ylim = c(0, 15))+
  geom_vline(xintercept=15.31, color='black', linetype='dashed', alpha=.8)+
  geom_hline(yintercept=3.54, color='black', linetype='dashed', alpha=.8)+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position="none")+
  theme(title = element_text(hjust=.5, size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=16), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size=16),
        axis.text.x.bottom = element_text(size=16),axis.text.y = element_text(size=16))

#year2
#mean and median
mean(flphen2$max_propflarea)
median(flphen2$max_propflarea)
mean(flphen2$mean_prop_over_thres_plant_y)
median(flphen2$mean_prop_over_thres_plant_y)

#with names
ggplot(flphen2, aes(mean_prop_over_thres_plant_y,max_propflarea))+
  geom_point(size=3.0)+
  geom_text(aes(label = plant), size=6)+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Proportion sample rounds over seasonal threshold")+
  ylab("Max proportion sown area covered by flowers of species")+
  ggtitle("Floral area and phenology SF Mix plants year2")+ 
  coord_cartesian(xlim =c(0, 56), ylim = c(0, 15))+
  geom_vline(xintercept=4.975, color='black', linetype='dashed', alpha=.8)+
  geom_hline(yintercept=0.758, color='black', linetype='dashed', alpha=.8)+
  scale_fill_brewer(palette = "Dark2")+
  theme(title = element_text(hjust=.5, size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=16), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size=16),
        axis.text.x.bottom = element_text(size=16),axis.text.y = element_text(size=16))

#plot without species labels and legend
ggplot(flphen2, aes(mean_prop_over_thres_plant_y,max_propflarea))+
  geom_point(size=3.0)+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Proportion sample rounds over seasonal threshold")+
  ylab("Max proportion sown area covered by flowers of species")+
  ggtitle("Year 2")+ 
  coord_cartesian(xlim =c(0, 56), ylim = c(0, 15))+
  geom_vline(xintercept=4.975, color='black', linetype='dashed', alpha=.8)+
  geom_hline(yintercept=0.758, color='black', linetype='dashed', alpha=.8)+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position="none")+
  theme(title = element_text(hjust=.5, size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=16), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size=16),
        axis.text.x.bottom = element_text(size=16),axis.text.y = element_text(size=16))
#save this one

#year3
#mean and median
mean(flphen3$max_propflarea)
median(flphen3$max_propflarea)
mean(flphen3$mean_prop_over_thres_plant_y)
median(flphen3$mean_prop_over_thres_plant_y)

ggplot(flphen3, aes(mean_prop_over_thres_plant_y,max_propflarea))+
  geom_point(size=3.0)+
  geom_text(aes(label = plant), size=6)+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Proportion sample rounds over seasonal threshold")+
  ylab("Max proportion sown area covered by flowers of species")+
  ggtitle("Floral area and phenology SF Mix plants year3")+ 
  coord_cartesian(xlim =c(0, 56), ylim = c(0, 15))+
  geom_vline(xintercept=1.763, color='black', linetype='dashed', alpha=.8)+
  geom_hline(yintercept=0.171, color='black', linetype='dashed', alpha=.8)+
  scale_fill_brewer(palette = "Dark2")+
  theme(title = element_text(hjust=.5, size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=16), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size=16),
        axis.text.x.bottom = element_text(size=16),axis.text.y = element_text(size=16))

#no lables
ggplot(flphen3, aes(mean_prop_over_thres_plant_y,max_propflarea))+
  geom_point(size=3.0)+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Proportion sample rounds over seasonal threshold")+
  ylab("Max proportion sown area covered by flowers of species")+
  ggtitle("Floral area and phenology SF Mix plants year3")+ 
  coord_cartesian(xlim =c(0, 56), ylim = c(0, 15))+
  geom_vline(xintercept=1.763, color='black', linetype='dashed', alpha=.8)+
  geom_hline(yintercept=0.171, color='black', linetype='dashed', alpha=.8)+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position="none")+
  theme(title = element_text(hjust=.5, size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=16), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size=16),
        axis.text.x.bottom = element_text(size=16),axis.text.y = element_text(size=16))

#suggestion Neal: remove esccal, achmil, gricam
flphen3a <- filter(flphen3, plant != "esccal") 
flphen3a <- filter(flphen3a, plant != "gricam") 
flphen3a <- filter(flphen3a, plant != "achmil") 
table(flphen3a$plant)
#######

ggplot(flphen3a, aes(mean_prop_over_thres_plant_y,max_propflarea))+
  geom_point(size=3.0)+
  geom_text(aes(label = plant), size=6)+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Proportion sample rounds over seasonal threshold")+
  ylab("Max proportion sown area covered by flowers of species")+
  ggtitle("Floral area and phenology SF Mix plants year3")+ 
  coord_cartesian(xlim =c(0, 12.5), ylim = c(0, 3))+
  geom_vline(xintercept=1.763, color='black', linetype='dashed', alpha=.8)+
  geom_hline(yintercept=0.171, color='black', linetype='dashed', alpha=.8)+
  scale_fill_brewer(palette = "Dark2")+
  theme(title = element_text(hjust=.5, size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=16), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size=16),
        axis.text.x.bottom = element_text(size=16),axis.text.y = element_text(size=16))

#mean and median
mean(fl_phen_prop$max_propflarea)
median(fl_phen_prop$max_propflarea)
mean(fl_phen_prop$mean_prop_over_thres_plant_y)
median(fl_phen_prop$mean_prop_over_thres_plant_y)

#no lables
ggplot(flphen3a, aes(mean_prop_over_thres_plant_y,max_propflarea))+
  geom_point(aes(color=factor(plant), size=0.5))+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Proportion sample rounds over seasonal threshold")+
  ylab("Max proportion sown area covered by flowers of species")+
  ggtitle("Floral area and phenology SF Mix plants year3")+ 
  coord_cartesian(xlim =c(0, 12.5), ylim = c(0, 3))+
  geom_vline(xintercept=1.763, color='black', linetype='dashed', alpha=.8)+
  geom_hline(yintercept=0.171, color='black', linetype='dashed', alpha=.8)+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position="none")+
  theme(title = element_text(hjust=.5, size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=16), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size=16),
        axis.text.x.bottom = element_text(size=16),axis.text.y = element_text(size=16))


#plot all years
ggplot(fl_phen_prop, aes(mean_prop_over_thres_plant_y,max_propflarea))+
  geom_point(aes(color=factor(plant), size=0.5))+
  geom_text(aes(label = plant_y), size=6)+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Proportion sample rounds over seasonal threshold")+
  ylab("Max proportion sown area covered by flowers of species")+
  ggtitle("Floral area and phenology SF Mix plants all years")+ 
  coord_cartesian(xlim =c(0, 56), ylim = c(0, 15))+
  geom_vline(xintercept=5.5808, color='black', linetype='dashed', alpha=.8)+
  geom_hline(yintercept=0.4965, color='black', linetype='dashed', alpha=.8)+
  scale_fill_brewer(palette = "Dark2")+
  theme(title = element_text(hjust=.5, size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.text = element_text(size=16), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size=16),
        axis.text.x.bottom = element_text(size=16),axis.text.y = element_text(size=16))


#saved as Abundance_natbee_phenology_per