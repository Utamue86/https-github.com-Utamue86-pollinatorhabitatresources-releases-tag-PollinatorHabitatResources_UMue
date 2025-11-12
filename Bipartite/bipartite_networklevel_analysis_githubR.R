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

# csv BBHRFall_yearseason
#now make one version with only SF Mix plants filtered out
#achmil, ascfas, camche, claung, clawil, colhet, erilan, esccal, gilcap, gricam, helann, helbol, helcal, hetgra
#lasfre, lupden, lupfor, madele, malsax, nemmac, nemmen, phacal, phacam, phacil, phatan, sphamb, trifuc, trilan

SFmixes1<-c("achmil", "ascfas", "camche", "claung", "clawil", "colhet", "erilan", "esccal",
            "gilcap", "gricam", "helann", "helbol", "helcal", "hetgra", "lasfre", "lupden", "lupfor",
            "madele", "malsax", "nemmac", "nemmen", "phacal", "phacam", "phacil", "phatan", "sphamb", 
            "trifuc", "trilan")

BBHRFSF_yearseason<-filter(BBHRFall_yearseason, lower%in%SFmixes1)

#Construct networks:
yearseasons_all<-frame2webs(BBHRFall_yearseason, varnames = c("lower", "higher", "webID", "freq"), type.out = 
                          "list", emptylist = TRUE)

yearseasons_SF<-frame2webs(BBHRFSF_yearseason, varnames = c("lower", "higher", "webID", "freq"), type.out = 
                         "list", emptylist = TRUE)

# Get full-network topology for the networks
#all plants
yearseasonal_networks_features<-as.data.frame(t(sapply(yearseasons_all, networklevel, index=c("Fisher alpha", "connectance", "number of compartments", "compartment diversity", "cluster coefficient", "nestedness", "weighted nestedness", "weighted NODF", "ISA", "SA", "C score","links per species", "web asymmetry", "H2", "Shannon diversity", "compartment diversity", "interaction evenness", "linkage density", "H2"))))
view(yearseasonal_networks_features)

#key SF plants
yearseasonal_networks_features_keySFplants<-as.data.frame(t(sapply(yearseasons_SF, networklevel, index=c("Fisher alpha", "connectance", "number of compartments", "compartment diversity", "cluster coefficient", "nestedness", "weighted nestedness", "weighted NODF", "ISA", "SA", "C score","links per species", "web asymmetry", "H2", "Shannon diversity", "compartment diversity", "interaction evenness", "linkage density", "H2"))))
view(yearseasonal_networks_features_keySFplants)

#species level network
#all plants:
#species_level lower:
all_plants_featuresBBHRFall_yearseasons<-as.data.frame(t(sapply(yearseasons_all, specieslevel, 
                                                            level="lower", index=c("degree", "normalised degree", "nestedrank", "PDI", "resource range", "species specificity","PSI", "PDI", "betweenness","closeness", "Fisher alpha","effective partners","proportional generality", "d"))))


#all indices recognized:
#de-list the results
#early
sapply(all_plants_featuresBBHRFall_yearseasons, class)
all_plants_features_yearseasons<-do.call(rbind, all_plants_featuresBBHRFall_yearseasons)
view(all_plants_features_yearseasons)
specieslevelBBHRFall_yearseasons<-as.data.frame(all_plants_features_yearseasons)
UPDATEplant_specieslevelBBHRFall_2015early<-as.data.frame(specieslevelBBHRFall_yearseasons$`2015,early`)
UPDATEplant_specieslevelBBHRFall_2016early<-as.data.frame(specieslevelBBHRFall_yearseasons$`2016,early`)
UPDATEplant_specieslevelBBHRFall_2017early<-as.data.frame(specieslevelBBHRFall_yearseasons$`2017,early`)
UPDATEplant_specieslevelBBHRFall_2019early<-as.data.frame(specieslevelBBHRFall_yearseasons$`2019,early`)

#species level higher
all_bees_featuresBBHRFall_yearseasons<-as.data.frame(t(sapply(yearseasons_all, specieslevel, 
                                                                level="higher", index=c("degree", "normalised degree", "nestedrank", "PDI", "resource range", "species specificity","PSI", "PDI", "betweenness","closeness", "Fisher alpha","effective partners","proportional generality", "d"))))


#all indices recognized:
#de-list the results
#early
sapply(all_bees_featuresBBHRFall_yearseasons, class)
all_bees_features_yearseasons<-do.call(rbind, all_bees_featuresBBHRFall_yearseasons)
view(all_bees_features_yearseasons)
beespecieslevelBBHRFall_yearseasons<-as.data.frame(all_bees_features_yearseasons)
UPDATEbee_specieslevelBBHRFall_2015early<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2015,early`)
UPDATEbee_specieslevelBBHRFall_2016early<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2016,early`)
UPDATEbee_specieslevelBBHRFall_2017early<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2017,early`)
UPDATEbee_specieslevelBBHRFall_2019early<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2019,early`)

#get the species names
bees2015early<-yearseasons_all$`2015,early`
t(bees2015early)

#bees:
view(t(bees2015early))
#plants
view(bees2015early)

bees2016early<-yearseasons_all$`2016,early`
#bees:
view(t(bees2016early))
#plants
view(bees2016early)

bees2017early<-yearseasons_all$`2017,early`
#bees:
view(t(bees2017early))
#plants
view(bees2017early)

bees2019early<-yearseasons_all$`2019,early`
#bees:
view(t(bees2019early))
#plants
view(bees2019early)

####mid
sapply(all_plants_featuresBBHRFall_yearseasons, class)
all_plants_features_yearseasons<-do.call(rbind, all_plants_featuresBBHRFall_yearseasons)
view(all_plants_features_yearseasons)
specieslevelBBHRFall_yearseasons<-as.data.frame(all_plants_features_yearseasons)
UPDATEplant_specieslevelBBHRFall_2015mid<-as.data.frame(specieslevelBBHRFall_yearseasons$`2015,mid`)
UPDATEplant_specieslevelBBHRFall_2016mid<-as.data.frame(specieslevelBBHRFall_yearseasons$`2016,mid`)
UPDATEplant_specieslevelBBHRFall_2017mid<-as.data.frame(specieslevelBBHRFall_yearseasons$`2017,mid`)
UPDATEplant_specieslevelBBHRFall_2019mid<-as.data.frame(specieslevelBBHRFall_yearseasons$`2019,mid`)


#all indices recognized:
#de-list the results
#mid
sapply(all_bees_featuresBBHRFall_yearseasons, class)
all_bees_features_yearseasons<-do.call(rbind, all_bees_featuresBBHRFall_yearseasons)
view(all_bees_features_yearseasons)
beespecieslevelBBHRFall_yearseasons<-as.data.frame(all_bees_features_yearseasons)
UPDATEbee_specieslevelBBHRFall_2015mid<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2015,mid`)
UPDATEbee_specieslevelBBHRFall_2016mid<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2016,mid`)
UPDATEbee_specieslevelBBHRFall_2017mid<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2017,mid`)
UPDATEbee_specieslevelBBHRFall_2019mid<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2019,mid`)

#get the species names
bees2015mid<-yearseasons_all$`2015,mid`
t(bees2015mid)
#works!
#bees:
view(t(bees2015mid))
#plants
view(bees2015mid)

bees2016mid<-yearseasons_all$`2016,mid`
#bees:
view(t(bees2016mid))
#plants
view(bees2016mid)

bees2017mid<-yearseasons_all$`2017,mid`
#bees:
view(t(bees2017mid))
#plants
view(bees2017mid)

bees2019mid<-yearseasons_all$`2019,mid`
#bees:
view(t(bees2019mid))
#plants
view(bees2019mid)

#late
sapply(all_plants_featuresBBHRFall_yearseasons, class)
all_plants_features_yearseasons<-do.call(rbind, all_plants_featuresBBHRFall_yearseasons)
view(all_plants_features_yearseasons)
specieslevelBBHRFall_yearseasons<-as.data.frame(all_plants_features_yearseasons)
UPDATEplant_specieslevelBBHRFall_2015late<-as.data.frame(specieslevelBBHRFall_yearseasons$`2015,late`)
UPDATEplant_specieslevelBBHRFall_2016late<-as.data.frame(specieslevelBBHRFall_yearseasons$`2016,late`)
UPDATEplant_specieslevelBBHRFall_2017late<-as.data.frame(specieslevelBBHRFall_yearseasons$`2017,late`)
UPDATEplant_specieslevelBBHRFall_2019late<-as.data.frame(specieslevelBBHRFall_yearseasons$`2019,late`)

#bees
sapply(all_bees_featuresBBHRFall_yearseasons, class)
all_bees_features_yearseasons<-do.call(rbind, all_bees_featuresBBHRFall_yearseasons)
view(all_bees_features_yearseasons)
beespecieslevelBBHRFall_yearseasons<-as.data.frame(all_bees_features_yearseasons)
UPDATEbee_specieslevelBBHRFall_2015late<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2015,late`)
UPDATEbee_specieslevelBBHRFall_2016late<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2016,late`)
UPDATEbee_specieslevelBBHRFall_2017late<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2017,late`)
UPDATEbee_specieslevelBBHRFall_2019late<-as.data.frame(beespecieslevelBBHRFall_yearseasons$`2019,late`)

#get the species names
bees2015late<-yearseasons_all$`2015,late`
t(bees2015late)
#works!
#bees:
view(t(bees2015late))
#plants
view(bees2015late)

bees2016late<-yearseasons_all$`2016,late`
#bees:
view(t(bees2016late))
#plants
view(bees2016late)

bees2017late<-yearseasons_all$`2017,late`
#bees:
view(t(bees2017late))
#plants
view(bees2017late)

bees2019late<-yearseasons_all$`2019,late`
#bees:
view(t(bees2019late))
#plants
view(bees2019late)

####

#key SF plants
#species_level lower:
SF_plants_featuresBBHRF_seasons<-as.data.frame(t(sapply(yearseasons_SF, specieslevel, 
                                                        level="lower", index=c("degree", "normalised degree", "nestedrank", "PDI", "resource range", "species specificity","PSI", "PDI", "betweenness","closeness", "Fisher alpha","effective partners","proportional generality", "d"))))

#all indices recognized:
#de-list the results
#early
sapply(SF_plants_featuresBBHRF_seasons, class)
SF_plants_features_seasons<-do.call(rbind, SF_plants_featuresBBHRF_seasons)
view(SF_plants_features_seasons)
specieslevelBBHRFSF_seasons<-as.data.frame(SF_plants_features_seasons)
plant_specieslevelBBHRFSF_yearsearly<-as.data.frame(specieslevelBBHRFSF_seasons$`early`)

#mid
plant_specieslevelBBHRFSF_yearsmid<-as.data.frame(specieslevelBBHRFSF_seasons$`mid`)
#late
plant_specieslevelBBHRFSF_yearslate<-as.data.frame(specieslevelBBHRFSF_seasons$`late`)

###########
#species_level higher:
#first try will all the indices: .
all_bees_features<-as.data.frame(t(sapply(yearseasons_all, specieslevel, 
                                          level="higher", index=c("degree", "normalised degree", "species strenght", "nestedrank", "interaction","PDI", "resource range", "species specificity","PSI", "NS", "PDI", "betweenness","closeness", "Fisher alpha", "diversity", "effective partners", "number of species", "mean number of links", "mean number of shared partners","proportional generality", "proportional specificity"))))
#not recognised&not computed:
#species strenght'
#interaction
#NS
#diversity
#number of species
#mean number of links
#proportional specificity

#exclude those
all_bees_features_seasons<-as.data.frame(t(sapply(seasons_all, specieslevel, 
                                                  level="higher", index=c("degree", "normalised degree", "nestedrank", "PDI", "resource range", "species specificity","PSI", "PDI", "betweenness","closeness", "Fisher alpha","effective partners","proportional generality"))))
view(all_bees_features_seasons)
#de-list the results
sapply(all_bees_features_seasons, class)
all_bees_features_seasons<-do.call(rbind, all_bees_features_seasons)
view(all_bees_features_seasons)
specieslevelBBbees_seasons<-as.data.frame(all_bees_features_seasons)
specieslevelBBbees_seasons_early<-as.data.frame(specieslevelBBbees_seasons$`early`)
specieslevelBBbees_seasons_mid<-as.data.frame(specieslevelBBbees_seasons$`mid`)
specieslevelBBbees_seasons_late<-as.data.frame(specieslevelBBbees_seasons$`late`)


#get the separate datasets per year:
beesearly<-seasons_all$`early`
t(beesearly)
#works!
#bees:
view(t(beesearly))
#plants
view(beesearly)

beesmid<-seasons_all$`mid`
t(beesmid)
#works!
#bees:
view(t(beesmid))
#plants
view(beesmid)

beeslate<-seasons_all$`late`
t(beeslate)
#works!
#bees:
view(t(beeslate))
#plants
view(beeslate)
#from there copy the plant and bees species in the excel tables

#same for only SF bees
all_bees_features_seasons<-as.data.frame(t(sapply(seasons_all, specieslevel, 
                                                  level="higher", index=c("degree", "normalised degree", "nestedrank", "PDI", "resource range", "species specificity","PSI", "PDI", "betweenness","closeness", "Fisher alpha","effective partners","proportional generality"))))
view(all_bees_features_seasons)
#de-list the results
sapply(all_bees_features_seasons, class)
all_bees_features_seasons<-do.call(rbind, all_bees_features_seasons)
view(all_bees_features_seasons)
specieslevelBBbees_seasons<-as.data.frame(all_bees_features_seasons)
specieslevelBBbees_seasons_early<-as.data.frame(specieslevelBBbees_seasons$`early`)
specieslevelBBbees_seasons_mid<-as.data.frame(specieslevelBBbees_seasons$`mid`)
specieslevelBBbees_seasons_late<-as.data.frame(specieslevelBBbees_seasons$`late`)


#get the separate datasets per year:
beesearly<-seasons_all$`early`
t(beesearly)
#works!
#bees:
view(t(beesearly))
#plants
view(beesearly)

beesmid<-seasons_all$`mid`
t(beesmid)
#works!
#bees:
view(t(beesmid))
#plants
view(beesmid)

beeslate<-seasons_all$`late`
t(beeslate)
#works!
#bees:
view(t(beeslate))
#plants
view(beeslate)

##need to get the species names for the SF data:
plantsearly<-seasons_SF$`early`
plantsearly

plantsmid<-seasons_SF$`mid`
plantsmid

plantslate<-seasons_SF$`late`
plantslate





