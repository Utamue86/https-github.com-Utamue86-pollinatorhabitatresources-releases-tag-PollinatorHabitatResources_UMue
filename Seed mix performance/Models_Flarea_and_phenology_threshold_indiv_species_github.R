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
library(optimx)
library(visreg)
library(tidyverse)
library(performance)
library(lmerTest)
library(emmeans)
library(ggeffects)
library(glmmTMB)
library(ggplot2)
library(patchwork)

#csv fl_phen_update
#
fl_phen_prop<-fl_phen_update
view(fl_phen_prop)
#########
str(fl_phen_prop)
#make factors
hist(fl_phen_prop$max_propflarea)
hist(log(fl_phen_prop$max_propflarea))

#need to test for significance of relationship with models
m1<- lmer(log(max_propflarea+1) ~ mean_prop_over_thres_plant_y 
          + (1|plant)+ + (1|year),
               data = fl_phen_prop)
check_model(m1)



