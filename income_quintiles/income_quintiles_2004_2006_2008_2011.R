#This package is a major package for importing data
library(haven)
#This package is useful for generating path names that enables code sharing even with different path directories.
library(here)
#check current working directory
here()
library(labelled)
#the 2006 pumf file should be in the data subfolder. WE can get access to the Data subfolder by giving "Data" to the here() command.
here("Data")
#LIst files in the data folder to see that it is there.
list.files(here("Data"))
#read in the file.
slid_2004<-read_sav(file=here("Data/2004_slid_census_family.sav"))
lookfor(slid_2004, "income")
library(survey)
library(srvyr)
library(tidyverse)
summary(slid_2004$ttinc46)

ggplot(slid_2004, aes(x=ttinc46))+geom_histogram()
#Unweighted Quintiles
unweighted_quintiles_2004<-quantile(slid_2004$ttinc46, probs=c(0.2,0.4,0.6,0.8), na.rm=T)
#Weighted Quintiles
lookfor(slid_2004, "weight")
lookfor(slid_2004, "income")

#Create Survey Design
slid_2004_des<-svydesign(id=~1, weights=~icswt26,data=slid_2004)
#Weighted Quintiles
weighted_quintiles_2004<-svyquantile(~as.numeric(ttinc46), design=slid_2004_des, quantiles=c(0.2,0.4,0.6,0.8))
weighted_quintiles_2004
#Unweighted Tertiles
tertiles_2004<-quantile(slid_2004$ttinc46, probs = seq(0, 1, 1/3), na.rm=T)
#Weighted Tertiles
weighted_tertiles_2004<-svyquantile(~as.numeric(ttinc46), design=slid_2004_des, quantiles=seq(0, 1, 1/3))
weighted_tertiles_2004
# 
# census71 %>% 
#   mutate(tertile=quantcut(HHLDINC, q=3,  labels=c(seq(1,3,1)))) %>% 
#   group_by(tertile) %>% 
#   summarise(avg=mean(HHLDINC, na.rm=T), n=n())  %>% 
#   mutate(year=rep('1971', nrow(.)))-> tertile_average_1971
#### 2006
#read in the file.
slid_2006<-read_sav(file=here("Data/2006_slid_census_family.sav"))
lookfor(slid_2006, "income")
lookfor(slid_2006, "income")
library(survey)
library(srvyr)
library(tidyverse)
summary(slid_2006$ttinc46)

ggplot(slid_2006, aes(x=ttinc46))+geom_histogram()
#Unweighted Quintiles
unweighted_quintiles_2006<-quantile(slid_2006$ttinc46, probs=c(0.2,0.4,0.6,0.8), na.rm=T)
#Weighted Quintiles
lookfor(slid_2006, "weight")
lookfor(slid_2006, "income")

#Create Survey Design
slid_2006_des<-svydesign(id=~1, weights=~icswt26,data=slid_2006)
#Weighted Quintiles
weighted_quintiles_2006<-svyquantile(~as.numeric(ttinc46), design=slid_2006_des, quantiles=c(0.2,0.4,0.6,0.8))
weighted_quintiles_2006
#Unweighted Tertiles
tertiles_2006<-quantile(slid_2006$ttinc46, probs = seq(0, 1, 1/3), na.rm=T)
#Weighted Tertiles
weighted_tertiles_2006<-svyquantile(~as.numeric(ttinc46), design=slid_2006_des, quantiles=seq(0, 1, 1/3))
weighted_tertiles_2006

#### 2008
#read in the file.
slid_2008<-read_sav(file=here("Data/2008_slid_census_family.sav"))

lookfor(slid_2008, "income")
lookfor(slid_2008, "income")
library(survey)
library(srvyr)
library(tidyverse)
summary(slid_2008$ttinc46)

ggplot(slid_2008, aes(x=ttinc46))+geom_histogram()
lookfor(slid_2008, "weight")
slid_2008_des<-svydesign(id=~1, weights=~wtcsld26,data=slid_2008)

#Unweighted Tertiles
tertiles_2008<-quantile(slid_2008$ttinc46, probs = seq(0, 1, 1/3), na.rm=T)
#Weighted Tertiles
weighted_tertiles_2008<-svyquantile(~as.numeric(ttinc46), design=slid_2008_des, quantiles=seq(0, 1, 1/3))

weighted_tertiles_2008

#### 2011
#read in the file.
slid_2011<-read_sav(file=here("Data/2011_slid_census_family.sav"))
lookfor(slid_2011, "weight")
slid_2011_des<-svydesign(ids=~0, weights=~wtcsld26,data=slid_2011)
lookfor(slid_2011, "income")
lookfor(slid_2011, "income")
slid_2011$ttinc46
library(survey)
library(srvyr)
library(tidyverse)
summary(slid_2011$ttinc46)

ggplot(slid_2011, aes(x=ttinc46))+geom_histogram()

#Unweighted Tertiles
tertiles_2011<-quantile(slid_2011$ttinc46, probs = seq(0, 1, 1/3), na.rm=T)
#Weighted Tertiles
weighted_tertiles_2011<-svyquantile(~as.numeric(ttinc46), design=slid_2011_des, quantiles=seq(0, 1, 1/3), na.rm=T)

tertiles_2011
weighted_tertiles_2011
?svyquantile
