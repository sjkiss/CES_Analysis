#Read in Andersen's CES 1993 
library(haven)
andersen<-read_sav(file="Data/ces1993_andersen.sav")
library(cesdata)
data("ces93")
#Run our recode script
library(labelled)
library(tidyverse)
library(car)
source("R_Scripts/2_9_ces93_recode.R")
names(andersen)
summary(andersen)

ces93 %>% 
  select(region, age, male, religion, degree, occupation, vote) %>% 
  summary()
nrow(ces93)
nrow(andersen)

andersen %>% 
  select(year, region, age, men, relig, degree, class, vote) %>% 
  summary()

table(ces93$occupation)
table(andersen$class)
table(ces93$vote)
table(andersen$vote)
