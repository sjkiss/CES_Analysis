#This package is a major package for importing data
library(haven)
#This package is useful for generating path names that enables code sharing even with different path directories.
library(here)
#check current working directory
here()
#the 2006 pumf file should be in the data subfolder. WE can get access to the Data subfolder by giving "Data" to the here() command.
here("Data")
#LIst files in the data folder to see that it is there.
list.files(here("Data"))
#read in the file.
census06<-read_sav(file=here("Data", "2006_individual_pumf.sav"))
#load tidyverse library of useful commands
library(tidyverse)
#The glimpse command give us a look at the data
glimpse(census06)
#This library helps search for keywords in variable labels
library(labelled)
#look for income
look_for(census06, "income")
census06$HHINC
table(as_factor(census06$HHINC))
cumsum(as_factor(census06$HHINC))
census06 %>% 
  group_by(HHINC) %>% 
  select(HHINC) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(HHINC)) %>% 
  mutate(prop=(n/sum(n))*100) %>% 
  mutate(cum_prop=cumsum(prop)) %>% 
  print(n=29) 
tertiles_2006<-c(0, 42500, 75750, 500000)
names(tertiles_2006)<-c("0%" ,"33.33333%", "66.66667%"  ,  "100%")
tertiles_2006
