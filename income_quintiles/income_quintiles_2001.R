#This package is a major package for importing data
library(haven)
#This package is useful for generating path names that enables code sharing even with different path directories.
library(here)
#check current working directory
here()
#the 2001 pumf file should be in the data subfolder. WE can get access to the Data subfolder by giving "Data" to the here() command.
here("Data")
#LIst files in the data folder to see that it is there.
list.files(here("Data"))
#read in the file.
census01<-read_sav(file=here("Data", "2001_household_pumf.sav"))
#load tidyverse library of useful commands
library(tidyverse)
#The glimpse command give us a look at the data
glimpse(census01)
#This library helps search for keywords in variable labels
library(labelled)
#look for income
look_for(census01, "income")
census01$TOTINCH
#the variable is TOTINCH, use summary command to check
summary(census01$TOTINCH)

#Check the histogram
census01 %>% 
  ggplot(., aes(x=TOTINCH))+geom_histogram()
#find cases with negative income
census01 %>% 
  #this filters the data set to rows with income less than zero
  filter(TOTINCH<0) %>% 
  #then select the variable and print it
  select(TOTINCH)
#so it looks like there are about 243 cases of people with negative income;
#I guess in theory it is possible for a business owner to have lost income; so let's gamble and keep it. I have not seen any referene in technical eocumentation to negative values
#check summary again,
summary(census01$TOTINCH)
#The maximum value in the dataset is 200000. 

#find the quintiles
library(gtools)
#This calculates the quintiles
quintiles_2001<-quantile(census01$TOTINCH, probs=c(0.2,0.4,0.6,0.8), na.rm=T)
#Ths calculates the average by quintile
census01 %>% 
  mutate(quintile=quantcut(TOTINCH, q=5,  labels=c(seq(1,5,1)))) %>% 
group_by(quintile) %>% 
  summarise(avg=mean(TOTINCH, na.rm=T), n=n())  %>% 
  mutate(year=rep('2001', nrow(.)))-> quintile_average_2001
quintile_average_2001

#Create Tertiles
tertiles_2001<-quantile(census01$TOTINCH, probs=c(seq(0,1,1/3)), na.rm=T)
census01 %>% 
  mutate(tertile=quantcut(TOTINCH, q=3, labels=c(seq(1,3,1)))) %>% 
  group_by(tertile) %>% 
  summarise(avg=mean(TOTINCH, na.rm=T), n=n())  %>% 
  mutate(year=rep('2001', nrow(.)))-> tertile_average_2001
tertiles_2001

