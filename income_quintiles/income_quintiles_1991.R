#This package is a major package for importing data
library(haven)
#This package is useful for generating path names that enables code sharing even with different path directories.
library(here)
#check current working directory
here()
#the 1991 pumf file should be in the data subfolder. WE can get access to the Data subfolder by giving "Data" to the here() command.
here("Data")
#LIst files in the data folder to see that it is there.
list.files(here("Data"))
#read in the file.
census91<-read_sav(file=here("Data", "1991_household_pumf.sav"))
#load tidyverse library of useful commands
library(tidyverse)
#The glimpse command give us a look at the data
glimpse(census91)
#This library helps search for keywords in variable labels
library(labelled)
#look for income
look_for(census91, "income")
census91$TOTINCH
#the variable is HH, use summary command to check
summary(census91$TOTINCH)
#Why is the minimum value -49000?
census91$TOTINCH
min(census91$TOTINCH, na.rm=T)
#Check the histogram
census91 %>% 
  ggplot(., aes(x=TOTINCH))+geom_histogram()
#find cases with negative income
census91 %>% 
  #this filters the data set to rows with income less than zero
  filter(TOTINCH<0) %>% 
  #then select the variable and print it
  select(TOTINCH)
#so it looks like there are about 716 cases of people with negative income;
#I guess in theory it is possible for a business owner to have lost income; so let's gamble and keep it. I have not seen any referene in technical eocumentation to negative values
#check summary again,
summary(census91$TOTINCH)
#The maximum value in the dataset is 200000. 

#find the quintiles
library(gtools)

quintiles_1991<-quantile(census91$TOTINCH, probs=c(0.2,0.4,0.6,0.8), na.rm=T)

census91 %>% 
  mutate(quintile=quantcut(TOTINCH, q=5,  labels=c(seq(1,5,1)))) %>% 
group_by(quintile) %>% 
  summarise(avg=mean(TOTINCH, na.rm=T), n=n())  %>% 
  mutate(year=rep('1991', nrow(.)))-> quintile_average_1991
#Create Tertiles
tertiles_1991<-quantile(census91$TOTINCH, probs=c(seq(0,1,1/3)), na.rm=T)
census91 %>% 
  mutate(tertile=quantcut(TOTINCH, q=3, labels=c(seq(1,3,1)))) %>% 
  group_by(tertile) %>% 
  summarise(avg=mean(TOTINCH, na.rm=T), n=n())  %>% 
  mutate(year=rep('1991', nrow(.)))-> tertile_average_1991
tertiles_1991

