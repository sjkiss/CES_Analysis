#This package is a major package for importing data
library(haven)
#This package is useful for generating path names that enables code sharing even with different path directories.
library(here)
#check current working directory
here()
#the 1971 pumf file should be in the data subfolder. WE can get access to the Data subfolder by giving "Data" to the here() command.
here("Data")
#LIst files in the data folder to see that it is there.
list.files(here("Data"))
#read in the file.
census71<-read_sav(file=here("Data", "1971_household_pumf.sav"))
#load tidyverse library of useful commands
library(tidyverse)
#The glimpse command give us a look at the data
glimpse(census71)
#This library helps search for keywords in variable labels
library(labelled)
#look for income
look_for(census71, "income")
census71$HHLDINC
#the variable is HH, use summary command to check
summary(census71$HHLDINC)
#Why is the minimum value -49000?
census71$HHLDINC
min(census71$HHLDINC, na.rm=T)
#Check the histogram
census71 %>% 
  ggplot(., aes(x=HHLDINC))+geom_histogram()
#find cases with negative income
census71 %>% 
  #this filters the data set to rows with income less than zero
  filter(HHLDINC<0) %>% 
  #then select the variable and print it
  select(HHLDINC)
#so it looks like there are about 194 cases of people with negative income;
#I guess in theory it is possible for a business owner to have lost income; so let's gamble and keep it. I have not seen any referene in technical eocumentation to negative values
#check summary again,
summary(census71$HHLDINC)
#The maximum value in the dataset is 363574. 

#find the quintiles
library(gtools)

quintiles_1971<-quantile(census71$HHLDINC, probs=c(0.2,0.4,0.6,0.8), na.rm=T)
#Create Design

quintiles_1971
census71 %>% 
  mutate(quintile=quantcut(HHLDINC, q=5,  labels=c(seq(1,5,1)))) %>% 
group_by(quintile) %>% 
  summarise(avg=mean(HHLDINC, na.rm=T), n=n())  %>% 
  mutate(year=rep('1971', nrow(.)))-> quintile_average_1971

quintile_average_1971
tertiles_1971<-quantile(census71$HHLDINC, probs = seq(0, 1, 1/3), na.rm=T)

census71 %>% 
  mutate(tertile=quantcut(HHLDINC, q=3,  labels=c(seq(1,3,1)))) %>% 
  group_by(tertile) %>% 
  summarise(avg=mean(HHLDINC, na.rm=T), n=n())  %>% 
  mutate(year=rep('1971', nrow(.)))-> tertile_average_1971
tertiles_1971

