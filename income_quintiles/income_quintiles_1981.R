#This package is a major package for importing data
library(haven)
#This package is useful for generating path names that enables code sharing even with different path directories.
library(here)
#check current working directory
here()
#the 1981 pumf file should be in the data subfolder. WE can get access to the Data subfolder by giving "Data" to the here() command.
here("Data")
#LIst files in the data folder to see that it is there.
list.files(here("Data"))
#read in the file.
census81<-read_sav(file=here("Data", "1981_household_pumf.sav"))
#load tidyverse library of useful commands
library(tidyverse)
#The glimpse command give us a look at the data
glimpse(census81)
#This library helps search for keywords in variable labels
library(labelled)
#look for income
look_for(census81, "income")
census81$HHINC
#the variable is HH, use summary command to check
summary(census81$HHINC)
#Why is the minimum value -49000?
census81$HHINC
min(census81$HHINC, na.rm=T)
#Check the histogram
census81 %>% 
  ggplot(., aes(x=HHINC))+geom_histogram()
#find cases with negative income
census81 %>% 
  #this filters the data set to rows with income less than zero
  filter(HHINC<0) %>% 
  #then select the variable and print it
  select(HHINC)
#so it looks like there are about 220 cases of people with negative income;
#I guess in theory it is possible for a business owner to have lost income; so let's gamble and keep it. I have not seen any referene in technical eocumentation to negative values
#check summary again,
summary(census81$HHINC)
#The maximum value in the dataset is 100000. That is *super* suspicous. Nobody in Canada earned more than $150,000 in 1981? It's just not plausible. But this is what Statistics Canada has given us, so....

#find the quintiles
#find the quintiles
library(gtools)

quintiles_1981<-quantile(census81$HHINC, probs=c(0.2,0.4,0.6,0.8), na.rm=T)
census81 %>% 
  mutate(quintile=quantcut(HHINC, q=5, labels=c(seq(1,5,1)))) %>% 
  group_by(quintile) %>% 
  summarise(avg=mean(HHINC, na.rm=T), n=n())  %>% 
  mutate(year=rep('1981', nrow(.)))-> quintile_average_1981
quintile_average_1981

tertiles_1981<-quantile(census81$HHINC, probs=c(seq(0,1,1/3)), na.rm=T)
census81 %>% 
  mutate(tertile=quantcut(HHINC, q=3, labels=c(seq(1,3,1)))) %>% 
  group_by(tertile) %>% 
  summarise(avg=mean(HHINC, na.rm=T), n=n())  %>% 
  mutate(year=rep('1981', nrow(.)))-> tertile_average_1981
tertile_average_1981
tertiles_1981
