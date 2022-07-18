#This package is a major package for importing data
library(haven)
#This package is useful for generating path names that enables code sharing even with different path directories.
library(here)
#check current working directory
here()
#the 1986 pumf file should be in the data subfolder. WE can get access to the Data subfolder by giving "Data" to the here() command.
here("Data")
#LIst files in the data folder to see that it is there.
list.files(here("Data"))
#read in the file.
census86<-read_sav(file=here("Data", "1986_household_pumf.sav"))
#load tidyverse library of useful commands
library(tidyverse)
#The glimpse command give us a look at the data
glimpse(census86)
#This library helps search for keywords in variable labels
library(labelled)
#look for income
look_for(census86, "income")
census86$HHTOTINC
#the variable is HH, use summary command to check
summary(census86$HHTOTINC)
#Why is the minimum value -49000?
census86$HHTOTINC
min(census86$HHTOTINC, na.rm=T)
#Check the histogram
census86 %>% 
  ggplot(., aes(x=HHTOTINC))+geom_histogram()
#find cases with negative income
census86 %>% 
  #this filters the data set to rows with income less than zero
  filter(HHTOTINC<0) %>% 
  #then select the variable and print it
  select(HHTOTINC)
#so it looks like there are about 194 cases of people with negative income;
#I guess in theory it is possible for a business owner to have lost income; so let's gamble and keep it. I have not seen any referene in technical eocumentation to negative values
#check summary again,
summary(census86$HHTOTINC)
#The maximum value in the dataset is 363574. 

#find the quintiles

quintiles_1986<-quantile(census86$HHTOTINC, probs=c(0.2,0.4,0.6,0.8), na.rm=T)
quintiles_1986
#find the quintiles
library(gtools)
census86 %>% 
  mutate(quintile=quantcut(HHTOTINC, q=5, labels=c(seq(1,5,1)))) %>% 
  group_by(quintile) %>% 
  summarise(avg=mean(HHTOTINC, na.rm=T), n=n())  %>% 
  mutate(year=rep('1986', nrow(.)))-> quintile_average_1986


tertiles_1986<-quantile(census86$HHTOTINC, probs=c(seq(0,1,1/3)), na.rm=T)
census86 %>% 
  mutate(tertile=quantcut(HHTOTINC, q=3, labels=c(seq(1,3,1)))) %>% 
  group_by(tertile) %>% 
  summarise(avg=mean(HHTOTINC, na.rm=T), n=n())  %>% 
  mutate(year=rep('1986', nrow(.)))-> tertile_average_1986
tertiles_1986
