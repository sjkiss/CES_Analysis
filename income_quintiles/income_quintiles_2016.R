#This package is a major package for importing data
library(haven)
#This package is useful for generating path names that enables code sharing even with different path directories.
library(here)
#check current working directory
here()
#the 2016 pumf file should be in the data subfolder. WE can get access to the Data subfolder by giving "Data" to the here() command.
here("Data")
#LIst files in the data folder to see that it is there.
list.files(here("Data"))
#read in the file.
income_2015<-read_sav(file=here("Data", "2015_income_individual.sav"))
library(labelled)
lookfor(income_2015, "census family")
lookfor(income_2015, "household")
lookfor(income_2015, "size")
income_2015$HHCOMP
table(as_factor(income_2015$HHCOMP), as_factor(income_2015$CFCOMP))
lookfor(income_2015, "income")
income_2015$EFMJIE
income_2015 %>% 
  filter(EFMJIE==1) %>% 
  select(CFTTINC, EFTTINC, CFCOMP, HHSIZE, EFMJIE) %>% 
  group_by(CFCOMP, HHSIZE) %>% 
  summarize(avg=mean(CFTTINC, na.rm=T), avg2=mean(EFTTINC)) %>%
  as_factor() %>% 
  View()


income_2015%>% 
  filter(EFMJIE=="1")->income_2015
lookfor(income_2015, "weight")
income_2015 %>% 
  ggplot(., aes(x=CFTTINC))+geom_histogram()
summary(income_2015$CFTTINC)
library(survey)
income_2015_des<-svydesign(ids=~0, data=income_2015, weights=~FWEIGHT)
tertiles_2015<-svyquantile(~as.numeric(CFTTINC), design=income_2015_des, quantiles=seq(0,1,1/3))
tertiles_2015


#### Income 2018 for the 2019 election
income_2018<-read_sav(file=here("Data", "2018_income_individual.sav"))
library(labelled)
lookfor(income_2018, "census family")
lookfor(income_2018, "household")
lookfor(income_2018, "size")
income_2018$HHCOMP
table(as_factor(income_2018$HHCOMP), as_factor(income_2018$CFCOMP))
lookfor(income_2018, "income")
income_2018$EFMJIE
income_2018 %>% 
  filter(EFMJIE=="1") %>% 
  select(CFTTINC, EFTTINC, CFCOMP, HHSIZE, EFMJIE) %>% 
  group_by(CFCOMP, HHSIZE) %>% 
  summarize(avg=mean(CFTTINC, na.rm=T), avg2=mean(EFTTINC)) %>%
  as_factor() %>% 
  View()


income_2018%>% 
  filter(EFMJIE=="1")->income_2018
lookfor(income_2018, "weight")
income_2018 %>% 
  ggplot(., aes(x=CFTTINC))+geom_histogram()
summary(income_2018$CFTTINC)
library(survey)
income_2018_des<-svydesign(ids=~0, data=income_2018, weights=~FWEIGHT)
tertiles_2018<-svyquantile(~as.numeric(CFTTINC), design=income_2018_des, quantiles=seq(0,1,1/3))
tertiles_2018
?svyquantile
