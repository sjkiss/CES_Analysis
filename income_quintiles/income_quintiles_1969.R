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
famex_69<-read_sav(file=here("Data/1969_famex_family.sav"))
library(labelled)
lookfor(famex_69, "weight")
lookfor(famex_69, "income")

library(survey)
famex_69_des<-svydesign(ids=~0, weights=~WEIGHT, data=famex_69)


#Weighted Quintiles
weighted_quintiles_1969<-svyquantile(~as.numeric(IBT), design=famex_69_des, quantiles=c(0.2,0.4,0.6,0.8))
weighted_tertiles_1969<-svyquantile(~as.numeric(IBT), design=famex_69_des, quantiles=seq(0,1,1/3))
weighted_tertiles_1969
