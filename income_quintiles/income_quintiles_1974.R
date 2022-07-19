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
scf_74<-read_sav(file=here("Data/1974_scf_fe_household.sav"))
library(labelled)
lookfor(scf_74, "weight")
lookfor(scf_74, "income")
scf_74$TOTINC
library(survey)
scf_74_des<-svydesign(ids=~0, weights=~WEIGHT, data=scf_74)
svyquantile(scf_74_des, )

#Weighted Quintiles
weighted_quintiles_1974<-svyquantile(~as.numeric(TOTINC), design=scf_74_des, quantiles=c(0.2,0.4,0.6,0.8))
weighted_tertiles_1974<-svyquantile(~as.numeric(TOTINC), design=scf_74_des, quantiles=seq(0,1,1/3))
weighted_tertiles_1974
