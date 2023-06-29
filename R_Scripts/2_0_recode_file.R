#This loads the raw data installed in Simon's cesdata package
#These are direct files with no recodes; they've only been renamed to be things like ces65, ces79, etc.
#Uncomment if updates must be installed
#remove.packages('cesdata')
#remotes::install_github("sjkiss/cesdata", force=T)


library(cesdata)

library(tidyverse)
library(car)
library(labelled)
library(here)

nrow(ces65)==2118
nrow(ces74)==2562
nrow(ces7980)==2761
nrow(ces84)==3377
nrow(ces88)==3609
####Check that the datasets do not contain the recoded variable names
tail(names(ces74))
tail(names(ces84))
tail(names(ces0411))
tail(names(ces15phone))
#extra checks for ces74
table(ces74$sector)
table(ces74$union)
# this is a check to see about a lot of NAs in ces00$cpsf6
#Some checks
ces00 %>% 
  filter(is.na(cpsf6)) %>% 
  select(cpsf6)


## Note: If it appears that the loaded datasets include recoded variable names at this point you may  neee do uncomment the next line, and run it once
#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#### and then go back up and re-run the code, not re-running the rm() line above again. 


#load the here() library
library(here)
###How to use the here()package
#here()
#here("R_Scripts/2_ces65_recode.R")


# ### Run the recode scripts
# ### These take forever; that's why I'm doing this. 
source(here("R_Scripts/2_1_ces65_recode.R"))
source(here("R_Scripts/2_2_ces68_recode.R"))
source(here("R_Scripts/2_3_ces72_nov_recode.R"))
source(here("R_Scripts/2_4_ces74_recode.R"))
source(here("R_Scripts/2_5_ces7980_recode.R"))
source(here("R_Scripts/2_6_ces80_recode.R"))
source(here("R_Scripts/2_7_ces84_recode.R"))
source(here("R_Scripts/2_8_ces88_recode.R"))
source(here("R_Scripts/2_9_ces93_recode.R"))
source(here("R_Scripts/2_10_ces97_recode.R"))
source(here("R_Scripts/2_11_ces00_recode.R"))
source(here("R_Scripts/2_12_ces0411_recode.R"))
source(here("R_Scripts/2_13_ces15_recode.R"))
source(here("R_Scripts/2_14_ces19_recode.R"))
# #source(here("R_Scripts/2_15_ces19_web_recode.R"))
#source(here("R_Scripts/2_16_ces21_recode.R"))
#This saves the ojbects that we 
#save(ces65, ces68, ces72_nov, ces74, ces7980, ces84, ces88, ces93, ces97, ces00, ces0411, ces15phone, ces19web, ces19phone, ces21, file="Data/recoded_cesdata.Rdata")


#### Update the file recoded_cesdata with any recodes ####
#Define Resave function
#install.packages("cgwtools")

#use resave to update the file recoded_cesdata.Rdata with just *one* specific file. All other files in recoded_cesdata.rdata are *untouched*. If you want you can update more than one file and all others are still untouched
#cgwtools::resave(ces65, ces68, ces72_nov, ces74, ces7980, ces84, ces88, ces93, ces97, ces00, ces0411, ces15phone, ces19phone,   file="Data/recoded_cesdata.Rdata")
#cgwtools::resave(ces0411, file="Data/recoded_cesdata.Rdata")
#load(file="Data/recoded_cesdata.Rdata")
#tail(names(ces21))
#now clear everything out

## Detach package "cesdata"

#detach("package:cesdata")
rm(list=ls())
#load('Data/recoded_cesdata.Rdata')
