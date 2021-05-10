library(cesdata)
library(labelled)
library(tidyverse)
library(car)
#This was my messy way
look_for(ces97, "public")
#ces97$sector97<-Recode(ces97$cpsm7, "1='Private' ; 3:7='Public' ; 0=NA ; 8:9=NA", levels=c('Private', 'Public'))

#I prefer this way

ces97$public<-Recode(ces97$cpsm7, "1=0; 3:7=1; else=NA")

#add value labels
val_labels(ces97$public)<-c(public=1, private=0)

#Provide a check
table(ces97$public)
table(as_factor(ces97$cpsm7),ces97$public)
table(as_factor(ces97$public))
