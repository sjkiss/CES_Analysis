
library(cesdata)
library(labelled)
library(tidyverse)
library(car)

#this is my messy way
look_for(ces00, 'private')
ces00$CPSM7
ces00$sector00<-recode(as.numeric(ces00$CPSM7), "1='Private' ; 3:7='Public' ; 0=NA ; 8:9=NA", levels=c('Private', 'Public'))

#I prefer this way

ces00$public<-Recode(ces00$cpsm7, "1=0; 3:7=1; else=NA")
#Aadd value labels
val_labels(ces00$public)<-c(public=1, private=0)

table(as_factor(ces00$cpsm7), as_factor(ces00$public))
