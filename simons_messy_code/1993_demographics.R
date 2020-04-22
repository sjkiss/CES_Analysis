library(cesdata)
library(labelled)
library(tidyverse)
library(car)

data("ces93")
look_for(ces93, "sector")


#Instead of doing it this way, recode this 

#ces93$public<-recode(as.numeric(ces93$CPSJOB5), "1='Private'; 3:5='Public'", levels=c('Private', 'Public'), as.factor.result=T)
look('sector', labs97)

#I prefer this way
ces93$CPSJOB5
ces93$public<-Recode(ces93$CPSJOB5, "1=0; 3:5=1; else=NA")
val_labels(ces93$public)<-c(public=1, private=0)

#Always provide a check
table(ces93$public)
table(as_factor(ces93$public))
