#File to Recode 1974 CES Data for 1980 election
library(tidyverse)
library(car)
library(labelled)
library(cesdata)
#load data
data("ces74")

#recode Gender (V2156)
look_for(ces74, "sex")
ces74$male80<-Recode(ces74$V2156, "1=1; 2=0; 0=NA")
val_labels(ces74$male80)<-c(Female=0, Male=1)
#checks
val_labels(ces74$male80)
table(ces74$male80)

#No Union Household variable

#No Union Combined variable

#No Education variable

#recode Region (V2002)
look_for(ces74, "province")
ces74$region80<-Recode(ces74$V2002, "0:3=1; 5=2; 6:9=3; 4=NA; 99=NA")
val_labels(ces74$region80)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces74$region80)
table(ces74$region80)

#recode Quebec (V2002)
look_for(ces74, "province")
ces74$quebec80<-Recode(ces74$V2002, "0:3=0; 5:9=0; 4=1; 99=NA")
val_labels(ces74$quebec80)<-c(Other=0, Quebec=1)
#checks
val_labels(ces74$quebec80)
table(ces74$quebec80)

#recode Age (V2155)
look_for(ces74, "age")
ces74$age80<-ces74$V1535
ces74$age80<-Recode(ces74$V2155, "0=NA")
#check
table(ces74$age80)

#No Religion variable

#Recode Language (V2013)
look_for(ces74, "language")
ces74$language80<-Recode(ces74$V2013, "2=0; 1=1; 0=NA")
val_labels(ces74$language80)<-c(French=0, English=1)
#checks
val_labels(ces74$language80)
table(ces74$language80)

#No Employment variable

#No Sector variable

#recode Party ID (V2043)
look_for(ces74, "federal")
ces74$party_id80<-Recode(ces74$V2043, "1=1; 2=2; 3=3; 0=0; 4:7=0; else=NA")
val_labels(ces74$party_id80)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces74$party_id80)
table(ces74$party_id80)

#recode Vote (V2062)
look_for(ces74, "vote")
ces74$vote80<-Recode(ces74$V2062, "1=1; 2=2; 3=3; 4:5=0; else=NA")
val_labels(ces74$vote80)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces74$vote80)
table(ces74$vote80)

# No Occupation variable

# No Income variable
