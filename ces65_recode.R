<<<<<<< HEAD
#File to Recode 1965 CES Data 
library(tidyverse)
library(car)
library(labelled)
library(cesdata)
#load data
data("ces65")

#recode Gender (v337)
look_for(ces65, "sex")
ces65$male<-Recode(ces65$v337, "1=1; 2=0")
val_labels(ces65$male)<-c(Female=0, Male=1)
#checks
val_labels(ces65$male)
table(ces65$male)

#No Union Household variable

#No Union Combined variable

#recode Education (v307)
look_for(ces65, "school")
ces65$degree<-Recode(ces65$v307, "16:30=1; 0:15=0")
val_labels(ces65$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces65$degree)
table(ces65$degree)

#recode Region (v5)
look_for(ces65, "province")
ces65$region<-Recode(ces65$v5, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces65$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces65$region)
table(ces65$region)

#recode Quebec (v5)
look_for(ces65, "province")
ces65$quebec<-Recode(ces65$v5, "0:3=0; 5:9=0; 4=1")
val_labels(ces65$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces65$quebec)
table(ces65$quebec)

#recode Age (v335)
look_for(ces65, "age")
ces65$age<-ces65$v335
#check
table(ces65$age)

#recode Religion (v309)
look_for(ces65, "church")
ces65$religion<-Recode(ces65$v309, "0=0; 10:19=2; 20=1; 70:71=1; else=3")
val_labels(ces65$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces65$religion)
table(ces65$religion)

#recode Language (v314)
look_for(ces65, "language")
ces65$language<-Recode(ces65$v314, "2=0; 1=1; else=NA")
val_labels(ces65$language)<-c(French=0, English=1)
#checks
val_labels(ces65$language)
table(ces65$language)

#recode Employment (v54)
look_for(ces65, "employ")
ces65$employment<-Recode(ces65$v54, "2=1; 1=0; else=NA")
val_labels(ces65$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces65$employment)
table(ces65$employment)

#No Sector variable

#recode Party ID (v221)
look_for(ces65, "identification")
ces65$party_id<-Recode(ces65$v221, "20=1; 10=2; 30=3; 40:60=0; else=NA")
val_labels(ces65$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces65$party_id)
table(ces65$party_id)

#recode Vote (v262)
look_for(ces65, "vote")
ces65$vote<-Recode(ces65$v262, "12=1; 11=2; 13=3; 14:19=0; else=NA")
val_labels(ces65$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces65$vote)
table(ces65$vote)

#recode Occupation (v306)
look_for(ces65, "occupation")
ces65$occupation<-Recode(ces65$v306, "10=1; 20=2; 32:37=3; 69=3; 40=4; 70:80=5; else=NA")
val_labels(ces65$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces65$occupation)
table(ces65$occupation)

#recode Income (v336)
look_for(ces65, "income")
ces65$income<-Recode(ces65$v336, "1:3=1; 4:5=2; 6=3; 7:8=4; 9:11=5; else=NA")
val_labels(ces65$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces65$income)
table(ces65$income)
=======
#File to Recode 1965 CES Data 
library(tidyverse)
library(car)
library(labelled)
library(cesdata)
#load data
data("ces65")

#recode Gender (v337)
look_for(ces65, "sex")
ces65$male<-Recode(ces65$v337, "1=1; 2=0")
val_labels(ces65$male)<-c(Female=0, Male=1)
#checks
val_labels(ces65$male)
table(ces65$male)

#No Union Household variable

#No Union Combined variable

#recode Education (v307)
look_for(ces65, "school")
ces65$degree<-Recode(ces65$v307, "16:30=1; 0:15=0")
val_labels(ces65$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces65$degree)
table(ces65$degree)

#recode Region (v5)
look_for(ces65, "province")
ces65$region<-Recode(ces65$v5, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces65$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces65$region)
table(ces65$region)

#recode Quebec (v5)
look_for(ces65, "province")
ces65$quebec<-Recode(ces65$v5, "0:3=0; 5:9=0; 4=1")
val_labels(ces65$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces65$quebec)
table(ces65$quebec)

#recode Age (v335)
look_for(ces65, "age")
ces65$age<-ces65$v335
#check
table(ces65$age)

#recode Religion (v309)
look_for(ces65, "church")
ces65$religion<-Recode(ces65$v309, "0=0; 10:19=2; 20=1; 70:71=1; else=3")
val_labels(ces65$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces65$religion)
table(ces65$religion)

#recode Language (v314)
look_for(ces65, "language")
ces65$language<-Recode(ces65$v314, "2=0; 1=1; else=NA")
val_labels(ces65$language)<-c(French=0, English=1)
#checks
val_labels(ces65$language)
table(ces65$language)

#recode Employment (v54)
look_for(ces65, "employ")
ces65$employment<-Recode(ces65$v54, "2=1; 1=0; else=NA")
val_labels(ces65$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces65$employment)
table(ces65$employment)

#No Sector variable

#recode Party ID (v221)
look_for(ces65, "identification")
ces65$party_id<-Recode(ces65$v221, "20=1; 10=2; 30=3; 40:60=0; else=NA")
val_labels(ces65$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces65$party_id)
table(ces65$party_id)

#recode Vote (v262)
look_for(ces65, "vote")
ces65$vote<-Recode(ces65$v262, "12=1; 11=2; 13=3; 14:19=0; else=NA")
val_labels(ces65$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces65$vote)
table(ces65$vote)

#recode Occupation (v306)
look_for(ces65, "occupation")
ces65$occupation<-Recode(ces65$v306, "10=1; 20=2; 32:37=3; 69=3; 40=4; 70:80=5; else=NA")
val_labels(ces65$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces65$occupation)
table(ces65$occupation)

#recode Income (v336)
look_for(ces65, "income")
ces65$income<-Recode(ces65$v336, "1:3=1; 4:5=2; 6=3; 7:8=4; 9:11=5; else=NA")
val_labels(ces65$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces65$income)
table(ces65$income)
>>>>>>> master
