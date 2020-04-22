#File to Recode 1988 CES Data 
library(tidyverse)
library(car)
library(labelled)
library(cesdata)
#load data
data("ces88")

#recode Gender (rsex)
look_for(ces88, "sex")
ces88$male<-Recode(ces88$rsex, "1=1; 5=0")
val_labels(ces88$male)<-c(Female=0, Male=1)
#checks
val_labels(ces88$male)
table(ces88$male)

#recode Union Household (n9)
look_for(ces88, "union")
ces88$union<-Recode(ces88$n9, "1=1; 5=0; else=NA")
val_labels(ces88$union)<-c(None=0, Union=1)
#checks
val_labels(ces88$union)
table(ces88$union)

#No Union Combined variable

#recode Education (n3)
look_for(ces88, "education")
ces88$degree<-Recode(ces88$n3, "9:11=1; 1:8=0; else=NA")
val_labels(ces88$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces88$degree)
table(ces88$degree)

#recode Region (province)
look_for(ces88, "province")
ces88$region<-Recode(ces88$province, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces88$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces88$region)
table(ces88$region)

#recode Quebec (province)
look_for(ces88, "province")
ces88$quebec<-Recode(ces88$province, "0:3=0; 5:9=0; 4=1")
val_labels(ces88$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces88$quebec)
table(ces88$quebec)

#recode Age (xn1)
look_for(ces88, "birth")
ces88$yob<-Recode(ces88$xn1, "9998:9999=NA")
ces88$age<-1988-ces88$yob
#check
table(ces88$age)

#recode Religion (n11)
look_for(ces88, "relig")
ces88$religion<-Recode(ces88$n11, "7=0; 2=1; 1=2; 8:9=NA; else=3")
val_labels(ces88$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces88$religion)
table(ces88$religion)

#recode Language (n17)
look_for(ces88, "language")
ces88$language<-Recode(ces88$n17, "1=0; 3=1; else=NA")
val_labels(ces88$language)<-c(French=0, English=1)
#checks
val_labels(ces88$language)
table(ces88$language)

#recode Employment (n5)
look_for(ces88, "employment")
ces88$employment<-Recode(ces88$n5, "2:7=0; 1=1; else=NA")
val_labels(ces88$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces88$employment)
table(ces88$employment)

#recode Sector (n8)
look_for(ces88, "sector")
look_for(ces88, "firm")
ces88$sector<-Recode(ces88$n8, "3:5=1; 1=0; else=NA")
val_labels(ces88$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces88$sector)
table(ces88$sector)

#recode Party ID (i1)
look_for(ces88, "identification")
ces88$party_id<-Recode(ces88$i1, "1=1; 2=2; 3=3; else=NA")
val_labels(ces88$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces88$party_id)
table(ces88$party_id)

#recode Vote (xb2)
look_for(ces88, "vote")
ces88$vote<-Recode(ces88$xb2, "2=1; 1=2; 3=3; 4=2; 5=0; else=NA")
val_labels(ces88$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces88$vote)
table(ces88$vote)

#recode Occupation (pinpor81)
look_for(ces88, "occupation")
ces88$occupation<-Recode(ces88$pinpor81, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
val_labels(ces88$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces88$occupation)
table(ces88$occupation)

#recode Income (n19)
look_for(ces88, "income")
ces88$income<-Recode(ces88$n19, "1:2=1; 3=2; 4=3; 5:6=4; 7:9=5; else=NA")
val_labels(ces88$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces88$income)
table(ces88$income)
