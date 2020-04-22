<<<<<<< HEAD
#File to Recode 1974b CES Data for 1974 election 
library(tidyverse)
library(car)
library(labelled)
library(cesdata)
#load data
data("ces74b")

#recode Gender (V480)
look_for(ces74b, "sex")
ces74b$male<-Recode(ces74b$V480, "1=1; 2=0; 9=NA")
val_labels(ces74b$male)<-c(Female=0, Male=1)
#checks
val_labels(ces74b$male)
table(ces74b$male)

#recode Union Household (V477)
look_for(ces74b, "union")
ces74b$union<-Recode(ces74b$V477, "1=1; 2=0; 8=NA")
val_labels(ces74b$union)<-c(None=0, Union=1)
#checks
val_labels(ces74b$union)
table(ces74b$union)

#No Union Combined variable

#recode Education (V414)
look_for(ces74b, "school")
look_for(ces74b, "degree")
ces74b$degree<-Recode(ces74b$V414, "25=1; 0:13=0")
val_labels(ces74b$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces74b$degree)
table(ces74b$degree)

#recode Region (V6)
look_for(ces74b, "province")
ces74b$region<-Recode(ces74b$V6, "0:3=1; 5=2; 6:9=3; 4=NA; 99=NA")
val_labels(ces74b$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces74b$region)
table(ces74b$region)

#recode Quebec (V6)
look_for(ces74b, "province")
ces74b$quebec<-Recode(ces74b$V6, "0:3=0; 5:9=0; 4=1; 99=NA")
val_labels(ces74b$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces74b$quebec)
table(ces74b$quebec)

#recode Age (V478)
look_for(ces74b, "age")
ces74b$age<-ces74b$V478
ces74b$age<-Recode(ces74b$V478, "0=NA")
#check
table(ces74b$age)

#recode Religion (V453)
look_for(ces74b, "relig")
ces74b$religion<-Recode(ces74b$V453, "0=0; 15=0; 1=1; 2:6=2; 7:8=1; 10:14=2; 16:25=2; 27:88=NA; else=3")
val_labels(ces74b$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces74b$religion)
table(ces74b$religion)

#recode Language (V472)
look_for(ces74b, "language")
ces74b$language<-Recode(ces74b$V472, "2=0; 1=1; 7=0; 4=1; else=NA")
val_labels(ces74b$language)<-c(French=0, English=1)
#checks
val_labels(ces74b$language)
table(ces74b$language)

#recode Employment (V381)
look_for(ces74b, "employ")
look_for(ces74b, "occupation")
ces74b$employment<-Recode(ces74b$V381, "1:6=0; 11:50=1; else=NA")
val_labels(ces74b$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces74b$employment)
table(ces74b$employment)

#recode Sector (V386)
look_for(ces74b, "sector")
look_for(ces74b, "business")
ces74b$sector<-Recode(ces74b$V386, "13=1; 1:12=0; else=NA")
val_labels(ces74b$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces74b$sector)
table(ces74b$sector)

#recode Party ID (V131)
look_for(ces74b, "federal")
ces74b$party_id<-Recode(ces74b$V131, "1=1; 2=2; 3=3; 0=0; 4:5=0; else=NA")
val_labels(ces74b$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces74b$party_id)
table(ces74b$party_id)

#recode Vote (V162)
look_for(ces74b, "vote")
ces74b$vote<-Recode(ces74b$V162, "1=1; 2=2; 3=3; 4:5=0; else=NA")
val_labels(ces74b$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces74b$vote)
table(ces74b$vote)

#recode Occupation (V381)
look_for(ces74b, "occupation")
ces74b$occupation<-Recode(ces74b$V381, "11:12=1; 21:22=2; 30=3; 41:42=4; 43:50=5; else=NA")
val_labels(ces74b$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces74b$occupation)
table(ces74b$occupation)

#recode Income (V479)
look_for(ces74b, "income")
ces74b$income<-Recode(ces74b$V479, "1:2=1; 3:4=2; 5=3; 6=4; 7:8=5; else=NA")
val_labels(ces74b$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces74b$income)
table(ces74b$income)
=======
#File to Recode 1974b CES Data for 1974 election 
library(tidyverse)
library(car)
library(labelled)
library(cesdata)
#load data
data("ces74b")

#recode Gender (V480)
look_for(ces74b, "sex")
ces74b$male<-Recode(ces74b$V480, "1=1; 2=0; 9=NA")
val_labels(ces74b$male)<-c(Female=0, Male=1)
#checks
val_labels(ces74b$male)
table(ces74b$male)

#recode Union Household (V477)
look_for(ces74b, "union")
ces74b$union<-Recode(ces74b$V477, "1=1; 2=0; 8=NA")
val_labels(ces74b$union)<-c(None=0, Union=1)
#checks
val_labels(ces74b$union)
table(ces74b$union)

#No Union Combined variable

#recode Education (V414)
look_for(ces74b, "school")
look_for(ces74b, "degree")
ces74b$degree<-Recode(ces74b$V414, "25=1; 0:13=0")
val_labels(ces74b$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces74b$degree)
table(ces74b$degree)

#recode Region (V6)
look_for(ces74b, "province")
ces74b$region<-Recode(ces74b$V6, "0:3=1; 5=2; 6:9=3; 4=NA; 99=NA")
val_labels(ces74b$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces74b$region)
table(ces74b$region)

#recode Quebec (V6)
look_for(ces74b, "province")
ces74b$quebec<-Recode(ces74b$V6, "0:3=0; 5:9=0; 4=1; 99=NA")
val_labels(ces74b$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces74b$quebec)
table(ces74b$quebec)

#recode Age (V478)
look_for(ces74b, "age")
ces74b$age<-ces74b$V478
ces74b$age<-Recode(ces74b$V478, "0=NA")
#check
table(ces74b$age)

#recode Religion (V453)
look_for(ces74b, "relig")
ces74b$religion<-Recode(ces74b$V453, "0=0; 15=0; 1=1; 2:6=2; 7:8=1; 10:14=2; 16:25=2; 27:88=NA; else=3")
val_labels(ces74b$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces74b$religion)
table(ces74b$religion)

#recode Language (V472)
look_for(ces74b, "language")
ces74b$language<-Recode(ces74b$V472, "2=0; 1=1; 7=0; 4=1; else=NA")
val_labels(ces74b$language)<-c(French=0, English=1)
#checks
val_labels(ces74b$language)
table(ces74b$language)

#recode Employment (V381)
look_for(ces74b, "employ")
look_for(ces74b, "occupation")
ces74b$employment<-Recode(ces74b$V381, "1:6=0; 11:50=1; else=NA")
val_labels(ces74b$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces74b$employment)
table(ces74b$employment)

#recode Sector (V386)
look_for(ces74b, "sector")
look_for(ces74b, "business")
ces74b$sector<-Recode(ces74b$V386, "13=1; 1:12=0; else=NA")
val_labels(ces74b$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces74b$sector)
table(ces74b$sector)

#recode Party ID (V131)
look_for(ces74b, "federal")
ces74b$party_id<-Recode(ces74b$V131, "1=1; 2=2; 3=3; 0=0; 4:5=0; else=NA")
val_labels(ces74b$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces74b$party_id)
table(ces74b$party_id)

#recode Vote (V162)
look_for(ces74b, "vote")
ces74b$vote<-Recode(ces74b$V162, "1=1; 2=2; 3=3; 4:5=0; else=NA")
val_labels(ces74b$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces74b$vote)
table(ces74b$vote)

#recode Occupation (V381)
look_for(ces74b, "occupation")
ces74b$occupation<-Recode(ces74b$V381, "11:12=1; 21:22=2; 30=3; 41:42=4; 43:50=5; else=NA")
val_labels(ces74b$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces74b$occupation)
table(ces74b$occupation)

#recode Income (V479)
look_for(ces74b, "income")
ces74b$income<-Recode(ces74b$V479, "1:2=1; 3:4=2; 5=3; 6=4; 7:8=5; else=NA")
val_labels(ces74b$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces74b$income)
table(ces74b$income)
>>>>>>> master
