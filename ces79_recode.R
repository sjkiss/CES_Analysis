<<<<<<< HEAD
#File to Recode 1979 CES Data for 1979 election
library(tidyverse)
library(car)
library(labelled)
library(cesdata)
#load data
data("ces79")

#recode Gender (V1537)
look_for(ces79, "sex")
ces79$male<-Recode(ces79$V1537, "1=1; 2=0; 0=NA")
val_labels(ces79$male)<-c(Female=0, Male=1)
#checks
val_labels(ces79$male)
table(ces79$male)

#recode Union Household (V1514)
look_for(ces79, "union")
ces79$union<-Recode(ces79$V1514, "1=1; 2:8=0; 9=NA")
val_labels(ces79$union)<-c(None=0, Union=1)
#checks
val_labels(ces79$union)
table(ces79$union)

#recode Union Combined (V1512 and V1514)
ces79 %>% 
  mutate(union_both=case_when(
    V1512==1 | V1514==1 ~ 1,
    V1512==8 | V1512==9 ~ NA_real_,
    V1514==8 | V1514==9 ~ NA_real_,
    TRUE~ 0
  ))->ces79

val_labels(ces79$union_both)<-c(None=0, Union=1)
#checks
val_labels(ces79$union_both)
table(ces79$union_both)

#recode Education (V1502)
look_for(ces79, "school")
look_for(ces79, "degree")
ces79$degree<-Recode(ces79$V1502, "0:21=0; 22:24=1; 99=NA")
val_labels(ces79$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces79$degree)
table(ces79$degree)

#recode Region (V1005)
look_for(ces79, "province")
ces79$region<-Recode(ces79$V1005, "0:3=1; 5=2; 6:9=3; 4=NA; 99=NA")
val_labels(ces79$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces79$region)
table(ces79$region)

#recode Quebec (V1005)
look_for(ces79, "province")
ces79$quebec<-Recode(ces79$V1005, "0:3=0; 5:9=0; 4=1; 99=NA")
val_labels(ces79$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces79$quebec)
table(ces79$quebec)

#recode Age (V1535)
look_for(ces79, "age")
ces79$age<-ces79$V1535
ces79$age<-Recode(ces79$V1535, "0=NA")
#check
table(ces79$age)

#recode Religion (V1506)
look_for(ces79, "relig")
ces79$religion<-Recode(ces79$V1506, "0=0; 15=0; 1=1; 2:6=2; 7:8=1; 10:14=2; 16:25=2; 99=NA; else=3")
val_labels(ces79$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces79$religion)
table(ces79$religion)

#recode Language (V1510)
look_for(ces79, "language")
ces79$language<-Recode(ces79$V1510, "2=0; 1=1; 7=0; 4=1; else=NA")
val_labels(ces79$language)<-c(French=0, English=1)
#checks
val_labels(ces79$language)
table(ces79$language)

#recode Employment (V1471)
look_for(ces79, "employ")
look_for(ces79, "occup")
ces79$employment<-Recode(ces79$V1471, "1:6=0; 11:50=1; else=NA")
val_labels(ces79$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces79$employment)
table(ces79$employment)

#recode Sector (V1473)
look_for(ces79, "sector")
look_for(ces79, "company")
ces79$sector<-Recode(ces79$V1473, "13=1; 1:12=0; else=NA")
val_labels(ces79$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces79$sector)
table(ces79$sector)

#recode Party ID (V1192)
look_for(ces79, "federal")
ces79$party_id<-Recode(ces79$V1192, "1=1; 2=2; 3=3; 0=0; 4:7=0; else=NA")
val_labels(ces79$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces79$party_id)
table(ces79$party_id)

#recode Vote (V1234)
look_for(ces79, "vote")
ces79$vote<-Recode(ces79$V1234, "1=1; 2=2; 3=3; 4:5=0; else=NA")
val_labels(ces79$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces79$vote)
table(ces79$vote)

#recode Occupation (V410)
look_for(ces79, "occupation")
ces79$occupation<-Recode(ces79$V410, "11:12=1; 21:22=2; 30=3; 41:42=4; 43:50=5; else=NA")
val_labels(ces79$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces79$occupation)
table(ces79$occupation)

#recode Income (V1516)
look_for(ces79, "income")
ces79$income<-Recode(ces79$V1516, "1:2=1; 3=2; 4:5=3; 6:7=4; 8=5; else=NA")
val_labels(ces79$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces79$income)
table(ces79$income)
=======
#File to Recode 1979 CES Data for 1979 election
library(tidyverse)
library(car)
library(labelled)
library(cesdata)
#load data
data("ces79")

#recode Gender (V1537)
look_for(ces79, "sex")
ces79$male<-Recode(ces79$V1537, "1=1; 2=0; 0=NA")
val_labels(ces79$male)<-c(Female=0, Male=1)
#checks
val_labels(ces79$male)
table(ces79$male)

#recode Union Household (V1514)
look_for(ces79, "union")
ces79$union<-Recode(ces79$V1514, "1=1; 2:8=0; 9=NA")
val_labels(ces79$union)<-c(None=0, Union=1)
#checks
val_labels(ces79$union)
table(ces79$union)

#recode Union Combined (V1512 and V1514)
ces79 %>% 
  mutate(union_both=case_when(
    V1512==1 | V1514==1 ~ 1,
    V1512==8 | V1512==9 ~ NA_real_,
    V1514==8 | V1514==9 ~ NA_real_,
    TRUE~ 0
  ))->ces79

val_labels(ces79$union_both)<-c(None=0, Union=1)
#checks
val_labels(ces79$union_both)
table(ces79$union_both)

#recode Education (V1502)
look_for(ces79, "school")
look_for(ces79, "degree")
ces79$degree<-Recode(ces79$V1502, "0:21=0; 22:24=1; 99=NA")
val_labels(ces79$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces79$degree)
table(ces79$degree)

#recode Region (V1005)
look_for(ces79, "province")
ces79$region<-Recode(ces79$V1005, "0:3=1; 5=2; 6:9=3; 4=NA; 99=NA")
val_labels(ces79$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces79$region)
table(ces79$region)

#recode Quebec (V1005)
look_for(ces79, "province")
ces79$quebec<-Recode(ces79$V1005, "0:3=0; 5:9=0; 4=1; 99=NA")
val_labels(ces79$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces79$quebec)
table(ces79$quebec)

#recode Age (V1535)
look_for(ces79, "age")
ces79$age<-ces79$V1535
ces79$age<-Recode(ces79$V1535, "0=NA")
#check
table(ces79$age)

#recode Religion (V1506)
look_for(ces79, "relig")
ces79$religion<-Recode(ces79$V1506, "0=0; 15=0; 1=1; 2:6=2; 7:8=1; 10:14=2; 16:25=2; 99=NA; else=3")
val_labels(ces79$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces79$religion)
table(ces79$religion)

#recode Language (V1510)
look_for(ces79, "language")
ces79$language<-Recode(ces79$V1510, "2=0; 1=1; 7=0; 4=1; else=NA")
val_labels(ces79$language)<-c(French=0, English=1)
#checks
val_labels(ces79$language)
table(ces79$language)

#recode Employment (V1471)
look_for(ces79, "employ")
look_for(ces79, "occup")
ces79$employment<-Recode(ces79$V1471, "1:6=0; 11:50=1; else=NA")
val_labels(ces79$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces79$employment)
table(ces79$employment)

#recode Sector (V1473)
look_for(ces79, "sector")
look_for(ces79, "company")
ces79$sector<-Recode(ces79$V1473, "13=1; 1:12=0; else=NA")
val_labels(ces79$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces79$sector)
table(ces79$sector)

#recode Party ID (V1192)
look_for(ces79, "federal")
ces79$party_id<-Recode(ces79$V1192, "1=1; 2=2; 3=3; 0=0; 4:7=0; else=NA")
val_labels(ces79$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces79$party_id)
table(ces79$party_id)

#recode Vote (V1234)
look_for(ces79, "vote")
ces79$vote<-Recode(ces79$V1234, "1=1; 2=2; 3=3; 4:5=0; else=NA")
val_labels(ces79$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces79$vote)
table(ces79$vote)

#recode Occupation (V410)
look_for(ces79, "occupation")
ces79$occupation<-Recode(ces79$V410, "11:12=1; 21:22=2; 30=3; 41:42=4; 43:50=5; else=NA")
val_labels(ces79$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces79$occupation)
table(ces79$occupation)

#recode Income (V1516)
look_for(ces79, "income")
ces79$income<-Recode(ces79$V1516, "1:2=1; 3=2; 4:5=3; 6:7=4; 8=5; else=NA")
val_labels(ces79$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces79$income)
table(ces79$income)
>>>>>>> master
