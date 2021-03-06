#File to Recode 1974 CES Data for 1974 election 

#load data
data("ces74")
nrow(ces74)

#recode Gender (V480)
look_for(ces74, "sex")
ces74$male<-Recode(ces74$V480, "1=1; 2=0; 9=NA")
val_labels(ces74$male)<-c(Female=0, Male=1)
#checks
val_labels(ces74$male)
table(ces74$male)

#recode Union Household (V477)
look_for(ces74, "union")
ces74$union<-Recode(ces74$V477, "1=1; 2=0; 8=0")
val_labels(ces74$union)<-c(None=0, Union=1)
#checks
val_labels(ces74$union)
table(ces74$union)
table(ces74$V476,ces74$V477)
ces74$V477
ces74$V478

#Union Combined variable (identical copy of union)
ces74$union_both<-ces74$union
#checks
val_labels(ces74$union_both)
table(ces74$union_both)

#recode Education (V414)
look_for(ces74, "school")
look_for(ces74, "degree")
ces74$degree<-Recode(ces74$V414, "25=1; 0:13=0")
val_labels(ces74$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces74$degree)
table(ces74$degree)

#recode Region (V6)
look_for(ces74, "province")
ces74$region<-Recode(ces74$V6, "0:3=1; 5=2; 6:9=3; 4=NA; 99=NA")
val_labels(ces74$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces74$region)
table(ces74$region)

#recode Quebec (V6)
look_for(ces74, "province")
ces74$quebec<-Recode(ces74$V6, "0:3=0; 5:9=0; 4=1; 99=NA")
val_labels(ces74$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces74$quebec)
table(ces74$quebec)

#recode Age (V478)
look_for(ces74, "age")
ces74$age<-ces74$V478
ces74$age<-Recode(ces74$V478, "0=NA")
#check
table(ces74$age)

#recode Religion (V453)
look_for(ces74, "relig")
ces74$religion<-Recode(ces74$V453, "0=0; 15=0; 1=1; 2:6=2; 7:8=1; 10:14=2; 16:25=2; 27:88=NA; else=3")
val_labels(ces74$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces74$religion)
table(ces74$religion)

#recode Language (V481)
look_for(ces74, "language")
ces74$language<-Recode(ces74$V481, "2=0; 1=1; else=NA")
val_labels(ces74$language)<-c(French=0, English=1)
#checks
val_labels(ces74$language)
table(ces74$language)

#recode Non-charter Language (V471)
look_for(ces74, "language")
ces74$non_charter_language<-Recode(ces74$V471, "1:2=0; 3=1; 4:7=1; else=NA")
val_labels(ces74$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces74$non_charter_language)
table(ces74$non_charter_language)

#recode Employment (V381)
look_for(ces74, "employ")
look_for(ces74, "occupation")
ces74$employment<-Recode(ces74$V381, "1:6=0; 11:50=1; else=NA")
val_labels(ces74$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces74$employment)
table(ces74$employment)

#recode Sector (V386)
look_for(ces74, "sector")
look_for(ces74, "business")
ces74 %>% 
  mutate(sector=case_when(
    V395==69.25 ~ 1,
    V395==71.77 ~ 1,
    V386==13 ~ 1,
    V386> 0 & V386 < 13 ~ 0,
    V381> 0 & V381 < 7 ~ 0,
    V381==50 ~ 0,
  ))->ces74

val_labels(ces74$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces74$sector)
table(ces74$sector)

#recode Party ID (V131)
look_for(ces74, "federal")
ces74$party_id<-Recode(ces74$V131, "1=1; 2=2; 3=3; 0=0; 4:5=0; else=NA")
val_labels(ces74$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces74$party_id)
table(ces74$party_id)

#recode Vote (V162)
look_for(ces74, "vote")
ces74$vote<-Recode(ces74$V162, "1=1; 2=2; 3=3; 4:5=0; else=NA")
val_labels(ces74$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces74$vote)
table(ces74$vote)

#recode Occupation (V381)
look_for(ces74, "occupation")
ces74$occupation<-Recode(ces74$V381, "11:12=1; 21:22=2; 30=3; 41:42=4; 43:50=5; else=NA")
val_labels(ces74$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces74$occupation)
table(ces74$occupation)

#recode Income (V479)
look_for(ces74, "income")
ces74$income<-Recode(ces74$V479, "1:2=1; 3:4=2; 5=3; 6=4; 7:8=5; else=NA")
val_labels(ces74$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces74$income)
table(ces74$income)

#recode Community Size (V9)
look_for(ces74, "community")
ces74$V480
nrow(ces74)
ces74$size<-Recode(ces74$V9, "8:9=1; 7=2; 5:6=3; 4=4; 1:3=5; else=NA")
val_labels(ces74$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces74$size)
table(ces74$size)

names(ces74)