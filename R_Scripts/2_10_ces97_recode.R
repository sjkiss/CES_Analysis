#File to Recode 1997 CES Data 
#load data
data("ces97")

#recode Gender (cpsrgen)
look_for(ces97, "gender")
ces97$male<-Recode(ces97$cpsrgen, "1=1; 5=0")
val_labels(ces97$male)<-c(Female=0, Male=1)
#checks
val_labels(ces97$male)
table(ces97$male)

#recode Union Household (cpsm9)
look_for(ces97, "union")
ces97$union<-Recode(ces97$cpsm9, "1=1; 5=0; else=NA")
val_labels(ces97$union)<-c(None=0, Union=1)
#checks
val_labels(ces97$union)
table(ces97$union)

#Union Combined variable (identical copy of union)
ces97$union_both<-ces97$union
#checks
val_labels(ces97$union_both)
table(ces97$union_both)

#recode Education (cpsm3)
look_for(ces97, "education")
ces97$degree<-Recode(ces97$cpsm3, "9:11=1; 1:8=0; else=NA")
val_labels(ces97$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces97$degree)
table(ces97$degree)

#recode Region (province)
look_for(ces97, "province")
ces97$region<-Recode(ces97$province, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces97$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces97$region)
table(ces97$region)

#recode Quebec (province)
look_for(ces97, "province")
ces97$quebec<-Recode(ces97$province, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces97$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces97$quebec)
table(ces97$quebec)

#recode Age (cpsage)
look_for(ces97, "age")
ces97$yob<-Recode(ces97$cpsage, "9999=NA")
ces97$age<-1997-ces97$yob
#check
table(ces97$age)

#recode Religion (cpsm10)
look_for(ces97, "relig")
ces97$religion<-Recode(ces97$cpsm10, "0=0; 2=1; 1=2; 3:5=3; else=NA")
val_labels(ces97$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces97$religion)
table(ces97$religion)

#recode Language (cpslang)
look_for(ces97, "language")
ces97$language<-Recode(ces97$cpslang, "1=1; 2=0; else=NA")
val_labels(ces97$language)<-c(French=0, English=1)
#checks
val_labels(ces97$language)
table(ces97$language)

#recode Non-charter Language (cpsm15)
look_for(ces97, "language")
ces97$non_charter_language<-Recode(ces97$cpsm15, "1:5=0; 0=1; 10:27=1; else=NA")
val_labels(ces97$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces97$non_charter_language)
table(ces97$non_charter_language)

#recode Employment (cpsm4)
look_for(ces97, "employment")
ces97$employment<-Recode(ces97$cpsm4, "2:7=0; 1=1; 8=1; else=NA")
val_labels(ces97$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces97$employment)
table(ces97$employment)

#recode Sector (cpsm7 & cpsm4)
look_for(ces97, "firm")
ces97 %>% 
  mutate(sector=case_when(
    cpsm7==3 ~1,
    cpsm7==5 ~1,
    cpsm7==7 ~1,
    cpsm7==1 ~0,
    cpsm7==0 ~0,
    cpsm4> 1 & cpsm4< 9 ~ 0,
    cpsm7==9 ~NA_real_ ,
    cpsm7==8 ~NA_real_ ,
  ))->ces97

val_labels(ces97$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces97$sector)
table(ces97$sector)

#recode Party ID (cpsk1 and cpsk4)
look_for(ces97, "federal")
ces97 %>% 
  mutate(party_id=case_when(
    cpsk1==1 | cpsk4==1 ~ 1,
    cpsk1==2 | cpsk4==2 ~ 2,
    cpsk1==3 | cpsk4==3 ~ 3,
    cpsk1==4 | cpsk4==4 ~ 2,
    cpsk1==5 | cpsk4==5 ~ 0,
    cpsk1==6 | cpsk4==6 ~ 0,
  ))->ces97

val_labels(ces97$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces97$party_id)
table(ces97$party_id)

#recode Vote (pesa4)
look_for(ces97, "vote")
ces97$vote<-Recode(ces97$pesa4, "1=1; 2=2; 3=3; 5=4; 4=2; 0=0; else=NA")
val_labels(ces97$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces97$vote)
table(ces97$vote)

#recode Occupation (pinporr)
look_for(ces97, "occupation")
look_for(ces97, "pinporr")
ces97$occupation<-Recode(ces97$pinporr, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
val_labels(ces97$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces97$occupation)
table(ces97$occupation)

#recode Occupation3 as 6 class schema with self-employed (cpsm4)
look_for(ces97, "employ")
ces97$occupation3<-ifelse(ces97$cpsm4==8, 6, ces97$occupation)
val_labels(ces97$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces97$occupation3)
table(ces97$occupation3)

#recode Income (cpsm16 and cpsm16a)
look_for(ces97, "income")
ces97 %>% 
  mutate(income=case_when(
    cpsm16a==1 | cpsm16> 0 & cpsm16 < 20 ~ 1,
    cpsm16a==2 | cpsm16> 19 & cpsm16 < 30 ~ 2,
    cpsm16a==3 | cpsm16> 29 & cpsm16 < 50 ~ 3,
    cpsm16a==4 | cpsm16> 29 & cpsm16 < 50 ~ 3,
    cpsm16a==5 | cpsm16> 49 & cpsm16 < 70 ~ 4,
    cpsm16a==6 | cpsm16> 49 & cpsm16 < 70 ~ 4,
    cpsm16a==7 | cpsm16> 69 & cpsm16 < 998 ~ 5,
    cpsm16a==8 | cpsm16> 69 & cpsm16 < 998 ~ 5,
    cpsm16a==9 | cpsm16> 69 & cpsm16 < 998 ~ 5,
    cpsm16a==10 | cpsm16> 69 & cpsm16 < 998 ~ 5,
  ))->ces97

val_labels(ces97$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces97$income)
table(ces97$income)

#recode Redistribution (mbsa4)
look_for(ces97, "rich")
val_labels(ces97$mbsa4)
ces97$redistribution<-Recode(ces97$mbsa4, "; 1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces97$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces97$redistribution)
table(ces97$redistribution)

#recode Pro-Redistribution (mbsa4)
ces97$pro_redistribution<-Recode(ces97$mbsa4, "1:2=1; 3:4=0; else=NA", as.numeric=T)
val_labels(ces97$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces97$pro_redistribution)
table(ces97$pro_redistribution)