#File to Recode 2000 CES Data 
#load data
data("ces00")

#recode Gender (cpsrgen)
look_for(ces00, "gender")
ces00$male<-Recode(ces00$cpsrgen, "1=1; 5=0")
val_labels(ces00$male)<-c(Female=0, Male=1)
#checks
val_labels(ces00$male)
table(ces00$male)

#recode Union Household (cpsm9)
look_for(ces00, "union")
ces00$union<-Recode(ces00$cpsm9, "1=1; 5=0; else=NA")
val_labels(ces00$union)<-c(None=0, Union=1)
#checks
val_labels(ces00$union)
table(ces00$union)

#Union Combined variable (identical copy of union)
ces00$union_both<-ces00$union
#checks
val_labels(ces00$union_both)
table(ces00$union_both)

#recode Education (cpsm3)
look_for(ces00, "education")
ces00$degree<-Recode(ces00$cpsm3, "9:11=1; 1:8=0; else=NA")
val_labels(ces00$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces00$degree)
table(ces00$degree)

#recode Region (province)
look_for(ces00, "province")
ces00$region<-Recode(ces00$province, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces00$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces00$region)
table(ces00$region)

#recode Quebec (province)
look_for(ces00, "province")
ces00$quebec<-Recode(ces00$province, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces00$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces00$quebec)
table(ces00$quebec)

#recode Age (cpsage)
look_for(ces00, "age")
ces00$yob<-Recode(ces00$cpsage, "9999=NA")
ces00$age<-2000-ces00$yob
#check
table(ces00$age)

#recode Religion (cpsm10)
look_for(ces00, "relig")
ces00$religion<-Recode(ces00$cpsm10, "0=0; 2=1; 1=2; 3:5=3; else=NA")
val_labels(ces00$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces00$religion)
table(ces00$religion)

#recode Language (cpslang)
look_for(ces00, "language")
ces00$language<-Recode(ces00$cpslang, "1=1; 2=0; else=NA")
val_labels(ces00$language)<-c(French=0, English=1)
#checks
val_labels(ces00$language)
table(ces00$language)

#recode Non-charter Language (cpsm15)
look_for(ces00, "language")
ces00$non_charter_language<-Recode(ces00$cpsm15, "1:5=0; 0=1; else=NA")
val_labels(ces00$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces00$non_charter_language)
table(ces00$non_charter_language)

#recode Employment (cpsm4)
look_for(ces00, "employ")
ces00$employment<-Recode(ces00$cpsm4, "4:8=0; 1:3=1; else=NA")
val_labels(ces00$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces00$employment)
table(ces00$employment)

#recode Sector (cpsm7 & cpsm4)
look_for(ces00, "company")
ces00 %>% 
  mutate(sector=case_when(
    cpsm7==3 ~1,
    cpsm7==5 ~1,
    cpsm7==7 ~1,
    cpsm7==1 ~0,
    cpsm7==0 ~0,
    cpsm4==1 ~0,
    cpsm4> 3 & cpsm4< 9 ~ 0,
    cpsm7==9 ~NA_real_ ,
    cpsm7==8 ~NA_real_ ,
  ))->ces00

#ces00$sector2<-Recode(ces00$cpsm7, "3:7=1; 0:1=0; else=NA")

val_labels(ces00$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces00$sector)
table(ces00$sector)

#recode Party ID (pesk1a and pesk1ab)
look_for(ces00, "yourself")
ces00 %>% 
  mutate(party_id=case_when(
    pesk1a==1 | pesk1b==1 ~ 1,
    pesk1a==2 | pesk1b==2 ~ 2,
    pesk1a==3 | pesk1b==3 ~ 3,
    pesk1a==4 | pesk1b==4 ~ 2,
    pesk1a==0 | pesk1b==0 ~ 0,
    pesk1a==5 | pesk1b==5 ~ 0,
  ))->ces00

val_labels(ces00$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces00$party_id)
table(ces00$party_id)

#recode Vote (pesa3a and pesa3b)
look_for(ces00, "vote")
ces00 %>% 
  mutate(vote=case_when(
    pesa3a==1 | pesa3b==1 ~ 1,
    pesa3a==2 | pesa3b==2 ~ 2,
    pesa3a==3 | pesa3b==3 ~ 3,
    pesa3a==4 | pesa3b==4 ~ 2,
    pesa3a==0 | pesa3b==0 ~ 0,
    pesa3a==5 | pesa3b==5 ~ 4,
  ))->ces00

val_labels(ces00$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces00$vote)
table(ces00$vote)

# No occupation variable

#recode Income (cpsm16 and cpsm16a)
look_for(ces00, "income")
ces00 %>% 
  mutate(income=case_when(
    cpsm16a==1 | cpsm16> -1 & cpsm16 < 20 ~ 1,
    cpsm16a==2 | cpsm16> 19 & cpsm16 < 40 ~ 2,
    cpsm16a==3 | cpsm16> 19 & cpsm16 < 40 ~ 2,
    cpsm16a==4 | cpsm16> 39 & cpsm16 < 60 ~ 3,
    cpsm16a==5 | cpsm16> 39 & cpsm16 < 60 ~ 3,
    cpsm16a==6 | cpsm16> 59 & cpsm16 < 80 ~ 4,
    cpsm16a==7 | cpsm16> 59 & cpsm16 < 80 ~ 4,
    cpsm16a==8 | cpsm16> 79 & cpsm16 < 998 ~ 5,
    cpsm16a==9 | cpsm16> 79 & cpsm16 < 998 ~ 5,
    cpsm16a==10 | cpsm16> 79 & cpsm16 < 998 ~ 5,
  ))->ces00

val_labels(ces00$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces00$income)
table(ces00$income)
look_for(ces00, "noc")
look_for(ces00, "employment")
look_for(ces00, "career")
ces00$bycat_15
