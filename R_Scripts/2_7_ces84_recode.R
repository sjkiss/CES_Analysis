#File to Recode 1984 CES Data 
#load data
data("ces84")

#recode Gender (VAR456)
look_for(ces84, "sex")
ces84$male<-Recode(ces84$VAR456, "1=1; 2=0")
val_labels(ces84$male)<-c(Female=0, Male=1)
#checks
val_labels(ces84$male)
table(ces84$male)

#recode Union Respondent (VAR381)
look_for(ces84, "union")
table(ces84$VAR381)
ces84$union<-Recode(ces84$VAR381, "1=1; 2:8=0; else=NA")
val_labels(ces84$union)<-c(None=0, Union=1)
#checks
val_labels(ces84$union)
table(ces84$union)
table(ces84$VAR378, ces84$VAR381)
ces84$VAR378

#recode Union Combined (VAR378 and VAR381)
ces84 %>% 
  mutate(union_both=case_when(
    VAR378==1 | VAR381==1 ~ 1,
    VAR378==2 | VAR381==2 ~ 0, 
    VAR378==8  ~ NA_real_,
    VAR381==8  ~ NA_real_
  ))->ces84

val_labels(ces84$union_both)<-c(None=0, Union=1)
#checks
val_labels(ces84$union_both)
table(ces84$union_both)

#recode Education (VAR262)
look_for(ces84, "education")
ces84$degree<-Recode(ces84$VAR362, "8=1; 1:7=0; 9=0; else=NA")
val_labels(ces84$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces84$degree)
table(ces84$degree)

#recode Region (VAR003)
look_for(ces84, "region")
ces84$region<-Recode(ces84$VAR003, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces84$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces84$region)
table(ces84$region)

#recode Quebec (VAR003)
look_for(ces84, "region")
ces84$quebec<-Recode(ces84$VAR003, "0:3=0; 5:9=0; 4=1")
val_labels(ces84$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces84$quebec)
table(ces84$quebec)

#recode Age (VAR437)
look_for(ces84, "age")
ces84$age<-Recode(ces84$VAR437, "0=NA")
#check
table(ces84$age)

#recode Religion (VAR371)
look_for(ces84, "relig")
ces84$religion<-Recode(ces84$VAR371, "0=0; 34=0; 1=1; 2:6=2; 7:8=1; 10:29=2; 88=NA; else=3")
val_labels(ces84$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces84$religion)
table(ces84$religion)

#recode Language (VAR457)
look_for(ces84, "language")
ces84$language<-Recode(ces84$VAR457, "2=0; 1=1; else=NA")
val_labels(ces84$language)<-c(French=0, English=1)
#checks
val_labels(ces84$language)
table(ces84$language)

#recode Non-charter Language (VAR375)
look_for(ces84, "language")
ces84$non_charter_language<-Recode(ces84$VAR375, "1:5=0; 6:7=1; else=NA")
val_labels(ces84$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces84$non_charter_language)
table(ces84$non_charter_language)

#recode Employment (VAR524)
look_for(ces84, "employment")
ces84$employment<-Recode(ces84$VAR524, "2:6=0; 1=1; else=NA")
val_labels(ces84$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces84$employment)
table(ces84$employment)

#recode Sector (VAR530, VAR526, VAR525 & VAR524)
look_for(ces84, "company")
look_for(ces84, "occupation")
look_for(ces84, "employment")

ces84 %>% 
  mutate(sector=case_when(
    #what does your company do (government)
    VAR530==13 ~1,
    # VAR530<13~0,
    #     VAR524 >1 ~ 0,
    #assume these are teachers and nurses
      VAR526> 2710 & VAR526 < 2800 ~ 1,
    VAR526> 3129 & VAR526 < 3136 ~ 1,
 #   VAR530==99 ~NA_real_ ,
 #all else gets as per Blais' footnote (reading between the lines)
    TRUE ~ 0
  ))->ces84

table(as_factor(ces84$VAR524), ces84$sector, useNA='ifany')
table(as_factor(ces84$VAR524), as_factor(ces84$VAR530), useNA="ifany")
val_labels(ces84$sector)<-c(Private=0, Public=1)
ces84$sector
#checks
table(as_factor(ces84$VAR530), as_factor(ces84$sector))
# ces84 %>% 
#   filter(VAR526>2710 & VAR526< 3000) %>% 
#   filter(VAR526>3129 & VAR526< 3136) %>%
#   select(VAR526, sector) 

val_labels(ces84$sector)
table(ces84$sector)

#recode Party ID (VAR081)
look_for(ces84, "fed. id")
ces84$party_id<-Recode(ces84$VAR081, "1=1; 2=2; 3=3; 0=0; 4:19=0; else=NA")
val_labels(ces84$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces84$party_id)
table(ces84$party_id)

#recode Vote (VAR125)
look_for(ces84, "vote")
ces84$vote<-Recode(ces84$VAR125, "1=1; 2=2; 3=3; 9=5; 15=2; 4:8=0; 10:12=0; 14=0; 16:20=0; else=NA")
val_labels(ces84$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces84$vote)
table(ces84$vote)

#recode Occupation (VAR525)
look_for(ces84, "occupation")
ces84$occupation<-Recode(ces84$VAR525, "1=1; 2=2; 3:4=3; 5=4; 6:7=5; else=NA")
val_labels(ces84$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces84$occupation)
table(ces84$occupation)

#recode Occupation3 as 6 class schema with self-employed (VAR533)
look_for(ces84, "employed")
ces84$occupation3<-ifelse(ces84$VAR533==1, 6, ces84$occupation)
val_labels(ces84$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces84$occupation3)
table(ces84$occupation3)

#recode Income (VAR442 and VAR443)
look_for(ces84, "income")
ces84 %>% 
  mutate(income=case_when(
    VAR442==1 | VAR443==1 ~ 1,
    VAR442==2 | VAR443==2 ~ 1,
    VAR442==3 | VAR443==3 ~ 1,
    VAR442==4 | VAR443==4 ~ 2,
    VAR442==5 | VAR443==5 ~ 2,
    VAR442==6 | VAR443==6 ~ 3,
    VAR442==7 | VAR443==7 ~ 3,
    VAR442==8 | VAR443==8 ~ 4,
    VAR442==9 | VAR443==9 ~ 5,
    VAR442==10 | VAR443==10 ~ 5,
    VAR442==11 | VAR443==11 ~ 5,
  ))->ces84

val_labels(ces84$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces84$income)
table(ces84$income)

#recode Community Size (VAR464)
look_for(ces84, "community")
look_for(ces84, "city")
ces84$size<-Recode(ces84$VAR464, "6=1; 5=2; 3:4=3; 2=4; 1=5; else=NA")
val_labels(ces84$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces84$size)
table(ces84$size)

#recode Religiosity (VAR372)
look_for(ces84, "church")
ces84$religiosity<-Recode(ces84$VAR372, "5=1; 4=2; 3=3; 2=4; 1=5; else=NA")
val_labels(ces84$religiosity)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces84$religiosity)
table(ces84$religiosity)

#recode Liberal leader (VAR301)
look_for(ces84, "Turner")
ces84$liberal_leader<-Recode(ces84$VAR301, "0=1; 997:999=NA")
#checks
table(ces84$liberal_leader)

#recode conservative leader (VAR302)
look_for(ces84, "Mulroney")
ces84$conservative_leader<-Recode(ces84$VAR302, "0=1; 997:999=NA")
#checks
table(ces84$conservative_leader)

#recode NDP leader (VAR303)
look_for(ces84, "Broadbent")
ces84$ndp_leader<-Recode(ces84$VAR303, "0=1; 997:999=NA")
#checks
table(ces84$ndp_leader)
