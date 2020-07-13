#File to Recode 2019 CES Data 
#load data
data("ces19phone")

#recode Gender (q3)
look_for(ces19phone, "gender")
ces19phone$male<-Recode(ces19phone$q3, "1=1; 2=0; else=NA")
val_labels(ces19phone$male)<-c(Female=0, Male=1)
#checks
val_labels(ces19phone$male)
table(ces19phone$male)

#recode Union Household (p51)
look_for(ces19phone, "union")
ces19phone$union<-Recode(ces19phone$p51, "1=1; 2=0; else=NA")
val_labels(ces19phone$union)<-c(None=0, Union=1)
#checks
val_labels(ces19phone$union)
table(ces19phone$union)

#Union Combined variable (identical copy of union) ### Respondent only
ces19phone$union_both<-ces19phone$union
#checks
val_labels(ces19phone$union_both)
table(ces19phone$union_both)

#recode Education (q61)
look_for(ces19phone, "education")
ces19phone$degree<-Recode(ces19phone$q61, "9:11=1; 1:8=0; else=NA")
val_labels(ces19phone$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces19phone$degree)
table(ces19phone$degree)

#recode Region (q4)
look_for(ces19phone, "province")
ces19phone$region<-Recode(ces19phone$q4, "1:4=1; 6=2; 7:10=3; 4=NA; else=NA")
val_labels(ces19phone$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces19phone$region)
table(ces19phone$region)

#recode Quebec (q4)
look_for(ces19phone, "province")
ces19phone$quebec<-Recode(ces19phone$q4, "1:4=0; 6:10=0; 5=1; else=NA")
val_labels(ces19phone$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces19phone$quebec)
table(ces19phone$quebec)

#recode Age (age)
##Just use underlying age variable
#no missing values apparent
look_for(ces19phone, "age")
# ces19phone$age<-ces19phone$age
# summary(ces19phone$age)
# #check
# table(ces19phone$age)

#recode Religion (q62)
look_for(ces19phone, "relig")
ces19phone$religion<-Recode(ces19phone$q62, "21=0; 6=1; 8=1; 1=2; 3:4=2; 5=2; 7=2; 9=2; 12:14=2; 2=3; 5=3; 10:11=3; 15=3; 22=3; 16:20=2; 21=0; else=NA")
val_labels(ces19phone$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces19phone$religion)
table(ces19phone$religion)

#recode Language (language_CES)
look_for(ces19phone, "language")
ces19phone$language<-Recode(ces19phone$language_CES, "2=0; 1=1; else=NA")
val_labels(ces19phone$language)<-c(French=0, English=1)
#checks
val_labels(ces19phone$language)
table(ces19phone$language)

#recode Non-charter Language (q67)
look_for(ces19phone, "language")
ces19phone$non_charter_language<-Recode(ces19phone$q67, "1=0; 2:3=1; 4=0; 5:31=1; else=NA")
val_labels(ces19phone$non_charter_language)<-c(Charter=0, Non_Charter=1)
table(as_factor(ces19phone$q67),ces19phone$non_charter_language )
#checks
val_labels(ces19phone$non_charter_language)
table(ces19phone$non_charter_language)

#recode Employment (q68)
look_for(ces19phone, "employment")
ces19phone$employment<-Recode(ces19phone$q68, "3:8=0; 1:2=1; 9:11=1; else=NA")
val_labels(ces19phone$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces19phone$employment)
table(ces19phone$employment)

#recode Sector (p53 & q68)
look_for(ces19phone, "public")
ces19phone %>% 
  mutate(sector=case_when(
    p53==1 ~1,
    p53==2 ~0,
    p53==3 ~0,
    p53==4 ~0,
    q68>2 & q68< 12 ~ 0,
    p53==-9 ~NA_real_ ,
    p53==-8 ~NA_real_ ,
  ))->ces19phone

val_labels(ces19phone$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces19phone$sector)
table(ces19phone$sector)

#recode Party ID (p47)
look_for(ces19phone, "closest")
ces19phone$party_id<-Recode(ces19phone$p47, "1=1; 2=2; 3=3; 4:7=0; else=NA")
val_labels(ces19phone$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces19phone$party_id)
table(ces19phone$party_id)

#recode Vote (p3)
look_for(ces19phone, "party did you vote")
ces19phone$vote<-Recode(ces19phone$p3, "1=1; 2=2; 3=3; 4=4; 5=5; 6:7=0; else=NA")
val_labels(ces19phone$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$vote)
table(ces19phone$vote)

#recode Occupation (p52 to NOC)
look_for(ces19phone, "occupation")
ces19phone$occupation<-Recode(ces19phone$NOC, "0:1000=2; 1100:1199=1; 2100:3300=1; 4100:6399=1; 1200:1400=3; 6400:6800=3; 3400:3500=3; 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
val_labels(ces19phone$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces19phone$occupation)
table(ces19phone$occupation)

#recode Income (q70r)
look_for(ces19phone, "income")
ces19phone$income<-Recode(ces19phone$q70r, "1:2=1; 3=2; 4=3; 5:6=4; 7:8=5; else=NA")
val_labels(ces19phone$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces19phone$income)
table(ces19phone$income)

#recode Immigration sentiment (p22_a, p22_b, p22_c) into an index 0-1
#1 = pro-immigration sentiment 0 = anti-immigration sentiment
look_for(ces19phone, "immigr")
ces19phone$immigration_economy<-Recode(ces19phone$p22_a, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; else=NA")
val_labels(ces19phone$immigration_economy)<-c(Negative=0, Somewhat_negative=0.25, Neither=0.5, Somewhat_positive=0.75, Positive=1)
#checks
val_labels(ces19phone$immigration_economy)
table(ces19phone$immigration_economy)

ces19phone$immigration_culture<-Recode(ces19phone$p22_b, "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
val_labels(ces19phone$immigration_culture)<-c(Negative=0, Somewhat_negative=0.25, Neither=0.5, Somewhat_positive=0.75, Positive=)
#checks
val_labels(ces19phone$immigration_culture)
table(ces19phone$immigration_culture)

ces19phone$immigration_crime<-Recode(ces19phone$p22_c, "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
val_labels(ces19phone$immigration_crime)<-c(Negative=0, Somewhat_negative=0.25, Neither=0.5, Somewhat_positive=0.75, Positive=)
#checks
val_labels(ces19phone$immigration_crime)
table(ces19phone$immigration_crime)

#Combine the 3 immigration variables and divide by 3
ces19phone$immigration2<-(ces19phone$immigration_economy + ces19phone$immigration_culture + ces19phone$immigration_crime)
table(ces19phone$immigration2)
ces19phone$immigration<-(ces19phone$immigration2 /3)
table(ces19phone$immigration)

#Calculate Cronbach's alpha
library(psych)

ces19phone %>% 
  select(immigration_economy:immigration_crime) %>% 
  alpha(.)

ces19phone %>% 
  select(p22_a:p22_c) %>% 
  alpha(.)
