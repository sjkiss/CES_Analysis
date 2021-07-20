
#File to Recode 2015 CES Data 
data("ces15phone")
#recode Gender (RGENDER)
look_for(ces15phone, "gender")
ces15phone$male<-Recode(ces15phone$RGENDER, "1=1; 5=0")
val_labels(ces15phone$male)<-c(Female=0, Male=1)
#checks
val_labels(ces15phone$male)
table(ces15phone$male)

#recode Union Respondent (PES15_93)
look_for(ces15phone, "union")
ces15phone$union<-Recode(ces15phone$PES15_93, "1=1; 5=0; else=NA")
val_labels(ces15phone$union)<-c(None=0, Union=1)
#checks
val_labels(ces15phone$union)
table(ces15phone$union)

#recode Union Combined (PES15_93 and PES15_94)
ces15phone %>% 
  mutate(union_both=case_when(
    PES15_93==1 | PES15_94==1 ~ 1,
    PES15_93==5 | PES15_94==5 ~ 0,
    PES15_93==8 & PES15_94==8 ~ NA_real_,
    PES15_93==9 & PES15_94==9 ~ NA_real_,
  ))->ces15phone

val_labels(ces15phone$union_both)<-c(None=0, Union=1)
#checks
val_labels(ces15phone$union_both)
table(ces15phone$union_both, useNA="ifany")
table(ces15phone$PES15_93, ces15phone$union_both, useNA="ifany")
table(ces15phone$PES15_93, ces15phone$PES15_94, useNA="ifany")
table(ces15phone$union_both)
table(ces15phone$PES15_93, ces15phone$union_both, useNA="ifany")
table(ces15phone$PES15_94, ces15phone$union_both, useNA="ifany")

#recode Education (CPS15_79)
look_for(ces15phone, "education")
ces15phone$degree<-Recode(ces15phone$CPS15_79, "9:11=1; 1:8=0; else=NA")
val_labels(ces15phone$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces15phone$degree)
table(ces15phone$degree)

#recode Region (CPS15_PROVINCE)
look_for(ces15phone, "province")
ces15phone$region<-Recode(ces15phone$CPS15_PROVINCE, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces15phone$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces15phone$region)
table(ces15phone$region)

#recode Quebec (CPS15_PROVINCE)
look_for(ces15phone, "province")
ces15phone$quebec<-Recode(ces15phone$CPS15_PROVINCE, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces15phone$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces15phone$quebec)
table(ces15phone$quebec)

#recode Age (CPS15_78)
look_for(ces15phone, "age")
ces15phone$yob<-Recode(ces15phone$CPS15_78, "9998:9999=NA")
ces15phone$age<-2015-ces15phone$yob
#check
table(ces15phone$age, useNA = "ifany" )
#recode Age2 (0-1 variable)
ces15phone$age2<-(ces15phone$age /100)
#checks
table(ces15phone$age2)
#recode Religion (CPS15_80)
look_for(ces15phone, "relig")
ces15phone$religion<-Recode(ces15phone$CPS15_80, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:20=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")
val_labels(ces15phone$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces15phone$religion)
table(ces15phone$religion, useNA = "ifany" )

#recode Language (CPS15_INTLANG)
look_for(ces15phone, "language")
ces15phone$language<-Recode(ces15phone$CPS15_INTLANG, "5=0; 1=1; else=NA")
val_labels(ces15phone$language)<-c(French=0, English=1)
#checks
val_labels(ces15phone$language)
table(ces15phone$language, useNA = "ifany")

#recode Non-charter Language (CPS15_90)
look_for(ces15phone, "language")
ces15phone$non_charter_language<-Recode(ces15phone$CPS15_90, "1:5=0; 8:64=1; 65=0; 95:97=1; else=NA")
val_labels(ces15phone$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces15phone$non_charter_language)
table(ces15phone$non_charter_language)

#recode Employment (CPS15_91)
look_for(ces15phone, "employment")
ces15phone$employment<-Recode(ces15phone$CPS15_91, "3:7=0; 1:2=1; 8:11=1; else=NA")
val_labels(ces15phone$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces15phone$employment)
table(ces15phone$employment, useNA = "ifany" )

#recode Sector (PES15_92 & CPS15_91)
look_for(ces15phone, "company")
look_for(ces15phone, "private")
ces15phone %>% 
  mutate(sector=case_when(
    PES15_92==5 ~1,
    PES15_92==1 ~0,
    PES15_92==0 ~0,
    CPS15_91==1 ~0,
    CPS15_91>2 & CPS15_91< 12 ~ 0,
    PES15_92==9 ~NA_real_ ,
    PES15_92==8 ~NA_real_ ,
  ))->ces15phone

val_labels(ces15phone$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces15phone$sector)
table(ces15phone$sector, useNA = "ifany" )

#recode Party ID (PES15_59a)
look_for(ces15phone, "identify")
ces15phone$party_id<-Recode(ces15phone$PES15_59a, "1=1; 2=2; 3=3; 4=4; 5:6=0; 0=0; else=NA")
val_labels(ces15phone$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4)
#checks
val_labels(ces15phone$party_id)
table(ces15phone$party_id)

#recode Vote (PES15_6)
look_for(ces15phone, "party did you vote")
ces15phone$vote<-Recode(ces15phone$PES15_6, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces15phone$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces15phone$vote)
table(ces15phone$vote)

#recode Occupation (PES15_NOC)
look_for(ces15phone, "occupation")
ces15phone$occupation<-Recode(as.numeric(ces15phone$PES15_NOC), "0:1099=2; 
1100:1199=1;
2100:2199=1; 
 3000:3199=1;
 4000:4099=1; 
 4100:4199=1;
 5100:5199=1;
 1200:1599=3; 
 2200:2999=3;
 3200:3299=3;
 3400:3500=3; 
 4200:4499=3;
 5200:5999=3;
 6200:6399=3;
 6400:6799=3; 
 7200:7399=4; 
 7400:7700=5; 
 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
val_labels(ces15phone$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces15phone$occupation)
table(ces15phone$occupation)
#Count missing values
ces15phone %>% 
  select(PES15_NOC, occupation) %>% 
  group_by(occupation) %>% 
  filter(is.na(occupation)) %>% 
  count(PES15_NOC)

ces15phone %>% 
  select(occupation, CPS15_91) %>% 
  filter(is.na(occupation)) %>% 
  group_by(occupation, CPS15_91) %>% 
  count()
#Show occupations of those missing responses on NOC variable
ces15phone %>% 
  select(PES15_NOC, occupation) %>% 
  group_by(occupation) %>% 
  summarise(n=n())

#recode Occupation3 as 6 class schema with self-employed (CPS15_91)
look_for(ces15phone, "employ")
ces15phone$occupation3<-ifelse(ces15phone$CPS15_91==1, 6, ces15phone$occupation)
val_labels(ces15phone$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces15phone$occupation3)
table(ces15phone$occupation3)
table(is.na(ces15phone$occupation3))
table(is.na(ces15phone$occupation))

#recode Income (cpsm16 and cpsm16a)
look_for(ces15phone, "income")
ces15phone %>% 
  mutate(income=case_when(
    CPS15_93==1 | CPS15_92> -1 & CPS15_92 < 30 ~ 1,
    CPS15_93==2 | CPS15_92> 29 & CPS15_92 < 60 ~ 2,
    CPS15_93==3 | CPS15_92> 59 & CPS15_92 < 90 ~ 3,
    CPS15_93==4 | CPS15_92> 89 & CPS15_92 < 110 ~ 4,
    CPS15_93==5 | CPS15_92> 109 & CPS15_92 < 998 ~ 5,
  ))->ces15phone

val_labels(ces15phone$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces15phone$income)
table(ces15phone$income)

#recode Religiosity (CPS15_82)
look_for(ces15phone, "relig")
ces15phone$religiosity<-Recode(ces15phone$CPS15_82, "7=1; 5=2; 98=3; 3=4; 1=5; else=NA")
val_labels(ces15phone$religiosity)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces15phone$religiosity)
table(ces15phone$religiosity)

#recode Native-born (CPS15_83)
look_for(ces15phone, "born")
ces15phone$native<-Recode(ces15phone$CPS15_83, "0:1=1; 2:97=0; else=NA")
val_labels(ces15phone$native)<-c(Foreign=0, Native=1)
#checks
val_labels(ces15phone$native)
table(ces15phone$native)

#recode Ideology (MBS15_K5) (only 1250 respondents)
look_for(ces15phone, "scale")
ces15phone$ideology<-Recode(ces15phone$MBS15_K5, "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA")
val_labels(ces15phone$ideology)<-c(Left=0, Right=1)
#checks
val_labels(ces15phone$ideology)
table(ces15phone$ideology)

#recode Immigration sentiment (PES15_51, PES15_19, PES15_28) into an index 0-1
#1 = pro-immigration sentiment 0 = anti-immigration sentiment
look_for(ces15phone, "immigr")
ces15phone$immigration_jobs<-Recode(ces15phone$PES15_51, "7=1; 5=0.75; 3=0.25; 1=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces15phone$immigration_jobs)<-c(Strongly_agree=0, Somewhat_agree=0.33, Somewhat_disagree=0.66, Strongly_disagree=1)
#checks
#val_labels(ces15phone$immigration_jobs)
table(ces15phone$immigration_jobs)
ces15phone$PES15_19

ces15phone$immigration_feel<-car::Recode(as.numeric(ces15phone$PES15_19), "998:999=NA", as.numeric=T)

ces15phone$immigration_feel<-ces15phone$immigration_feel /100
summary(ces15phone$immigration_feel)
class(ces15phone$immigration_feel)
class(ces15phone$immigration_feel)
#checks
#val_labels(ces15phone$immigrations_feel)
table(ces15phone$immigration_feel)

ces15phone$immigration_rate<-Recode(ces15phone$PES15_28, "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces15phone$immigration_rate)<-c(Less=0, Same=0.5, More=1)
#checks
#val_labels(ces15phone$immigration_rate)
table(ces15phone$immigration_rate)

#Combine the 3 immigration variables and divide by 3
ces15phone$immigration_feel

ces15phone %>% 
  rowwise() %>% 
  mutate(immigration=mean(c(immigration_jobs, immigration_rate, immigration_feel), na.rm=T))->ces15phone

#Check distribution of immigration
qplot(ces15phone$immigration, geom="histogram")

#recode Racial Minorities sentiment (PES15_18, PES15_42) into an index 0-1 
#1 = pro-racial minority sentiment 0 = anti-racial minority sentiment
look_for(ces15phone, "minor")
ces15phone$r_minorities_feel<-Recode(as.numeric(ces15phone$PES15_18), "998:999=NA")
table(ces15phone$r_minorities_feel)
ces15phone$minorities_feel<-(ces15phone$r_minorities_feel /100)
summary(ces15phone$minorities_feel)
#checks
#val_labels(ces15phone$minorities_feel)
table(ces15phone$minorities_feel)
ces15phone$minorities_help<-Recode(as.numeric(ces15phone$PES15_42), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)

#val_labels(ces15phone$minorities_help)<-c(Much_less=0, Somewhat_less=0.25, Same=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces15phone$minorities_help)
table(ces15phone$minorities_help, useNA = "ifany" )

#Combine the 2 racial minority variables and divide by 2

ces15phone %>% 
  rowwise() %>% 
  mutate(minorities=mean(c_across(c(minorities_help, minorities_feel)), na.rm=T))->ces15phone

#Check distribution of immigration
qplot(ces15phone$minorities, geom="histogram")

#Calculate Cronbach's alpha
library(psych)

ces15phone %>% 
  select(immigration_jobs, immigration_feel, immigration_rate) %>% 
  psych::alpha(.)


## Or Create a 5 variable immigration/racial minority sentiment index by dividing by 5
ces15phone %>% 
  select(starts_with('immigration_'))
#Remove value labels
ces15phone %>% 
  mutate(across(.cols=starts_with('immigration_|minorities_'), remove_val_labels) )->ces15phone



ces15phone %>% 
  mutate(immigration2=mean(c(immigration_jobs, immigration_feel, immigration_rate, minorities_feel, minorities_help), na.rm=T))->ces15phone
qplot(ces15phone$immigration2, geom="histogram")

#Calculate Cronbach's alpha
library(psych)
ces15phone %>% 
  select(immigration_jobs, immigration_feel, immigration_rate, minorities_feel, minorities_help) %>% 
  psych::alpha(.)

# #recode Tom Mulclair (CPS15_25)
# look_for(ces15phone, "Mulcair")
# ces15phone$Mulcair<-Recode(ces15phone$CPS15_25, "996:999=NA")
# #checks
# table(ces15phone$Mulcair)
# ces15phone$Tom_Mulcair<-(ces15phone$Mulcair /100)
# table(ces15phone$Tom_Mulcair)
# 
# #recode Justin Trudeau (CPS15_24)
# look_for(ces15phone, "Trudeau")
# ces15phone$Trudeau<-Recode(ces15phone$CPS15_24, "996:999=NA")
# #checks
# table(ces15phone$Trudeau)
# ces15phone$Justin_Trudeau<-(ces15phone$Trudeau /100)
# table(ces15phone$Justin_Trudeau)
# 
# #recode Stephen Harper (CPS15_23)
# look_for(ces15phone, "Harper")
# ces15phone$Harper<-Recode(ces15phone$CPS15_23, "996:999=NA")
# #checks
# table(ces15phone$Harper)
# ces15phone$Stephen_Harper<-(ces15phone$Harper /100)
# table(ces15phone$Stephen_Harper)
# 
# #recode Gilles Duceppe (CPS15_26)
# look_for(ces15phone, "Duceppe")
# ces15phone$Duceppe<-Recode(ces15phone$CPS15_26, "996:999=NA")
# #checks
# table(ces15phone$Duceppe)
# ces15phone$Gilles_Duceppe<-(ces15phone$Duceppe /100)
# table(ces15phone$Gilles_Duceppe)

#recode leaders including don't knows
#recode Tom Mulclair (CPS15_25)
ces15phone$Mulcair15<-Recode(ces15phone$CPS15_25, "998=50; 996:997=NA; 999=NA")
#checks
table(ces15phone$Mulcair15, useNA = "ifany" )
ces15phone$ndp_leader<-(ces15phone$Mulcair15 /100)
table(ces15phone$ndp_leader, useNA = "ifany" )

#recode Justin Trudeau (CPS15_24)
ces15phone$Trudeau15<-Recode(ces15phone$CPS15_24, "998=50; 996:997=NA; 999=NA")
#checks
table(ces15phone$Trudeau15)
ces15phone$liberal_leader<-(ces15phone$Trudeau15 /100)
table(ces15phone$liberal_leader)

#recode Stephen Harper (CPS15_23)
ces15phone$Harper15<-Recode(ces15phone$CPS15_23, "998=50; 996:997=NA; 999=NA")
#checks
table(ces15phone$Harper15)
ces15phone$conservative_leader<-(ces15phone$Harper15 /100)
table(ces15phone$conservative_leader)

#recode Gilles Duceppe (CPS15_26)
ces15phone$Duceppe15<-Recode(ces15phone$CPS15_26, "998=50; 996:997=NA; 999=NA")
#checks
table(ces15phone$Duceppe15)
ces15phone$bloc_leader<-(ces15phone$Duceppe15 /100)
table(ces15phone$bloc_leader)

#recode Environment (CPS15_35)
look_for(ces15phone, "enviro")
ces15phone$environment<-Recode(ces15phone$CPS15_35, "5=0.5; 1=1; 3=0; 8=0.5; else=NA")
#val_labels(ces15phone$environment)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
val_labels(ces15phone$environment)
table(ces15phone$environment, useNA = "ifany" )



#recode Redistribution (PES15_41)
look_for(ces15phone, "rich")
ces15phone$redistribution<-Recode(ces15phone$PES15_41, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces15phone$redistribution)
table(ces15phone$redistribution)

#recode Pro-Redistribution (PES15_41)
ces15phone$pro_redistribution<-Recode(ces15phone$PES15_41, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces15phone$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces15phone$pro_redistribution)
table(ces15phone$pro_redistribution)

#recode NDP rating (CPS15_20)
look_for(ces15phone, "NDP")
ces15phone$NDP_therm<-Recode(ces15phone$CPS15_20, "996=NA; 998=NA; 999=NA")
#checks
table(ces15phone$NDP_therm)
ces15phone$NDP_rating<-(ces15phone$NDP_therm /100)
table(ces15phone$NDP_rating)

#recode Liberal rating (CPS15_19)
look_for(ces15phone, "Liberal")
ces15phone$Liberal_therm<-Recode(ces15phone$CPS15_19, "996=NA; 998=NA; 999=NA")
#checks
table(ces15phone$Liberal_therm)
ces15phone$Liberal_rating<-(ces15phone$Liberal_therm /100)
table(ces15phone$Liberal_rating)

#recode Conservative rating (CPS15_18)
look_for(ces15phone, "Conservative")
ces15phone$Conservative_therm<-Recode(ces15phone$CPS15_18, "996=NA; 998=NA; 999=NA")
#checks
table(ces15phone$Conservative_therm)
ces15phone$Conservative_rating<-(ces15phone$Conservative_therm /100)
table(ces15phone$Conservative_rating)

#recode Manage economy (CPS15_40b)
look_for(ces15phone, "economy")
ces15phone$CPS15_40b
ces15phone$manage_economy<-Recode(ces15phone$CPS15_40b, "1=1; 2=2; 3=3; 4=4; 0=5; else=NA")
val_labels(ces15phone$manage_economy)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces15phone$manage_economy)
table(ces15phone$manage_economy)

#recode Market Liberalism (PES15_22 and PES15_49)
look_for(ces15phone, "leave")
look_for(ces15phone, "blame")
ces15phone$market1<-Recode(ces15phone$PES15_22, "1=1; 3=0.75; 8=0.5; 5=0.25; 7=0; else=NA", as.numeric=T)
ces15phone$market2<-Recode(ces15phone$PES15_49, "1=1; 3=0.75; 8=0.5; 5=0.25; 7=0; else=NA", as.numeric=T)
#val_labels(ces15phone$market1)<-c(Strongly_disagree=0, Somewhat_disagree=0.25, Neither=0.5, Somewhat_agree=0.75, Strongly_agree=1)
#checks
table(ces15phone$market1, useNA="ifany")
table(ces15phone$market2, useNA="ifany")

ces15phone %>% 
  rowwise() %>% 
  mutate(market_liberalism=mean(
    c(market1, market2), na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c(market1, market2, market_liberalism)) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces15phone %>% 
  rowwise() %>% 
  mutate(market_liberalism=mean(c(market1, market2), na.rm=T  )) %>% 
  ungroup()->ces15phone
ces15phone %>% 
  select(starts_with("market")) %>% 
  summary()
#Check distribution of market_liberalism
qplot(ces15phone$market_liberalism, geom="histogram")
table(ces15phone$market_liberalism, useNA="ifany")

#recode Moral Traditionalism (PES15_26, PES15_43, PES15_16)
look_for(ces15phone, "women")
look_for(ces15phone, "gays")
ces15phone$moral_1<-Recode(ces15phone$PES15_26, "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
ces15phone$moral_2<-Recode(ces15phone$PES15_43, "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; 8=0.5; else=NA", as.numeric=T)

# moral traditionalism3
# There is a way easier way to do this. 
# I think you want to reverse them; high scores go to zero, low scores go to high
# I noticed you set the DK to 50. That's neat. I had never thought about that. 

#First rescale this from 0 to 1
ces15phone$moral_3<-Recode(ces15phone$PES15_16, "998=50; 999=NA", as.numeric=T)
#table to check
table(ces15phone$moral_3)
#Rescale to 0 and 1 by dividing by 100
ces15phone$moral_3<-ces15phone$moral_3/100
#Reverse
ces15phone$moral_3<-as.numeric(reverse.code(-1, ces15phone[,'moral_3']))
ces15phone$moral_3

ces15phone %>% 
  select(starts_with('moral_'))
#Scale Averaging 

ces15phone %>% 
  rowwise() %>% 
  mutate(moral_traditionalism=mean(c(moral_1, moral_2, moral_3), na.rm=T  
  )) %>% 
  ungroup()->ces15phone
ces15phone %>% 
  select(starts_with("moral_")) %>% 
  summary()
#Check distribution of moral_traditionalism
qplot(ces15phone$moral_traditionalism, geom="histogram")
table(ces15phone$moral_traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces15phone %>% 
  select(moral_1, moral_2, moral_3) %>% 
  psych::alpha(.)

#recode Political Disaffection (PES15_48)
look_for(ces15phone, "care")
ces15phone$political_disaffection<-Recode(ces15phone$PES15_48, "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces15phone$political_disaffection)<-c(Strongly_disagree=0, Somewhat_disagree=0.25, Neither=0.5, Somewhat_agree=0.75, Strongly_agree=1)
#checks
table(ces15phone$political_disaffection, useNA="ifany")

#recode Continentalism (PES15_45)
look_for(ces15phone, "united states")
ces15phone$continentalism<-Recode(ces15phone$PES15_45, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 6=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces15phone$continentalism)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
val_labels(ces15phone$continentalism)
table(ces15phone$continentalism, ces15phone$PES15_45, useNA = "ifany" )

#recode Quebec Sovereignty (CPS15_75)
look_for(ces15phone, "quebec")
ces15phone$quebec_sovereignty<-Recode(ces15phone$CPS15_75, "1=1; 3=0.75; 8=0.5; 5=0.25; 7=0; else=NA", as.numeric=T)
#val_labels(ces15phone$quebec_sovereignty)<-c(Much_less=0, Somewhat_less=0.25, Dont_know=0.5, Somewhat_more=0.75, Much_more=1)
#checks
val_labels(ces15phone$quebec_sovereignty)
table(ces15phone$quebec_sovereignty, ces15phone$CPS15_75 , useNA = "ifany" )

#recode Personal Retrospective (CPS15_66)
look_for(ces15phone, "situation")
ces15phone$personal_retrospective<-Recode(ces15phone$CPS15_66, "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
val_labels(ces15phone$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces15phone$personal_retrospective)
table(ces15phone$personal_retrospective, ces15phone$CPS15_66 , useNA = "ifany" )

#recode National Retrospective (CPS15_39)
look_for(ces15phone, "economy")
ces15phone$national_retrospective<-Recode(ces15phone$CPS15_39, "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
val_labels(ces15phone$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces15phone$national_retrospective)
table(ces15phone$national_retrospective, ces15phone$CPS15_39 , useNA = "ifany" )

#recode Defence (CPS15_37)
look_for(ces15phone, "defence")
ces15phone$defence<-Recode(ces15phone$CPS15_37, "1=1; 3=0; 5=0.5; 8=0.5; else=NA")
val_labels(ces15phone$defence)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
val_labels(ces15phone$defence)
table(ces15phone$defence, ces15phone$CPS15_37 , useNA = "ifany" )

#recode Crime and Justice (CPS15_36)
look_for(ces19phone, "justice")
ces15phone$justice<-Recode(ces15phone$CPS15_36, "1=1; 3=0; 5=0.5; 8=0.5; else=NA")
val_labels(ces15phone$justice)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
val_labels(ces15phone$justice)
table(ces15phone$justice , ces15phone$CPS15_36, useNA = "ifany" )

#recode Education (CPS15_34)
look_for(ces15phone, "education")
ces15phone$education<-Recode(ces15phone$CPS15_34, "1=1; 3=0; 5=0.5; 8=0.5; else=NA")
#val_labels(ces15phone$education)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
#val_labels(ces15phone$education)
table(ces15phone$education, ces15phone$CPS15_34 , useNA = "ifany" )

#recode Most Important Question (CPS15_1)
look_for(ces15phone, "important")
ces15phone$mip<-Recode(ces15phone$CPS15_1, "75=1; 71=2; 77=2; 18=2; 4=2; 5=3; 2=3; 12=3; 90:91=3; 65:66=4; 13=5; 39=5; 10=6;  
                                                  36=7; 15:16=7; 30=7; 29=7; 56:57=8; 14=9; 50=9; 20:26=10; 7=11; 11=11; 83=11;  
                                                  48=12; 79=12; 34=13; 9=14; 55=14; 73:74=14; 76=14; 49=14; 60:64=15; 72=15; 
                                                  80:82=0; 84=0; 92:97=0; 6=0; 8=0; 46=0; 31:33=0; 58:59=0; 35=0; 1=0; else=NA")
val_labels(ces15phone$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, 
                              Deficit_Debt=10, Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15)

#### Visible minority status####
ces15phone$CPS15_85
ces15phone %>% 
  mutate(vismin=case_when(
      CPS15_85==1~ 0,
  CPS15_85==2~ 0,
  CPS15_85==3~0,
  CPS15_85==5~1,
    CPS15_85==6~1,
    CPS15_85==7~0,
    CPS15_85==8~1,
    CPS15_85==9~0,
    CPS15_85==10~0,
    CPS15_85==11~0,
    CPS15_85==12~0,
    CPS15_85==13~0,
    CPS15_85==14~1,
    CPS15_85==15~1,
    CPS15_85==16~0,
    CPS15_85==17~0,
    CPS15_85==18~0,
  CPS15_85==19~0,
    CPS15_85==20~1,
    CPS15_85==21~1,
    CPS15_85==22~0,
    CPS15_85==23~0,
    CPS15_85==24~0,
    CPS15_85==25~0,
    CPS15_85==26~1,
    CPS15_85==27~0,
    CPS15_85==28~1,
    CPS15_85==29~1,
    CPS15_85==30~0,
    CPS15_85==31~1,
    CPS15_85==32~1,
    CPS15_85==34~0,
    CPS15_85==37~0,
    CPS15_85==38~1,
  CPS15_85==39~1,
    CPS15_85==40~0,
    CPS15_85==41~0,
    CPS15_85==42~0,
    CPS15_85==43~0,
    CPS15_85==44~0,
    CPS15_85==45~1,
    CPS15_85==47~0,
    CPS15_85==48~0,
    CPS15_85==49~1,
  CPS15_85==50~0,
    CPS15_85==51~1,
    CPS15_85==52~1,
    CPS15_85==53~0,
    CPS15_85==54~1,
    CPS15_85==55~0,
    CPS15_85==56~0,
  CPS15_85==57~0,
    CPS15_85==58~0,
    CPS15_85==59~1,
    CPS15_85==60~1,
    CPS15_85==61~1,
    CPS15_85==62~1,
    CPS15_85==63~1,
    CPS15_85==64~1,
    CPS15_85==66~0,
    CPS15_85==70~0,
    CPS15_85==94~0,  
  CPS15_85==95~1,
    CPS15_85==96~0,
    CPS15_85==98~NA_real_,
  TRUE ~ NA_real_
      ))->ces15phone
val_labels(ces15phone$vismin)<-c('Visible Minority'=1, 'Non Visible Minority'=0)

table(ces15phone$CPS15_85, ces15phone$vismin, useNA = "ifany")

#recode Immigration (PES15_28)
look_for(ces15phone, "imm")
ces15phone$immigration_rates<-Recode(ces15phone$PES15_28, "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
table(ces15phone$immigration_rates, ces15phone$PES15_28 , useNA = "ifany" )

#recode Environment (MBS15_C14)
look_for(ces15phone, "env")
ces15phone$enviro<-Recode(ces15phone$MBS15_C14, "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
table(ces15phone$enviro , ces15phone$MBS15_C14 , useNA = "ifany" )

#recode Capital Punishment (MBS15_H2)
look_for(ces15phone, "death")
ces15phone$death_penalty<-Recode(ces15phone$MBS15_H2, "1=1; 2=0; 8=0.5; else=NA")
#checks
table(ces15phone$death_penalty, ces15phone$MBS15_H2 , useNA = "ifany" )

#recode Crime (MBS15_I5)
look_for(ces15phone, "crime")
ces15phone$crime<-Recode(ces15phone$MBS15_I5, "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
table(ces15phone$crime, ces15phone$MBS15_I5 , useNA = "ifany")

#recode Gay Rights (PES15_29)
look_for(ces15phone, "gays")
ces15phone$gay_rights<-Recode(ces15phone$PES15_29, "1=0; 5=1; 8=0.5; else=NA")
#checks
table(ces15phone$gay_rights, ces15phone$PES15_29 , useNA = "ifany" )

#recode Abortion (MBS15_H3)
look_for(ces15phone, "abort")
ces15phone$MBS15_H3
ces15phone$abortion<-Recode(ces15phone$MBS15_H3, "1=1; 2=0; 8=0.5; else=NA")
#checks
table(ces15phone$abortion, ces15phone$MBS15_H3 , useNA = "ifany" )

#recode Lifestyle (MBS15_C6)
look_for(ces15phone, "lifestyle")
ces15phone$lifestyles<-Recode(ces15phone$MBS15_C6, "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
c#checks
table(ces15phone$lifestyles, ces15phone$MBS15_C6 , useNA = "ifany" )

#recode Stay Home (PES15_26)
look_for(ces15phone, "home")
ces15phone$PES15_26
ces15phone$stay_home<-Recode(ces15phone$PES15_26, "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
table(ces15phone$stay_home, ces15phone$PES15_25 , useNA = "ifany" )

#recode Marriage Children (MBS15_I4)
look_for(ces15phone, "children")
ces15phone$marriage_children<-Recode(ces15phone$MBS15_I4, "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
table(ces15phone$marriage_children, ces15phone$MBS15_I4 , useNA = "ifany" )

#recode Values (MBS15_C8)
look_for(ces15phone, "traditional")
ces15phone$values<-Recode(ces15phone$MBS15_C8, "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
table(ces15phone$values, ces15phone$MBS15_C8,  useNA = "ifany")

#recode Morals (MBS15_C7)
look_for(ces15phone, "moral")
ces15phone$morals<-Recode(ces15phone$MBS15_C7, "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
table(ces15phone$morals, ces15phone$MBS15_C7 , useNA = "ifany" )

#recode Moral Trad (abortion, lifestyles, stay home, values, marriage childen, morals)
ces15phone$trad3<-ces15phone$abortion
ces15phone$trad7<-ces15phone$lifestyles
ces15phone$trad1<-ces15phone$stay_home
ces15phone$trad4<-ces15phone$values
ces15phone$trad5<-ces15phone$marriage_children
ces15phone$trad6<-ces15phone$morals
ces15phone$trad2<-ces15phone$gay_rights
table(ces15phone$trad1 , useNA = "ifany" )
table(ces15phone$trad2, useNA = "ifany" )
table(ces15phone$trad3 , useNA = "ifany" )
table(ces15phone$trad4 , useNA = "ifany" )
table(ces15phone$trad5 , useNA = "ifany" )
table(ces15phone$trad6 , useNA = "ifany" )
table(ces15phone$trad7 , useNA = "ifany" )

ces15phone %>% 
  rowwise() %>% 
  mutate(traditionalism=mean(
    c_across(trad1:trad7)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('trad1', 'trad2', 'trad3', 'trad4', 'trad5', 'trad6', 'trad7', 'traditionalism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na<8)
  
#Scale Averaging 
ces15phone %>% 
  rowwise() %>% 
  mutate(traditionalism=mean(
    c_across(num_range('trad', 1:7)), na.rm=T  
  )) %>% 
  ungroup()->ces15phone

ces15phone %>% 
  select(starts_with("trad")) %>% 
  summary()
#Check distribution of traditionalism
qplot(ces15phone$traditionalism, geom="histogram")
table(ces15phone$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces15phone %>% 
  select(trad1, trad2, trad3, trad4, trad5, trad6, trad7) %>% 
  psych::alpha(.)
#Check correlation
ces15phone %>% 
  select(trad1, trad2, trad3, trad4, trad5, trad6, trad7) %>% 
  cor(., use="complete.obs")

#recode Moral Traditionalism 2 (stay home & gay rights) (Left-Right)
ces15phone %>% 
  rowwise() %>% 
  mutate(traditionalism2=mean(
    c_across(trad1:trad2)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('trad1', 'trad2', 'traditionalism2')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na<5)

#Scale Averaging 
ces15phone %>% 
  rowwise() %>% 
  mutate(traditionalism2=mean(
    c_across(c('trad1', 'trad2')), na.rm=T  
  )) %>% 
  ungroup()->ces15phone

ces15phone %>% 
  select(starts_with("trad")) %>% 
  summary()
#Check distribution of traditionalism
qplot(ces15phone$traditionalism2, geom="histogram")
table(ces15phone$traditionalism2, useNA="ifany")

#Calculate Cronbach's alpha
ces15phone %>% 
  select(trad1, trad2) %>% 
  psych::alpha(.)
#Check correlation
ces15phone %>% 
  select(trad1, trad2) %>% 
  cor(., use="complete.obs")

#recode 2nd Dimension (stay home, immigration, gay rights, crime)
ces15phone$author1<-ces15phone$stay_home
ces15phone$author2<-ces15phone$immigration_rates
ces15phone$author3<-ces15phone$gay_rights
ces15phone$author4<-ces15phone$crime
table(ces15phone$author1)
table(ces15phone$author2)
table(ces15phone$author3)
table(ces15phone$author4)

ces15phone %>% 
  rowwise() %>% 
  mutate(authoritarianism=mean(
    c_across(author1:author4)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('author1', 'author2', 'author3', 'author4', 'authoritarianism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<6)
#Scale Averaging 
ces15phone %>% 
  rowwise() %>% 
  mutate(authoritarianism=mean(
    c_across(num_range('author', 4)), na.rm=T  
  )) %>% 
  ungroup()->ces15phone

ces15phone %>% 
  select(starts_with("author")) %>% 
  summary()
#Check distribution of traditionalism
qplot(ces15phone$authoritarianism, geom="histogram")
table(ces15phone$authoritarianism, useNA="ifany")

#Calculate Cronbach's alpha
ces15phone %>% 
  select(author1, author2, author3, author4) %>% 
  psych::alpha(.)

#Check correlation
ces15phone %>% 
  select(author1, author2, author3, author4) %>% 
  cor(., use="complete.obs")

#recode Quebec Accommodation (PES15_44) (Left=more accom)
look_for(ces15phone, "quebec")
ces15phone$quebec_accom<-Recode(ces15phone$PES15_44, "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; 8=0.5; else=NA")
#checks
table(ces15phone$quebec_accom)
