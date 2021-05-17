#File to Recode 1988 CES Data 

#load data
data("ces88")

#recode Gender (rsex)
look_for(ces88, "sex")
ces88$male<-Recode(ces88$rsex, "1=1; 5=0")
val_labels(ces88$male)<-c(Female=0, Male=1)
#checks
val_labels(ces88$male)
table(ces88$male)

#recode Union Respondent (n9)
look_for(ces88, "union")
ces88$union<-Recode(ces88$n9, "1=1; 5=0; else=NA")
val_labels(ces88$union)<-c(None=0, Union=1)
#checks
val_labels(ces88$union)
table(ces88$union)

#Union Combined variable (identical copy of union) ### Respondent only
ces88$union_both<-ces88$union
#checks
val_labels(ces88$union_both)
table(ces88$union_both)

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

#recode Language (intlang)
look_for(ces88, "language")
ces88$language<-Recode(ces88$intlang, "1=1; 2=0; else=NA")
val_labels(ces88$language)<-c(French=0, English=1)
#checks
val_labels(ces88$language)
table(ces88$language)

#recode Non-charter Language (n16)
look_for(ces88, "language")
ces88$non_charter_language<-Recode(ces88$n16, "1:3=0; 5=1; else=NA")
val_labels(ces88$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces88$non_charter_language)
table(ces88$non_charter_language)

#recode Employment (n5)
look_for(ces88, "employment")
ces88$employment<-Recode(ces88$n5, "2:7=0; 1=1; else=NA")
val_labels(ces88$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces88$employment)
table(ces88$employment)

#recode Sector (n8 & n5)
look_for(ces88, "sector")
look_for(ces88, "firm")
ces88 %>% 
  mutate(sector=case_when(
    n8==3 ~1,
    n8==5 ~1,
    n8==1 ~0,
    n5> 1 & n5< 8 ~ 0,
    n8==9 ~NA_real_ ,
    n8==8 ~NA_real_ ,
  ))->ces88

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

#recode Occupation3 as 6 class schema with self-employed (n7)
look_for(ces88, "employed")
ces88$occupation3<-ifelse(ces88$n7==1, 6, ces88$occupation)
val_labels(ces88$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces88$occupation3)
table(ces88$occupation3)

#recode Income (n19)
look_for(ces88, "income")
ces88$income<-Recode(ces88$n19, "1:2=1; 3=2; 4=3; 5:6=4; 7:9=5; else=NA")
val_labels(ces88$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces88$income)
table(ces88$income)

#recode Market Liberalism (qc13 and qc2) (Left-Right)
look_for(ces88, "profit")
look_for(ces88, "regulation")
ces88$qc13
ces88$qc2
ces88$market1<-Recode(ces88$qc13, "1=1; 2=0; 3=0.5; 8=0.5; else=NA", as.numeric=T)
ces88$market2<-Recode(ces88$qc2, "1=1; 2=0; 3=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
table(ces88$market1)
table(ces88$market2)

ces88 %>% 
  rowwise() %>% 
  mutate(market_liberalism=mean(
    c_across(market1:market2)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('market1', 'market2', 'market_liberalism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces88 %>% 
  rowwise() %>% 
  mutate(market_liberalism=mean(
    c_across(c('market1', 'market2')), na.rm=T  
  )) %>% 
  ungroup()->ces88

ces88 %>% 
  select(starts_with("market")) %>% 
  summary()
#Check distribution of market_liberalism
qplot(ces88$market_liberalism, geom="histogram")
table(ces88$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces88 %>% 
  select(market1, market2) %>% 
  psych::alpha(.)
#Check correlation
ces88 %>% 
  select(market1, market2) %>% 
  cor(., use="complete.obs")

#recode Redistribution (xk2 and qc16) (Right-Left)
look_for(ces88, "wealth")
look_for(ces88, "poor")
ces88$xk2
table(ces88$xk2, useNA = "ifany")
table(ces88$qc16, useNA="ifany")
val_labels(ces88$xk2)
ces88$redistro1<-Recode(ces88$xk2, "1=1; 3=0.5; 5=0; 8=0.5; else=NA", as.numeric=T)
ces88$redistro2<-Recode(ces88$qc16, "1=1; 2=0; 3=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
table(ces88$redistro1, ces88$xk2)
table(ces88$redistro2, ces88$qc16)

ces88 %>% 
  rowwise() %>% 
  mutate(redistribution=mean(
    c_across(redistro1:redistro2)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('redistro1', 'redistro2', 'redistribution')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces88 %>% 
  rowwise() %>% 
  mutate(redistribution=mean(
    c_across(c('redistro1', 'redistro2')), na.rm=T  
  )) %>% 
  ungroup()->ces88

ces88 %>% 
  select(starts_with("redistro")) %>% 
  summary()
#Check distribution of market_liberalism
qplot(ces88$redistribution, geom="histogram")
table(ces88$redistribution, useNA="ifany")

#Calculate Cronbach's alpha
ces88 %>% 
  select(redistro1, redistro2) %>% 
  psych::alpha(.)
#Check correlation
ces88 %>% 
  select(redistro1, redistro2) %>% 
  cor(., use="complete.obs")

#recode Pro-Redistribution
ces88$pro_redistribution<-Recode(ces88$redistribution, "0.75=1; 1=1; 0:0.5=0; else=NA", as.numeric=T)
val_labels(ces88$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces88$pro_redistribution)
table(ces88$pro_redistribution)

#recode Immigration (l5, qf2, gf10) (Left-Right)
look_for(ces88, "imm")
ces88$l5
ces88$immigration_rates<-Recode(ces88$l5, "1=0; 3=0.5; 5=1; 8=0.5; else=NA", as.numeric=T)
ces88$immigration_better<-Recode(ces88$qf2, "1=0; 2=1; 8=0.5; else=NA", as.numeric=T)
ces88$immigration_encourage<-Recode(ces88$qf10, "1=0; 2=1; 8=0.5; else=NA", as.numeric=T)
#checks
table(ces88$immigration_rates, ces88$l5 , useNA = "ifany" )
table(ces88$immigration_better, ces88$qf2 , useNA = "ifany" )
table(ces88$immigration_encourage, ces88$qf10 , useNA = "ifany" )

ces88 %>% 
  rowwise() %>% 
  mutate(immigration=mean(
    c_across(immigration_rates:immigration_encourage)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('immigration_rates', 'immigration_better', 'immigration_encourage', 'immigration')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces88 %>% 
  rowwise() %>% 
  mutate(immigration=mean(
    c_across(c('immigration_rates', 'immigration_better', 'immigration_encourage')), na.rm=T  
  )) %>% 
  ungroup()->ces88

ces88 %>% 
  select(starts_with("immigration")) %>% 
  summary()
#Check distribution of immigration
qplot(ces88$immigration, geom="histogram")
table(ces88$immigration, useNA="ifany")

#Calculate Cronbach's alpha
ces88 %>% 
  select(immigration_rates, immigration_better, immigration_encourage) %>% 
  psych::alpha(.)
#Check correlation
ces88 %>% 
  select(immigration_rates, immigration_better, immigration_encourage) %>% 
  cor(., use="complete.obs")

#recode Environment (qf9) (Left-Right)
look_for(ces88, "env")
ces88$enviro<-Recode(ces88$qf9, "1=0; 2=1; 8=0.5; else=NA")
#checks
table(ces88$enviro, ces88$qf9) #No one had 8

#recode Capital Punishment (qf1) (Left-Right)
look_for(ces88, "punish")
ces88$qf1
ces88$death_penalty<-Recode(ces88$qf1, "1=0; 2=1; 8=0.5; else=NA")
#checks
table(ces88$death_penalty, ces88$qf1)

#recode Crime (qh11) (Left-Right)
look_for(ces88, "crime")
#remotes::install_github('sjkiss/skpersonal')
#library(skpersonal)
ces88$qh11

ces88$crime<-skpersonal::revScale(as.numeric(ces88$qh11), reverse=T)

summary(ces88$crime)
table(ces88$crime, ces88$qh11 , useNA = "ifany" )
summary(ces88$qh11)
#ces88$crime<-Recode(ces88$qh11, "1=1; 2=0.9; 3=0.8; 4=0.7; 5=0.6; 6=0.5; 7=0.4; 8=0.3; 9=0.2; 10=0.1; 11:12=0; else=NA")
ces88$qh11

ces88$qh11

var_label(ces88$qh11)
#checks
table(ces88$crime)

#recode Gay Rights (qe9) (Left-Right)
look_for(ces88, "homo")
ces88$gay_rights<-Recode(ces88$qe9, "1=1; 2=0; 3=0.5; 8=0.5; else=NA")
#checks
table(ces88$gay_rights)

#recode Abortion (l6a and l6b) (Left-Right)
look_for(ces88, "abort")
ces88 %>% 
  mutate(abortion=case_when(
    l6a==5 ~0,
    l6a==1 ~1,
    l6a==3 ~0.5,
    l6b==5 ~1,
    l6b==1 ~0,
    l6b==3 ~0.5,
  ))->ces88
#checks
table(ces88$abortion)

#recode Censorship (qa10) (Left-Right)
look_for(ces88, "porn")
ces88$censorship<-Recode(ces88$qa10, "1=1; 2=0; 3=0.5; 8=0.5; else=NA")
#checks
table(ces88$censorship)

#recode Moral Traditionalism (abortion & censorship) (Left-Right)
ces88$trad1<-ces88$abortion
ces88$trad2<-ces88$censorship
table(ces88$trad1)
table(ces88$trad2)

ces88 %>% 
  rowwise() %>% 
  mutate(traditionalism=mean(
    c_across(trad1:trad2)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('trad1', 'trad2', 'traditionalism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces88 %>% 
  rowwise() %>% 
  mutate(traditionalism=mean(
    c_across(c('trad1', 'trad2')), na.rm=T  
  )) %>% 
  ungroup()->ces88

ces88 %>% 
  select(starts_with("trad")) %>% 
  summary()
#Check distribution of traditionalism
qplot(ces88$traditionalism, geom="histogram")
table(ces88$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces88 %>% 
  select(trad1, trad2) %>% 
  psych::alpha(.)

#Check correlation
ces88 %>% 
  select(trad1, trad2) %>% 
  cor(., use="complete.obs")

