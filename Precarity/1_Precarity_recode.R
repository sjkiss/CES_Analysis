#ces19 precarity script coding
#### Package Management ####
#remotes::install_github("sjkiss/cesdata", force=T)
library(cesdata)
data("ces19_kiss")
library(tidyverse)
library(haven)
library(here)
library(car)
#### Data Import ####
#Read in the data file 
str(ces19_kiss)
nrow(ces19_kiss)

#Just rename to cps to make it simpler
cps<-ces19_kiss

#### Search For precarity variables#### 
#search for our variables

conv_names <- function(x) {
  setNames(x, iconv(names(x), from="latin1", to="UTF-8"))
}
conv_val_labels <- function(x) {
  if(is.labelled(x)) {
  val_labels(x) <- conv_names(val_labels(x))
  x}
  else{x}
}
library(labelled)
is.labelled(cps$cps19_imp_iss)
cps <- map_dfc(cps, conv_val_labels)

look_for(cps, "income") #income volatility is kiss_q1
look_for(cps, "job")
look_for(cps, "business")
##looks like kiss_q1 is income volatility; kiss_q2_* are the probability of loss; and kiss_q3_* are the consequences. 

#Print all the variable labels
cps %>% 
  select(starts_with('kiss')) %>% 
  map(., var_label)

#check that the missing values do not overlap
table(cps$kiss_q2_business, cps$kiss_q3_job)
##There is no overlap. 

#Check value labels 
cps %>% 
  select(starts_with('kiss')) %>% 
  val_labels()

#### Scaling Precarity Variables
## Note, low numbers are currently high risk, high numbers are low risk; we should reverse these and set 6 to missing 
#this code takes 1q, sets 5 to be NA and reverses the values. 

cps %>% 
  mutate(kiss_q1_out=case_when(
    kiss_q1== 1 ~ 3,
    kiss_q1== 2 ~ 2,
    kiss_q1== 3 ~ 1 ,
    kiss_q1== 4 ~ NA_real_,
    kiss_q1== 5 ~ NA_real_),
  ) -> cps

#This code takes the q2 and q3 questions, sets 6 to be NA and reverses the values
cps %>% 
  mutate_at(vars(starts_with("kiss_q2")|starts_with("kiss_q3")), 
            list(out=
                   ~ case_when(
                     . == 5 ~ 1,
                     . == 4 ~ 2,
                     . == 3 ~ 3,
                     . == 2 ~ 4, 
                     . == 1 ~ 5,
                     . == 6 ~ NA_real_
                   ))) -> cps

#let's rename the variables right away
cps %>% 
  rename(., "volatility"=kiss_q1_out, "job_prob"=kiss_q2_job_out, "job_cons"=kiss_q3_job_out, "bus_prob"=kiss_q2_business_out, "bus_cons"=kiss_q3_business_out)-> cps

names(cps)
table(cps$volatility,  useNA = "ifany" )
table(cps$job_prob,  useNA = "ifany" )
table(cps$bus_prob,  useNA = "ifany" )
table(cps$job_cons,  useNA = "ifany" )
table(cps$bus_cons,  useNA = "ifany" )

#### Combine Business and Job Loss and Consequence Variables 
#Recode Probability of losing job or business (job_prob & bus_prob)
cps %>% 
  mutate(probability=case_when(
    job_prob==1 | bus_prob==1 ~ 1,
    job_prob==2 | bus_prob==2 ~ 2,
    job_prob==3 | bus_prob==3 ~ 3,
    job_prob==4 | bus_prob==4 ~ 4,
    job_prob==5 | bus_prob==5 ~ 5,
  ))->cps
table(cps$probability, useNA = "ifany" )
# 

#Recode Consequence of losing job or business (job_prob & bus_prob)
cps %>% 
  mutate(consequence=case_when(
    job_cons==1 | bus_cons==1 ~ 1,
    job_cons==2 | bus_cons==2 ~ 2,
    job_cons==3 | bus_cons==3 ~ 3,
    job_cons==4 | bus_cons==4 ~ 4,
    job_cons==5 | bus_cons==5 ~ 5,
  ))->cps
table(cps$consequence, useNA = "ifany" )

#Calculate Cronbach's alpha
cps %>% 
  select(probability, volatility, consequence) %>% 
  psych::alpha(.)
#Check correlation
cps %>% 
  select(probability, volatility, consequence) %>% 
  cor(., use="complete.obs")
cps %>% 
  select(probability, volatility, consequence) %>% 
  cor(., use="complete.obs")->precarity_matrix
library(psych)

scree(cps[,c("probability", "volatility", "consequence")])
#### Rescale 0 to 1
cps %>% 
  mutate(across(c(probability, volatility, consequence), scales::rescale, .names="{.col}_x"))->cps
# Check the rescale of probability, volatility and consequence
# Should be 1
cor(cps$probability, cps$probability_x, use="complete.obs")
cor(cps$volatility, cps$volatility_x, use="complete.obs")
cor(cps$consequence, cps$consequence_x, use="complete.obs")
names(cps)
#Scale Averaging 
cps %>% 
  rowwise() %>% 
  #Calculate precarity as average of probability and volatility
  mutate(precarity_x=mean(
    c_across(c(probability_x, volatility_x)), na.rm=T )) %>% 
  ungroup()->cps

cps %>% 
  select(c(precarity_x, probability_x, volatility_x)) %>% 
  summary()



#Check distribution
qplot(cps$precarity, geom="histogram")
table(cps$precarity, useNA="ifany")





#### recode control variables ####
#recode Gender (cps19_gender)
look_for(cps, "gender")
cps$male<-Recode(cps$cps19_gender, "1=1; 2=0; else=NA")
val_labels(cps$male)<-c(Female=0, Male=1)
#checks
val_labels(cps$male)
table(cps$male, cps$cps19_gender, useNA = "ifany" )

#recode Union Household (cps$cps19_union)
look_for(cps, "union")
cps$union<-Recode(cps$cps19_union, "1=1; 2=0; else=NA")
val_labels(cps$union)<-c(None=0, Union=1)
#checks
val_labels(cps$union)
table(cps$union , cps$cps19_union, useNA = "ifany" )

#recode Degree (cps19_education)
look_for(cps, "education")
cps$degree<-Recode(cps$cps19_education, "9:11=1; 1:8=0; else=NA")
val_labels(cps$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(cps$degree)
table(cps$degree , useNA = "ifany" )

#recode Education (cps19_education)
look_for(cps, "education")
cps$education<-Recode(cps$cps19_education, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; 8=8; 9=9; 10=10; 11=11; else=NA")
#checks
table(cps$education , cps$cps19_education,  useNA = "ifany" )

#recode Region (cps19_province)
look_for(cps, "province")
cps$region<-Recode(cps$cps19_province, "17:18=1; 20=1; 23=1; 24=2; 22=3; 14:16=4; 25=4; else=NA")
val_labels(cps$region)<-c(Atlantic=1, Quebec=2, Ontario=3, West=4)
#checks
val_labels(cps$region)
table(cps$region , cps$cps19_province, useNA = "ifany" )

#recode Region2 (cps19_province) - No Quebec
look_for(cps, "province")
cps$region2<-Recode(cps$cps19_province, "17:18=1; 20=1; 23=1; 22=2; 14:16=3; 25=3; else=NA")
val_labels(cps$region2)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(cps$region2)
table(cps$region2 , useNA = "ifany" )

#recode Quebec (cps19_province)
look_for(cps, "province")
cps$quebec<-Recode(cps$cps19_province, "14:18=0; 20=0; 22=0; 23=0; 25=0; 24=1; else=NA")
val_labels(cps$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(cps$quebec)
table(cps$quebec , useNA = "ifany" )

#recode Age (cps19_yob)
look_for(cps, "yob")
table(cps$cps19_yob , useNA = "ifany" )
cps$age2<-1919+cps$cps19_yob
table(cps$age2)
cps$age<-2019-cps$age2
table(cps$age, cps$cps19_yob, useNA = "ifany")

#recode Religion (cps19_religion)
look_for(cps, "relig")
cps$religion<-Recode(cps$cps19_religion, "1:2=0; 3:7=3; 8:9=2; 10=1; 11=1; 12=0; 13:21=2; 22=3; else=NA")
val_labels(cps$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(cps$religion)
table(cps$religion , cps$cps19_religion, useNA = "ifany" )

# Turn religion into factor with None as reference case
cps$religion2<-Recode(as.factor(cps$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(cps$religion2)
table(cps$religion2)
# Religion dummies
cps$catholic<-Recode(cps$religion, "1=1; 2:3=0; 0=0; NA=NA")
cps$no_religion<-Recode(cps$religion, "0=1; 1:3=0; NA=NA")

#recode Language 
look_for(cps, "language")
cps %>% 
  mutate(language=case_when(
    cps19_language_68==1 ~1,
    cps19_language_69==1 ~0,
  ))->cps
val_labels(cps$language)<-c(French=0, English=1)
#checks
val_labels(cps$language)
table(cps$language , cps$cps19_language_68 , useNA = "ifany" )
table(cps$language , cps$cps19_language_69 , useNA = "ifany" )

#recode Married (cps19_marital)
look_for(cps, "marital")
cps$married<-Recode(cps$cps19_marital, "1=1; 2:6=0; else=NA")
val_labels(cps$married)<-c(Not_Married=0, Married=1)
#checks
val_labels(cps$married)
table(cps$married, cps$cps19_marital, useNA = "ifany" )

#recode Vote (cps19_votechoice )
look_for(cps, "votechoice")
cps$vote<-Recode(cps$cps19_votechoice , "1=1; 2=2; 3=3; 4=4; 5=5; 7=0; 6=6; else=NA")
val_labels(cps$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, PP=6)
#checks
val_labels(cps$vote)
table(cps$vote , cps$cps19_votechoice , useNA = "ifany" )

#Recode voting
cps$ndp<-Recode(cps$vote, "3=1; 0:2=0; 4:6=0; NA=NA")
cps$liberal<-Recode(cps$vote, "1=1; 2:6=0; NA=NA")
cps$conservative<-Recode(cps$vote, "0:1=0; 2=1; 3:6=0; NA=NA")
cps$bloc<-Recode(cps$vote, "4=1; 0:3=0; 5:6=0; else=NA")
cps$green<-Recode(cps$vote, "5=1; 0:4=0; 6=0; else=NA")
cps$pparty<-Recode(cps$vote, "6=1; 0:5=0; else=NA")
table(cps$ndp)
table(cps$liberal)
table(cps$conservative)
table(cps$bloc)
table(cps$green)
table(cps$pparty)

cps$left_vs_right<-Recode(cps$vote, "1=1; 2=0; 3=1; 5=1; 6=0; else=NA")
cps$bloc_vs_right<-Recode(cps$vote, "4=1; 2=0; 6=0; else=NA")
cps$right<-Recode(cps$vote, "2=1; 6=1; 1=0; 3:5=0; 0=0; else=NA")
table(cps$left_vs_right)
table(cps$bloc_vs_right)
table(cps$right)
#recode Income (cps19_income_cat)
look_for(cps, "income")
cps %>% 
  mutate(income=case_when(
    cps19_income_cat==1 | cps19_income_number> -1 & cps19_income_number < 30000 ~ 1,
    cps19_income_cat==2 | cps19_income_number> -1 & cps19_income_number < 30000 ~ 1,
    cps19_income_cat==3 | cps19_income_number> 29000 & cps19_income_number < 60000 ~ 2,
    cps19_income_cat==4 | cps19_income_number> 59000 & cps19_income_number < 90000 ~ 3,
    cps19_income_cat==5 | cps19_income_number> 89000 & cps19_income_number < 150000 ~ 4,
    cps19_income_cat==6 | cps19_income_number> 89000 & cps19_income_number < 150000 ~ 4,
    cps19_income_cat==7 | cps19_income_number> 149000 & cps19_income_number < 1000001 ~ 5,
    cps19_income_cat==8 | cps19_income_number> 149000 & cps19_income_number < 1000001 ~ 5,
  ))->cps
val_labels(cps$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(cps$income)
table(cps$income, cps$cps19_income_cat, useNA = "ifany" )

#recode Religiosity (cps19_rel_imp )
look_for(cps, "relig")
cps$religiosity<-Recode(cps$cps19_rel_imp , "1=1; 2=0.66; 3=0.33; 4=0; else=NA")
#checks
table(cps$religiosity)
table(cps$religiosity, cps$cps19_rel_imp, useNA = "ifany" )

#recode Native-born (cps19_bornin_canada)
look_for(cps, "born")
cps$native<-Recode(cps$cps19_bornin_canada, "1=1; 2=0; else=NA")
val_labels(cps$native)<-c(Foreign=0, Native=1)
#checks
val_labels(cps$native)
table(cps$native , cps$cps19_bornin_canada , useNA = "ifany" )

#recode Homeowner (cps19_property_1 & cps19_property_6)
look_for(cps, "property")
cps$home<-Recode(cps$cps19_property_1, "1=1; else=0")
table(cps$home, useNA = "ifany" )
cps %>% 
  mutate(homeowner=case_when(
    home==1 ~1, 
    home==0 & cps19_property_6==1 ~NA_real_,
    home==0 ~0, 
  ))->cps
val_labels(cps$homeowner)<-c(Non_Owner=0, Homeowner=1)
#checks
val_labels(cps$homeowner)
table(cps$homeowner,  useNA = "ifany" )
table(cps$homeowner, cps$cps19_property_6,  useNA = "ifany" )

#recode Savings (cps19_property_4 & cps19_property_6)
look_for(cps, "property")
cps$money<-Recode(cps$cps19_property_4, "1=1; else=0")
table(cps$money, useNA = "ifany" )
cps %>% 
  mutate(savings=case_when(
    money==1 ~1, 
    money==0 & cps19_property_6==1 ~NA_real_,
    money==0 ~0, 
  ))->cps
val_labels(cps$savings)<-c(None=0, Savings=1)
#checks
val_labels(cps$savings)
table(cps$savings,  useNA = "ifany" )
table(cps$savings, cps$cps19_property_6,  useNA = "ifany" )

#recode Ideology (cps19_lr_scale_bef_1 & cps19_lr_scale_aft_1)
look_for(cps, "scale")
cps %>% 
  mutate(ideology=case_when(
    cps19_lr_scale_bef_1==0 | cps19_lr_scale_aft_1==0 ~ 0,
    cps19_lr_scale_bef_1==1 | cps19_lr_scale_aft_1==1 ~ 0.1,
    cps19_lr_scale_bef_1==2 | cps19_lr_scale_aft_1==2 ~ 0.2,
    cps19_lr_scale_bef_1==3 | cps19_lr_scale_aft_1==3 ~ 0.3,
    cps19_lr_scale_bef_1==4 | cps19_lr_scale_aft_1==4 ~ 0.4,
    cps19_lr_scale_bef_1==5 | cps19_lr_scale_aft_1==5 ~ 0.5,
    cps19_lr_scale_bef_1==6 | cps19_lr_scale_aft_1==6 ~ 0.6,
    cps19_lr_scale_bef_1==7 | cps19_lr_scale_aft_1==7 ~ 0.7,
    cps19_lr_scale_bef_1==8 | cps19_lr_scale_aft_1==8 ~ 0.8,
    cps19_lr_scale_bef_1==9 | cps19_lr_scale_aft_1==9 ~ 0.9,
    cps19_lr_scale_bef_1==10 | cps19_lr_scale_aft_1==10 ~ 1,
  ))->cps
val_labels(cps$ideology)<-c(Left=0, Right=1)
#checks
val_labels(cps$ideology)
table(cps$ideology, cps$cps19_lr_scale_bef_1 , useNA = "ifany" )
table(cps$ideology, cps$cps19_lr_scale_aft_1 , useNA = "ifany" )
table(cps$ideology, useNA = "ifany" )

#recode Satisfaction with Democracy (cps19_demsat)
look_for(cps, "demsat")
cps$satisfaction_demos<-Recode(cps$cps19_demsat, "1=1; 2=0.66; 3=0.33; 4=0; else=NA")
#checks
table(cps$satisfaction_demos, cps$cps19_demsat, useNA = "ifany" )

#or left-right (0-1)
cps$satisfaction_demos2<-Recode(cps$cps19_demsat, "1=0; 2=0.33; 3=0.66; 4=1; else=NA")
#checks
table(cps$satisfaction_demos2, cps$cps19_demsat, useNA = "ifany" )

#recode Financial Retrospective (cps19_own_fin_retro)
look_for(cps, "situation")
cps$financial_retrospective<-Recode(cps$cps19_own_fin_retro, "1=1; 2=0.5; 3=0; else=NA", as.numeric=T)
val_labels(cps$financial_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(cps$financial_retrospective)
table(cps$financial_retrospective , cps$cps19_own_fin_retro, useNA = "ifany" )

#recode Economy Retrospective (cps19_econ_retro)
look_for(cps, "economy")
cps$economy_retrospective<-Recode(cps$cps19_econ_retro, "1=1; 2=0.5; 3=0; else=NA", as.numeric=T)
val_labels(cps$economy_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(cps$economy_retrospective)
table(cps$economy_retrospective , cps$cps19_econ_retro, useNA = "ifany" )

#### recode political attitudes thermometers ####
look_for(cps, "therm")
library(psych)
#Rescale to 0 and 1 by dividing by 100
#Then reverse so it runs left to right

#recode racial minorities
cps$minorities<-cps$cps19_groups_therm_1/100
table(cps$minorities, useNA = "ifany" )
cps$minorities<-as.numeric(reverse.code(-1, cps[,'minorities']))
table(cps$minorities, useNA = "ifany" )

#recode immigrants
cps$immigrants<-cps$cps19_groups_therm_2/100
table(cps$immigrants, useNA = "ifany" )
cps$immigrants<-as.numeric(reverse.code(-1, cps[,'immigrants']))
table(cps$immigrants, useNA = "ifany" )

#recode francophones
cps$francophones<-cps$cps19_groups_therm_3/100
table(cps$francophones, useNA = "ifany" )
cps$francophones<-as.numeric(reverse.code(-1, cps[,'francophones']))
table(cps$francophones, useNA = "ifany" )

#recode feminists
cps$feminists<-cps$cps19_groups_therm_4/100
table(cps$feminists, useNA = "ifany" )
cps$feminists<-as.numeric(reverse.code(-1, cps[,'feminists']))
table(cps$feminists, useNA = "ifany" )

# Index apha checks
cps %>% 
  select(minorities, immigrants) %>% 
  alpha(.)

cps %>% 
  select(minorities, immigrants, feminists) %>% 
  alpha(.)

cps %>% 
  select(minorities, immigrants, feminists, francophones) %>% 
  alpha(.)

cps %>% 
  select(immigrants, feminists) %>% 
  alpha(.)

#recode nativism index (combines racial minorities and immigrants)
cps$nativism1<-cps$minorities
cps$nativism2<-cps$immigrants

cps %>% 
  rowwise() %>% 
  mutate(nativism=mean(
    c_across(nativism1:nativism2)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('nativism1', 'nativism2', 'nativism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na<3)

#Scale Averaging 
cps %>% 
  rowwise() %>% 
  mutate(nativism=mean(
    c_across(num_range('nativism', 1:2)), na.rm=T  
  )) %>% 
  ungroup()->cps

cps %>% 
  select(starts_with("nativism")) %>% 
  summary()
#Check distribution
qplot(cps$nativism, geom="histogram")
table(cps$nativism, useNA="ifany")

#Calculate Cronbach's alpha
cps %>% 
  select(nativism1, nativism2) %>% 
  psych::alpha(.)
#Check correlation
cps %>% 
  select(nativism1, nativism2) %>% 
  cor(., use="complete.obs")

#recode authoritarianism (combines racial minorities + immigrants + feminists)
cps$authoritarianism1<-cps$minorities
cps$authoritarianism2<-cps$immigrants
cps$authoritarianism3<-cps$feminists

cps %>% 
  rowwise() %>% 
  mutate(authoritarianism=mean(
    c_across(authoritarianism1:authoritarianism3)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('authoritarianism1', 'authoritarianism2', 'authoritarianism3', 'authoritarianism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na<4)

#Scale Averaging 
cps %>% 
  rowwise() %>% 
  mutate(authoritarianism=mean(
    c_across(num_range('authoritarianism', 1:3)), na.rm=T  
  )) %>% 
  ungroup()->cps

cps %>% 
  select(starts_with("authoritarianism")) %>% 
  summary()
#Check distribution
qplot(cps$authoritarianism, geom="histogram")
table(cps$authoritarianism, useNA="ifany")

#Calculate Cronbach's alpha
cps %>% 
  select(authoritarianism1, authoritarianism2, authoritarianism3) %>% 
  psych::alpha(.)
#Check correlation
cps %>% 
  select(authoritarianism1, authoritarianism2, authoritarianism3) %>% 
  cor(., use="complete.obs")

#recode authoritarian (combines immigrants + feminists)
cps$authoritarian1<-cps$immigrants
cps$authoritarian2<-cps$feminists

cps %>% 
  rowwise() %>% 
  mutate(authoritarian=mean(
    c_across(authoritarian1:authoritarian2)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('authoritarian1', 'authoritarian2', 'authoritarian')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na<3)

#Scale Averaging 
cps %>% 
  rowwise() %>% 
  mutate(authoritarian=mean(
    c_across(num_range('authoritarian', 1:2)), na.rm=T  
  )) %>% 
  ungroup()->cps

cps %>% 
  select(starts_with("authoritarian")) %>% 
  summary()
#Check distribution
qplot(cps$authoritarian, geom="histogram")
table(cps$authoritarian, useNA="ifany")

#Calculate Cronbach's alpha
cps %>% 
  select(authoritarian1, authoritarian2) %>% 
  psych::alpha(.)
#Check correlation
cps %>% 
  select(authoritarian1, authoritarian2) %>% 
  cor(., use="complete.obs")
cps %>% 
  select(c("volatility", "consequence", "probability"), male) %>% 
  as_factor() %>% 
  pivot_longer(1:3) %>% 
  group_by(male, name) %>% 
  summarize(average=mean(value, na.rm=T)) %>% 
  ggplot(., aes(x=male, y=average))+geom_point()+facet_wrap(~name)
  


