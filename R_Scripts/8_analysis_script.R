# Primary Script for Class 2019 Paper
library(stargazer)
library(broom)
library(nnet)
library(purrr)

#Install these packages
#devtools::install_github("r-lib/vctrs")
#install.packages(srvyr)


#CREATE WORKING CLASS DICHOTOMOUS VARIABLE; NOTE HERE ONLY EMPLOYED AND SELF-EMPLOYED PEOPLE ARE SET TO 0 OR 1; ELSE = NA
ces$working_class<-Recode(ces$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
####WHAT IS THE DIFFERENCE BEETWEEN WORKING_CLASS AND WORKING_CLASS2
ces$working_class2<-Recode(ces$occupation, "4:5=1; else=0")

#This collapses the two labour categories into one working class
ces$occupation2<-Recode(as.factor(ces$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))

#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces$occupation4<-Recode(as.factor(ces$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
ces$working_class3<-Recode(ces$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
ces$working_class4<-Recode(ces$occupation3, "4:5=1; else=0")
##checks
table(ces$working_class, ces$election)
table(ces$working_class2, ces$election)
table(ces$working_class3, ces$election)
table(ces$working_class4, ces$election)

#### First Graph historical NDP vote ####
library(ggeffects)
#Get ROC Models to 2019
ces %>% 
  filter(election!=2000& vote!=0) %>% 
  nest(-election,-quebec ) %>% 
  mutate(model=map(data, function(x) multinom(as_factor(vote)~occupation2, data=x)),
         predicted=map(model, ggpredict)) ->class_vote_2019

#Start with where the predicted values are stored
class_vote_2019%>% 
  #unnest_wider is a new command; I just found out about it today;
  #unnest the predicted values objects
  unnest_wider(predicted) %>%
  #Now unnest occupation2
  unnest(occupation2) %>%
  #filter in only probability of voting for left
  filter(response.level!="Green"& response.level!="Other"& x=="Working_Class"& !is.na(quebec)) %>% 
  #PLot as line plot 
  ggplot(., aes(x=election, y=predicted, group=x))+geom_line()+facet_grid(as_factor(quebec)~response.level)+theme(axis.text.x=element_text(angle=90))+labs(x="Election", y="Marginal Effect")

ggsave(here("Plots", "class_voting_2019.png"), width=6, height=3)

#### Some 2015 recodes ####
ces15phone$working_class<-Recode(ces15phone$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
#This collapses the two labour categories into one working class
ces15phone$occupation2<-Recode(as.factor(ces15phone$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces15phone$occupation4<-Recode(as.factor(ces15phone$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
#this is the NDP vote variable
ces15phone$ndp<-car::Recode(ces15phone$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
table(ces15phone$working_class)
table(ces15phone$ndp)
#Let's put the working class variables in order
ces15phone$occupation2<-fct_relevel(ces15phone$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')
ces15phone$occupation4<-fct_relevel(ces15phone$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')
table(ces15phone$occupation4)
ces15phone$working_class2<-Recode(ces15phone$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
table(ces15phone$working_class2)

#Turn region into factor with East as reference case
ces15phone$region3<-Recode(as.factor(ces15phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces15phone$region3)
table(ces15phone$region3)

#Other dummies
ces15phone$low_income<-Recode(ces15phone$income, "2:5=0; 1=1")
ces15phone$high_income<-Recode(ces15phone$income, "1:4=0; 5=1")
ces15phone$no_religion<-Recode(ces15phone$religion, "0=1; 1:3=0; NA=NA")
ces15phone$catholic<-Recode(ces15phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces15phone$young<-Recode(ces15phone$age, "35:115=0; 18:34=1")
ces15phone$old<-Recode(ces15phone$age, "55:115=1; 18:54=0")
ces15phone$foreign<-Recode(ces15phone$native, "1=0; 0=1")
val_labels(ces15phone$foreign)<-c(Foreign=1, Native=0)
#Dummies coded missing as 0
#ces15phone$low_income<-Recode(ces15phone$income, "else=0; 1=1")
#ces15phone$high_income<-Recode(ces15phone$income, "else=0; 5=1")
#ces15phone$no_religion<-Recode(ces15phone$religion, "0=1; else=0")
#ces15phone$catholic<-Recode(ces15phone$religion, "1=1; else=0")
#ces15phone$young<-Recode(ces15phone$age, "else=0; 18:34=1")
#ces15phone$old<-Recode(ces15phone$age, "55:100=1; else=0")
#ces15phone$foreign<-Recode(ces15phone$native, "else=0; 0=1")

ces15phone$working_class<-Recode(ces15phone$working_class, "1=1; else=0")
ces15phone$working_class2<-Recode(ces15phone$working_class2, "1=1; else=0")
#ces15phone$union_both<-Recode(ces15phone$union_both, "1=1; else=0")
#ces15phone$male<-Recode(ces15phone$male, "1=1; else=0")
#ces15phone$sector<-Recode(ces15phone$sector, "1=1; else=0")
#ces15phone$degree<-Recode(ces15phone$degree, "1=1; else=0")
#ces15phone$language<-Recode(ces15phone$language, "1=1; else=0")

# Party Id
#ces15phone$liberal_id<-Recode(ces15phone$party_id, "1=1; 0=0; 2:4=0; else=NA")
#ces15phone$conservative_id<-Recode(ces15phone$party_id, "2=1; 0:1=0; 3:4=0; else=NA")
#ces15phone$ndp_id<-Recode(ces15phone$party_id, "3=1; 0:2=0; 4=0; else=NA")
#ces15phone$bloc_id<-Recode(ces15phone$party_id, "4=1; 0:3=0; else=NA")
ces15phone$liberal_id<-Recode(ces15phone$party_id, "1=1; else=0")
ces15phone$conservative_id<-Recode(ces15phone$party_id, "2=1; else=0")
ces15phone$ndp_id<-Recode(ces15phone$party_id, "3=1; else=0")
ces15phone$bloc_id<-Recode(ces15phone$party_id, "4=1; else=0")

# Party vote
#ces15phone$liberal<-Recode(ces15phone$vote, "1=1; 0=0; 2:5=0; else=NA")
#ces15phone$conservative<-Recode(ces15phone$vote, "2=1; 0:1=0; 3:5=0; else=NA")
#ces15phone$ndp<-Recode(ces15phone$vote, "3=1; 0:2=0; 4:5=0; else=NA")
#ces15phone$bloc<-Recode(ces15phone$vote, "4=1; 0:3=0; 5=0; else=NA")
#ces15phone$green<-Recode(ces15phone$vote, "5=1; 0:4=0; else=NA")
ces15phone$liberal<-Recode(ces15phone$vote, "1=1; else=0")
ces15phone$conservative<-Recode(ces15phone$vote, "2=1; else=0")
ces15phone$ndp<-Recode(ces15phone$vote, "3=1; else=0")
ces15phone$bloc<-Recode(ces15phone$vote, "4=1; else=0")
ces15phone$green<-Recode(ces15phone$vote, "5=1; else=0")


#### 2019 Recodes ####

#Recodes
#CREATE WORKING CLASS DICHOTOMOUS VARIABLE; NOTE HERE ONLY EMPLOYED AND SELF-EMPLOYED PEOPLE ARE SET TO 0 OR 1; ELSE = NA
ces19phone$working_class<-Recode(ces19phone$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
#This collapses the two labour categories into one working class
ces19phone$occupation2<-Recode(as.factor(ces19phone$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces19phone$occupation4<-Recode(as.factor(ces19phone$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
#this is the NDP vote variable
ces19phone$ndp<-car::Recode(ces19phone$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
table(ces19phone$working_class)
table(ces19phone$ndp)
#Let's put the working class variables in order
ces19phone$occupation2<-fct_relevel(ces19phone$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')
ces19phone$occupation4<-fct_relevel(ces19phone$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')
table(ces19phone$occupation4)
ces19phone$working_class2<-Recode(ces19phone$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
#Turn region into all of Canada (4 regions)
ces19phone %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces19phone
table(ces19phone$region2)
ces19phone$region4<-factor(ces19phone$region2, levels=c("Atlantic", "Quebec", "Ontario", "West"))
levels(ces19phone$region4)
table(ces19phone$region4)
ces19phone$region4<-factor(ces19phone$region2, levels=c("Atlantic", "Quebec", "Ontario", "West"))
table(ces19phone$working_class2)

#Turn region into factor with East as reference case
ces19phone$region3<-Recode(as.factor(ces19phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces19phone$region3)
table(ces19phone$region3)

#Other dummies
ces19phone$low_income<-Recode(ces19phone$income, "2:5=0; 1=1")
ces19phone$high_income<-Recode(ces19phone$income, "1:4=0; 5=1")
ces19phone$no_religion<-Recode(ces19phone$religion, "0=1; 1:3=0; NA=NA")
ces19phone$catholic<-Recode(ces19phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces19phone$young<-Recode(ces19phone$age, "35:100=0; 18:34=1")
ces19phone$old<-Recode(ces19phone$age, "55:100=1; 18:54=0")
ces19phone$foreign<-Recode(ces19phone$native, "1=0; 0=1")
val_labels(ces19phone$foreign)<-c(Foreign=1, Native=0)
#Dummies coded missing as 0
#ces19phone$low_income<-Recode(ces19phone$income, "else=0; 1=1")
#ces19phone$high_income<-Recode(ces19phone$income, "else=0; 5=1")
#ces19phone$no_religion<-Recode(ces19phone$religion, "0=1; else=0")
#ces19phone$catholic<-Recode(ces19phone$religion, "1=1; else=0")
#ces19phone$young<-Recode(ces19phone$age, "else=0; 18:34=1")
#ces19phone$old<-Recode(ces19phone$age, "55:100=1; else=0")
#ces19phone$foreign<-Recode(ces19phone$native, "else=0; 0=1")

ces19phone$working_class<-Recode(ces19phone$working_class, "1=1; else=0")
ces19phone$working_class2<-Recode(ces19phone$working_class2, "1=1; else=0")
ces19phone$working_class3<-Recode(ces19phone$working_class2, "1=1; else=0")
#ces19phone$union_both<-Recode(ces19phone$union_both, "1=1; else=0")
#ces19phone$male<-Recode(ces19phone$male, "1=1; else=0")
#ces19phone$sector<-Recode(ces19phone$sector, "1=1; else=0")
#ces19phone$degree<-Recode(ces19phone$degree, "1=1; else=0")
#ces19phone$language<-Recode(ces19phone$language, "1=1; else=0")

# Party Id
#ces19phone$liberal_id<-Recode(ces19phone$party_id, "1=1; 0=0; 2:4=0; else=NA")
#ces19phone$conservative_id<-Recode(ces19phone$party_id, "2=1; 0:1=0; 3:4=0; else=NA")
#ces19phone$ndp_id<-Recode(ces19phone$party_id, "3=1; 0:2=0; 4=0; else=NA")
#ces19phone$bloc_id<-Recode(ces19phone$party_id, "4=1; 0:3=0; else=NA")
ces19phone$liberal_id<-Recode(ces19phone$party_id, "1=1; else=0")
ces19phone$conservative_id<-Recode(ces19phone$party_id, "2=1; else=0")
ces19phone$ndp_id<-Recode(ces19phone$party_id, "3=1; else=0")
ces19phone$bloc_id<-Recode(ces19phone$party_id, "4=1; else=0")
table(ces19phone$liberal_id)
table(ces19phone$conservative_id)
table(ces19phone$ndp_id)
table(ces19phone$bloc_id)

# Party vote
#ces19phone$liberal<-Recode(ces19phone$vote, "1=1; 0=0; 2:5=0; else=NA")
#ces19phone$conservative<-Recode(ces19phone$vote, "2=1; 0:1=0; 3:5=0; else=NA")
#ces19phone$ndp<-Recode(ces19phone$vote, "3=1; 0:2=0; 4:5=0; else=NA")
#ces19phone$bloc<-Recode(ces19phone$vote, "4=1; 0:3=0; 5=0; else=NA")
#ces19phone$green<-Recode(ces19phone$vote, "5=1; 0:4=0; else=NA")
ces19phone$liberal<-Recode(ces19phone$vote, "1=1; else=0")
ces19phone$conservative<-Recode(ces19phone$vote, "2=1; else=0")
ces19phone$ndp<-Recode(ces19phone$vote, "3=1; else=0")
ces19phone$bloc<-Recode(ces19phone$vote, "4=1; else=0")
ces19phone$green<-Recode(ces19phone$vote, "5=1; else=0")
table(ces19phone$liberal)
table(ces19phone$conservative)
table(ces19phone$ndp)
table(ces19phone$bloc)
table(ces19phone$green)

#Create manage economy dummy variables
ces15phone$liberal_economy<-Recode(ces15phone$manage_economy, "1=1; else=0")
ces15phone$conservative_economy<-Recode(ces15phone$manage_economy, "2=1; else=0")
ces15phone$ndp_economy<-Recode(ces15phone$manage_economy, "3=1; else=0")
ces15phone$bloc_economy<-Recode(ces15phone$manage_economy, "4=1; else=0")
ces15phone$green_economy<-Recode(ces15phone$manage_economy, "5=1; else=0")
table(ces15phone$liberal_economy)
table(ces15phone$conservative_economy)
table(ces15phone$ndp_economy)
table(ces15phone$bloc_economy)
table(ces15phone$green_economy)

ces19phone$liberal_economy<-Recode(ces19phone$manage_economy, "1=1; else=0")
ces19phone$conservative_economy<-Recode(ces19phone$manage_economy, "2=1; else=0")
ces19phone$ndp_economy<-Recode(ces19phone$manage_economy, "3=1; else=0")
ces19phone$bloc_economy<-Recode(ces19phone$manage_economy, "4=1; else=0")
ces19phone$green_economy<-Recode(ces19phone$manage_economy, "5=1; else=0")
table(ces19phone$liberal_economy)
table(ces19phone$conservative_economy)
table(ces19phone$ndp_economy)
table(ces19phone$bloc_economy)
table(ces19phone$green_economy)

#Create manage environment dummy variables
ces19phone$liberal_environment<-Recode(ces19phone$manage_environment, "1=1; else=0")
ces19phone$conservative_environment<-Recode(ces19phone$manage_environment, "2=1; else=0")
ces19phone$ndp_environment<-Recode(ces19phone$manage_environment, "3=1; else=0")
ces19phone$bloc_environment<-Recode(ces19phone$manage_environment, "4=1; else=0")
ces19phone$green_environment<-Recode(ces19phone$manage_environment, "5=1; else=0")
table(ces19phone$liberal_environment)
table(ces19phone$conservative_environment)
table(ces19phone$ndp_environment)
table(ces19phone$bloc_environment)
table(ces19phone$green_environment)

#Create address main issue dummy variables
ces19phone$liberal_issue<-Recode(ces19phone$address_issue, "1=1; else=0")
ces19phone$conservative_issue<-Recode(ces19phone$address_issue, "2=1; else=0")
ces19phone$ndp_issue<-Recode(ces19phone$address_issue, "3=1; else=0")
ces19phone$bloc_issue<-Recode(ces19phone$address_issue, "4=1; else=0")
ces19phone$green_issue<-Recode(ces19phone$address_issue, "5=1; else=0")
table(ces19phone$liberal_issue)
table(ces19phone$conservative_issue)
table(ces19phone$ndp_issue)
table(ces19phone$bloc_issue)
table(ces19phone$green_issue)

#### Logistic models ####

#Weight the sample 
#load library
library(srvyr)
ces19phone %>%
  #filter out those cases that have no weights for the PES variable
  filter(!is.na(weight_PES)) %>% 
  as_survey_design(weights=weight_PES) ->ces19phone.weight
#Check nrows()
nrow(ces19.roc)
nrow(ces19.roc.weight)
nrow(ces19phone)
nrow(ces19phone.weight)

#Split QC out into ces19.qc
#unweighted
ces19phone %>% 
  filter(quebec==1)->ces19.qc
ces19phone %>% 
  filter(quebec!=1)->ces19.roc
#Weighted QC sample
ces19phone.weight %>% 
  filter(quebec==1)->ces19.qc.weight
#WEighted roc sample
ces19phone.weight %>% 
  filter(quebec!=1)->ces19.roc.weight
#Logistic Regression Models for Class Variables
m1.all<-glm(ndp~working_class3+union_both+income+degree+sector, data=ces19phone, family="binomial")
m1.roc<-glm(ndp~working_class3+union_both+income+degree+sector, data=ces19.roc, family="binomial")
m1.qc<-glm(ndp~working_class3+union_both+income+degree+sector, data=ces19.qc, family="binomial")
library(survey)
#Compare and
m1.all.weight<-svyglm(ndp~working_class3+union_both+income+degree+sector,ces19phone.weight)
m1.roc.weight<-svyglm(ndp~working_class3+union_both+income+degree+sector, design=ces19.roc.weight)
m1.qc.weight<-svyglm(ndp~working_class3+union_both+income+degree+sector,design=ces19.qc.weight)
#Compare with model
stargazer(m1.all, m1.all.weight, m1.roc, m1.roc.weight,m1.qc, m1.qc.weight, type="html", out=here("Tables", "class_models_weighted_unweighted.html"), column.labels = c('CAN', 'CAN', 'ROC', 'ROC','QC', 'QC'))

#Play with weighting 

#Logistic Regression MOdels with Demographic Controls
m2.all<-glm(ndp~working_class3+union_both+income+degree+sector+age+male+vismin+as.factor(region4), vismin, data=ces19phone, family="binomial")
m2.roc<-glm(ndp~working_class3+union_both+income+degree+sector+age+male+vismin+region3, data=ces19.roc, family="binomial")
m2.qc<-glm(ndp~working_class3+union_both+income+degree+sector+age+male+vismin, data=ces19.qc, family="binomial")

#Logistic Regression MOdels
m2.all.weight<-svyglm(ndp~working_class3+union_both+income+degree+sector+age+male+vismin+as.factor(region4)+vismin, design=ces19phone.weight, family="binomial")
m2.roc.weight<-svyglm(ndp~working_class3+union_both+income+degree+sector+age+male+vismin+region3, design=ces19phone.weight, family="binomial")
m2.qc.weight<-svyglm(ndp~working_class3+union_both+income+degree+sector+age+male+vismin, design=ces19phone.weight, family="binomial")
#Compare m2 v.
stargazer(m2.all, m2.all.weight, m2.roc, m2.roc.weight, m2.qc, m2.qc.weight, column.labels = c('CAN', 'CAN', 'ROC', 'ROC','QC', 'QC'), out=here("Tables", "class_demographic_controls_weighted_unweighted.html"),type="html")


#Print complete models
stargazer(m1.all, m1.roc, m1.qc, m2.all, m2.roc, m2.qc, type="html", out=here("Tables", "basic_class_models1.html"), column.labels = rep(c("CAN", "ROC", "QC"),2))

#### Comparing ces15 and ces19 Block Recursive Models ####

#First make a ces15 roc data frame
ces15phone %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language,          market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, liberal_economy, ndp_economy, conservative_economy, bloc_economy, 
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec, occupation4, minorities, immigration, immigration2, immigration_rate, minorities_help, mip, vismin)->out15
#Now an ces19data frame
ces19phone %>% 
#  filter(quebec!=1) %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, liberal_environment, ndp_environment, conservative_environment,
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, liberal_economy, ndp_economy, conservative_economy, bloc_economy, liberal_issue, conservative_issue, ndp_issue,
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec, occupation4, minorities, immigration, immigration2, immigration_rate, minorities_help, mip, vismin)->out19

#### Build out combining ces2015 and 2019 ####
out15$survey<-rep(0, nrow(out15))
out19$survey<-rep(1, nrow(out19))
out15 %>% 
  bind_rows(., out19)->out
#Split out into ROC 
roc<-out %>% 
  filter(quebec!=1)
#Split out into Quebec
qc<-out %>% 
  filter(quebec==1)
table(roc$vismin, roc$foreign, useNA = "ifany")
#### NDP ROC ####

block1<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence)*survey, family="binomial", data=roc)
block6<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)

#Turn into a list
roc_ndp<-list(block1, block2, block3, block4, block5, block6)
names(roc_ndp)<-c("block1", "block2", "block3", "block4", "block5", "block6")

library(kableExtra)
library(knitr)
## This code  pulls it all nicely together. 
roc_ndp %>% 
  #Tidy each of thle models 
  map(., tidy) %>% 
  #bind them; the argument .id="Block" makes a new variable called Block filling it with the names of the list items from line 235
  bind_rows(., .id="Block") %>% 
  #filter in only the interaction terms; they all contain :survey
  filter(str_detect(term,":survey")) %>% 
  #Keep only the first instance of each interaction term
  #First group by term; this forms groups of the terms
  group_by(term) %>% 
  #this picks only the first term
  slice(1) %>% 
  #get rid of the :survey
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  #arrange them by block so that block1 variables appear first
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->roc_ndp_table

#### NDP QC ####

block1<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+vismin)*survey, family="binomial", data=qc)
block2<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence)*survey, family="binomial", data=qc)
block6<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
#Turn into a list
qc_ndp<-list(block1, block2, block3, block4, block5, block6)
names(qc_ndp)<-c("block1", "block2", "block3", "block4", "block5", "block6")

qc_ndp %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->qc_ndp_table

#### Conservative ROC####

block1<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence)*survey, family="binomial", data=roc)
block6<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)
#Turn into a list
roc_conservative<-list(block1, block2, block3, block4, block5, block6)
names(roc_conservative)<-c("block1", "block2", "block3", "block4", "block5", "block6")

roc_conservative %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->roc_conservative_table

#### Conservative QC ####
block1<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+vismin)*survey, family="binomial", data=qc)
block2<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence)*survey, family="binomial", data=qc)
block6<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
#Turn into a list
qc_conservative<-list(block1, block2, block3, block4, block5, block6)
names(qc_conservative)<-c("block1", "block2", "block3", "block4", "block5", "block6")

qc_conservative %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->qc_conservative_table

#### Liberal ROC Interation ####
block1<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence)*survey, family="binomial", data=roc)
block6<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)
#Turn into a list
roc_liberal<-list(block1, block2, block3, block4, block5, block6)
names(roc_liberal)<-c("block1", "block2", "block3", "block4", "block5", "block6")

roc_liberal %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->roc_liberal_table

#### Liberal QC Interation ####
block1<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+vismin)*survey, family="binomial", data=qc)
block2<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence)*survey, family="binomial", data=qc)
block6<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
#Turn into a list
qc_liberal<-list(block1, block2, block3, block4, block5, block6)
names(qc_liberal)<-c("block1", "block2", "block3", "block4", "block5", "block6")

qc_liberal %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->qc_liberal_table

#### Bloc QC Interation ####
block1<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+vismin)*survey, family="binomial", data=qc)
block2<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence)*survey, family="binomial", data=qc)

block6<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
#Turn into a list
qc_bloc<-list(block1, block2, block3, block4, block5, block6)
names(qc_bloc)<-c("block1", "block2", "block3", "block4", "block5", "block6")

qc_bloc %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->qc_bloc_table
roc_ndp_table %>% View()
roc_liberal_table
roc_conservative_table
#### Format Nice comprehensive QC and ROC Tables
#Step 1 combine all the parties' tables into roc and qc
roc_table<-cbind(roc_ndp_table, roc_liberal_table, roc_conservative_table)

qc_table<-cbind(qc_ndp_table, qc_bloc_table, qc_liberal_table, qc_conservative_table)
library(flextable)
#### C
#Drop out terms we don't need.
names(roc_table)
#Keep the first Block and the first term
#So drop columns 5 and 9, 6 and 10
roc_table %>% 
  select(-5, -9, -6, -10) %>% 
  #Rename them just proper names
rename(., Block=1, term=2, NDP=3, sig_ndp=4, Liberal=5,sig_liberal=6, Conservative=7, sig_con=8) %>% 
  #Turn this object into a flextable object. See https://davidgohel.github.io/flextable/
  flextable(.) %>% 
  #format the flextable to two digits
  colformat_num(digits=2) %>% 
  #bold function bolds rows i that meet conditions and column j
  #So here, it bolds rows i where sig_ndp < 0.05 and only bolds columns j
  #note that it uses formula notation ~
bold(., i=~sig_ndp< 0.05, j=~NDP+sig_ndp) %>% 
  #Repeat for LIberals
  bold(., i=~sig_liberal< 0.05, j=~Liberal+sig_liberal) %>% 
  #conservatives
bold(., i=~sig_con< 0.05, j=~Conservative+sig_con) %>% 
  #This sets the background colour conditional on the term
  #So if it is block1, 3 or 5, grey it out. 
  bg(., i=~str_detect(Block, "block1|block3|block5"), bg="grey") %>% 
    add_header_lines(values=c("ROC Block Recursive Model Coefficients, 2015 and 2019")) %>% 
#  add_footer_row(pseudos, colwidths=c(4,4))
save_as_html("Tables/roc_block_recursive_table.html")

####Combine Quebec Table ####
#Drop out terms we don't need.
#First chekc the names
names(qc_table)
#Keep the first Block and the first term
#So drop columns 5, 6 and 9, 10 and 13 and 14
qc_table %>% 
  select(-5,  -6, -9,-10, -13, -14) %>% 
rename(., Block=1, term=2, NDP=3, sig_ndp=4, Liberal=5,sig_liberal=6, Conservative=7, sig_con=8, BQ=9, sig_bq=10) %>% 
  flextable(.) %>% 
  colformat_num(digits=2) %>% 
bold(., i=~sig_ndp< 0.05, j=~NDP+sig_ndp) %>% 
  bold(., i=~sig_liberal< 0.05, j=~Liberal+sig_liberal) %>% 
bold(., i=~sig_con< 0.05, j=~Conservative+sig_con) %>% 
bg(., i=~str_detect(Block, "block1|block3|block5"), bg="grey") %>% 
  add_header_lines(values=c("Quebec Block Recursive Model Coefficients, 2015 and 2019")) %>% 
  save_as_html(., "Tables/qc_block_recursive_model.html")
#save_as_docx(., path="Tables/qc_block_recursive_model.docx")


#### Run some checks with what appears itn the table####
  #Could you please just check a few coefficients randomly to be sure they are correct
  #EAch model is stored in either roc_ndp, qc_ndp etc. etc. etc. followed by $block1, $block2, #Just pick four or five randomly in different blocks and in qc, roc. Just enough to be sure we are not making a mistake. 
summary(roc_ndp$block1)#Interaction coefficient for male:survey is -0.38; I have confirmed visually it is -0.39 in the file roc_block_recursive_table. 
summary(roc_liberal$block1)
summary(roc_conservative$block3)
#### Check on union variable ####

## Did the union movement really go down for all parties?
roc_ndp_table %>% 
  filter(term=="union_both")
roc_liberal_table %>% 
  filter(term=="union_both")
roc_conservative_table %>% 
  filter(term=="union_both")

#### Vote Flow ####

ces19phone %>% 
  select(past_vote, vote, occupation4) %>% 
  filter(past_vote!=0 & vote!=0) %>% 
    group_by(vote, past_vote) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(perc=n/sum(n)) %>% 
  mutate(vote19=case_when(
    past_vote==1&vote==1 ~ "Stay Liberal",
    past_vote==2&vote==2 ~ "Stay Conservative",
    past_vote==3&vote==3 ~ "Stay NDP",
    past_vote==4 & vote==4 ~ "Stay BQ",
    past_vote==5 & vote==5 ~ "Stay Green",
   vote==1 & past_vote!=1 ~ "To Liberal",
   vote==2 &past_vote!=2 ~ "To Conservative",
   vote==3 & past_vote!=3 ~ "To NDP",
   vote==4 & past_vote!=4 ~ "To BQ",
   vote==5 & past_vote!=5 ~ "To Green"
  )) %>% 
  mutate(vote19=factor(vote19, levels=c("Stay Liberal","To Liberal" ,"Stay Conservative","To Conservative", "Stay NDP", "To NDP", "Stay BQ", "To BQ", "Stay Green", "To Green"))) %>% 
  mutate(past_vote=fct_relevel(as_factor(past_vote),"Green", "Bloc", "NDP", "Conservative", "Liberal"), vote=as_factor(vote))%>%
  ggplot(., aes(x=vote19, y=perc, fill=past_vote))+geom_col()+coord_flip()+labs(y="Percent", x="Past Vote")+scale_fill_grey(start=0.8, end=0.2, name="Past Vote")



#### Policy variation change between 2015-19####

## Currently all policy variables are coded 0 to 1 such that 1 is *more* of the variable name
library(psych)
out %>% 
  select(survey, market_liberalism, moral_traditionalism, defence, environment) %>% 
  summary()
## This works well for the purposes of fitting models, because a positive coefficient reflects an inclination that greater support for that variable is positively linked to whatever DV you ahve. 
  out %>% 
  select(survey, market_liberalism, moral_traditionalism, continentalism, defence, environment) %>% 
pivot_longer(cols=-survey) %>% 
  group_by(survey, name) %>% 
  summarize(avg=mean(value, na.rm=T))

#Howevr, for th gtraph that we want, we need this to look different.
#We want a positive number to reflect a *right-ward* shift from 2015 to 2019 and a negative number to reflect a left-ward shift. This is just purely visual. 
  
#We know from the above there was a shift *away* from market liberalism, *toward* moral_traditionalism, *toward* environme ntalism and *toward* defence spending, *awa* from continentalism

test1<-t.test(market_liberalism~survey, data=out)
test2<-t.test(moral_traditionalism~survey, data=out)
#The term estimate reports the difference
tidy(test1)
tidy(test2)

#So the term estimate reports the value for 2015-2019. When 2015-2019 is a right-ward shift, we want that number to be positive; when 2015-2019 is a *left-ward* shift, we need it to be negative. 

#So right now, there is a positive sign for the shift in market_liberalism, because the 2015 score was higher than in 2019
#But it actually was a *left-ward shift*. 
#So, if we reverse the sign, on those variables where 1 = a right-wing position, we'll get the results we want. 
#I am storing the negative-coded variables in _x
out$moral_traditionalism_x<-out$moral_traditionalism*-1
out$market_liberalism_x<-out$market_liberalism*-1
out$continentalism_x<-out$continentalism*-1
out$defence_x<-out$defence*-1
out$immigration_rate<-out$immigration_rate*-1
#Now check:
test3<-t.test(market_liberalism_x~survey, data=out)
tidy(test3)
tidy(test1)
#The sign for market liberalism is just reversed, so it works. 
rm(test1, test2, test3)

#Policy rating changes
#Start with tghe combined 15 and 19 data frame
out %>% 
  #select the variables we need for the attitudinal shifts
  #pay attention to selecting the variables that have been recoded i.e. they end with _x
  select(ends_with("_x"),  environment,redistribution, immigration, immigration_rate, minorities_help, survey, quebec, occupation4) %>% 
  #Rename the variables for aesthetic purposes
rename(., `Moral Traditionalism`=1, `Market Liberalism`=2, `Continentalism`=3, Defence=4, Environment=5, Redistribution=6, Immigration=7, `Immigration Rate`=8, `Help Minorities`=9) %>% 
  #Pivot these down into one single variable
  pivot_longer(cols=1:9) %>% 
  #There are a few cases missing on this variable
    filter(!is.na(quebec)) %>% 
 # form groups by variable name, occupation and quebec
  nest_by(name, occupation4, quebec=as_factor(quebec)) %>% 
#Tidy the t.test results for value ~ survey
  mutate(mod=tidy(t.test(value~survey, data=data))) %>% 
  #Plot with occupation4 on the x, relevelled to put workers at the bottom and mamnagers at the top; y is the estimate
 ggplot(., aes(x=fct_relevel(occupation4, "Working_Class", "Routine_Nonmanual", "Self-Employed", "Professionals", "Managers"), y=mod$estimate))+labs(x="Class", y="Difference (2019-2015)", subtitle="CES 2015 and 2019")+geom_point(aes(col=quebec), size=0.5,position=position_dodge(width=0.5))+facet_wrap(~name)+coord_flip(expand=T, clip="off")+geom_hline(aes(yintercept=0), linetype=2)+scale_color_grey(start=0.2 ,end=0.5)+geom_linerange(aes(ymin=mod$conf.low, ymax=mod$conf.high, col=quebec), position=position_dodge(width=0.5))
ggsave(here("Plots", "attitudinal_differences_2015_2019.png"), width=6, height=4)

#Leader rating changes
out %>% 
  select(`Liberal`=liberal_leader, `Conservative`=conservative_leader, `NDP`=ndp_leader, `BQ`=bloc_leader, Survey=survey, Class=occupation4, Quebec=quebec) %>%
  filter(!is.na(Quebec)) %>% 
  pivot_longer(cols=1:4, names_to=c("Party")) %>% 
    filter(!is.na(value)) %>% 
  nest_by(Class, Quebec=as_factor(Quebec), Party) %>% 
  mutate(mod=tidy(t.test(data=data, value~Survey))) %>% 
  ggplot(., aes(x=fct_relevel(Class, "Working_Class", "Routine_Nonmanual", "Self-Employed", "Professionals", "Managers"), y=mod$estimate*-1, col=Quebec))+geom_point()+ylim(-0.2,0.2)+labs(x="Class", y="Difference (2019-2015)")+coord_flip()+scale_color_grey()+facet_wrap(~Party)+geom_hline(yintercept=0, linetype=2)
ggsave(here("Plots", "leader_approval_ratings.png"), width=6, height=4)
#### Most Important Issue ####
out %>% 
group_by(Survey=as_factor(survey), `Most Important Problem`=as_factor(mip), Quebec=as_factor(quebec), occupation4) %>%
  summarise(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  mutate(Election=car::Recode(Survey, "0=2015; 1=2019", as.factor=T, levels=c("2019", "2015")))%>% 
  filter(!is.na(`Most Important Problem`)) %>% 
  ggplot(., aes(y=reorder(`Most Important Problem`,n), x=n))+geom_col(position=position_dodge(preserve="single"))+scale_fill_grey()+labs(y="Most Important Problem")+facet_wrap(~Election)
ggsave("Plots/mip_2015_2019.png")

#### Status of Race####

#Are the working classes more racialized?
out %>% 
select(survey, vismin, occupation4) %>% 
  as_factor() %>% 
  group_by(survey, occupation4, vismin) %>% 
  filter(!is.na(vismin)) %>% 
  filter(!is.na(occupation4)) %>% 
  summarize(n=n()) %>% 
  mutate(perc=n/sum(n)) %>% 
  print(n=100)


#Racial and Immigration Issues
singh1.roc<-lm(ndp_leader~(immigration+working_class2)*working_class2,data=ces19.roc)
singh2.roc<-lm(ndp_leader~(immigration+minorities_help)*working_class2, data=ces19.roc)
singh3.roc<-lm(ndp_leader~(immigration+minorities_help+environment)*working_class2, data=ces19.roc)
singh1.qc<-lm(ndp_leader~(immigration+working_class2)*working_class2,data=ces19.qc)
singh2.qc<-lm(ndp_leader~(immigration+minorities_help)*working_class2, data=ces19.qc)
singh3.qc<-lm(ndp_leader~(immigration+minorities_help+environment)*working_class2, data=ces19.qc)
stargazer(singh1.roc, singh2.roc, singh3.roc,singh1.qc, singh2.qc, singh3.qc, type="text", out=here("Tables", "singh_approval_interactions.html"), covariate.labels=c("Immigration", "Help Minorities", "Environment", "Working Class", "Immigration* Working Class", "Help Minorities * Working Class", "Environment * Working Class"), dep.var.caption="Approval of Jagmeet Singh, 2019, Outside of Quebec", dep.var.labels=c("Singh Thermometer Rating"), column.labels=c(rep("ROC",3), rep("QC", 3)))

#------------------------------------------------------------------------------------------------
#### Redistribution descriptives ####

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of respondents in ces studies")

ces %>% 
  group_by(election, working_class) %>% 
  filter(!is.na(working_class)) %>%
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(pro_redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average pro-redistribution of respondents in ces studies")

ces %>% 
  group_by(election, working_class) %>% 
  filter(!is.na(working_class)) %>%
  summarize(avg_age=mean(pro_redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average pro-redistribution of WC respondents in ces studies")

ces %>%
  filter(!is.na(redistribution)) %>%
  group_by(working_class, election) %>%
  summarize(mean_redistribution = mean(redistribution))

ces %>%
  filter(!is.na(pro_redistribution)) %>%
  group_by(working_class, election) %>%
  summarize(mean_pro_redistribution = mean(pro_redistribution))

## Share of Pro-redistribution Working Class members voting NDP
ces %>% 
  group_by(election, pro_redistribution, working_class, ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting NDP")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_NDP.png"))

## Share of Pro-redistribution Working Class members voting Conservative
ces %>% 
  group_by(election, pro_redistribution, working_class, conservative) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(conservative)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(conservative==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting Conservative")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_Conservative.png"))

## Share of Pro-redistribution Working Class members voting Liberal
ces %>% 
  group_by(election, pro_redistribution, working_class, liberal) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(liberal)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(liberal==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting Liberal")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_Liberal.png"))

# Working Class redistribution by year
### I couldn't get your code below to work. This next section starts getting at it. 
ces %>% 
  select(election, occupation2, redistribution) %>% 
  group_by(election, occupation2) %>% 
  summarize(support_redistribution=mean(redistribution, na.rm=T)) %>% 
  filter(occupation2=="Working_Class"& election> 1988)
  

ces93 %>%
  select(occupation, redistribution, pro_redistribution) %>% 
  group_by(occupation) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)
# 
# ces97 %>%
#   select(working_class, redistribution, pro_redistribution) %>% 
#   group_by(working_class) %>%
#   summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)
# 
# ces0411 %>%
#   select(working_class04, redistribution04, pro_redistribution04) %>% 
#   group_by(working_class04) %>%
#   summarise_at(vars(redistribution04, pro_redistribution04), mean, na.rm=T)
# 
# ces0411 %>%
#   select(working_class06, redistribution06, pro_redistribution06) %>% 
#   group_by(working_class06) %>%
#   summarise_at(vars(redistribution06, pro_redistribution06), mean, na.rm=T)
# 
# ces0411 %>%
#   select(working_class08, redistribution08, pro_redistribution08) %>% 
#   group_by(working_class08) %>%
#   summarise_at(vars(redistribution08, pro_redistribution08), mean, na.rm=T)
# 
# ces0411 %>%
#   select(working_class11, redistribution11, pro_redistribution11) %>% 
#   group_by(working_class11) %>%
#   summarise_at(vars(redistribution11, pro_redistribution11), mean, na.rm=T)
# 
# ces15phone %>%
#   select(working_class, redistribution, pro_redistribution) %>% 
#   group_by(working_class) %>%
#   summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)
# 
# ces19phone %>%
#   select(working_class, redistribution, pro_redistribution) %>% 
#   group_by(working_class) %>%
#   summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

# Working Class voting by pro-redistribution
# ces19phone %>%
#   select(working_class, pro_redistribution, liberal, conservative, ndp, bloc, green) %>% 
#   group_by(working_class, pro_redistribution) %>%
#   summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
#   as.data.frame() %>% 
#   stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro_redistribution_Working_Class_Vote_2019.html"))
# 
# # Working Class voting pro-redistribution by election
# ces %>%
#   select(election, working_class, pro_redistribution, liberal, conservative, ndp) %>% 
#   group_by(election, working_class, pro_redistribution) %>%
#   summarise_at(vars(liberal, conservative, ndp), mean, na.rm=T) %>% 
#   as.data.frame() %>%
#   filter(!is.na(working_class)) %>% 
#   stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro_redistribution_Working_Class_Vote_by_election.html"))
# 
# #------------------------------------------------------------------------------------------------
# #### Working Class descriptives ####
# 
# #Share of Working class voting NDP
# ces %>% 
#   group_by(election, working_class, ndp) %>% 
#   summarize(n=n()) %>% 
#   filter(is.na(working_class)==F) %>% 
#   filter(is.na(ndp)==F) %>% 
#   mutate(percent=n/sum(n)) %>% 
#   filter(working_class==1) %>% 
#   filter(ndp==1) %>% 
#   ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting NDP")
# ggsave(here("Plots", "NDP_working_class_vote.png"))
# 
# #Share of Working class voting Liberal
# ces %>% 
#   group_by(election, working_class, liberal) %>% 
#   summarize(n=n()) %>% 
#   filter(is.na(working_class)==F) %>% 
#   filter(is.na(liberal)==F) %>% 
#   mutate(percent=n/sum(n)) %>% 
#   filter(working_class==1) %>% 
#   filter(liberal==1) %>% 
#   ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Liberal")
# ggsave(here("Plots", "Liberal_working_class_vote.png"))
# 
# #Share of Working class voting Conservative
# ces %>% 
#   group_by(election, working_class, conservative) %>% 
#   summarize(n=n()) %>% 
#   filter(is.na(working_class)==F) %>% 
#   filter(is.na(conservative)==F) %>% 
#   mutate(percent=n/sum(n)) %>% 
#   filter(working_class==1) %>% 
#   filter(conservative==1) %>% 
#   ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Conservative")
# ggsave(here("Plots", "Conservative_working_class_vote.png"))
# 
# #Share of Working class voting Other
# ces %>% 
#   group_by(election, working_class, other) %>% 
#   summarize(n=n()) %>% 
#   filter(is.na(working_class)==F) %>% 
#   filter(is.na(other)==F) %>% 
#   mutate(percent=n/sum(n)) %>% 
#   filter(working_class==1) %>% 
#   filter(other==1) %>% 
#   ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Other")
# ggsave(here("Plots", "Other_working_class_vote.png"))
# 
# #Party Vote Shares of Working Class
# #This was your code
# 
# # ces %>% 
# #   group_by(election, working_class, vote) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)) %>%
# #   filter(working_class==1 & (vote<4 & vote>0)) %>% 
# #   ggplot(.,aes(x=as.numeric(election), y=pct))+
# #   geom_point()+
# #   geom_smooth(method="lm", se=F)+
# #   facet_grid(~as_factor(vote))+
# #   labs(title="Share of Working Class voting for political parties over time")
# # ggsave(here("Plots", "Party_shares_working_class_vote.png"))
# 
# # #My modifications
# # ces %>% 
# #   group_by(election, working_class, vote) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)*100) %>%
# #   filter(working_class==1 & (vote<4 & vote>0)) %>% 
# #   ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
# #   geom_line()+
# #   geom_point()+
# #   scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
# #   labs(title="Share of Working Class voting for political parties over time", x="Year", y="Percent")
# # ggsave(here("Plots", "Party_shares_working_class_vote.png"))
# # #Percent of NDP Voters Working Class
# # ces %>% 
# #   group_by(election, vote, working_class) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)) %>%
# #   filter(working_class==1 & vote==3) %>% 
# #   ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="NDP Voter % that are Working Class")
# # ggsave(here("Plots", "NDP_Voters_Working_Class_Percent.png"))
# # 
# # #Percent of Liberal Voters Working Class
# # ces %>% 
# #   group_by(election, vote, working_class) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)) %>%
# #   filter(working_class==1 & vote==1) %>% 
# #   ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Liberal Voter % that are Working Class")
# # ggsave(here("Plots", "Lib_Voters_Working_Class_Percent.png"))
# # 
# # #Percent of Conservative Voters Working Class
# # ces %>% 
# #   group_by(election, vote, working_class) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)) %>%
# #   filter(working_class==1 & vote==2) %>% 
# #   ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Conservative Voter % that are Working Class")
# # ggsave(here("Plots", "Con_Voters_Working_Class_Percent.png"))
# 
# 

#### Performance Models ####
library(DescTools)

table(roc$quebec)
table(qc$quebec)

#### NDP ####
#ROC
roc %>% 
  nest(-survey) %>% 
  mutate(ndp_demographics=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income, family="binomial", data=x)), 
         ndp_values=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x)),
         ndp_partisanship=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x)),
             ndp_retrospection=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
         ndp_policy=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x)),
                               ndp_leaders=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>% 
  pivot_longer(., ndp_demographics:ndp_leaders, values_to=c('Model'), names_to=c('Block')) %>% 
  mutate(r2=map(Model, PseudoR2)) %>% 
  unnest(r2) ->ndp_pseudos


ndp_pseudos %>% 
  ggplot(., aes(x=fct_relevel(Block, "ndp_demographics", "ndp_values", "ndp_partisanship", "ndp_retrospection","ndp_policy", "ndp_leaders"), y=r2, col=as.factor(survey)))+geom_point()+coord_flip()
# Block 1 - Demographics

 

roc%>%
  #nest by everything survey to fit one model per survey year
  nest(-survey)%>%
#Create a column called mod that is the result of fitting binomial m odel; data=x i.e. data is each separate survety
  mutate(ndp_demographics=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income, family="binomial", data=x))) %>%
  #Tidy each model for nice use 
 #mutate(tidied=map(demographics, tidy)) %>%
  #add column of PseudoR2 for each 
  mutate(ndp_demographics_r2=map(ndp_demographics, PseudoR2))%>%
  unnest(ndp_demographics_r2)->ndp_demographics_roc_pseudos

qc%>%
  nest(-survey)%>%
  mutate(ndp_demographics=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+vismin, family="binomial", data=x))) %>%
  mutate(ndp_demographics_r2=map(ndp_demographics, PseudoR2))%>%
  unnest(ndp_demographics_r2)->ndp_demographics_qc_pseudos

# Block 2 - Underlying Values
roc%>%
  nest(-survey)%>%
  mutate(ndp_values=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x))) %>%
  mutate(ndp_values_r2=map(ndp_values, PseudoR2))%>%
  unnest(ndp_values_r2)->ndp_values_roc_pseudos

qc%>%
  nest(-survey)%>%
  mutate(ndp_values=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x))) %>%
  mutate(ndp_values_r2=map(ndp_values, PseudoR2))%>%
  unnest(ndp_values_r2)->ndp_values_qc_pseudos

# Block 3 - Partisanship
roc%>%
  nest(-survey)%>%
  mutate(ndp_partisanship=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(ndp_partisanship_r2=map(ndp_partisanship, PseudoR2))%>%
  unnest(ndp_partisanship_r2)->ndp_partisanship_roc_pseudos

qc%>%
  nest(-survey)%>%
  mutate(ndp_partisanship=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x))) %>%
  mutate(ndp_partisanship_r2=map(ndp_partisanship, PseudoR2))%>%
  unnest(ndp_partisanship_r2)->ndp_partisanship_qc_pseudos

# Block 4 - Retrospection
roc%>%
  nest(-survey)%>%
  mutate(ndp_retrospection=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(ndp_retrospection_r2=map(ndp_retrospection, PseudoR2))%>%
  unnest(ndp_retrospection_r2)->ndp_retrospection_roc_pseudos

qc%>%
  nest(-survey)%>%
  mutate(ndp_retrospection=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(ndp_retrospection_r2=map(ndp_retrospection, PseudoR2))%>%
  unnest(ndp_retrospection_r2)->ndp_retrospection_qc_pseudos

# Block 5 - Policy
roc%>%
  nest(-survey)%>%
  mutate(ndp_policy=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(ndp_policy_r2=map(ndp_policy, PseudoR2))%>%
  unnest(ndp_policy_r2)->ndp_partisanship_roc_pseudos

qc%>%
  nest(-survey)%>%
  mutate(ndp_policy=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(ndp_policy_r2=map(ndp_policy, PseudoR2))%>%
  unnest(ndp_policy_r2)->ndp_partisanship_qc_pseudos

# Block 6 - Leaders
roc%>%
  nest(-survey)%>%
  mutate(ndp_leaders=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(ndp_leaders_r2=map(ndp_leaders, PseudoR2))%>%
  unnest(ndp_leaders_r2)->ndp_leaders_roc_pseudos

qc%>%
  nest(-survey)%>%
  mutate(ndp_leaders=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>%
  mutate(ndp_leaders_r2=map(ndp_leaders, PseudoR2))%>%
  unnest(ndp_leaders_r2)->ndp_leaders_qc_pseudos

#### Liberals ####
# Block 1 - Demographics
roc%>%
  nest(-survey)%>%
  mutate(liberal_demographics=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x))) %>%
  mutate(liberal_demographics_r2=map(liberal_demographics, PseudoR2))%>%
  unnest(liberal_demographics_r2)

qc%>%
  nest(-survey)%>%
  mutate(liberal_demographics=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x))) %>%
  mutate(liberal_demographics_r2=map(liberal_demographics, PseudoR2))%>%
  unnest(liberal_demographics_r2)

# Block 2 - Underlying Values
roc%>%
  nest(-survey)%>%
  mutate(liberal_values=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x))) %>%
  mutate(liberal_values_r2=map(liberal_values, PseudoR2))%>%
  unnest(liberal_values_r2)

qc%>%
  nest(-survey)%>%
  mutate(liberal_values=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x))) %>%
  mutate(liberal_values_r2=map(liberal_values, PseudoR2))%>%
  unnest(liberal_values_r2)

# Block 3 - Partisanship
roc%>%
  nest(-survey)%>%
  mutate(liberal_partisanship=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(liberal_partisanship_r2=map(liberal_partisanship, PseudoR2))%>%
  unnest(liberal_partisanship_r2)

qc%>%
  nest(-survey)%>%
  mutate(liberal_partisanship=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x))) %>%
  mutate(liberal_partisanship_r2=map(liberal_partisanship, PseudoR2))%>%
  unnest(liberal_partisanship_r2)

# Block 4 - Retrospection
roc%>%
  nest(-survey)%>%
  mutate(liberal_retrospection=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(liberal_retrospection_r2=map(liberal_retrospection, PseudoR2))%>%
  unnest(liberal_retrospection_r2)

qc%>%
  nest(-survey)%>%
  mutate(liberal_retrospection=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(liberal_retrospection_r2=map(liberal_retrospection, PseudoR2))%>%
  unnest(liberal_retrospection_r2)

# Block 5 - Policy
roc%>%
  nest(-survey)%>%
  mutate(liberal_policy=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(liberal_policy_r2=map(liberal_policy, PseudoR2))%>%
  unnest(liberal_policy_r2)

qc%>%
  nest(-survey)%>%
  mutate(liberal_policy=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(liberal_policy_r2=map(liberal_policy, PseudoR2))%>%
  unnest(liberal_policy_r2)

# Block 6 - Leaders
roc%>%
  nest(-survey)%>%
  mutate(liberal_leaders=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(liberal_leaders_r2=map(liberal_leaders, PseudoR2))%>%
  unnest(liberal_leaders_r2)

qc%>%
  nest(-survey)%>%
  mutate(liberal_leaders=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>%
  mutate(liberal_leaders_r2=map(liberal_leaders, PseudoR2))%>%
  unnest(liberal_leaders_r2)

#### Conservatives ####
# Block 1 - Demographics
roc%>%
  nest(-survey)%>%
  mutate(conservative_demographics=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x))) %>%
  mutate(conservative_demographics_r2=map(conservative_demographics, PseudoR2))%>%
  unnest(conservative_demographics_r2)

qc%>%
  nest(-survey)%>%
  mutate(conservative_demographics=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x))) %>%
  mutate(conservative_demographics_r2=map(conservative_demographics, PseudoR2))%>%
  unnest(conservative_demographics_r2)

# Block 2 - Underlying Values
roc%>%
  nest(-survey)%>%
  mutate(conservative_values=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x))) %>%
  mutate(conservative_values_r2=map(conservative_values, PseudoR2))%>%
  unnest(conservative_values_r2)

qc%>%
  nest(-survey)%>%
  mutate(conservative_values=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x))) %>%
  mutate(conservative_values_r2=map(conservative_values, PseudoR2))%>%
  unnest(conservative_values_r2)

# Block 2 - Underlying Values (market liberalism only)
roc%>%
  nest(-survey)%>%
  mutate(conservative_values1=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism, family="binomial", data=x))) %>%
  mutate(conservative_values1_r2=map(conservative_values1, PseudoR2))%>%
  unnest(conservative_values1_r2)

# Block 2 - Underlying Values (moral traditionalism only)
roc%>%
  nest(-survey)%>%
  mutate(conservative_values2=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+moral_traditionalism, family="binomial", data=x))) %>%
  mutate(conservative_values2_r2=map(conservative_values2, PseudoR2))%>%
  unnest(conservative_values2_r2)

# Block 2 - Underlying Values (continentalism only)
roc%>%
  nest(-survey)%>%
  mutate(conservative_values3=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+continentalism, family="binomial", data=x))) %>%
  mutate(conservative_values3_r2=map(conservative_values3, PseudoR2))%>%
  unnest(conservative_values3_r2)

# Block 2 - Underlying Values (political disaffection only)
roc%>%
  nest(-survey)%>%
  mutate(conservative_values4=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+political_disaffection, family="binomial", data=x))) %>%
  mutate(conservative_values4_r2=map(conservative_values4, PseudoR2))%>%
  unnest(conservative_values4_r2)

# Block 3 - Partisanship
roc%>%
  nest(-survey)%>%
  mutate(conservative_partisanship=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(conservative_partisanship_r2=map(conservative_partisanship, PseudoR2))%>%
  unnest(conservative_partisanship_r2)

qc%>%
  nest(-survey)%>%
  mutate(conservative_partisanship=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x))) %>%
  mutate(conservative_partisanship_r2=map(conservative_partisanship, PseudoR2))%>%
  unnest(conservative_partisanship_r2)

# Block 4 - Retrospection
roc%>%
  nest(-survey)%>%
  mutate(conservative_retrospection=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(conservative_retrospection_r2=map(conservative_retrospection, PseudoR2))%>%
  unnest(conservative_retrospection_r2)

qc%>%
  nest(-survey)%>%
  mutate(conservative_retrospection=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(conservative_retrospection_r2=map(conservative_retrospection, PseudoR2))%>%
  unnest(conservative_retrospection_r2)

# Block 5 - Policy
roc%>%
  nest(-survey)%>%
  mutate(conservative_policy=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(conservative_policy_r2=map(conservative_policy, PseudoR2))%>%
  unnest(conservative_policy_r2)

qc%>%
  nest(-survey)%>%
  mutate(conservative_policy=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(conservative_policy_r2=map(conservative_policy, PseudoR2))%>%
  unnest(conservative_policy_r2)

# Block 6 - Leaders
roc%>%
  nest(-survey)%>%
  mutate(conservative_leaders=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(conservative_leaders_r2=map(conservative_leaders, PseudoR2))%>%
  unnest(conservative_leaders_r2)

qc%>%
  nest(-survey)%>%
  mutate(conservative_leaders=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>%
  mutate(conservative_leaders_r2=map(conservative_leaders, PseudoR2))%>%
  unnest(conservative_leaders_r2)

#### Bloc Quebecois ####
# Block 1 - Demographics
qc%>%
  nest(-survey)%>%
  mutate(bloc_demographics=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x))) %>%
  mutate(bloc_demographics_r2=map(bloc_demographics, PseudoR2))%>%
  unnest(bloc_demographics_r2)

# Block 2 - Underlying Values
qc%>%
  nest(-survey)%>%
  mutate(bloc_values=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x))) %>%
  mutate(bloc_values_r2=map(bloc_values, PseudoR2))%>%
  unnest(bloc_values_r2)

# Block 2 - Underlying Values (Quebec sovereignty only)
qc%>%
  nest(-survey)%>%
  mutate(bloc_values1=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+quebec_sovereignty, family="binomial", data=x))) %>%
  mutate(bloc_values1_r2=map(bloc_values1, PseudoR2))%>%
  unnest(bloc_values1_r2)

# Block 3 - Partisanship
qc%>%
  nest(-survey)%>%
  mutate(bloc_partisanship=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x))) %>%
  mutate(bloc_partisanship_r2=map(bloc_partisanship, PseudoR2))%>%
  unnest(bloc_partisanship_r2)

# Block 4 - Retrospection
qc%>%
  nest(-survey)%>%
  mutate(bloc_retrospection=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(bloc_retrospection_r2=map(bloc_retrospection, PseudoR2))%>%
  unnest(bloc_retrospection_r2)

# Block 5 - Policy
qc%>%
  nest(-survey)%>%
  mutate(bloc_policy=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(bloc_policy_r2=map(bloc_policy, PseudoR2))%>%
  unnest(bloc_policy_r2)

# Block 6 - Leaders
qc%>%
  nest(-survey)%>%
  mutate(bloc_leaders=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>%
  mutate(bloc_leaders_r2=map(bloc_leaders, PseudoR2))%>%
  unnest(bloc_leaders_r2)

#for subset just start with out
#out
#### NDP ####
# Block 1 - Demographics
out%>%
  filter(working_class2==1 & quebec!=1)%>%
  nest(-survey)%>%
  mutate(ndp_demographics=map(data, function(x) glm(ndp~region3+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x))) %>%
  mutate(ndp_demographics_r2=map(ndp_demographics, PseudoR2))%>%
  unnest(ndp_demographics_r2)

# QC
out%>%
  filter(working_class2==1 & quebec==1)%>%
  nest(-survey)%>%
  mutate(ndp_QC_demographics=map(data, function(x) glm(ndp~union_both+young+old+male+degree+language+foreign, family="binomial", data=x))) %>%
  mutate(ndp_QC_demographics_r2=map(ndp_QC_demographics, PseudoR2))%>%
  unnest(ndp_QC_demographics_r2)

# Block 2 - Underlying Values
out%>%
  filter(working_class2==1 & quebec!=1)%>%
  nest(-survey)%>%
  mutate(ndp_values=map(data, function(x) glm(ndp~region3+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x))) %>%
  mutate(ndp_values_r2=map(ndp_values, PseudoR2))%>%
  unnest(ndp_values_r2)

# QC
out%>%
  filter(working_class2==1 & quebec==1)%>%
  nest(-survey)%>%
  mutate(ndp_QC_values=map(data, function(x) glm(ndp~union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x))) %>%
  mutate(ndp_QC_values_r2=map(ndp_QC_values, PseudoR2))%>%
  unnest(ndp_QC_values_r2)

# Block 3 - Partisanship
out%>%
  filter(working_class2==1 & quebec!=1)%>%
  nest(-survey)%>%
  mutate(ndp_partisanship=map(data, function(x) glm(ndp~region3+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(ndp_partisanship_r2=map(ndp_partisanship, PseudoR2))%>%
  unnest(ndp_partisanship_r2)

# QC
out%>%
  filter(working_class2==1 & quebec==1)%>%
  nest(-survey)%>%
  mutate(ndp_QC_partisanship=map(data, function(x) glm(ndp~union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x))) %>%
  mutate(ndp_QC_partisanship_r2=map(ndp_QC_partisanship, PseudoR2))%>%
  unnest(ndp_QC_partisanship_r2)

# Block 4 - Retrospection
out%>%
  filter(working_class2==1 & quebec!=1)%>%
  nest(-survey)%>%
  mutate(ndp_retrospection=map(data, function(x) glm(ndp~region3+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(ndp_retrospection_r2=map(ndp_retrospection, PseudoR2))%>%
  unnest(ndp_retrospection_r2)

# QC
out%>%
  filter(working_class2==1 & quebec==1)%>%
  nest(-survey)%>%
  mutate(ndp_QC_retrospection=map(data, function(x) glm(ndp~union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(ndp_QC_retrospection_r2=map(ndp_QC_retrospection, PseudoR2))%>%
  unnest(ndp_QC_retrospection_r2)

# Block 5 - Policy
out%>%
  filter(working_class2==1 & quebec!=1)%>%
  nest(-survey)%>%
  mutate(ndp_policy=map(data, function(x) glm(ndp~region3+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(ndp_policy_r2=map(ndp_policy, PseudoR2))%>%
  unnest(ndp_policy_r2)

# QC
out%>%
  filter(working_class2==1 & quebec==1)%>%
  nest(-survey)%>%
  mutate(ndp_QC_policy=map(data, function(x) glm(ndp~union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(ndp_QC_policy_r2=map(ndp_QC_policy, PseudoR2))%>%
  unnest(ndp_QC_policy_r2)

# Block 6 - Leaders
out%>%
  filter(working_class2==1 & quebec!=1)%>%
  nest(-survey)%>%
  mutate(ndp_leaders=map(data, function(x) glm(ndp~region3+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(ndp_leaders_r2=map(ndp_leaders, PseudoR2))%>%
  unnest(ndp_leaders_r2)

# QC
out%>%
  filter(working_class2==1 & quebec==1)%>%
  nest(-survey)%>%
  mutate(ndp_QC_leaders=map(data, function(x) glm(ndp~union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>%
  mutate(ndp_QC_leaders_r2=map(ndp_QC_leaders, PseudoR2))%>%
  unnest(ndp_QC_leaders_r2)

#NDP All of Canada
# Block 1 - Demographics
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_demographics=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x))) %>%
  mutate(ndp_AOC_demographics_r2=map(ndp_AOC_demographics, PseudoR2))%>%
  unnest(ndp_AOC_demographics_r2)

# Block 2 - Underlying Values
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_values=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x))) %>%
  mutate(ndp_AOC_values_r2=map(ndp_AOC_values, PseudoR2))%>%
  unnest(ndp_AOC_values_r2)

# Block 2 - Underlying Values (only market liberalism)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_values4=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism, family="binomial", data=x))) %>%
  mutate(ndp_AOC_values4_r2=map(ndp_AOC_values4, PseudoR2))%>%
  unnest(ndp_AOC_values4_r2)

# Block 2 - Underlying Values (only moral traditionalism)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_values4=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+moral_traditionalism, family="binomial", data=x))) %>%
  mutate(ndp_AOC_values4_r2=map(ndp_AOC_values4, PseudoR2))%>%
  unnest(ndp_AOC_values4_r2)

# Block 2 - Underlying Values (only continentalism)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_values4=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+continentalism, family="binomial", data=x))) %>%
  mutate(ndp_AOC_values4_r2=map(ndp_AOC_values4, PseudoR2))%>%
  unnest(ndp_AOC_values4_r2)

# Block 2 - Underlying Values (only political disaffection)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_values4=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+political_disaffection, family="binomial", data=x))) %>%
  mutate(ndp_AOC_values4_r2=map(ndp_AOC_values4, PseudoR2))%>%
  unnest(ndp_AOC_values4_r2)

# Block 3 - Partisanship
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_partisanship=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(ndp_AOC_partisanship_r2=map(ndp_AOC_partisanship, PseudoR2))%>%
  unnest(ndp_AOC_partisanship_r2)

# Block 4 - Retrospection
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_retrospection=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(ndp_AOC_retrospection_r2=map(ndp_AOC_retrospection, PseudoR2))%>%
  unnest(ndp_AOC_retrospection_r2)

# Block 5 - Policy
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_policy=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(ndp_AOC_policy_r2=map(ndp_AOC_policy, PseudoR2))%>%
  unnest(ndp_AOC_policy_r2)

# Block 5 - Policy (only immigration)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_policy1=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate, family="binomial", data=x))) %>%
  mutate(ndp_AOC_policy1_r2=map(ndp_AOC_policy1, PseudoR2))%>%
  unnest(ndp_AOC_policy1_r2)

# Block 5 - Policy (only environment)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_policy2=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+environment, family="binomial", data=x))) %>%
  mutate(ndp_AOC_policy2_r2=map(ndp_AOC_policy2, PseudoR2))%>%
  unnest(ndp_AOC_policy2_r2)

# # Block 5 - Policy (only defence)
# out%>%
#   filter(working_class2==1)%>%
#   nest(-survey)%>%
#   mutate(ndp_AOC_policy3=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+defence, family="binomial", data=x))) %>%
#   mutate(ndp_AOC_policy3_r2=map(ndp_AOC_policy3, PseudoR2))%>%
#   unnest(ndp_AOC_policy3_r2)
# 
# # Block 5 - Policy (only redistribution)
# out%>%
#   filter(working_class2==1)%>%
#   nest(-survey)%>%
#   mutate(ndp_AOC_policy4=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+redistribution, family="binomial", data=x))) %>%
#   mutate(ndp_AOC_policy4_r2=map(ndp_AOC_policy4, PseudoR2))%>%
#   unnest(ndp_AOC_policy4_r2)

# Block 6 - Leaders
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(ndp_AOC_leaders=map(data, function(x) glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(ndp_AOC_leaders_r2=map(ndp_AOC_leaders, PseudoR2))%>%
  unnest(ndp_AOC_leaders_r2)

#Liberals All of Canada
# Block 1 - Demographics
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(liberal_AOC_demographics=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x))) %>%
  mutate(liberal_AOC_demographics_r2=map(liberal_AOC_demographics, PseudoR2))%>%
  unnest(liberal_AOC_demographics_r2)

# Block 2 - Underlying Values
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(liberal_AOC_values=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x))) %>%
  mutate(liberal_AOC_values_r2=map(liberal_AOC_values, PseudoR2))%>%
  unnest(liberal_AOC_values_r2)

# Block 3 - Partisanship
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(liberal_AOC_partisanship=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(liberal_AOC_partisanship_r2=map(liberal_AOC_partisanship, PseudoR2))%>%
  unnest(liberal_AOC_partisanship_r2)

# Block 4 - Retrospection
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(liberal_AOC_retrospection=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(liberal_AOC_retrospection_r2=map(liberal_AOC_retrospection, PseudoR2))%>%
  unnest(liberal_AOC_retrospection_r2)

# Block 5 - Policy
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(liberal_AOC_policy=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(liberal_AOC_policy_r2=map(liberal_AOC_policy, PseudoR2))%>%
  unnest(liberal_AOC_policy_r2)

# Block 5 - Policy (only immigration)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(liberal_AOC_policy1=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate, family="binomial", data=x))) %>%
  mutate(liberal_AOC_policy1_r2=map(liberal_AOC_policy1, PseudoR2))%>%
  unnest(liberal_AOC_policy1_r2)

# Block 5 - Policy (only environment)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(liberal_AOC_policy2=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+environment, family="binomial", data=x))) %>%
  mutate(liberal_AOC_policy2_r2=map(liberal_AOC_policy2, PseudoR2))%>%
  unnest(liberal_AOC_policy2_r2)

# Block 5 - Policy (only defence)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(liberal_AOC_policy3=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+defence, family="binomial", data=x))) %>%
  mutate(liberal_AOC_policy3_r2=map(liberal_AOC_policy3, PseudoR2))%>%
  unnest(liberal_AOC_policy3_r2)

# # Block 5 - Policy (only redistribution)
# out%>%
#   filter(working_class2==1)%>%
#   nest(-survey)%>%
#   mutate(liberal_AOC_policy4=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+redistribution, family="binomial", data=x))) %>%
#   mutate(liberal_AOC_policy4_r2=map(liberal_AOC_policy4, PseudoR2))%>%
#   unnest(liberal_AOC_policy4_r2)

# Block 6 - Leaders
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(liberal_AOC_leaders=map(data, function(x) glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(liberal_AOC_leaders_r2=map(liberal_AOC_leaders, PseudoR2))%>%
  unnest(liberal_AOC_leaders_r2)

#Conservatives All of Canada
# Block 1 - Demographics
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_demographics=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x))) %>%
  mutate(conservative_AOC_demographics_r2=map(conservative_AOC_demographics, PseudoR2))%>%
  unnest(conservative_AOC_demographics_r2)

# Block 2 - Underlying Values
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_values=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x))) %>%
  mutate(conservative_AOC_values_r2=map(conservative_AOC_values, PseudoR2))%>%
  unnest(conservative_AOC_values_r2)

# Block 2 - Underlying Values (only market liberalism)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_values4=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism, family="binomial", data=x))) %>%
  mutate(conservative_AOC_values4_r2=map(conservative_AOC_values4, PseudoR2))%>%
  unnest(conservative_AOC_values4_r2)

# Block 2 - Underlying Values (only moral traditionalism)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_values4=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+moral_traditionalism, family="binomial", data=x))) %>%
  mutate(conservative_AOC_values4_r2=map(conservative_AOC_values4, PseudoR2))%>%
  unnest(conservative_AOC_values4_r2)

# Block 2 - Underlying Values (only continentalism)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_values4=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+continentalism, family="binomial", data=x))) %>%
  mutate(conservative_AOC_values4_r2=map(conservative_AOC_values4, PseudoR2))%>%
  unnest(conservative_AOC_values4_r2)

# Block 2 - Underlying Values (only political disaffection)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_values4=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+political_disaffection, family="binomial", data=x))) %>%
  mutate(conservative_AOC_values4_r2=map(conservative_AOC_values4, PseudoR2))%>%
  unnest(conservative_AOC_values4_r2)

# Block 3 - Partisanship
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_partisanship=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(conservative_AOC_partisanship_r2=map(conservative_AOC_partisanship, PseudoR2))%>%
  unnest(conservative_AOC_partisanship_r2)

# Block 4 - Retrospection
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_retrospection=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x))) %>%
  mutate(conservative_AOC_retrospection_r2=map(conservative_AOC_retrospection, PseudoR2))%>%
  unnest(conservative_AOC_retrospection_r2)

# Block 5 - Policy
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_policy=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence, family="binomial", data=x))) %>%
  mutate(conservative_AOC_policy_r2=map(conservative_AOC_policy, PseudoR2))%>%
  unnest(conservative_AOC_policy_r2)

# Block 5 - Policy (only immigration)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_policy1=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate, family="binomial", data=x))) %>%
  mutate(conservative_AOC_policy1_r2=map(conservative_AOC_policy1, PseudoR2))%>%
  unnest(conservative_AOC_policy1_r2)

# Block 5 - Policy (only environment)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_policy2=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+environment, family="binomial", data=x))) %>%
  mutate(conservative_AOC_policy2_r2=map(conservative_AOC_policy2, PseudoR2))%>%
  unnest(conservative_AOC_policy2_r2)

# Block 5 - Policy (only defence)
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_policy3=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+defence, family="binomial", data=x))) %>%
  mutate(conservative_AOC_policy3_r2=map(conservative_AOC_policy3, PseudoR2))%>%
  unnest(conservative_AOC_policy3_r2)

# # Block 5 - Policy (only redistribution)
# out%>%
#   filter(working_class2==1)%>%
#   nest(-survey)%>%
#   mutate(conservative_AOC_policy4=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+redistribution, family="binomial", data=x))) %>%
#   mutate(conservative_AOC_policy4_r2=map(conservative_AOC_policy4, PseudoR2))%>%
#   unnest(conservative_AOC_policy4_r2)

# Block 6 - Leaders
out%>%
  filter(working_class2==1)%>%
  nest(-survey)%>%
  mutate(conservative_AOC_leaders=map(data, function(x) glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(conservative_AOC_leaders_r2=map(conservative_AOC_leaders, PseudoR2))%>%
  unnest(conservative_AOC_leaders_r2)

#### Performance Model #### (Leadership, Partisanship, Most Important Issues)

# NDP Leadership
roc%>%
  nest(-survey)%>%
  mutate(ndp_leader=map(data, function(x) glm(ndp~liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(ndp_leader_r2=map(ndp_leader, PseudoR2))%>%
  unnest(ndp_leader_r2)

qc%>%
  nest(-survey)%>%
  mutate(ndp_leader=map(data, function(x) glm(ndp~liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>%
  mutate(ndp_leader_r2=map(ndp_leader, PseudoR2))%>%
  unnest(ndp_leader_r2)

# NDP Partisanship
roc%>%
  nest(-survey)%>%
  mutate(ndp_partyID=map(data, function(x) glm(ndp~ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(ndp_partyID_r2=map(ndp_partyID, PseudoR2))%>%
  unnest(ndp_partyID_r2)

qc%>%
  nest(-survey)%>%
  mutate(ndp_partyID=map(data, function(x) glm(ndp~ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x))) %>%
  mutate(ndp_partyID_r2=map(ndp_partyID, PseudoR2))%>%
  unnest(ndp_partyID_r2)

# NDP Addressing Issues
roc%>%
  nest(-survey)%>%
  mutate(ndp_economy=map(data, function(x) glm(ndp~ndp_economy+liberal_economy+conservative_economy, family="binomial", data=x))) %>%
  mutate(ndp_economy_r2=map(ndp_economy, PseudoR2))%>%
  unnest(ndp_economy_r2)

qc%>%
  nest(-survey)%>%
  mutate(ndp_economy=map(data, function(x) glm(ndp~ndp_economy+liberal_economy+conservative_economy+bloc_economy, family="binomial", data=x))) %>%
  mutate(ndp_economy_r2=map(ndp_economy, PseudoR2))%>%
  unnest(ndp_economy_r2)

# Liberals Leaders
roc%>%
  nest(-survey)%>%
  mutate(liberal_leader=map(data, function(x) glm(liberal~liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(liberal_leader_r2=map(liberal_leader, PseudoR2))%>%
  unnest(liberal_leader_r2)

qc%>%
  nest(-survey)%>%
  mutate(liberal_leader=map(data, function(x) glm(liberal~liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>%
  mutate(liberal_leader_r2=map(liberal_leader, PseudoR2))%>%
  unnest(liberal_leader_r2)

# Liberals Partisanship
roc%>%
  nest(-survey)%>%
  mutate(liberal_partyID=map(data, function(x) glm(liberal~ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(liberal_partyID_r2=map(liberal_partyID, PseudoR2))%>%
  unnest(liberal_partyID_r2)

qc%>%
  nest(-survey)%>%
  mutate(liberal_partyID=map(data, function(x) glm(liberal~ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x))) %>%
  mutate(liberal_partyID_r2=map(liberal_partyID, PseudoR2))%>%
  unnest(liberal_partyID_r2)

# Liberals Addressing Issues
roc%>%
  nest(-survey)%>%
  mutate(liberal_economy=map(data, function(x) glm(liberal~ndp_economy+liberal_economy+conservative_economy, family="binomial", data=x))) %>%
  mutate(liberal_economy_r2=map(liberal_economy, PseudoR2))%>%
  unnest(liberal_economy_r2)

qc%>%
  nest(-survey)%>%
  mutate(liberal_economy=map(data, function(x) glm(liberal~ndp_economy+liberal_economy+conservative_economy+bloc_economy, family="binomial", data=x))) %>%
  mutate(liberal_economy_r2=map(liberal_economy, PseudoR2))%>%
  unnest(liberal_economy_r2)

# Conservatives  Leaders
roc%>%
  nest(-survey)%>%
  mutate(conservative_leader=map(data, function(x) glm(conservative~liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>%
  mutate(conservative_leader_r2=map(conservative_leader, PseudoR2))%>%
  unnest(conservative_leader_r2)

qc%>%
  nest(-survey)%>%
  mutate(conservative_leader=map(data, function(x) glm(conservative~liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>%
  mutate(conservative_leader_r2=map(conservative_leader, PseudoR2))%>%
  unnest(conservative_leader_r2)

# Conservatives  Partisanship
roc%>%
  nest(-survey)%>%
  mutate(conservative_partyID=map(data, function(x) glm(conservative~ndp_id+liberal_id+conservative_id, family="binomial", data=x))) %>%
  mutate(conservative_partyID_r2=map(conservative_partyID, PseudoR2))%>%
  unnest(conservative_partyID_r2)

qc%>%
  nest(-survey)%>%
  mutate(conservative_partyID=map(data, function(x) glm(conservative~ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x))) %>%
  mutate(conservative_partyID_r2=map(conservative_partyID, PseudoR2))%>%
  unnest(conservative_partyID_r2)

# Conservatives Addressing Issues
roc%>%
  nest(-survey)%>%
  mutate(conservative_economy=map(data, function(x) glm(conservative~ndp_economy+liberal_economy+conservative_economy, family="binomial", data=x))) %>%
  mutate(conservative_economy_r2=map(conservative_economy, PseudoR2))%>%
  unnest(conservative_economy_r2)

qc%>%
  nest(-survey)%>%
  mutate(conservative_economy=map(data, function(x) glm(conservative~ndp_economy+liberal_economy+conservative_economy+bloc_economy, family="binomial", data=x))) %>%
  mutate(conservative_economy_r2=map(conservative_economy, PseudoR2))%>%
  unnest(conservative_economy_r2)

# Bloc
qc%>%
  nest(-survey)%>%
  mutate(bloc_leader=map(data, function(x) glm(bloc~liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>%
  mutate(bloc_leader_r2=map(bloc_leader, PseudoR2))%>%
  unnest(bloc_leader_r2)

qc%>%
  nest(-survey)%>%
  mutate(bloc_partyID=map(data, function(x) glm(bloc~ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x))) %>%
  mutate(bloc_partyID_r2=map(bloc_partyID, PseudoR2))%>%
  unnest(bloc_partyID_r2)

qc%>%
  nest(-survey)%>%
  mutate(bloc_economy=map(data, function(x) glm(bloc~ndp_economy+liberal_economy+conservative_economy+bloc_economy, family="binomial", data=x))) %>%
  mutate(bloc_economy_r2=map(bloc_economy, PseudoR2))%>%
  unnest(bloc_economy_r2)

#Detach Desctools
detach(package:DescTools)

