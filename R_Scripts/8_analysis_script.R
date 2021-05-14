# Primary Script for Class 2019 Paper
library(stargazer)
library(broom)
library(nnet)
library(purrr)

#Install these packages


#### First Graph historical NDP vote ####
library(ggeffects)
#Get ROC Models to 2019

#Note we have to use the variable that does not carve out self-employed be
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

#Collapse the occupation categories to match what we did inthe CES dataframe
#Working class categories collapsed no self-employed
ces15phone$occupation2<-Recode(as.factor(ces15phone$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'")
#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
#Let's put the working class variables in order
ces15phone$occupation2<-fct_relevel(ces15phone$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')

ces15phone$occupation4<-Recode(as.factor(ces15phone$occupation3),"4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'")
table(ces15phone$occupation3)
table(ces15phone$occupation4)
ces15phone$occupation4<-fct_relevel(ces15phone$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')
table(ces15phone$occupation4)

#working_class3 is workforce only
table(ces15phone$occupation4)
ces15phone$working_class3<-Recode(ces15phone$occupation4, "'Working_Class'=1; NA=NA ; else=0")
table(ces15phone$working_class3, ces15phone$occupation4, useNA = "ifany")

#working_class4 is full sample
ces15phone$working_class4<-Recode(ces15phone$occupation4, "'Working_Class'=1; else=0")
ces15phone$working_class4
table(ces15phone$working_class4, ces15phone$occupation4, useNA = "ifany")

#this is the NDP vote variable
ces15phone$ndp<-car::Recode(ces15phone$vote, "3=1; 0:2=0; 4:5=0; NA=NA")

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
val_labels(ces15phone$liberal)<-c(Other=0, Liberal=1)
val_labels(ces15phone$conservative)<-c(Other=0, Conservative=1)
val_labels(ces15phone$ndp)<-c(Other=0, NDP=1)
val_labels(ces15phone$green)<-c(Other=0, Green=1)
val_labels(ces15phone$bloc)<-c(Other=0, Bloc=1)


#### 2019 Recodes ####

#Recodes
ces19phone$occupation2<-Recode(as.factor(ces19phone$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'")
#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces19phone$occupation4<-Recode(as.factor(ces19phone$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'")
#Let's put the working class variables in order
ces19phone$occupation2<-fct_relevel(ces19phone$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')
ces19phone$occupation4<-fct_relevel(ces19phone$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')
#working_class3 is workforce only
table(ces19phone$occupation4)
ces19phone$working_class3<-Recode(ces19phone$occupation4, "'Working_Class'=1; NA=NA;else=0")
table(ces19phone$working_class3, ces19phone$occupation4)
table(ces19phone$occupation4)
table(ces19phone$working_class3)
#working_class4 is full sample
ces19phone$working_class4<-Recode(ces19phone$occupation4, "'Working_Class'=1;else=0")
table(ces19phone$working_class4, ces19phone$occupation4, useNA = "ifany")

#this is the NDP vote variable
ces19phone$ndp<-car::Recode(ces19phone$vote, "3=1; 0:2=0; 4:5=0; NA=NA")


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


ces19phone$region4<-factor(ces19phone$region2, levels=c("Atlantic", "Quebec", "Ontario", "West"))


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
val_labels(ces19phone$liberal)<-c(Other=0, Liberal=1)
val_labels(ces19phone$conservative)<-c(Other=0, Conservative=1)
val_labels(ces19phone$ndp)<-c(Other=0, NDP=1)
val_labels(ces19phone$green)<-c(Other=0, Green=1)
val_labels(ces19phone$bloc)<-c(Other=0, Bloc=1)
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
m1.all<-glm(ndp~working_class4+union_both+income+degree+sector, data=ces19phone, family="binomial")
m1.roc<-glm(ndp~working_class4+union_both+income+degree+sector, data=ces19.roc, family="binomial")
m1.qc<-glm(ndp~working_class4+union_both+income+degree+sector, data=ces19.qc, family="binomial")

library(survey)
#Compare and
m1.all.weight<-svyglm(ndp~working_class4+union_both+income+degree+sector,ces19phone.weight)
m1.roc.weight<-svyglm(ndp~working_class4+union_both+income+degree+sector, design=ces19.roc.weight)
m1.qc.weight<-svyglm(ndp~working_class4+union_both+income+degree+sector,design=ces19.qc.weight)
#Compare with model
stargazer(m1.all, m1.all.weight, m1.roc, m1.roc.weight,m1.qc, m1.qc.weight, type="html", out=here("Tables", "class_models_weighted_unweighted.html"), column.labels = c('CAN', 'CAN', 'ROC', 'ROC','QC', 'QC'))

#Play with weighting 

#Logistic Regression MOdels with Demographic Controls
m2.all<-glm(ndp~working_class4+union_both+income+degree+sector+age+male+vismin+as.factor(region4), vismin, data=ces19phone, family="binomial")
m2.roc<-glm(ndp~working_class4+union_both+income+degree+sector+age+male+vismin+region3, data=ces19.roc, family="binomial")
m2.qc<-glm(ndp~working_class4+union_both+income+degree+sector+age+male+vismin, data=ces19.qc, family="binomial")

#Logistic Regression MOdels
m2.all.weight<-svyglm(ndp~working_class4+union_both+income+degree+sector+age+male+vismin+as.factor(region4)+vismin, design=ces19phone.weight, family="binomial")
m2.roc.weight<-svyglm(ndp~working_class4+union_both+income+degree+sector+age+male+vismin+region3, design=ces19phone.weight, family="binomial")
m2.qc.weight<-svyglm(ndp~working_class4+union_both+income+degree+sector+age+male+vismin, design=ces19phone.weight, family="binomial")
#Compare m2 v.
stargazer(m2.all, m2.all.weight, m2.roc, m2.roc.weight, m2.qc, m2.qc.weight, column.labels = c('CAN', 'CAN', 'ROC', 'ROC','QC', 'QC'), out=here("Tables", "class_demographic_controls_weighted_unweighted.html"),type="html")

#Print complete models
stargazer(m1.all, m1.roc, m1.qc, m2.all, m2.roc, m2.qc, type="html", out=here("Tables", "basic_class_models1.html"), column.labels = rep(c("CAN", "ROC", "QC"),2))

#Calculate odds ratio
m2.all$coefficients
exp(m2.all$coefficients)

#### Comparing ces15 and ces19 Block Recursive Models ####

#First make a ces15 roc data frame
ces15phone %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class4, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language,          market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, 
         ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, manage_economy, liberal_economy, ndp_economy, conservative_economy, bloc_economy, green_economy,
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec, occupation4, minorities, immigration, immigration2, immigration_rate, minorities_help, mip, vismin, vote)->out15
#Now an ces19data frame
ces19phone %>% 
#  filter(quebec!=1) %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class4, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, liberal_environment, ndp_environment, conservative_environment, bloc_environment, green_environment, green_economy, green_issue,
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, manage_economy, manage_environment, address_issue,liberal_economy, ndp_economy, conservative_economy, bloc_economy, liberal_issue, conservative_issue, ndp_issue,
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec, occupation4, minorities, immigration, immigration2, immigration_rate, minorities_help, mip, vismin, vote)->out19

#### Build out combining ces2015 and 2019 ####
out15$survey<-rep(0, nrow(out15))
out19$survey<-rep(1, nrow(out19))
out15 %>% 
  bind_rows(., out19)->out
#Value Labels
val_labels(out$working_class4)<-c(Other=0, Working_class=1)
val_labels(out$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, 
                              Deficit_Debt=10, Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15)

#Split out into ROC 
roc<-out %>% 
  filter(quebec!=1)
#Split out into Quebec
qc<-out %>% 
  filter(quebec==1)
table(roc$vismin, roc$foreign, useNA = "ifany")
#### NDP ROC ####

block1<-glm(ndp~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(ndp~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(ndp~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(ndp~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(ndp~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(ndp~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)

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

block1<-glm(ndp~(working_class4+union_both+young+old+male+degree+language+vismin)*survey, family="binomial", data=qc)
block2<-glm(ndp~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(ndp~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(ndp~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(ndp~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)
block6<-glm(ndp~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)

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

block1<-glm(conservative~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(conservative~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(conservative~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(conservative~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(conservative~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(conservative~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)

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
block1<-glm(conservative~(working_class4+union_both+young+old+male+degree+language+vismin)*survey, family="binomial", data=qc)
block2<-glm(conservative~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(conservative~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(conservative~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(conservative~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)
block6<-glm(conservative~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
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
block1<-glm(liberal~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(liberal~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(liberal~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(liberal~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(liberal~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(liberal~(region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)
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
block1<-glm(liberal~(working_class4+union_both+young+old+male+degree+language+vismin)*survey, family="binomial", data=qc)
block2<-glm(liberal~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(liberal~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(liberal~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(liberal~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)
block6<-glm(liberal~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
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
block1<-glm(bloc~(working_class4+union_both+young+old+male+degree+language+vismin)*survey, family="binomial", data=qc)
block2<-glm(bloc~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(bloc~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(bloc~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(bloc~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)

block6<-glm(bloc~(working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
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


#### Format Nice comprehensive QC and ROC Tables ####
#Step 1 combine all the parties' tables into roc and qc
roc_table<-cbind(roc_ndp_table, roc_liberal_table, roc_conservative_table)
qc_table<-cbind(qc_ndp_table, qc_bloc_table, qc_liberal_table, qc_conservative_table)

library(flextable)
#Drop out terms we don't need.
#### Get PseudoR2s for final Block Models ####
roc.block.model<-list(roc_ndp, roc_liberal, roc_conservative)
qc.block.model<-list(qc_ndp, qc_liberal, qc_conservative, qc_bloc)
stargazer(roc_conservative$block6, type="text")

library(DescTools)
# Get Pseudos and nobs
map(roc.block.model, `[[`, 'block6') %>% 
  map(., PseudoR2) %>%
  unlist() %>% 
  round(., digits=2)->roc_pseudos

map(qc.block.model, `[[`, 'block6') %>% 
  map(., PseudoR2) %>%
  unlist() %>% 
  round(., digits=2)->qc_pseudos
detach("package:DescTools")
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
  #This add thte table header
    add_header_lines(values=c("ROC Block Recursive Model Coefficients, 2015 and 2019")) %>% 
  #This adds a footer row that contains te text McFadden PSeudo R2 and the values from the roc_pseudos from the block6 model. 
    add_footer_row(values=c("McFadden Pseudo R2", roc_pseudos), colwidths=c(2,2,2,2)) %>% 
  #This adds a footer row that contains te text McFadden PSeudo R2 and the values from the qc_pseudos from the block6 model. 
    add_footer_row(.,values=paste("N = ",length(roc_ndp$block6$residuals), sep=""), colwidths=c(8)) %>% 
  #This sets the alignment for the 2nd row of the footer to left
    align(., i=2,align=c("left"), part="footer") %>% 
  #then this modifies that alignment for cells 3 to 8 to be center
    align(., i=2,j=3:8,align=c("center"), part="footer") %>% 
save_as_html("Tables/roc_block_recursive_table.html")

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
    #This adds a footer row that contains te text McFadden PSeudo R2 and the values from the roc_pseudos from the block6 model. 
    add_footer_row(values=c("McFadden Pseudo R2", qc_pseudos), colwidths=c(2,2,2,2,2))  %>% 
  #This adds a footer row that contains te text McFadden PSeudo R2 and the values from the qc_pseudos from the block6 model. 
    add_footer_row(.,values=paste("N = ",length(qc_ndp$block6$residuals), sep=""), colwidths=c(10)) %>% 
  #This sets the alignment for the 2nd row of the footer to left
    align(., i=2,align=c("left"), part="footer") %>% 
  #then this modifies that alignment for cells 3 to 8 to be center
    align(., i=2,j=3:10,align=c("center"), part="footer") %>% 
  save_as_html(., "Tables/qc_block_recursive_model.html")

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
  select(occupation4, past_vote, vote) %>% 
  group_by(occupation4, past_vote, vote) %>% 
  filter(past_vote==3) %>% 
  filter(past_vote!=0) %>% 
  filter(vote!=0) %>% 
  filter(!is.na(vote)) %>% 
  as_factor() %>% 
  summarize(n=n()) %>% 
  mutate(percent=n/sum(n)) %>% 
  ggplot(., aes(x=fct_reorder(vote,percent), y=percent, fill=vote))+geom_col(position="dodge", width=0.5)+facet_wrap(~occupation4, labeller=label_wrap_gen(width=15, multi_line = T))+coord_flip()+labs(x="2019 Vote", y="Percent")+scale_fill_manual(name="2019 Vote", values=c("NDP"="orange","Liberal"="darkred", "Conservative"="darkblue", "Green"="darkgreen", "Bloc"="cyan"))
  ggsave(here("Plots", "ndp_vote_flow_2019.png"), width=8, height=3)





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

#Start with tghe combined 15 and 19 data frame
out %>% 
  #select the variables we need for the attitudinal shifts
  #pay attention to selecting the variables that have been recoded i.e. they end with _x
  select(ends_with("_x"),  environment,redistribution, immigration, immigration_rate, minorities_help, survey, quebec, occupation4) %>% 
  #Rename the variables for aesthetic purposes
rename(., `Moral Traditionalism`=1, `Market Liberalism`=2, `Continentalism`=3, Defence=4, Environment=5, Redistribution=6, Immigration=7, `Immigration Rate`=8, `Racial Minorities`=9) %>% select(-Immigration) %>% 
  #Pivot these down into one single variable
  pivot_longer(cols=1:8) %>% 
  #There are a few cases missing on this variable
    filter(!is.na(quebec)) %>% 
 # form groups by variable name, occupation and quebec
  nest_by(name, occupation4, quebec=as_factor(quebec)) %>% 
#Tidy the t.test results for value ~ survey
  mutate(mod=tidy(t.test(value~survey, data=data))) %>% 
  #Plot with occupation4 on the x, relevelled to put workers at the bottom and mamnagers at the top; y is the estimate
 ggplot(., aes(x=fct_relevel(occupation4, "Working_Class", "Routine_Nonmanual", "Self-Employed", "Professionals", "Managers"), y=mod$estimate))+labs(x="Class", y="Difference (2019-2015)", subtitle="CES 2015 and 2019", col="Region")+geom_point(aes(col=quebec), size=0.5,position=position_dodge(width=0.5))+facet_wrap(~name)+coord_flip(expand=T, clip="off")+geom_hline(aes(yintercept=0), linetype=2)+scale_color_grey(start=0.2 ,end=0.5)+geom_linerange(aes(ymin=mod$conf.low, ymax=mod$conf.high, col=quebec), position=position_dodge(width=0.5))
ggsave(here("Plots", "attitudinal_differences_2015_2019.png"), width=6, height=4)

#### Most Important Issue Changes####
table(as_factor(out$mip), out$survey)
out %>% 
  group_by(survey, mip) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  as_factor() %>% 
print(n=100)
val_labels(out$mip)

out %>% 
  mutate(mip2=case_when(
      #Other=0
  mip==0~0,
    #Environment=1
  mip==1~1,
#Crime = Other
mip==2~0,
#Ethics=2
mip==3~2,
#Welfare=3
mip==4~3,
#Energy=4
mip==5~4,
  #Jobs==5
  mip==6~5,
  #Economy=5
  mip==7~5,
  #Health=3
  mip==8~3,
  #Taxes=6
mip==9 ~6,
#Debt-Deficit=0
mip==10~0,
#Democracy=2
mip==11~2,
#Foreign Affairs==7
mip==12~7,
  #Immigratio=8
  mip==13~8,
  #SocioCultural ==0
  mip>14~0,
#Social Programs==3
  mip==15~3,
  TRUE~ NA_real_
  ))->out

val_labels(out$mip2)<-c(Other=0, Environment=1, Ethics=2, Social_Welfare=3, Energy=4,Jobs_Economy=5, Taxes=6, Foreign_Affairs=7, Immigration=8)


out %>% 
group_by(survey, occupation4,`Most Important Problem`=as_factor(mip2)) %>% 
  summarise(n=n()) %>%
  rename(Class=occupation4) %>% 
    filter(!is.na(`Most Important Problem`)) %>%
  mutate(pct=n/sum(n)) %>% 
  #filter(!is.na(Class)) %>% 
  mutate(Election=car::Recode(survey, "0=2015; 1=2019", as.factor=T, levels=c("2015","2019")))%>% 
  ggplot(., aes(x=`Most Important Problem`, y=pct, fill=fct_relevel(Class, "Working_Class", "Routine_Nonmanual", "Professionals", "Self-Employed", "Managers")))+geom_col(position = position_dodge(preserve = "single"))+labs(x="Most Important Problem", y="Percent")+facet_grid(~Election)+scale_fill_grey(name="Class", na.value="black", start=0.2, end=0.8,guide = guide_legend(reverse = T) )+coord_flip()
ggsave("Plots/mip_bar_2015_2019.png", width=8, height=4)


#### Performance ####
val_labels(out$manage_economy)
val_labels(out$address_issue)
val_labels(out$manage_environment)
out %>% 
  select(survey, occupation4, manage_economy, address_issue, manage_environment) %>%
  rename(Election=survey, Class=occupation4, `Manage Economy`=manage_economy, `Address Issue`=address_issue, `Manage Environment`=manage_environment) %>% 
  pivot_longer(cols=c(`Manage Economy`, `Address Issue`, `Manage Environment`), names_to=c("Issue"), values_to=c("Party")) %>% 
  as_factor() %>% 
  group_by(Election, Class, Issue, Party) %>% 
filter(!is.na(Party)) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n), 
         Election=Recode(Election, "0=2015;1=2019", as.factor=T, levels=c("2019", "2015"))) %>%
  filter(Party!="Bloc"&Party!="Green") %>% 
  ggplot(., aes(x=pct, y=fct_relevel(Class, "Working_Class", "Routine_Nonmanual", "Self-Employed", "Professionals", "Managers"), fill=Election))+geom_col(position="dodge")+facet_grid(Party~fct_relevel(Issue, "Manage Economy", "Address Issue", "Manage Environment"))+scale_fill_grey(start=0.2, end=0.8, guide = guide_legend(reverse = T))+xlim(0,1)+labs(y="Class")
ggsave(filename=here("Plots/performance_2015_2019.png"))
####Leader rating changes ####
out %>% 
  select(`Liberal`=liberal_leader, `Conservative`=conservative_leader, `NDP`=ndp_leader, `BQ`=bloc_leader, Survey=survey, Class=occupation4, Quebec=quebec) %>%
  filter(!is.na(Quebec)) %>% 
  pivot_longer(cols=1:4, names_to=c("Party")) %>% 
    filter(!is.na(value)) %>% 
  nest_by(Class, Quebec=as_factor(Quebec), Party) %>% 
  mutate(mod=tidy(t.test(data=data, value~Survey))) %>% 
  ggplot(., aes(x=fct_relevel(Class, "Working_Class", "Routine_Nonmanual", "Self-Employed", "Professionals", "Managers"), y=mod$estimate*-1, col=Quebec))+geom_point(position=position_dodge(width=0.2))+ylim(-0.2,0.2)+labs(x="Class", y="Difference (2019-2015)")+coord_flip()+scale_color_grey()+facet_wrap(~Party)+geom_hline(yintercept=0, linetype=2)+geom_linerange(aes(ymin=mod$conf.low*-1, ymax=mod$conf.high*-1), position=position_dodge(width=0.2))
ggsave(here("Plots", "leader_approval_ratings.png"), width=6, height=4)
names(out)


#### Correlates of Working Class Vote ####

out %>%   
  mutate(vote=fct_relevel(as_factor(vote), "Liberal")) %>% 
  filter(!is.na(quebec)) %>% 
  nest(-quebec, -survey) %>% 
mutate(model=map(data, function(x) multinom(vote~working_class4+market_liberalism+moral_traditionalism+market_liberalism*working_class4+moral_traditionalism*working_class4, data=x))) %>% 
  # mutate(ndp2_tidied=map(ndp2, tidy)) %>% 
# unnest(ndp2_tidied) %>% 
mutate(market=map(model, ggpredict, terms=c('market_liberalism[0,0.5,1]', 'working_class4')), 
       moral=map(model, ggpredict, terms=c('moral_traditionalism[0,0.5,1]', 'working_class4'))) ->models

models %>% 
  unnest(market) %>% 
    filter(response.level!="Other"&response.level!="Bloc"& response.level!="Green") %>% 
  #filter(group!="Working_class") %>% 
  mutate(Survey=Recode(survey, "0='2015'; 1='2019'"), 
         Quebec=Recode(as.numeric(quebec),"0='ROC'; 1='QC'")) %>% 
  filter(response.level!="Liberal") %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+geom_point()+facet_grid(response.level+Quebec~Survey)+labs(title="Market Liberalism Class and vote\nHolding Moral Traditionalism at Its Average", x="Support for Market Liberalism")+geom_line()+scale_x_continuous(breaks=c(0,0.5,1))
ggsave(here("Plots", "market_liberalism_control_moral_traditionalism_qc_roc_2015_2019.png"))


models %>% 
  unnest(moral) %>% 
    filter(response.level!="Other"&response.level!="Bloc"& response.level!="Green") %>% 
  #filter(group!="Working_class") %>% 
  mutate(Survey=Recode(survey, "0='2015'; 1='2019'"), 
         Quebec=Recode(as.numeric(quebec),"0='ROC'; 1='QC'")) %>% 
  filter(response.level!="Liberal") %>% 
  mutate(Class=Recode(group, "0='Other'; 1='Working_Class'")) %>% 
  ggplot(., aes(x=x, y=predicted, col=Class))+geom_point()+facet_grid(response.level+Quebec~Survey)+labs(title="Moral Traditionalism and Vote\nHolding Market Liberalism at Its Average", x="Support for Moral Traditionalism")+geom_line()+scale_x_continuous(breaks=c(0,0.5,1))+scale_color_grey(start=0.8, end=0.2)
ggsave(here("Plots", "moral_traditionalism_control_market_liberalism_qc_roc_2015_2019.png"))

stargazer(models$model, type="html", out=here("Tables", "moral_market.html"))
#### Status of Race####

#Are the working classes more racialized?
ces19phone %>% 
  group_by(occupation4, vismin) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vismin)) %>% 
  mutate(percent=n/sum(n))

out %>% 
  filter(working_class4==1) %>% 
  group_by(survey, vismin, ndp) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n))
  ggplot()

  
#This code fits basic logistic models of vote for NDP on various ethnicity related variables#
  #Start with the out data frame
out %>% 
  #nest by survey; 
  nest(-survey) %>%
  #This fits the model 
  mutate(vismin_ndp=map(data, function(x) glm(ndp~quebec+old+male+degree+vismin+working_class4, data=x, family="binomial")),
    vismin_ndp1=map(data, function(x) glm(ndp~quebec+old+male+degree+vismin*working_class4, data=x, family="binomial")),
                minorities_ndp=map(data, function(x) glm(ndp~quebec+old+male+degree+minorities_help+working_class4, data=x, family="binomial")),
         minorities_ndp1=map(data, function(x) glm(ndp~quebec+old+male+degree+minorities_help*working_class4, data=x, family="binomial")),
                    immigration_ndp=map(data, function(x) glm(ndp~quebec+old+male+degree+immigration+working_class4, data=x, family="binomial")),
           immigration_ndp1=map(data, function(x) glm(ndp~quebec+old+male+degree+immigration*working_class4, data=x, family="binomial")),
         vismin_conservative=map(data, function(x) glm(conservative~quebec+old+male+degree+vismin*working_class4, data=x, family="binomial")),
         minorities_conservative=map(data, function(x) glm(conservative~quebec+old+male+degree+minorities_help+working_class4, data=x, family="binomial")),
           immigration_conservative=map(data, function(x) glm(conservative~quebec+old+male+degree+immigration+working_class4, data=x, family="binomial")),
         minorities_conservative1=map(data, function(x) glm(conservative~quebec+old+male+degree+minorities_help*working_class4, data=x, family="binomial")),
           immigration_conservative1=map(data, function(x) glm(conservative~quebec+old+male+degree+immigration*working_class4, data=x, family="binomial")),
         minorities_ndp_preds=map(minorities_ndp1, ggpredict, terms=c("minorities_help[0,0.5,1]", "working_class4")), 
         immigration_ndp_preds=map(immigration_ndp1, ggpredict, terms=c("immigration[0,0.5,1]", "working_class4")))->race_models 
race_models %>% 
unnest(minorities_ndp_preds) %>% 
  select(survey, x, predicted, conf.low, conf.high, group) %>% 
  mutate(survey=Recode(survey, "0=2015; 1=2019"), 
         class=Recode(group, "0='Other' ; 1='Working_Class'")) %>% 
  ggplot(., aes(x=x, y=predicted, col=class))+geom_point()+facet_grid(~survey)+labs(title="Probability of Voting NDP by Support for Minorities and Class, 2015-2019")+geom_line()

race_models %>% 
unnest(immigration_ndp_preds) %>% 
  select(survey, x, predicted, conf.low, conf.high, group) %>% 
  mutate(survey=Recode(survey, "0=2015; 1=2019"), 
         class=Recode(group, "0='Other' ; 1='Working_Class'")) %>% 
  ggplot(., aes(x=x, y=predicted, col=class))+geom_point()+facet_grid(~survey)+labs(title="Probability of Voting NDP by Support for Immigration, 2015-2019")+geom_line()


stargazer(
  list(race_models$vismin_ndp, race_models$vismin_ndp1,race_models$minorities_ndp,race_models$minorities_ndp1, race_models$immigration_ndp, race_models$immigration_ndp1), 
  column.labels=c(rep(c("2015", "2019"), 6)),digits=2,
  omit=c(1:4),
  dep.var.labels=c("Vote for NDP"),
  type="html", out=here("Tables/race_models_ndp.html"), covariate.labels=c("Visible Minority", "Support For Racial Minorities", "Support for Immigration", "Working Clas", "Visible Minority x Working Class", "Support For Racial Minorities x Working Class" , "Support for Immigration x Working Class"))


out %>% 
  nest(-survey) %>% 
  mutate(vismin=map(data, function(x) glm(ndp~vismin*working_class4, data=x, family="binomial")),
         minorities=map(data, function(x) glm(ndp~minorities_help*working_class4, data=x, family="binomial")),
         vismin_tidied=map(vismin, tidy),
         minorities_tidied=map(minorities,tidy)) %>% 
  unnest(vismin_tidied)


out %>% 
 # group_by(working_class4, vismin, ndp) %>% 
  filter(!is.na(vismin)) %>% 
  #filter(working_class4==1) %>% 
 # filter(ndp==1) %>% 
  mutate(Survey=Recode(survey, "0=2015;1=2019", as.factor=T)) %>% 
  ggplot(., aes(x=as_factor(ndp), fill=Survey))+geom_bar(position="dodge")+facet_grid(working_class4~as_factor(vismin))


#Racial and Immigration Issues
singh1.roc<-lm(ndp_leader~(immigration+working_class4)*working_class4,data=ces19.roc)
singh2.roc<-lm(ndp_leader~(immigration+minorities_help)*working_class4, data=ces19.roc)
singh3.roc<-lm(ndp_leader~(immigration+minorities_help)*working_class4, data=ces19.roc)
singh1.qc<-lm(ndp_leader~(immigration+working_class4)*working_class4,data=ces19.qc)
singh2.qc<-lm(ndp_leader~(immigration+minorities_help)*working_class4, data=ces19.qc)
singh3.qc<-lm(ndp_leader~(immigration+minorities_help)*working_class4, data=ces19.qc)

stargazer(singh1.roc, singh2.roc, singh1.qc, singh2.qc,type="text", out=here("Tables", "singh_approval_interactions.html"), covariate.labels=c("Immigration", "Help Minorities", "Environment", "Working Class", "Immigration* Working Class", "Help Minorities * Working Class", "Environment * Working Class"), dep.var.caption="Approval of Jagmeet Singh, 2019, Outside of Quebec", dep.var.labels=c("Singh Thermometer Rating"), column.labels=c(rep("ROC",3), rep("QC", 3)))


# val_labels(out$mip)
# out %>% 
#   mutate(mip_enviro=case_when(
#     mip==7~1,
#     TRUE~0
#   ))->out
# out %>% 
#   mutate(mip_jobs=case_when(
#     mip==6~1,
#     mip==7~1,
#     TRUE~0
#   ))->out
# 
# out %>% 
#   nest(-survey) %>% 
#   mutate(ndp_continentalism=map(data, function(x) glm(ndp~quebec+degree+old+male+working_class4+continentalism,family="binomial", data=x)), 
#         ndp_continentalism1=map(data, function(x) glm(ndp~quebec+degree+old+male+working_class4*continentalism, family="binomial", data=x)),
#        ndp_redisribution=map(data, function(x) glm(ndp~quebec+degree+old+male+working_class4+redistribution, family="binomial", data=x)), 
#         ndp_redistribution1=map(data, function(x) glm(ndp~quebec+degree+old+male+working_class4*redistribution,family="binomial",  data=x)), 
#        ndp_enviro=map(data, function(x) glm(ndp~quebec+degree+old+male+working_class4+mip_enviro,family="binomial",  data=x)), 
#         ndp_enviro1=map(data, function(x) glm(ndp~quebec+degree+old+male+working_class4*mip_enviro, family="binomial", data=x)),       
#        ndp_jobs=map(data, function(x) glm(ndp~quebec+degree+old+male+working_class4+mip_jobs,family="binomial",  data=x)), 
#         ndp_jobs1=map(data, function(x) glm(ndp~quebec+degree+old+male+working_class4*mip_jobs, family="binomial", data=x))
#         )->jobs_models
# 
# stargazer(list(jobs_models$ndp_continentalism,
#                jobs_models$ndp_continentalism1,
#                jobs_models$ndp_redisribution,
#                jobs_models$ndp_redistribution1,
#                jobs_models$ndp_enviro,
#                jobs_models$ndp_enviro1,
#                jobs_models$ndp_jobs,
#                jobs_models$ndp_jobs1), type="html", 
#           column.labels=c(rep(c("2015", "2019"), 8)),
#           out=here("Tables/jobs_models_ndp.html"))



#### Redistribution descriptives ####
#Redistribution point plot
ces %>% 
  select(election, working_class4, redistribution, vote, quebec) %>% 
  group_by(election, working_class4, quebec, vote) %>% 
    filter(!is.na(quebec)) %>% 
    filter(vote>0 &vote<4) %>% 
  summarize(avg=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg,col=as_factor(working_class4)))+geom_point()+facet_grid(as_factor(vote)~as_factor(quebec))

out %>% 
  filter(!is.na(quebec)) %>% 
  nest(-survey, -quebec) %>% 
  mutate(ndp1=map(data,function(x) glm(ndp~redistribution, family="binomial", data=x))) %>% 
  mutate(ndp1_tidied=map(ndp1, tidy)) %>% 
  unnest(ndp1_tidied) %>% 
  filter(str_detect(term, "Intercept", negate=T)) %>% 
  arrange(quebec, survey)

out %>% 
  filter(!is.na(quebec)) %>% 
  nest(-survey, -quebec) %>% 
  mutate(con1=map(data,function(x) glm(conservative~redistribution, family="binomial", data=x))) %>% 
  mutate(con1_tidied=map(con1, tidy)) %>% 
  unnest(con1_tidied) %>% 
  filter(str_detect(term, "Intercept", negate=T)) %>% 
  arrange(quebec, survey)

library(ggeffects)

library(nnet)

out %>%   
  mutate(vote=fct_relevel(as_factor(vote), "Liberal")) %>% 
  filter(!is.na(quebec)) %>% 
  nest(-quebec, -survey) %>% 
mutate(model=map(data, function(x) multinom(vote~working_class4*redistribution*moral_traditionalism, data=x))) %>% 
  # mutate(ndp2_tidied=map(ndp2, tidy)) %>% 
# unnest(ndp2_tidied) %>% 
mutate(redistribution=map(model, ggpredict, terms=c('redistribution[0,0.5,1]', 'working_class4')), 
       moral=map(model, ggpredict, terms=c('moral_traditionalism[0,0.5,1]', 'working_class4'))) ->models

models %>% 
  unnest(redistribution) %>% 
    filter(response.level!="Other"&response.level!="Bloc"& response.level!="Green") %>% 
  #filter(group!="Working_class") %>% 
  mutate(Survey=Recode(survey, "0='2015'; 1='2019'"), 
         Quebec=Recode(as.numeric(quebec),"0='ROC'; 1='QC'")) %>% 
  filter(response.level!="Liberal") %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+geom_point()+facet_grid(response.level+Quebec~Survey)+labs(title="Redistribution Class and vote\nHolding Moral Traditionalism at Its Average", x="Support for Redisribution")+geom_line()+scale_x_continuous(breaks=c(0,0.5,1))
ggsave(here("Plots", "redistribution_control_moral_traditionalism_qc_roc_2015_2019.png"))

models %>% 
  unnest(moral) %>% 
      filter(response.level!="Other"&response.level!="Bloc"& response.level!="Green"&response.level!="Liberal") %>% 
    mutate(Survey=Recode(survey, "0='2015'; 1='2019'"), 
         Quebec=Recode(as.numeric(quebec),"0='ROC'; 1='QC'")) %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+geom_point()+facet_grid(response.level~Quebec~Survey)+labs(title="Moral Traditionalism, Class and Vote\nHolding Support for Redistribution At Its Average", x="Moral Traditionalism")+geom_line()+scale_color_grey(name="Class")
ggsave(here("Plots", "moral_traditionalsm_control_redistribution_qc_roc_2015_2019.png"))


#Redistribution Bar Graph

out %>%
  select(survey, working_class4, redistribution, vote) %>%
  mutate(pro_redistribution=case_when(
    redistribution > 0.5~"Pro Redistribution",
    redistribution< 0.5~"Anti-Redistribution",
    TRUE~NA_character_
  )) %>%
  mutate(Election=Recode(survey, "0=2015; 1=2019")) %>%
 group_by(Election, working_class4, pro_redistribution,vote) %>%
   # filter(!is.na(quebec)) %>%
  #  filter(vote>0 &vote<4) %>%
summarize(n=n()) %>%
  mutate(pct=n/sum(n)) %>%
  filter(vote>0 &vote<4) %>%
  filter(!is.na(pro_redistribution)) %>%
  ggplot(., aes(x=as.factor(Election), y=pct, fill=as.factor(pro_redistribution)))+geom_col(position="dodge")+facet_grid(~as_factor(vote))+labs(x="Election", fill="Redistribution", y="Percent")+scale_fill_grey(start=0.8, end=0.2)
ggsave(here("Plots", "redistribution_vote_2015_2019.png"))

out %>% 
  mutate(pro_redistribution=case_when(
    redistribution>0.5~1,
    redistribution<0.75~0,
    TRUE~NA_real_
  )) %>% 
group_by(survey, vote, pro_redistribution) %>% 
  summarize(average=mean(moral_traditionalism)) %>% 
  filter(vote>0&vote<4) %>% 
  filter(!is.na(pro_redistribution)) %>% 
  ggplot(., aes(x=vote, y=average, col=as_factor(pro_redistribution)))+facet_grid(survey~as_factor(vote))+geom_jitter()+ylim(c(0,1))




 ces %>% 
  select(election, working_class4, pro_redistribution, vote) %>% 
   filter(working_class4==1) %>% 
  group_by(election, working_class4, pro_redistribution,vote) %>% 
   # filter(!is.na(quebec)) %>% 
  #  filter(vote>0 &vote<4) %>% 
summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(vote>0 &vote<4) %>% 
  filter(!is.na(pro_redistribution)) %>% 
  ggplot(., aes(x=election, y=pct, fill=as.factor(pro_redistribution)))+geom_col(position="dodge")+facet_grid(~as_factor(vote))
  

library(DescTools)
#### Pseudo R2 NDP ####
#ROC
roc %>% 
  nest(-survey) %>% 
  mutate(ndp_demographics=map(data, function(x) glm(ndp~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income, family="binomial", data=x)), 
         ndp_values=map(data, function(x) glm(ndp~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x)),
         ndp_partisanship=map(data, function(x) glm(ndp~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x)),
             ndp_retrospection=map(data, function(x) glm(ndp~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
         ndp_policy=map(data, function(x) glm(ndp~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
                               ndp_leaders=map(data, function(x) glm(ndp~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>% 
  pivot_longer(., ndp_demographics:ndp_leaders, values_to=c('Model'), names_to=c('Block')) %>% 
  mutate(r2=map(Model, PseudoR2)) %>% 
  unnest(r2) ->ndp_pseudos

# ndp_pseudos
# ndp_pseudos %>% 
#   ggplot(., aes(x=fct_relevel(Block, "ndp_demographics", "ndp_values", "ndp_partisanship", "ndp_retrospection","ndp_policy", "ndp_leaders"), y=r2, col=as.factor(survey)))+geom_point()+coord_flip()

#QC
qc %>% 
  nest(-survey) %>% 
  mutate(ndp_demographics=map(data, function(x) glm(ndp~working_class4+union_both+young+old+male+degree+language+vismin, family="binomial", data=x)), 
         ndp_values=map(data, function(x) glm(ndp~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
         ndp_partisanship=map(data, function(x) glm(ndp~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
         ndp_retrospection=map(data, function(x) glm(ndp~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
         ndp_policy=map(data, function(x) glm(ndp~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
         ndp_leaders=map(data, function(x) glm(ndp~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>% 
  pivot_longer(., ndp_demographics:ndp_leaders, values_to=c('Model'), names_to=c('Block')) %>% 
  mutate(r2=map(Model, PseudoR2)) %>% 
  unnest(r2) ->ndp_qc_pseudos


#### Pseudo R2 NDP Liberals ####
#ROC
roc %>% 
  nest(-survey) %>% 
  mutate(liberal_demographics=map(data, function(x) glm(liberal~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income, family="binomial", data=x)), 
         liberal_values=map(data, function(x) glm(liberal~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x)),
         liberal_partisanship=map(data, function(x) glm(liberal~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x)),
         liberal_retrospection=map(data, function(x) glm(liberal~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
         liberal_policy=map(data, function(x) glm(liberal~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
         liberal_leaders=map(data, function(x) glm(liberal~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>% 
  pivot_longer(., liberal_demographics:liberal_leaders, values_to=c('Model'), names_to=c('Block')) %>% 
  mutate(r2=map(Model, PseudoR2)) %>% 
  unnest(r2) ->liberal_pseudos

# liberal_pseudos %>% 
#   ggplot(., aes(x=fct_relevel(Block, "liberal_demographics", "liberal_values", "liberal_partisanship", "liberal_retrospection","liberal_policy", "liberal_leaders"), y=r2, col=as.factor(survey)))+geom_point()+coord_flip()

#QC
qc %>% 
  nest(-survey) %>% 
  mutate(liberal_demographics=map(data, function(x) glm(liberal~working_class4+union_both+young+old+male+degree+language+vismin, family="binomial", data=x)), 
         liberal_values=map(data, function(x) glm(liberal~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
         liberal_partisanship=map(data, function(x) glm(liberal~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
         liberal_retrospection=map(data, function(x) glm(liberal~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
         liberal_policy=map(data, function(x) glm(liberal~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
         liberal_leaders=map(data, function(x) glm(liberal~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>% 
  pivot_longer(., liberal_demographics:liberal_leaders, values_to=c('Model'), names_to=c('Block')) %>% 
  mutate(r2=map(Model, PseudoR2)) %>% 
  unnest(r2) ->liberal_qc_pseudos
# 
# liberal_qc_pseudos %>% 
#   ggplot(., aes(x=fct_relevel(Block, "liberal_demographics", "liberal_values", "liberal_partisanship", "liberal_retrospection","liberal_policy", "liberal_leaders"), y=r2, col=as.factor(survey)))+geom_point()+coord_flip()

#### Pseudo R2 NDP Conservatives ####
#ROC
roc %>% 
  nest(-survey) %>% 
  mutate(conservative_demographics=map(data, function(x) glm(conservative~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income, family="binomial", data=x)), 
         conservative_values=map(data, function(x) glm(conservative~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x)),
         conservative_partisanship=map(data, function(x) glm(conservative~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x)),
         conservative_retrospection=map(data, function(x) glm(conservative~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
         conservative_policy=map(data, function(x) glm(conservative~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
         conservative_leaders=map(data, function(x) glm(conservative~region3+working_class4+union_both+young+old+male+sector+catholic+no_religion+degree+vismin+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))) %>% 
  pivot_longer(., conservative_demographics:conservative_leaders, values_to=c('Model'), names_to=c('Block')) %>% 
  mutate(r2=map(Model, PseudoR2)) %>% 
  unnest(r2) ->conservative_pseudos

# conservative_pseudos %>% 
#   ggplot(., aes(x=fct_relevel(Block, "conservative_demographics", "conservative_values", "conservative_partisanship", "conservative_retrospection","conservative_policy", "conservative_leaders"), y=r2, col=as.factor(survey)))+geom_point()+coord_flip()



#QC
qc %>% 
  nest(-survey) %>% 
  mutate(conservative_demographics=map(data, function(x) glm(conservative~working_class4+union_both+young+old+male+degree+language+vismin, family="binomial", data=x)), 
         conservative_values=map(data, function(x) glm(conservative~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
         conservative_partisanship=map(data, function(x) glm(conservative~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
         conservative_retrospection=map(data, function(x) glm(conservative~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
         conservative_policy=map(data, function(x) glm(conservative~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
         conservative_leaders=map(data, function(x) glm(conservative~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>% 
  pivot_longer(., conservative_demographics:conservative_leaders, values_to=c('Model'), names_to=c('Block')) %>% 
  mutate(r2=map(Model, PseudoR2)) %>% 
  unnest(r2) ->conservative_qc_pseudos

# conservative_qc_pseudos %>% 
#   ggplot(., aes(x=fct_relevel(Block, "conservative_demographics", "conservative_values", "conservative_partisanship", "conservative_retrospection","conservative_policy", "conservative_leaders"), y=r2, col=as.factor(survey)))+geom_point()+coord_flip()

#### Pseudo R2 NDP Bloc ####
#QC
qc %>% 
  nest(-survey) %>% 
  mutate(bloc_demographics=map(data, function(x) glm(bloc~working_class4+union_both+young+old+male+degree+language+vismin, family="binomial", data=x)), 
         bloc_values=map(data, function(x) glm(bloc~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
         bloc_partisanship=map(data, function(x) glm(bloc~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
         bloc_retrospection=map(data, function(x) glm(bloc~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
         bloc_policy=map(data, function(x) glm(bloc~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
         bloc_leaders=map(data, function(x) glm(bloc~working_class4+union_both+young+old+male+degree+language+vismin+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))) %>% 
  pivot_longer(., bloc_demographics:bloc_leaders, values_to=c('Model'), names_to=c('Block')) %>% 
  mutate(r2=map(Model, PseudoR2)) %>% 
  unnest(r2) ->bloc_qc_pseudos


#### Combine Pseudo R2 ####
ndp_pseudos %>% 
  bind_rows(., liberal_pseudos) %>% 
  bind_rows(., conservative_pseudos) %>%
  bind_rows(., ndp_qc_pseudos) %>% 
  bind_rows(., liberal_qc_pseudos) %>% 
  bind_rows(., conservative_qc_pseudos) %>% 
  bind_rows(., bloc_qc_pseudos) %>% 
  mutate(Region=c(rep("ROC", 6*2*3), rep("QC", 6*2*4))) %>% 
separate(Block, sep="_", into=c("Party", "Block")) %>% 
  mutate(Party=str_to_title(Party)) %>% 
  mutate(Party=str_replace_all(Party, "Ndp","NDP")) %>% 
  mutate(Block=str_to_title(Block)) %>% 
  mutate(Election=car::Recode(survey, "0=2015; 1=2019", as.factor=T)) %>% 
  ggplot(., aes(x=fct_relevel(Block, "Demographics", "Values", "Partisanship", "Retrospection", "Policy", "Leaders"), y=r2, col=Election))+geom_point()+facet_grid(Region~Party)+coord_flip()+labs(x="Block", y="Pseudo R2")
ggsave(here("Plots", "block_models_pseudor2.png"), width=8, height=4)

  


detach(package:DescTools)
