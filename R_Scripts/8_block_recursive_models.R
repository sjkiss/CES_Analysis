#Comparing ces15 and ces19 Block Recursive Models
library(nnet)
library(broom)
library(purrr)
# ces15phone %>%
#   mutate(union_both=case_when(
#     PES15_93==1 | PES15_94==1 ~ 1,
#     PES15_93==5 | PES15_94==5 ~ 0,
#     PES15_93==8 & PES15_94==8 ~ NA_real_,
#     PES15_93==9 & PES15_94==9 ~ NA_real_,
#   ))->ces15phone
#### Some 2015 recodes ####
###These could be cleaned up
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

#Turn income into factor with Middle as reference
ces15phone$income3<-Recode(as.factor(ces15phone$income), "1='Low_Income' ; 2:4='Middle_Income' ; 5='High_Income'", levels=c('Low_Income', 'Middle_Income', 'High_Income'))
levels(ces15phone$income3)
table(ces15phone$income3)

#Other dummies
ces15phone$low_income<-Recode(ces15phone$income, "2:5=0; 1=1")
ces15phone$high_income<-Recode(ces15phone$income, "1:4=0; 5=1")
ces15phone$no_religion<-Recode(ces15phone$religion, "0=1; 1:3=0; NA=NA")
ces15phone$catholic<-Recode(ces15phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces15phone$young<-Recode(ces15phone$age, "35:115=0; 18:34=1")
ces15phone$old<-Recode(ces15phone$age, "55:115=1; 18:54=0")
ces15phone$foreign<-Recode(ces15phone$native, "1=0; 0=1")

#Dummies coded missing as 0
#ces15phone$low_income<-Recode(ces15phone$income, "else=0; 1=1")
#ces15phone$high_income<-Recode(ces15phone$income, "else=0; 5=1")
#ces15phone$no_religion<-Recode(ces15phone$religion, "0=1; else=0")
#ces15phone$catholic<-Recode(ces15phone$religion, "1=1; else=0")
#ces15phone$young<-Recode(ces15phone$age, "else=0; 18:34=1")
#ces15phone$old<-Recode(ces15phone$age, "55:100=1; else=0")
#ces15phone$foreign<-Recode(ces15phone$native, "else=0; 0=1")
table(ces15phone$low_income)
table(ces15phone$high_income)
table(ces15phone$no_religion)
table(ces15phone$catholic)
table(ces15phone$young)
table(ces15phone$old)
table(ces15phone$foreign)
ces15phone$working_class<-Recode(ces15phone$working_class, "1=1; else=0")
ces15phone$working_class2<-Recode(ces15phone$working_class2, "1=1; else=0")
#ces15phone$union_both<-Recode(ces15phone$union_both, "1=1; else=0")
#ces15phone$male<-Recode(ces15phone$male, "1=1; else=0")
#ces15phone$sector<-Recode(ces15phone$sector, "1=1; else=0")
#ces15phone$degree<-Recode(ces15phone$degree, "1=1; else=0")
#ces15phone$language<-Recode(ces15phone$language, "1=1; else=0")
table(ces15phone$working_class)
table(ces15phone$working_class2)
table(ces15phone$union_both)
table(ces15phone$male)
table(ces15phone$sector)
table(ces15phone$degree)
table(ces15phone$language)

# Party Id
#ces15phone$liberal_id<-Recode(ces15phone$party_id, "1=1; 0=0; 2:4=0; else=NA")
#ces15phone$conservative_id<-Recode(ces15phone$party_id, "2=1; 0:1=0; 3:4=0; else=NA")
#ces15phone$ndp_id<-Recode(ces15phone$party_id, "3=1; 0:2=0; 4=0; else=NA")
#ces15phone$bloc_id<-Recode(ces15phone$party_id, "4=1; 0:3=0; else=NA")
ces15phone$liberal_id<-Recode(ces15phone$party_id, "1=1; else=0")
ces15phone$conservative_id<-Recode(ces15phone$party_id, "2=1; else=0")
ces15phone$ndp_id<-Recode(ces15phone$party_id, "3=1; else=0")
ces15phone$bloc_id<-Recode(ces15phone$party_id, "4=1; else=0")
table(ces15phone$liberal_id)
table(ces15phone$conservative_id)
table(ces15phone$ndp_id)
table(ces15phone$bloc_id)

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
table(ces15phone$liberal)
table(ces15phone$conservative)
table(ces15phone$ndp)
table(ces15phone$bloc)
table(ces15phone$green)

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
table(ces19phone$working_class2)

#Turn region into factor with East as reference case
ces19phone$region3<-Recode(as.factor(ces19phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces19phone$region3)
table(ces19phone$region3)

#Turn income into factor with Middle as reference
ces19phone$income3<-Recode(as.factor(ces19phone$income), "1='Low_Income' ; 2:4='Middle_Income' ; 5='High_Income'", levels=c('Low_Income', 'Middle_Income', 'High_Income'))
levels(ces19phone$income3)
table(ces19phone$income3)

#Other dummies
ces19phone$low_income<-Recode(ces19phone$income, "2:5=0; 1=1")
ces19phone$high_income<-Recode(ces19phone$income, "1:4=0; 5=1")
ces19phone$no_religion<-Recode(ces19phone$religion, "0=1; 1:3=0; NA=NA")
ces19phone$catholic<-Recode(ces19phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces19phone$young<-Recode(ces19phone$age, "35:100=0; 18:34=1")
ces19phone$old<-Recode(ces19phone$age, "55:100=1; 18:54=0")
ces19phone$foreign<-Recode(ces19phone$native, "1=0; 0=1")

#Dummies coded missing as 0
#ces19phone$low_income<-Recode(ces19phone$income, "else=0; 1=1")
#ces19phone$high_income<-Recode(ces19phone$income, "else=0; 5=1")
#ces19phone$no_religion<-Recode(ces19phone$religion, "0=1; else=0")
#ces19phone$catholic<-Recode(ces19phone$religion, "1=1; else=0")
#ces19phone$young<-Recode(ces19phone$age, "else=0; 18:34=1")
#ces19phone$old<-Recode(ces19phone$age, "55:100=1; else=0")
#ces19phone$foreign<-Recode(ces19phone$native, "else=0; 0=1")
table(ces19phone$low_income)
table(ces19phone$high_income)
table(ces19phone$no_religion)
table(ces19phone$catholic)
table(ces19phone$young)
table(ces19phone$old)
table(ces19phone$foreign)
ces19phone$working_class<-Recode(ces19phone$working_class, "1=1; else=0")
ces19phone$working_class2<-Recode(ces19phone$working_class2, "1=1; else=0")
#ces19phone$union_both<-Recode(ces19phone$union_both, "1=1; else=0")
#ces19phone$male<-Recode(ces19phone$male, "1=1; else=0")
#ces19phone$sector<-Recode(ces19phone$sector, "1=1; else=0")
#ces19phone$degree<-Recode(ces19phone$degree, "1=1; else=0")
#ces19phone$language<-Recode(ces19phone$language, "1=1; else=0")
table(ces19phone$working_class)
table(ces19phone$working_class2)
table(ces19phone$union_both)
table(ces19phone$male)
table(ces19phone$sector)
table(ces19phone$degree)
table(ces19phone$language)

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

#First make a ces15 roc data frame
ces15phone %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, 
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, 
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec, occupation4, minorities, immigration, immigration2, minorities_help, mip=mip)->out15

#Now an ces19data frame
ces19phone %>% 
#  filter(quebec!=1) %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, 
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, 
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec, occupation4, minorities, immigration, immigration2, immigration_rate, minorities_help, mip=mip_cat)->out19

out15$survey<-rep(0, nrow(out15))
out19$survey<-rep(1, nrow(out19))
val_labels(out15$survey)<-c(`2015`=0, `2019`=1)
out15 %>% 
  bind_rows(., out19)->out
roc<-out %>% 
  filter(quebec!=1)
qc<-out %>% 
  filter(quebec==1)

#### NDP ROC ####

block1<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)

#Turn into a list
roc_ndp<-list(block1, block2, block3, block4, block5, block6)
names(roc_ndp)<-c("block1", "block2", "block3", "block4", "block5", "block6")

library(kableExtra)
library(knitr)
## This code  pulls it all nicely together. 
roc_ndp %>% 
  #Tidy each of the models 
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

# 
# #Save the values to be bolded here
# #This requires changing which object is used e.g. roc_ndp_table might become qc_ndp_table etc. 
# to_bold<-roc_ndp_table$p.value<0.05
# roc_ndp_table %>% 
#   kable(., digits=2) %>% 
#   #bold the third and fourth columns
#   column_spec(3:4, bold=to_bold) %>% 
#   save_kable(file="Tables/ndp_roc_interaction.html")

#### NDP QC ####

block1<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign)*survey, family="binomial", data=qc)
block2<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)
block6<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
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
# 
# #Save the values to be bolded here
# to_bold<-qc_ndp_table$p.value<0.05
# qc_ndp_table %>% 
#   kable(., digits=2) %>% 
#   column_spec(3:4, bold=to_bold) %>% 
#   save_kable(file="Tables/ndp_qc_interaction.html")

#### Conservative ROC####

block1<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)
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

# #Save the values to be bolded here
# to_bold<-roc_conservative_table$p.value<0.05
# roc_conservative_table %>% 
#   kable(., digits=2) %>% 
#   column_spec(3:4, bold=to_bold) %>% 
# save_kable(file="Tables/conservative_roc_interaction.html")

#### Conservative QC ####
block1<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign)*survey, family="binomial", data=qc)
block2<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)
block6<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
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

#Save the values to be bolded here
# to_bold<-qc_conservative_table$p.value<0.05
# qc_conservative_table %>% 
#   kable(., digits=2) %>% 
#   column_spec(3:4, bold=to_bold) %>% 
#   save_kable(file="Tables/conservative_qc_interaction.html")

#### Liberal ROC Interation ####
block1<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)
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

#Save the values to be bolded here
# to_bold<-roc_liberal_table$p.value<0.05
# roc_liberal_table %>% 
#   kable(., digits=2) %>% 
#   column_spec(3:4, bold=to_bold) %>% 
#   save_kable(file="Tables/liberal_roc_interaction.html")

#### Liberal QC Interation ####
block1<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign)*survey, family="binomial", data=qc)
block2<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)
block6<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
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

#Save the values to be bolded here
# to_bold<-qc_liberal_table$p.value<0.05
# qc_liberal_table %>% 
#   kable(., digits=2) %>% 
#   column_spec(3:4, bold=to_bold) %>% 
#   save_kable(file="Tables/liberal_qc_interaction.html")

#### Bloc QC Interation ####
block1<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign)*survey, family="binomial", data=qc)
block2<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)

block6<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
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

#Save the values to be bolded here
# to_bold<-qc_bloc_table$p.value<0.05
# qc_bloc_table %>% 
#   kable(., digits=2) %>% 
#   column_spec(3:4, bold=to_bold) %>% 
#   save_kable(file="Tables/bloc_qc_interaction.html")


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
    add_header_lines(values=c("ROC Block Recursive Model Coefficients, 2015 and 2019")) %>% save_as_html("Tables/roc_block_recursive_table.html")


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

## Did the union movement really go down for all parties?
roc_ndp_table %>% 
  filter(term=="union_both")
roc_liberal_table %>% 
  filter(term=="union_both")
roc_conservative_table %>% 
  filter(term=="union_both")

#### Policy variation change between 2015-19####

#scales goes from -1 Left to +1 Right
library(psych)

#Positive RW scales (no need to reverse code)
table(ces15phone$moral_traditionalism, useNA="ifany")
table(ces15phone$market_liberalism, useNA="ifany")
table(ces15phone$continentalism, useNA="ifany")
ces15phone$redistribution


#Reverse code positive LW scales to positive RW scales
out$environment<-reverse.code(-1, out[,'environment'])
out$redistribution<-reverse.code(-1, out[,'redistribution'])
out$immigration<-reverse.code(-1, out[,'immigration'])
out$immigration2<-reverse.code(-1, out[,'immigration2'])
out$immigration_rate<-reverse.code(-1, out[,'immigration_rate'])
out$minorities_help<-reverse.code(-1, out[,'minorities_help'])

#checks
table(out$environment, useNA="ifany")
table(out$redistribution, useNA="ifany")
table(out$immigration, useNA="ifany")
table(out$immigration2, useNA="ifany")
table(out$immigration_rate, useNA="ifany")
table(out$minorities_help, useNA="ifany")
table(out$environment, useNA="ifany")
table(out$redistribution, useNA="ifany")
table(out$immigration, useNA="ifany")
table(out$immigration2, useNA="ifany")
table(out$immigration_rate, useNA="ifany")
table(out$minorities_help, useNA="ifany")

#Policy rating changes

####Attitudinal change  ####
out %>% 
  select(immigration, immigration2, immigration_rate, minorities_help, environment, redistribution, continentalism, moral_traditionalism, market_liberalism, survey, occupation4) %>% 
  pivot_longer(cols=immigration:market_liberalism) %>% 
  group_by(survey, occupation4, name)  %>% 
  summarize(Average=mean(value, na.rm=T)) %>% 
  arrange(occupation4, name, survey) %>% 
  group_by(name, occupation4) %>% 
  mutate(Difference=Average-lag(Average)) %>% 
  filter(survey==1) %>% 
  ggplot(., aes(x=occupation4, y=Difference))+geom_point(position="jitter")+ylim(-0.115,0.15)+labs(x="Class", y="Difference (2019-2015)", caption="This graph shows the difference between 2015 and 2019 scores on a range of items by social class.\nThe items have all been scored from 0 to 1 so a shift by 0.1 equals a 10% shift in the underlying sentiment.\n All items have been scored here such that positive differences are a shift to the right\nand negative scores are a shift to the left", title="Attitudinal Differences by Social Class", subtitle="CES 2015 and 2019")+facet_wrap(~name)+coord_flip()+geom_hline(yintercept=0, linetype=2)
ggsave("Plots/attitudinal_differences_2015_2019.png")
#Leader rating changes
out %>% 
  select(liberal_leader, conservative_leader, ndp_leader, bloc_leader, survey, occupation4) %>% 
  pivot_longer(cols=liberal_leader:bloc_leader) %>% 
  group_by(survey, occupation4, name)  %>% 
  summarize(Average=mean(value, na.rm=T)) %>% 
  arrange(occupation4, name, survey) %>% 
  group_by(name, occupation4) %>% 
  mutate(Difference=Average-lag(Average)) %>% 
  filter(survey==1) %>% 
  ggplot(., aes(x=occupation4, y=Difference, col=name))+geom_point(position="jitter")+ylim(-0.2,0.2)+labs(x="Class", y="Difference (2019-2015)") 

#### Most important problem####
out %>% 
group_by(Survey=as_factor(survey), `Most Important Problem`=as_factor(mip)) %>% 
  summarise(n=n()) %>% 
  filter(!is.na(`Most Important Problem`)) %>% 
  ggplot(., aes(y=reorder(`Most Important Problem`,n), x=n, fill=Survey))+geom_col(position="dodge")+scale_fill_grey()+labs(y="Most Important Problem")
ggsave("Plots/mip_2015_2019.png")
