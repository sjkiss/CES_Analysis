#### Various models exploring ces15 and working class interactions 
#Run master file to load up data

library(stargazer)
library(broom)
library(nnet)
summary(ces15phone)

#Recodes
#CREATE WORKING CLASS DICHOTOMOUS VARIABLE; NOTE HERE ONLY EMPLOYED AND SELF-EMPLOYED PEOPLE ARE SET TO 0 OR 1; ELSE = NA
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
ces15phone$young<-Recode(ces15phone$age, "35:100=0; 18:34=1")
ces15phone$old<-Recode(ces15phone$age, "55:100=1; 18:54=0")
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

#Split QC out into ces15.qc
ces15phone %>% 
  filter(quebec==1)->ces15.qc
ces15phone %>% 
  filter(quebec!=1)->ces15.roc

#### 2015 Block recursive models ####

#Model 1 - income as 3 level factor
#NDP
ndp_models1ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces15.roc, family="binomial")
ndp_models1QC<-glm(ndp~working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces15.qc, family="binomial")
summary(ndp_models1ROC)
summary(ndp_models1QC)

#Liberal
lib_models1ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces15.roc, family="binomial")
lib_models1QC<-glm(liberal~working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces15.qc, family="binomial")
summary(lib_models1ROC)
summary(lib_models1QC)

#Conservative
con_models1ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces15.roc, family="binomial")
con_models1QC<-glm(conservative~working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces15.qc, family="binomial")
summary(con_models1ROC)
summary(con_models1QC)

#Model 2 - low and high income quintiles
ndp_models2ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces15.roc, family="binomial")
ndp_models2QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign, data=ces15.qc, family="binomial")
summary(ndp_models2ROC)
summary(ndp_models2QC)

#Liberal
lib_models2ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces15.roc, family="binomial")
lib_models2QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign, data=ces15.qc, family="binomial")
summary(lib_models2ROC)
summary(lib_models2QC)

#Conservative
con_models2ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces15.roc, family="binomial")
con_models2QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign, data=ces15.qc, family="binomial")
summary(con_models2ROC)
summary(con_models2QC)

#Bloc
bloc_models2QC<-glm(bloc~working_class+union_both+young+old+male+degree+language+foreign, data=ces15.qc, family="binomial")
summary(bloc_models2QC)

#Model 3 - Political Orientations
#NDP
ndp_models3ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, data=ces15.roc, family="binomial")
ndp_models3QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, data=ces15.qc, family="binomial")
summary(ndp_models3ROC)
summary(ndp_models3QC)

#Liberal
lib_models3ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, data=ces15.roc, family="binomial")
lib_models3QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, data=ces15.qc, family="binomial")
summary(lib_models3ROC)
summary(lib_models3QC)

#Conservative
con_models3ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, data=ces15.roc, family="binomial")
con_models3QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, data=ces15.qc, family="binomial")
summary(con_models3ROC)
summary(con_models3QC)

#Bloc
bloc_models3QC<-glm(bloc~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, data=ces15.qc, family="binomial")
summary(bloc_models3QC)

#Model 4 - Partisanship
#NDP
ndp_models4ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id, data=ces15.roc, family="binomial")
ndp_models4QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id, data=ces15.qc, family="binomial")
summary(ndp_models4ROC)
summary(ndp_models4QC)

#Liberal
lib_models4ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id, data=ces15.roc, family="binomial")
lib_models4QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id, data=ces15.qc, family="binomial")
summary(lib_models4ROC)
summary(lib_models4QC)

#Conservative
con_models4ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id, data=ces15.roc, family="binomial")
con_models4QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id, data=ces15.qc, family="binomial")
summary(con_models4ROC)
summary(con_models4QC)

#Bloc
bloc_models4QC<-glm(bloc~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id, data=ces15.qc, family="binomial")
summary(bloc_models4QC)

#Model 5 - Economic Perceptions
#NDP
ndp_models5ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+personal_retrospective+national_retrospective, data=ces15.roc, family="binomial")
ndp_models5QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+personal_retrospective+national_retrospective, data=ces15.qc, family="binomial")
summary(ndp_models5ROC)
summary(ndp_models5QC)

#Liberal
lib_models5ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id+personal_retrospective+national_retrospective, data=ces15.roc, family="binomial")
lib_models5QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id+personal_retrospective+national_retrospective, data=ces15.qc, family="binomial")
summary(lib_models5ROC)
summary(lib_models5QC)

#Conservative
con_models5ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id+personal_retrospective+national_retrospective, data=ces15.roc, family="binomial")
con_models5QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id+personal_retrospective+national_retrospective, data=ces15.qc, family="binomial")
summary(con_models5ROC)
summary(con_models5QC)

#Bloc
bloc_models5QC<-glm(bloc~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id+personal_retrospective+national_retrospective, data=ces15.qc, family="binomial")
summary(bloc_models5QC)

#Model 6 - Policy Issues
#NDP
ndp_models6ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces15.roc, family="binomial")
ndp_models6QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces15.qc, family="binomial")
summary(ndp_models6ROC)
summary(ndp_models6QC)

#Liberal
lib_models6ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces15.roc, family="binomial")
lib_models6QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces15.qc, family="binomial")
summary(lib_models6ROC)
summary(lib_models6QC)

#Conservative
con_models6ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id+personal_retrospective+immigration_rate+national_retrospective+environment+redistribution+defence, data=ces15.roc, family="binomial")
con_models6QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces15.qc, family="binomial")
summary(con_models6ROC)
summary(con_models6QC)

#Bloc
bloc_models6QC<-glm(bloc~+working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces15.qc, family="binomial")
summary(bloc_models6QC)

#Model 7 - Leadership
#NDP
ndp_models7ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15, data=ces15.roc, family="binomial")
ndp_models7QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15+Gilles_Duceppe15, data=ces15.qc, family="binomial")
summary(ndp_models7ROC)
summary(ndp_models7QC)

#Liberal
lib_models7ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15, data=ces15.roc, family="binomial")
lib_models7QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15+Gilles_Duceppe15, data=ces15.qc, family="binomial")
summary(lib_models7ROC)
summary(lib_models7QC)

#Conservative
con_models7ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id+personal_retrospective+immigration_rate+national_retrospective+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15, data=ces15.roc, family="binomial")
con_models7QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15+Gilles_Duceppe15, data=ces15.qc, family="binomial")
summary(con_models7ROC)
summary(con_models7QC)

#Bloc
bloc_models7QC<-glm(bloc~+working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15+Gilles_Duceppe15, data=ces15.qc, family="binomial")
summary(bloc_models7QC)

#Model 8 - Block1: using working_class2 variable instead
#NDP
ndp_models8ROC<-glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces15.roc, family="binomial")
ndp_models8QC<-glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign, data=ces15.qc, family="binomial")
summary(ndp_models8ROC)
summary(ndp_models8QC)

#Liberal
lib_models8ROC<-glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces15.roc, family="binomial")
lib_models8QC<-glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign, data=ces15.qc, family="binomial")
summary(lib_models8ROC)
summary(lib_models8QC)

#Conservative
con_models8ROC<-glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces15.roc, family="binomial")
con_models8QC<-glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign, data=ces15.qc, family="binomial")
summary(con_models8ROC)
summary(con_models8QC)

#Bloc
bloc_models8QC<-glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign, data=ces15.qc, family="binomial")
summary(bloc_models8QC)

#Model 9 - Block6: using working_class2 variable instead
#NDP
ndp_models9ROC<-glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15, data=ces15.roc, family="binomial")
ndp_models9QC<-glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15+Gilles_Duceppe15, data=ces15.qc, family="binomial")
summary(ndp_models9ROC)
summary(ndp_models9QC)

#Liberal
lib_models9ROC<-glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15, data=ces15.roc, family="binomial")
lib_models9QC<-glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15+Gilles_Duceppe15, data=ces15.qc, family="binomial")
summary(lib_models9ROC)
summary(lib_models9QC)

#Conservative
con_models9ROC<-glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15, data=ces15.roc, family="binomial")
con_models9QC<-glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15+Gilles_Duceppe15, data=ces15.qc, family="binomial")
summary(con_models9ROC)
summary(con_models9QC)

#Bloc
bloc_models9QC<-glm(bloc~+working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau15+Stephen_Harper15+Tom_Mulcair15+Gilles_Duceppe15, data=ces15.qc, family="binomial")
summary(bloc_models9QC)

#Model comparison
stargazer(ndp_models2ROC, lib_models2ROC, con_models2ROC, ndp_models2QC, lib_models2QC, con_models2QC, bloc_models2QC ,type="html", out=here("Tables", "ces15_block1.html"))
stargazer(ndp_models3ROC, lib_models3ROC, con_models3ROC, ndp_models3QC, lib_models3QC, con_models3QC, bloc_models3QC ,type="html", out=here("Tables", "ces15_block2.html"))
stargazer(ndp_models4ROC, lib_models4ROC, con_models4ROC, ndp_models4QC, lib_models4QC, con_models4QC, bloc_models4QC ,type="html", out=here("Tables", "ces15_block3.html"))
stargazer(ndp_models5ROC, lib_models5ROC, con_models5ROC, ndp_models5QC, lib_models5QC, con_models5QC, bloc_models5QC ,type="html", out=here("Tables", "ces15_block4.html"))
stargazer(ndp_models6ROC, lib_models6ROC, con_models6ROC, ndp_models6QC, lib_models6QC, con_models6QC, bloc_models6QC ,type="html", out=here("Tables", "ces15_block5.html"))
stargazer(ndp_models7ROC, lib_models7ROC, con_models7ROC, ndp_models7QC, lib_models7QC, con_models7QC, bloc_models7QC ,type="html", out=here("Tables", "ces15_block6.html"))
stargazer(ndp_models8ROC, lib_models8ROC, con_models8ROC, ndp_models8QC, lib_models8QC, con_models8QC, bloc_models8QC ,type="html", out=here("Tables", "ces15_block1_working_class2.html"))
stargazer(ndp_models9ROC, lib_models9ROC, con_models9ROC, ndp_models9QC, lib_models9QC, con_models9QC, bloc_models9QC ,type="html", out=here("Tables", "ces15_block6_working_class2.html"))
