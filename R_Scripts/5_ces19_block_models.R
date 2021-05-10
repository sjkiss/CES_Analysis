#### Various models exploring ces19 and working class interactions 
#Run master file to load up data

library(stargazer)
library(broom)
library(nnet)
summary(ces19phone)

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

#Recode low income sub-sample
ces19phone$income1<-Recode(ces19phone$income, "1=1; else=NA")
table(ces19phone$income1)

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

#Split QC out into ces19.qc
ces19phone %>% 
  filter(quebec==1)->ces19.qc
ces19phone %>% 
  filter(quebec!=1)->ces19.roc

#--------------------------------------------------------------------------------------------------------

#### 2019 Block recursive models ####

#Model 1 - income as 3 level factor
#NDP
ndp_model1ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces19.roc, family="binomial")

ndp_model1QC<-glm(ndp~working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces19.qc, family="binomial")
summary(ndp_model1ROC)
summary(ndp_model1QC)

#Liberal
lib_model1ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces19.roc, family="binomial")
lib_model1QC<-glm(liberal~working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces19.qc, family="binomial")
summary(lib_model1ROC)
summary(lib_model1QC)

#Conservative
con_model1ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces19.roc, family="binomial")
con_model1QC<-glm(conservative~working_class+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+as.factor(income3), data=ces19.qc, family="binomial")
summary(con_model1ROC)
summary(con_model1QC)

#Model 2 - low and high income quintiles
#NDP
ndp_model2ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces19.roc, family="binomial")
ndp_model2QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign, data=ces19.qc, family="binomial")
summary(ndp_model2ROC)
summary(ndp_model2QC)

#Liberal
lib_model2ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces19.roc, family="binomial")
lib_model2QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign, data=ces19.qc, family="binomial")
summary(lib_model2ROC)
summary(lib_model2QC)

#Conservative
con_model2ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces19.roc, family="binomial")
con_model2QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign, data=ces19.qc, family="binomial")
summary(con_model2ROC)
summary(con_model2QC)

#Bloc
bloc_model2QC<-glm(bloc~working_class+union_both+young+old+male+degree+language+foreign, data=ces19.qc, family="binomial")
summary(bloc_model2QC)

#Model 3 - Political Orientations
#NDP
ndp_model3ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, data=ces19.roc, family="binomial")
ndp_model3QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, data=ces19.qc, family="binomial")
summary(ndp_model3ROC)
summary(ndp_model3QC)

#Liberal
lib_model3ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, data=ces19.roc, family="binomial")
lib_model3QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, data=ces19.qc, family="binomial")
summary(lib_model3ROC)
summary(lib_model3QC)

#Conservative
con_model3ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, data=ces19.roc, family="binomial")
con_model3QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, data=ces19.qc, family="binomial")
summary(con_model3ROC)
summary(con_model3QC)

#Bloc
bloc_model3QC<-glm(bloc~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, data=ces19.qc, family="binomial")
summary(bloc_model3QC)

#Model 4 - Partisanship
#NDP
ndp_model4ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id, data=ces19.roc, family="binomial")
ndp_model4QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id, data=ces19.qc, family="binomial")
summary(ndp_model4ROC)
summary(ndp_model4QC)

#Liberal
lib_model4ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id, data=ces19.roc, family="binomial")
lib_model4QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id, data=ces19.qc, family="binomial")
summary(lib_model4ROC)
summary(lib_model4QC)

#Conservative
con_model4ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id, data=ces19.roc, family="binomial")
con_model4QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id, data=ces19.qc, family="binomial")
summary(con_model4ROC)
summary(con_model4QC)

#Bloc
bloc_model4QC<-glm(bloc~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id, data=ces19.qc, family="binomial")
summary(bloc_model4QC)

#Model 5 - Economic Perceptions
#NDP
ndp_model5ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+personal_retrospective+national_retrospective, data=ces19.roc, family="binomial")
ndp_model5QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+personal_retrospective+national_retrospective, data=ces19.qc, family="binomial")
summary(ndp_model5ROC)
summary(ndp_model5QC)

#Liberal
lib_model5ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id+personal_retrospective+national_retrospective, data=ces19.roc, family="binomial")
lib_model5QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id+personal_retrospective+national_retrospective, data=ces19.qc, family="binomial")
summary(lib_model5ROC)
summary(lib_model5QC)

#Conservative
con_model5ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id+personal_retrospective+national_retrospective, data=ces19.roc, family="binomial")
con_model5QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id+personal_retrospective+national_retrospective, data=ces19.qc, family="binomial")
summary(con_model5ROC)
summary(con_model5QC)

#Bloc
bloc_model5QC<-glm(bloc~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id+personal_retrospective+national_retrospective, data=ces19.qc, family="binomial")
summary(bloc_model5QC)

#Model 6 - Policy Issues
#NDP
ndp_model6ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces19.roc, family="binomial")
ndp_model6QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces19.qc, family="binomial")
summary(ndp_model6ROC)
summary(ndp_model6QC)

#Liberal
lib_model6ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces19.roc, family="binomial")
lib_model6QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces19.qc, family="binomial")
summary(lib_model6ROC)
summary(lib_model6QC)

#Conservative
con_model6ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id+personal_retrospective+immigration_rate+national_retrospective+environment+redistribution+defence, data=ces19.roc, family="binomial")
con_model6QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces19.qc, family="binomial")
summary(con_model6ROC)
summary(con_model6QC)

#Bloc
bloc_model6QC<-glm(bloc~+working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, data=ces19.qc, family="binomial")
summary(bloc_model6QC)

#Model 7 - Leadership
#NDP
ndp_model7ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19, data=ces19.roc, family="binomial")
ndp_model7QC<-glm(ndp~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19+Francois_Blanchet19, data=ces19.qc, family="binomial")
summary(ndp_model7ROC)
summary(ndp_model7QC)

#Liberal
lib_model7ROC<-glm(liberal~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19, data=ces19.roc, family="binomial")
lib_model7QC<-glm(liberal~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19+Francois_Blanchet19, data=ces19.qc, family="binomial")
summary(lib_model7ROC)
summary(lib_model7QC)

#Conservative
con_model7ROC<-glm(conservative~region3+working_class+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id+personal_retrospective+immigration_rate+national_retrospective+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19, data=ces19.roc, family="binomial")
con_model7QC<-glm(conservative~working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19+Francois_Blanchet19, data=ces19.qc, family="binomial")
summary(con_model7ROC)
summary(con_model7QC)

#Bloc
bloc_model7QC<-glm(bloc~+working_class+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19+Francois_Blanchet19, data=ces19.qc, family="binomial")
summary(bloc_model7QC)

#Model 8 - Block1: using working_class2 variable instead
#NDP
ndp_model8ROC<-glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces19.roc, family="binomial")
ndp_model8QC<-glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign, data=ces19.qc, family="binomial")
summary(ndp_model8ROC)
summary(ndp_model8QC)

#Liberal
lib_model8ROC<-glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces19.roc, family="binomial")
lib_model8QC<-glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign, data=ces19.qc, family="binomial")
summary(lib_model8ROC)
summary(lib_model8QC)

#Conservative
con_model8ROC<-glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, data=ces19.roc, family="binomial")
con_model8QC<-glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign, data=ces19.qc, family="binomial")
summary(con_model8ROC)
summary(con_model8QC)

#Bloc
bloc_model8QC<-glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign, data=ces19.qc, family="binomial")
summary(bloc_model8QC)

#Model 9 - Block6: using working_class2 variable instead
#NDP
ndp_model9ROC<-glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19, data=ces19.roc, family="binomial")
ndp_model9QC<-glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19+Francois_Blanchet19, data=ces19.qc, family="binomial")
summary(ndp_model9ROC)
summary(ndp_model9QC)

#Liberal
lib_model9ROC<-glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19, data=ces19.roc, family="binomial")
lib_model9QC<-glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+liberal_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19+Francois_Blanchet19, data=ces19.qc, family="binomial")
summary(lib_model9ROC)
summary(lib_model9QC)

#Conservative
con_model9ROC<-glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+conservative_id+personal_retrospective+immigration_rate+national_retrospective+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19, data=ces19.roc, family="binomial")
con_model9QC<-glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19+Francois_Blanchet19, data=ces19.qc, family="binomial")
summary(con_model9ROC)
summary(con_model9QC)

#Bloc
bloc_model9QC<-glm(bloc~+working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+Justin_Trudeau19+Andrew_Scheer19+Jagmeet_Singh19+Francois_Blanchet19, data=ces19.qc, family="binomial")
summary(bloc_model9QC)

#Model comparison
stargazer(ndp_model2ROC, lib_model2ROC, con_model2ROC, ndp_model2QC, lib_model2QC, con_model2QC, bloc_model2QC ,type="html", out=here("Tables", "ces19_block1.html"))
stargazer(ndp_model3ROC, lib_model3ROC, con_model3ROC, ndp_model3QC, lib_model3QC, con_model3QC, bloc_model3QC ,type="html", out=here("Tables", "ces19_block2.html"))
stargazer(ndp_model4ROC, lib_model4ROC, con_model4ROC, ndp_model4QC, lib_model4QC, con_model4QC, bloc_model4QC ,type="html", out=here("Tables", "ces19_block3.html"))
stargazer(ndp_model5ROC, lib_model5ROC, con_model5ROC, ndp_model5QC, lib_model5QC, con_model5QC, bloc_model5QC ,type="html", out=here("Tables", "ces19_block4.html"))
stargazer(ndp_model6ROC, lib_model6ROC, con_model6ROC, ndp_model6QC, lib_model6QC, con_model6QC, bloc_model6QC ,type="html", out=here("Tables", "ces19_block5.html"))
stargazer(ndp_model7ROC, lib_model7ROC, con_model7ROC, ndp_model7QC, lib_model7QC, con_model7QC, bloc_model7QC ,type="html", out=here("Tables", "ces19_block6.html"))
stargazer(ndp_model8ROC, lib_model8ROC, con_model8ROC, ndp_model8QC, lib_model8QC, con_model8QC, bloc_model8QC ,type="html", out=here("Tables", "ces19_block1_working_class2.html"))
stargazer(ndp_model9ROC, lib_model9ROC, con_model9ROC, ndp_model9QC, lib_model9QC, con_model9QC, bloc_model9QC ,type="html", out=here("Tables", "ces19_block6_working_class2.html"))

#Model 0 - low income sub-sample
#NDP
ndp_model0ROC<-glm(ndp~region3+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+income1+moral_traditionalism+redistribution, data=ces19.roc, family="binomial")
ndp_model0QC<-glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+income1+moral_traditionalism+redistribution, data=ces19.qc, family="binomial")
summary(ndp_model0ROC)
summary(ndp_model0QC)

#Liberal
lib_model0ROC<-glm(liberal~region3+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+income1+moral_traditionalism+redistribution, data=ces19.roc, family="binomial")
lib_model0QC<-glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+income1+moral_traditionalism+redistribution, data=ces19.qc, family="binomial")
summary(lib_model0ROC)
summary(lib_model0QC)

#Conservative
con_model0ROC<-glm(conservative~region3+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+income+moral_traditionalism+redistribution, data=ces19.roc, family="binomial")
con_model0QC<-glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+income+moral_traditionalism+redistribution, data=ces19.qc, family="binomial")
summary(con_model0ROC)
summary(con_model0QC)

#Model 10 - moral traditionalism and redistribution added
#NDP
ndp_model10ROC<-glm(ndp~region3+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+low_income+high_income+moral_traditionalism+redistribution, data=ces19.roc, family="binomial")
ndp_model10QC<-glm(ndp~union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+low_income+high_income+moral_traditionalism+redistribution, data=ces19.qc, family="binomial")
summary(ndp_model10ROC)
summary(ndp_model10QC)

#Liberal
lib_model10ROC<-glm(liberal~region3+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+low_income+high_income+moral_traditionalism+redistribution, data=ces19.roc, family="binomial")
lib_model10QC<-glm(liberal~union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+low_income+high_income+moral_traditionalism+redistribution, data=ces19.qc, family="binomial")
summary(lib_model10ROC)
summary(lib_model10QC)

#Conservative
con_model10ROC<-glm(conservative~region3+union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+low_income+high_income+moral_traditionalism+redistribution, data=ces19.roc, family="binomial")
con_model10QC<-glm(conservative~union_both+young+old+male+sector+catholic+no_religion+degree+language+foreign+low_income+high_income+moral_traditionalism+redistribution, data=ces19.qc, family="binomial")
summary(con_model10ROC)
summary(con_model10QC)

stargazer(ndp_model10ROC, lib_model10ROC, con_model10ROC, ndp_model10QC, lib_model10QC, con_model10QC, type="html", out=here("Tables", "ces19_block10.html"))
