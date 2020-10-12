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
#Can we just turn working_class2 into a dichotomous variable here and make the elses 0? We do that ultimately with working_class3. Why not simplify the code?
ces19phone$working_class2<-Recode(ces19phone$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
table(ces19phone$working_class2)

#Turn region into factor with East as reference case
ces19phone$region3<-Recode(as.factor(ces19phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces19phone$region3)
table(ces19phone$region3)

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

#Turn income into factor with Middle as reference
#ces19phone$income3<-Recode(as.factor(ces19phone$income), "1='Low_Income' ; 2:4='Middle_Income' ; 5='High_Income'", levels=c('Low_Income', 'Middle_Income', 'High_Income'))
#levels(ces19phone$income3)
#table(ces19phone$income3)

#Other dummies
ces19phone$low_income<-Recode(ces19phone$income, "2:5=0; 1=1")
table(ces19phone$low_income)
ces19phone$high_income<-Recode(ces19phone$income, "1:4=0; 5=1")
table(ces19phone$high_income)
ces19phone$no_religion<-Recode(ces19phone$religion, "0=1; 1:3=0; NA=NA")
table(ces19phone$no_religion)
ces19phone$catholic<-Recode(ces19phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
table(ces19phone$catholic)
ces19phone$young<-Recode(ces19phone$age, "35:100=0; 18:34=1")
table(ces19phone$young)
ces19phone$old<-Recode(ces19phone$age, "55:100=1; 18:54=0")
table(ces19phone$old)
ces19phone$foreign<-Recode(ces19phone$native, "1=0; 0=1")
table(ces19phone$foreign)
ces19phone$working_class3<-Recode(ces19phone$working_class2, "1=1; else=0")
table(ces19phone$working_class3)
table(ces19phone$working_class2)
# Party Id
ces19phone$liberal_id<-Recode(ces19phone$party_id, "1=1; 0=0; 2:4=0; else=NA")
ces19phone$conservative_id<-Recode(ces19phone$party_id, "2=1; 0:1=0; 3:4=0; else=NA")
ces19phone$ndp_id<-Recode(ces19phone$party_id, "3=1; 0:2=0; 4=0; else=NA")
ces19phone$bloc_id<-Recode(ces19phone$party_id, "4=1; 0:3=0; else=NA")
#ces19phone$liberal_id<-Recode(ces19phone$party_id, "1=1; else=0")
##ces19phone$conservative_id<-Recode(ces19phone$party_id, "2=1; else=0")
#ces19phone$ndp_id<-Recode(ces19phone$party_id, "3=1; else=0")
#ces19phone$bloc_id<-Recode(ces19phone$party_id, "4=1; else=0")
table(ces19phone$liberal_id)
table(ces19phone$conservative_id)
table(ces19phone$ndp_id)
table(ces19phone$bloc_id)

# Party vote
ces19phone$liberal<-Recode(ces19phone$vote, "1=1; 0=0; 2:5=0; else=NA")
ces19phone$conservative<-Recode(ces19phone$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces19phone$ndp<-Recode(ces19phone$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces19phone$bloc<-Recode(ces19phone$vote, "4=1; 0:3=0; 5=0; else=NA")
ces19phone$green<-Recode(ces19phone$vote, "5=1; 0:4=0; else=NA")
table(ces19phone$liberal)
table(ces19phone$conservative)
table(ces19phone$ndp)
table(ces19phone$bloc)
table(ces19phone$green)
ces19phone$ndp_vs_right<-Recode(ces19phone$vote, "3=1; 2=0; else=NA")
ces19phone$ndp_vs_liberal<-Recode(ces19phone$vote, "3=1; 1=0; else=NA")
table(ces19phone$ndp_vs_right)
table(ces19phone$ndp_vs_liberal)

#Code working class missing as 0
ces19phone$working_class4<-Recode(ces19phone$working_class, "1=1; else=0")
table(ces19phone$working_class4)

#### 2019 Models ####
#Split QC out into ces19.qc
ces19phone %>% 
  filter(quebec==1)->ces19.qc
ces19phone %>% 
  filter(quebec!=1)->ces19.roc

#Model basic with controls
modelCAN<-glm(ndp~region3+working_class+union_both+age+male+sector, data=ces19phone, family="binomial")
modelROC<-glm(ndp~region3+working_class+union_both+age+male+sector, data=ces19.roc, family="binomial")
modelQC<-glm(ndp~working_class+union_both+age+male+sector, data=ces19.qc, family="binomial")
summary(modelCAN)
summary(modelROC)
summary(modelQC)

#M1 with leadership (Jagmeet_Singh)
model1ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Jagmeet_Singh, data=ces19.roc, family="binomial")
model1QC<-glm(ndp~working_class+union_both+age+male+sector+Jagmeet_Singh, data=ces19.qc, family="binomial")
summary(model1ROC)
summary(model1QC)

#M2 with redistribution
model2ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution, data=ces19.roc, family="binomial")
model2QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution, data=ces19.qc, family="binomial")
summary(model2ROC)
summary(model2QC)

#M3 with environment
model3ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment, data=ces19.roc, family="binomial")
model3QC<-glm(ndp~working_class+union_both+age+male+sector+environment, data=ces19.qc, family="binomial")
summary(model3ROC)
summary(model3QC)

#M4 with immigration
model4ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration, data=ces19.roc, family="binomial")
model4QC<-glm(ndp~working_class+union_both+age+male+sector+immigration, data=ces19.qc, family="binomial")
summary(model4ROC)
summary(model4QC)

#M17 with minority
model17ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_help, data=ces19.roc, family="binomial")

model17QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_help, data=ces19.qc, family="binomial")
summary(model17ROC)
summary(model17QC)

#M18 with immigration2
model18ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration2, data=ces19.roc, family="binomial")
model18QC<-glm(ndp~working_class+union_both+age+male+sector+immigration2, data=ces19.qc, family="binomial")
summary(model18ROC)
summary(model18QC)

#M19 with immigration economy
model19ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration_economy, data=ces19.roc, family="binomial") 
model19QC<-glm(ndp~working_class+union_both+age+male+sector+immigration_economy, data=ces19.qc, family="binomial")
summary(model19ROC)
summary(model19QC)

#M20 with immigration culture
model20ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration_culture, data=ces19.roc, family="binomial")

model20QC<-glm(ndp~working_class+union_both+age+male+sector+immigration_culture, data=ces19.qc, family="binomial")
summary(model20ROC)
summary(model20QC)

#M21 with immigration crime
model21ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration_crime, data=ces19.roc, family="binomial")

model21QC<-glm(ndp~working_class+union_both+age+male+sector+immigration_crime, data=ces19.qc, family="binomial")
summary(model21ROC)
summary(model21QC)

#M22 with immigration rate
model22ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration_rate, data=ces19.roc, family="binomial")

model22QC<-glm(ndp~working_class+union_both+age+male+sector+immigration_rate, data=ces19.qc, family="binomial")
summary(model22ROC)
summary(model22QC)

#M23 basic model with degree
model23ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+degree, data=ces19.roc, family="binomial")

model23QC<-glm(ndp~working_class+union_both+age+male+sector+degree, data=ces19.qc, family="binomial")
summary(model23ROC)
summary(model23QC)

#M24 basic model with income
model24ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+income, data=ces19.roc, family="binomial")

model24QC<-glm(ndp~working_class+union_both+age+male+sector+income, data=ces19.qc, family="binomial")
summary(model24ROC)
summary(model24QC)

#M25 basic model with community size
model25ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+size, data=ces19.roc, family="binomial")

model25QC<-glm(ndp~working_class+union_both+age+male+sector+size, data=ces19.qc, family="binomial")
summary(model25ROC)
summary(model25QC)

#M26 basic model with foreign-born status
model26ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+foreign, data=ces19.roc, family="binomial")
model26QC<-glm(ndp~working_class+union_both+age+male+sector+foreign, data=ces19.qc, family="binomial")
summary(model26ROC)
summary(model26QC)

#M27 basic model with Age2 (coded as 0-1)
model27ROC<-glm(ndp~region3+working_class+union_both+age2+male+sector, data=ces19.roc, family="binomial")
model27QC<-glm(ndp~working_class+union_both+age2+male+sector, data=ces19.qc, family="binomial")
summary(model27ROC)
summary(model27QC)

#M28 basic model with no_relgiion
model28ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+no_religion, data=ces19.roc, family="binomial")

model28QC<-glm(ndp~working_class+union_both+age+male+sector+no_religion, data=ces19.qc, family="binomial")
summary(model28ROC)
summary(model28QC)

#M29 Full controls model
model29ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+degree+income+size+foreign+no_religion, data=ces19.roc, family="binomial")

model29QC<-glm(ndp~working_class+union_both+age+male+sector+degree+income+size+foreign+no_religion, data=ces19.qc, family="binomial")
summary(model29ROC)
summary(model29QC)

#M30 basic model with left-right ideology
model30ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+ideology, data=ces19.roc, family="binomial")

model30QC<-glm(ndp~working_class+union_both+age+male+sector+ideology, data=ces19.qc, family="binomial")
summary(model30ROC)
summary(model30QC)

#M31 Income as factor
model31ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+degree+as.factor(income)+foreign+no_religion, data=ces19.roc, family="binomial")

model31QC<-glm(ndp~working_class+union_both+age+male+sector+degree+as.factor(income)+foreign+no_religion, data=ces19.qc, family="binomial")
summary(model31ROC)
summary(model31QC)

#M32 Young/Old instead of Age
model32ROC<-glm(ndp~region3+working_class+union_both+young+old+male+sector+degree+income+foreign+no_religion, data=ces19.roc, family="binomial")

model32QC<-glm(ndp~working_class+union_both+young+old+male+sector+degree+income+foreign+no_religion, data=ces19.qc, family="binomial")
summary(model32ROC)
summary(model32QC)

#M33 Demographic and attitudinal model
model33ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+degree+income+no_religion+immigration2+environment+redistribution+Justin_Trudeau+Andrew_Scheer+Jagmeet_Singh, data=ces19.roc, family="binomial")
model33QC<-glm(ndp~working_class+union_both+age+male+sector+degree+income+no_religion+immigration2+environment+redistribution+Justin_Trudeau+Andrew_Scheer+Jagmeet_Singh, data=ces19.qc, family="binomial")
summary(model33ROC)
summary(model33QC)

#M34-M36 Basic class models
#M34 NDP vs Right
model34ROC<-glm(ndp_vs_right~working_class3+union_both+income+degree+sector, data=ces19.roc, family="binomial")
model34QC<-glm(ndp_vs_right~working_class3+union_both+income+degree+sector, data=ces19.qc, family="binomial")
summary(model34ROC)
summary(model34QC)

#M35 NDP vs Liberal
model35ROC<-glm(ndp_vs_liberal~working_class3+union_both+income+degree+sector, data=ces19.roc, family="binomial")
model35QC<-glm(ndp_vs_liberal~working_class3+union_both+income+degree+sector, data=ces19.qc, family="binomial")
summary(model35ROC)
summary(model35QC)

#M36 Basic Class Model with self-employed carved out
model36ALL<-glm(ndp~working_class3+union_both+income+degree+sector, data=ces19phone, family="binomial")
model36ROC<-glm(ndp~working_class3+union_both+income+degree+sector, data=ces19.roc, family="binomial")
model36QC<-glm(ndp~working_class3+union_both+income+degree+sector, data=ces19.qc, family="binomial")
summary(model36ALL)
summary(model36ROC)
summary(model36QC)

#### M37 Basic Class Model with self-employed lumped in ####
# #note: if we want to lump the self-employed with the working class we need to change this to working_class4
# model37ALL<-glm(ndp~working_class4+union_both+income+degree+sector, data=ces19phone, family="binomial")
# model37ROC<-glm(ndp~working_class4+union_both+income+degree+sector, data=ces19.roc, family="binomial")
# model37QC<-glm(ndp~working_class4+union_both+income+degree+sector, data=ces19.qc, family="binomial")
# summary(model37ALL)
# summary(model37ROC)
summary(model37QC)

#M38 Basic Class Model with self-employed careved out into 0 (including age, gender, region)
model38ALL<-glm(ndp~working_class3+union_both+income+degree+sector+age+male+as.factor(region4), data=ces19phone, family="binomial")
model38ROC<-glm(ndp~working_class3+union_both+income+degree+sector+age+male+region3, data=ces19.roc, family="binomial")
model38QC<-glm(ndp~working_class3+union_both+income+degree+sector+age+male, data=ces19.qc, family="binomial")
summary(model38ALL)
summary(model38ROC)
summary(model38QC)

stargazer(model34ROC, model34QC, type="html", out=here("Tables", "ndp_vs_right_models.html"))
stargazer(model35ROC, model35QC, type="html", out=here("Tables", "ndp_vs_liberal_models.html"))
stargazer(model36ALL, model36ROC, model36QC, model38ALL, model38ROC, model38QC, type="html", out=here("Tables", "basic_class_models1.html"), column.labels = rep(c("CAN", "ROC", "QC"),2))
#stargazer(model37ALL, model37ROC, model37QC, model38ALL, model38ROC, model38QC, column.labels=c("ALL", "ROC", "QC", "ALL", "ROC", "QC"), type="html", out=here("Tables", "basic_class_models2.html")) 

#Combine models into one table by region
#stargazer(modelROC, model23ROC, model24ROC, model1ROC, model2ROC, model3ROC, model4ROC, model17ROC, model18ROC, type="html", out=here("Tables", "ROC_ces19_attitudinal_variables.html"))
#stargazer(modelQC, model23QC, model24QC, model1QC, model2QC, model3QC, model4QC, model17QC, model18QC, type="html", out=here("Tables", "QC_ces19_attitudinal_variables.html"))
stargazer(modelROC, modelQC, model23ROC, model23QC, model24ROC, model24QC, model1ROC, model1QC, model2ROC, model2QC, model3ROC, model3QC, model4ROC, model4QC, model17ROC, model17QC, model18ROC, model18QC, type="html", out=here("Tables", "ces19_attitudinal_variables.html"))

#Combine immigration/racial minority into one table by region 
#stargazer(modelROC, model4ROC, model17ROC, model18ROC, model19ROC, model20ROC, model21ROC, model22ROC, type="html", out=here("Tables", "ROC_ces19_immigration_variables.html"))
#stargazer(modelQC, model4QC, model17QC, model18QC, model19QC, model20QC, model21QC, model22QC, type="html", out=here("Tables", "QC_ces19_immigration_variables.html"))
stargazer(modelROC, modelQC, model4ROC, model4QC, model17ROC, model17QC, model18ROC, model18QC, model19ROC, model19QC, model20ROC, model20QC, model21ROC, model21QC, model22ROC, model22QC, type="html", out=here("Tables", "ces19_immigration_variables.html"))

#Extra variable comparisons
stargazer(modelROC, modelQC, model25ROC, model25QC, model26ROC, model26QC, model27ROC, model27QC, model28ROC, model28QC, model29ROC, model29QC,type="html", out=here("Tables", "ces19_different_controls.html"))

#Combine best 2019/2015 model
stargazer(model33ROC, model33QC, type="html", out=here("Tables", "ces15_19_3_party.html"))

#### Leadership interactions ####
#M5 leadership:working class interaction
model5ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Jagmeet_Singh+working_class:Jagmeet_Singh, data=ces19.roc, family="binomial")
model5QC<-glm(ndp~working_class+union_both+age+male+sector+Jagmeet_Singh+working_class:Jagmeet_Singh, data=ces19.qc, family="binomial")
summary(model5ROC)
summary(model5QC)

#M6 leadership:union interaction
model6ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Jagmeet_Singh+union_both:Jagmeet_Singh, data=ces19.roc, family="binomial")
model6QC<-glm(ndp~working_class+union_both+age+male+sector+Jagmeet_Singh+union_both:Jagmeet_Singh, data=ces19.qc, family="binomial")
summary(model6ROC)
summary(model6QC)

#### Redistribution interactions ####
#M7 redistribution:working class interaction
model7ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution+working_class:redistribution, data=ces19.roc, family="binomial")
model7QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution+working_class:redistribution, data=ces19.qc, family="binomial")
summary(model7ROC)
summary(model7QC)

#M8 redistribution:union interaction
model8ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution+union_both:redistribution, data=ces19.roc, family="binomial")
model8QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution+union_both:redistribution, data=ces19.qc, family="binomial")
summary(model8ROC)
summary(model8QC)

#### Environment interactions ####
#M9 environment:working class interaction
model9ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment+working_class:environment, data=ces19.roc, family="binomial")
model9QC<-glm(ndp~working_class+union_both+age+male+sector+environment+working_class:environment, data=ces19.qc, family="binomial")
summary(model9ROC)
summary(model9QC)

#M10 environment:union interaction
model10ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment+union_both:environment, data=ces19.roc, family="binomial")
model10QC<-glm(ndp~working_class+union_both+age+male+sector+environment+union_both:environment, data=ces19.qc, family="binomial")
summary(model10ROC)
summary(model10QC)

#### Immigration interactions ####
#M11 immigration:working class interaction
model11ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration+working_class:immigration, data=ces19.roc, family="binomial")
model11QC<-glm(ndp~working_class+union_both+age+male+sector+immigration+working_class:immigration, data=ces19.qc, family="binomial")
summary(model11ROC)
summary(model11QC)

#M12 immigration:union interaction
model12ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration+union_both:immigration, data=ces19.roc, family="binomial")
model12QC<-glm(ndp~working_class+union_both+age+male+sector+immigration+union_both:immigration, data=ces19.qc, family="binomial")
summary(model12ROC)
summary(model12QC)

#M13 minority:working class interaction
model13ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_help+working_class:minorities_help, data=ces19.roc, family="binomial")
model13QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_help+working_class:minorities_help, data=ces19.qc, family="binomial")
summary(model13ROC)
summary(model13QC)

#M14 minority:union interaction
model14ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_help+union_both:minorities_help, data=ces19.roc, family="binomial")
model14QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_help+union_both:minorities_help, data=ces19.qc, family="binomial")
summary(model14ROC)
summary(model14QC)

#M15 immigration2:working class interaction
model15ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration2+working_class:immigration2, data=ces19.roc, family="binomial")
model15QC<-glm(ndp~working_class+union_both+age+male+sector+immigration2+working_class:immigration2, data=ces19.qc, family="binomial")
summary(model15ROC)
summary(model15QC)

#M16 immigration2:union interaction
model16ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration2+union_both:immigration2, data=ces19.roc, family="binomial")
model16QC<-glm(ndp~working_class+union_both+age+male+sector+immigration2+union_both:immigration2, data=ces19.qc, family="binomial")
summary(model16ROC)
summary(model16QC)

#Combine interaction models into one table
#stargazer(modelROC, model5ROC, model7ROC, model9ROC, model11ROC, model13ROC, model15ROC, type="html", out=here("Tables", "ROC_ces19_attitudinal_workingclass_interactions.html"))
#stargazer(modelQC, model5QC, model7QC, model9QC, model11QC, model13QC, model15QC, type="html", out=here("Tables", "QC_ces19_attitudinal_workingclass_interactions.html"))
#stargazer(modelROC, model6ROC, model8ROC, model10ROC, model12ROC, model14ROC, model16ROC, type="html", out=here("Tables", "ROC_ces19_attitudinal_union_interactions.html"))
#stargazer(modelQC, model6QC, model8QC, model10QC, model12QC, model14QC, model16QC, type="html", out=here("Tables", "QC_ces19_attitudinal_union_interactions.html"))
stargazer(modelROC, modelQC, model5ROC, model5QC, model7ROC, model7QC, model9ROC, model9QC, model11ROC, model11QC, model13ROC, model13QC, model15ROC, model15QC, type="html", out=here("Tables", "ces19_working_class_interactions.html"))
stargazer(modelROC, modelQC, model6ROC, model6QC, model8ROC, model8QC, model10ROC, model10QC, model12ROC, model12QC, model14ROC, model14QC, model16ROC, model16QC, type="html", out=here("Tables", "ces19_union_interactions.html"))

#--------------------------------------------------------------------------------------------------------

#### Summarizing ####

ces19phone$immigration

#### Attitudes by Class ####

ces19phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
  #  filter(!is.na(occupation4)) %>%
  select(occupation4, Jagmeet_Singh, immigration, redistribution, environment, ideology) %>% 
  group_by(occupation4) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution, environment, ideology), mean, na.rm=T)
library(knitr)
library(kableExtra)

ces19phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
  #  filter(!is.na(occupation4)) %>%
  select(occupation4, Jagmeet_Singh, immigration, redistribution, environment, minorities_help) %>% 
  group_by(occupation4) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution, environment, minorities_help), mean, na.rm=T) %>% 
  #Convert this to a data frame for printing
  as.data.frame() %>% 
  #summary=F tells stargazer to print the raw data, not summary statistics, digits=2 tells it to round to 2 digits
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Class attitudes 2019.html"))

## This is maybe useful, but ideally, I find it more useful to always graph this stuff. 
ces19phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
  #  filter(!is.na(occupation4)) %>%
  #Select the variables for graphing
  select(occupation4, Jagmeet_Singh, Justin_Trudeau, Andrew_Scheer, immigration, redistribution, environment) %>%
  #they are currently in a wide format, reduce it to long format with pivot_longer
  pivot_longer(-occupation4,values_to=c("Score"), names_to=c("Variable")) %>% 
  #Now form groups for each class category and each variable
  group_by(occupation4, Variable) %>% 
  #Now summarize those groups, creating the average score, the count of cases n(), the standard deviation, the standard error
  summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sqrt(sd)/n) %>% 
  ggplot(., aes(x=Variable, y=Average, col=occupation4))+geom_jitter()

#By Union status
ces19phone %>%
  #  filter(!is.na(union_both)) %>%
  select(union_both, Jagmeet_Singh, immigration, redistribution, environment, minorities_help) %>% 
  group_by(union_both) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution, environment, minorities_help), mean, na.rm=T)

ces19phone %>%
  #  filter(!is.na(union_both)) %>%
  select(union_both, Jagmeet_Singh, immigration, redistribution, environment, minorities_help) %>% 
  group_by(union_both) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution, environment, minorities_help), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Union attitudes ROC 2019.html"))

# By Regions
ces19phone %>%
  select(quebec, Jagmeet_Singh, immigration, redistribution, environment, minorities_help) %>% 
  group_by(quebec) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution, environment, minorities_help), mean, na.rm=T)

ces19phone %>%
  select(region, Jagmeet_Singh, immigration, redistribution, environment, minorities_help) %>% 
  group_by(region) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution, environment, minorities_help), mean, na.rm=T)

# By Income
ces19phone %>%
  select(income, Jagmeet_Singh, immigration, redistribution, environment, minorities_help) %>% 
  group_by(income) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution, environment, minorities_help), mean, na.rm=T)

# By Degree
ces19phone %>%
  select(degree, Jagmeet_Singh, immigration, redistribution, environment, minorities_help) %>% 
  group_by(degree) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution, environment, minorities_help), mean, na.rm=T)

# By Gender
ces19phone %>%
  select(male, Jagmeet_Singh, immigration, redistribution, environment, minorities_help) %>% 
  group_by(male) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution, environment, minorities_help), mean, na.rm=T)

## This is your code and it's great; I just showed some ways to aggregate it and present more information
ces19phone %>%
  filter(!is.na(occupation4)) %>%
  group_by(occupation4) %>%
  summarize(mean_redistribution = mean(redistribution, na.rm=T))
# 
# ces19phone %>%
#   filter(!is.na(redistribution)) %>%
#   group_by(occupation2) %>%
#   summarize(mean_redistribution = mean(redistribution))

# Block recursion variables
# By Class
ces19phone %>%
  #  filter(!is.na(union_both)) %>%
  select(occupation4, market_liberalism, moral_traditionalism, political_disaffection, continentalism, personal_retrospective, national_retrospective, defence, justice, education) %>% 
  group_by(occupation4) %>%
  summarise_at(vars(market_liberalism, moral_traditionalism, political_disaffection, continentalism, personal_retrospective, national_retrospective, defence, justice, education), mean, na.rm=T)

#Graph it
ces19phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
  #  filter(!is.na(occupation4)) %>%
  #Select the variables for graphing
  select(occupation4, market_liberalism, moral_traditionalism, political_disaffection, continentalism, personal_retrospective, national_retrospective, defence, justice, education) %>%
  #they are currently in a wide format, reduce it to long format with pivot_longer
  pivot_longer(-occupation4,values_to=c("Score"), names_to=c("Variable")) %>% 
  #Now form groups for each class category and each variable
  group_by(occupation4, Variable) %>% 
  #Now summarize those groups, creating the average score, the count of cases n(), the standard deviation, the standard error
  summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sqrt(sd)/n) %>% 
  ggplot(., aes(x=Variable, y=Average, col=occupation4))+geom_jitter()

#By Income
ces19phone %>%
  #  filter(!is.na(union_both)) %>%
  select(income, market_liberalism, moral_traditionalism, political_disaffection, continentalism, personal_retrospective, national_retrospective, defence, justice, education) %>% 
  group_by(income) %>%
  summarise_at(vars(market_liberalism, moral_traditionalism, political_disaffection, continentalism, personal_retrospective, national_retrospective, defence, justice, education), mean, na.rm=T)

#By Union
ces19phone %>%
  #  filter(!is.na(union_both)) %>%
  select(union_both, market_liberalism, moral_traditionalism, political_disaffection, continentalism, personal_retrospective, national_retrospective, defence, justice, education) %>% 
  group_by(union_both) %>%
  summarise_at(vars(market_liberalism, moral_traditionalism, political_disaffection, continentalism, personal_retrospective, national_retrospective, defence, justice, education), mean, na.rm=T)

#By Degree
ces19phone %>%
  #  filter(!is.na(union_both)) %>%
  select(degree, market_liberalism, moral_traditionalism, political_disaffection, continentalism, personal_retrospective, national_retrospective, defence, justice, education) %>% 
  group_by(degree) %>%
  summarise_at(vars(market_liberalism, moral_traditionalism, political_disaffection, continentalism, personal_retrospective, national_retrospective, defence, justice, education), mean, na.rm=T)

#Redistribution by Class and Union
ces19phone %>%
  filter(!is.na(redistribution)) %>%
  group_by(occupation4, union_both) %>%
  summarize(mean_redistribution = mean(redistribution))

ces19phone %>%
  filter(!is.na(redistribution)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_redistribution = mean(redistribution))

#Immigration by Class
ces19phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation4) %>%
  summarize(mean_immigration = mean(immigration))

ces19phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation2) %>%
  summarize(mean_immigration = mean(immigration))

#Immigration by Class and Union
ces19phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation4, union_both) %>%
  summarize(mean_immigration = mean(immigration))

ces19phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_immigration = mean(immigration))

####Leadership by Class
ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation4) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation2) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

#Leadership by Class and Union
ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation4, union_both) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

# Leadership by Class and Quebec
ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation4, quebec) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation2, quebec) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

#--------------------------------------------------------------------------------------------------------

#### Past NDP Vote ####

ces19phone$ndp_past<-Recode(ces19phone$past_vote, "3=1; 0:2=0; 4:5=0; else=NA")
table(ces19phone$ndp_past)

#Past Model basic with controls
past_modelROC<-glm(ndp_past~region3+working_class+union_both+age+male+sector, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces19.qc
past_modelQC<-glm(ndp_past~working_class+union_both+age+male+sector, data=ces19.qc, family="binomial")
summary(past_modelROC)
summary(past_modelQC)

#PM1 including past vote
past_model1ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+ndp_past, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces19.qc
past_model1QC<-glm(ndp~working_class+union_both+age+male+sector+ndp_past, data=ces19.qc, family="binomial")
summary(past_model1ROC)
summary(past_model1QC)

#PM2 including past vote interaction with class
past_model2ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+ndp_past+ndp_past:working_class, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces19.qc
past_model2QC<-glm(ndp~working_class+union_both+age+male+sector+ndp_past+ndp_past:working_class, data=ces19.qc, family="binomial")
summary(past_model2ROC)
summary(past_model2QC)

#PM3 including past vote interaction with union
past_model3ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+ndp_past+ndp_past:union_both, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces19.qc
past_model3QC<-glm(ndp~working_class+union_both+age+male+sector+ndp_past+ndp_past:union_both, data=ces19.qc, family="binomial")
summary(past_model3ROC)
summary(past_model3QC)

#PM4 including past vote class interaction with income & degree
past_model4ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+ndp_past+ndp_past:working_class+income+degree, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces19.qc
past_model4QC<-glm(ndp~working_class+union_both+age+male+sector+ndp_past+ndp_past:working_class+income+degree, data=ces19.qc, family="binomial")
summary(past_model4ROC)
summary(past_model4QC)

#PM5 including past vote union interaction with income & degree
past_model5ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+ndp_past+ndp_past:union_both+income+degree, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces19.qc
past_model5QC<-glm(ndp~working_class+union_both+age+male+sector+ndp_past+ndp_past:union_both+income+degree, data=ces19.qc, family="binomial")
summary(past_model5ROC)
summary(past_model5QC)

#Combine models into one table
#stargazer(past_model1ROC, past_model2ROC, past_model3ROC, past_model4ROC, past_model5ROC, type="html", out=here("Tables", "Past_NDP_Vote_ROC_ces19.html"))
#stargazer(past_model1QC, past_model2QC, past_model3QC, past_model4QC, past_model5QC, type="html", out=here("Tables", "Past_Vote_QC_ces19.html"))
stargazer(past_model1ROC, past_model1QC, past_model2ROC, past_model2QC, past_model3ROC, past_model3QC, past_model4ROC, past_model4QC, past_model5ROC, past_model5QC, type="html", out=here("Tables", "Past_NDP_Vote_ces19.html"))


#Past NDP by Occupation
ces19phone %>%
  filter(!is.na(occupation4)) %>%
  group_by(occupation4, ndp_past) %>%
  summarize(mean_ndp = mean(ndp, na.rm=T))

#Past NDP Vote by 2019 Vote
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(vote) %>%
  summarize(mean_ndp_past = mean(ndp_past))

#Past NDP Vote by 2019 Vote and class
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(vote, working_class) %>%
  summarize(mean_ndp_past = mean(ndp_past))

#Past NDP Vote by 2019 Vote and class
ces19phone %>%
  filter(!is.na(working_class)) %>%
  group_by(vote, ndp_past) %>%
  summarize(mean_working_class = mean(working_class))

# Past NDP vote - voted for which parties in 2019
ces19phone %>%
  select(ndp_past, liberal, conservative, ndp, bloc, green) %>% 
  group_by(ndp_past) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T)

#How many past NDP working class voters voted NDP in 2019 (47.1%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, working_class) %>%
  summarize(mean_ndp = mean(ndp, na.rm=T))

#How many past NDP working class voters voted Liberal in 2019 (23.5%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, working_class) %>%
  summarize(mean_ndp = mean(liberal, na.rm=T))

#How many past NDP working class voters voted Conservative in 2019 (17.6%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, working_class) %>%
  summarize(mean_ndp = mean(conservative, na.rm=T))

#How many past NDP working class voters voted Bloc in 2019 (5.9%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, working_class) %>%
  summarize(mean_ndp = mean(bloc, na.rm=T))

#How many past NDP working class voters voted Green in 2019 (5.9%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, working_class) %>%
  summarize(mean_ndp = mean(green, na.rm=T))

#How many past NDP union voters voted NDP in 2019 (53.3%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, union_both) %>%
  summarize(mean_ndp = mean(ndp, na.rm=T))

#How many past NDP union voters voted Liberal in 2019 (14%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, union_both) %>%
  summarize(mean_ndp = mean(liberal, na.rm=T))

#How many past NDP union voters voted Conservative in 2019 (9.35%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, union_both) %>%
  summarize(mean_ndp = mean(conservative, na.rm=T))

#How many past NDP union voters voted Bloc in 2019 (12.1%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, union_both) %>%
  summarize(mean_ndp = mean(bloc, na.rm=T))

#How many past NDP union voters voted Green in 2019 (12.1%)
ces19phone %>%
  filter(!is.na(ndp_past)) %>%
  group_by(ndp_past, union_both) %>%
  summarize(mean_ndp = mean(green, na.rm=T))

# Past NDP - to which parties in 2019
ces19phone %>%
  select(ndp_past, liberal, conservative, ndp, bloc, green) %>% 
  group_by(ndp_past) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Past NDP Vote 2019.html"))

ces19phone %>%
  select(ndp_past, working_class, liberal, conservative, ndp, bloc, green) %>% 
  group_by(ndp_past, working_class) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Past NDP Working Class Vote 2019.html"))

#Working Class voting by pro-redistribution
ces19phone %>%
  select(working_class, pro_redistribution, liberal, conservative, ndp, bloc, green) %>% 
  group_by(working_class, pro_redistribution) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro-redistribution Working Class Vote 2019.html"))

#-------------------------------------------------------------------------------------------------

#### Leader/party ratings overall ####
ces19phone%>% 
  select(Jagmeet_Singh) %>% 
  summary()

ces15phone%>% 
  select(Tom_Mulcair) %>% 
  summary()

ces19phone%>% 
  select(NDP_rating) %>% 
  summary()

ces15phone%>% 
  select(NDP_rating) %>% 
  summary()

