#Logistic model for ces19phone including immigration sentiment

library(stargazer)

#Recode variables
ces19phone$ndp<-car::Recode(ces19phone$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
ces19phone$green<-car::Recode(ces19phone$vote, "5=1; 0:4=0; NA=NA")
ces19phone$catholic<-car::Recode(ces19phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces19phone$no_religion<-car::Recode(ces19phone$religion, "0=1; 1:3=0; NA=NA")
ces19phone$working_class<-car::Recode(ces19phone$occupation, "5=1; else=0")
ces19phone$female<-car::Recode(ces19phone$male, "1=0; 0=1")
ces19phone$union1<-car::Recode(ces19phone$union_both, "1=1; 0=NA")
ces19phone$ndp15_1<-car::Recode(ces19phone$ndp15, "1=1; 0=NA")
ces19phone$ndp15<-car::Recode(ces19phone$past_vote, "3=1; 0:2=0; 4:5=0; NA=NA")
ces19phone$liberal15<-car::Recode(ces19phone$past_vote, "1=1; 2:5=0; 0=0; NA=NA")
ces19phone$conservative15<-car::Recode(ces19phone$past_vote, "2=1; 0=0; 3:5=0; NA=NA")
ces19phone$bloc15<-car::Recode(ces19phone$past_vote, "4=1; 0:3=0; 5=0; NA=NA")
ces19phone$green15<-car::Recode(ces19phone$past_vote, "5=1; 0:4=0; NA=NA")
val_labels(ces19phone$catholic)<-c(Other=0, Catholic=1)
val_labels(ces19phone$no_religion)<-c(any_religion=0, no_religion=1)
val_labels(ces19phone$working_class)<-c(Other=0, working_class=1)
val_labels(ces19phone$female)<-c(Male=0, Female=1)
val_labels(ces19phone$ndp15)<-c(Other=0, NDP=1)
val_labels(ces19phone$liberal15)<-c(Other=0, Liberal=1)
val_labels(ces19phone$conservative15)<-c(Other=0, Conservative=1)
val_labels(ces19phone$bloc15)<-c(Other=0, Bloc=1)
val_labels(ces19phone$green15)<-c(Other=0, Green=1)

ces19phone %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces19phone
ces19phone$region2<-factor(ces19phone$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
val_labels(ces19phone$region2)<-c(Quebec=0, Atlantic=1, Ontario=2, West=3)

table(ces19phone$ndp)
table(ces19phone$green)
table(ces19phone$catholic)
table(ces19phone$no_religion)
table(ces19phone$working_class)
table(ces19phone$region2)
table(ces19phone$female)
table(ces19phone$union1)

summary(ces19phone)

#### NDP Model 1 (basic) ####
ndp_model1<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=ces19phone, family="binomial")
summary(ndp_model1)
stargazer(ndp_model1, type="html", out=here("Tables", "NDP1_ces19_basic.html"))

#### NDP Model 2 (union sub-sample) ####
#ces19phone %>% 
#filter(union_both==1) %>%
ndp_model2<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union1+age+female+sector+degree+income+immigration, data=ces19phone, family="binomial")
summary(ndp_model2)
stargazer(ndp_model2, type="html", out=here("Tables", "NDP2_ces19_union_subsample_immigration.html"))

#### NDP Model 3 (union:immigration interaction) ####
ndp_model3<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+immigration+immigration:union_both, data=ces19phone, family="binomial")
summary(ndp_model3)
stargazer(ndp_model3, type="html", out=here("Tables", "NDP3_ces19_union_immigration_int.html"))

#### NDP Model 4 (working_class:immigration interaction) ####
ndp_model4<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+immigration+immigration:working_class, data=ces19phone, family="binomial")
summary(ndp_model4)
stargazer(ndp_model4, type="html", out=here("Tables", "NDP4_ces19_workingclass_immigration_int.html"))

#### NDP Model 5 (union only) ####
ndp_model5<-glm(ndp~union_both, data=ces19phone, family="binomial")
summary(ndp_model5)
stargazer(ndp_model5, type="html", out=here("Tables", "NDP5_ces19_union.html"))

#### NDP Model 6 (union only) ####
ndp_model6<-glm(ndp~union_both+immigration, data=ces19phone, family="binomial")
summary(ndp_model6)
stargazer(ndp_model6, type="html", out=here("Tables", "NDP6_ces19_union_immigration.html"))

#### Adding one variable at a time to union and immigration ####
#### NDP Model 7 (1 control) ####
ndp_model7<-glm(ndp~union_both+immigration+as.factor(region2), data=ces19phone, family="binomial")
summary(ndp_model7)
stargazer(ndp_model7, type="html", out=here("Tables", "NDP7_ces19_union_immigration7.html"))

#### NDP Model 8 (2 controls) ####
ndp_model8<-glm(ndp~union_both+immigration+as.factor(region2)+catholic, data=ces19phone, family="binomial")
summary(ndp_model8)
stargazer(ndp_model8, type="html", out=here("Tables", "NDP8_ces19_union_immigration8.html"))

#### NDP Model 9 (3 controls) ####
ndp_model9<-glm(ndp~union_both+immigration+as.factor(region2)+catholic+no_religion, data=ces19phone, family="binomial")
summary(ndp_model9)
stargazer(ndp_model9, type="html", out=here("Tables", "NDP9_ces19_union_immigration9.html"))

#### NDP Model 10 (4 controls) ####
ndp_model10<-glm(ndp~union_both+immigration+as.factor(region2)+catholic+no_religion+non_charter_language, data=ces19phone, family="binomial")
summary(ndp_model10)
stargazer(ndp_model10, type="html", out=here("Tables", "NDP10_ces19_union_immigration10.html"))

#NDP Model 11 (5 controls) ####
ndp_model11<-glm(ndp~union_both+immigration+as.factor(region2)+catholic+no_religion+non_charter_language+working_class, data=ces19phone, family="binomial")
summary(ndp_model11)
stargazer(ndp_model11, type="html", out=here("Tables", "NDP11_ces19_union_immigration11.html"))

#### NDP Model 12 (6 controls) = big drop in union effect (0.11) ####
ndp_model12<-glm(ndp~union_both+immigration+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age, data=ces19phone, family="binomial")
summary(ndp_model12)
stargazer(ndp_model12, type="html", out=here("Tables", "NDP12_ces19_union_immigration12.html"))

#### NDP Model 13 (7 controls) = small drop in union effect (0.03) ####
ndp_model13<-glm(ndp~union_both+immigration+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female, data=ces19phone, family="binomial")
summary(ndp_model13)
stargazer(ndp_model13, type="html", out=here("Tables", "NDP13_ces19_union_immigration13.html"))

#### NDP Model 14 (8 controls) = big drop in union effect (0.14) ####
ndp_model14<-glm(ndp~union_both+immigration+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector, data=ces19phone, family="binomial")
summary(ndp_model14)
stargazer(ndp_model14, type="html", out=here("Tables", "NDP14_ces19_union_immigration14.html"))

#### NDP Model 15 (9 controls) ####
ndp_model15<-glm(ndp~union_both+immigration+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector+degree, data=ces19phone, family="binomial")
summary(ndp_model15)
stargazer(ndp_model15, type="html", out=here("Tables", "NDP15_ces19_union_immigration15.html"))

#### NDP Model 16 (10 controls) ####
ndp_model16<-glm(ndp~union_both+immigration+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector+degree+income, data=ces19phone, family="binomial")
summary(ndp_model16)
stargazer(ndp_model16, type="html", out=here("Tables", "NDP16_ces19_union_immigration16.html"))

#### NDP Model 17 (controls + past vote) ####
ndp_model17<-glm(ndp~union_both+immigration+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector+degree+income+past_vote, data=ces19phone, family="binomial")
summary(ndp_model17)
stargazer(ndp_model17, type="html", out=here("Tables", "NDP17_ces19_past_vote.html"))

#### NDP Model 18 (NDP ces15 voters sub-sample) ####
#ces19phone %>% 
#filter(ndp15==1) %>%
ndp_model18<-glm(ndp~union_both+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector+degree+income+immigration+ndp15_1, data=ces19phone, family="binomial")
summary(ndp_model18)
stargazer(ndp_model18, type="html", out=here("Tables", "NDP18_ces19_pastNDP_subsample.html"))

#### NDP Model 19 (10 controls + minority) ####
ndp_model19<-glm(ndp~union_both+immigration+minorities_help+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector+degree+income, data=ces19phone, family="binomial")
summary(ndp_model19)
stargazer(ndp_model19, type="html", out=here("Tables", "NDP19_ces19_union_immigration_minorities.html"))

#### NDP Model 20 (union:minority interaction) ####
ndp_model20<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+minorities_help+minorities_help:union_both, data=ces19phone, family="binomial")
summary(ndp_model20)
stargazer(ndp_model20, type="html", out=here("Tables", "NDP20_ces19_union_minorities_int.html"))

#### NDP Model 21 (working_class:minority interaction) ####
ndp_model21<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+minorities_help+minorities_help:working_class, data=ces19phone, family="binomial")
summary(ndp_model21)
stargazer(ndp_model21, type="html", out=here("Tables", "NDP21_ces19_minorities_workingclass_int.html"))

#### NDP Model 22 (union only + minority) ####
ndp_model22<-glm(ndp~union_both+immigration+minorities_help, data=ces19phone, family="binomial")
summary(ndp_model22)
stargazer(ndp_model22, type="html", out=here("Tables", "NDP22_ces19_union_minorities.html"))

#--------------------------------------------------------------------------------------------------------
#### Green Model 1 (basic) ####
green_model1<-glm(green~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=ces19phone, family="binomial")
summary(green_model1)
stargazer(green_model1, type="html", out=here("Tables", "Green1_ces19_basic.html"))

#### Green Model 16 (10 controls) ####
green_model16<-glm(green~union_both+immigration+as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector+degree+income, data=ces19phone, family="binomial")
summary(green_model16)
stargazer(green_model16, type="html", out=here("Tables", "Green16_ces19_union_immigration16.html"))

#### Green Model 3 (union:immigration interaction) ####
green_model3<-glm(green~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+immigration+immigration:union_both, data=ces19phone, family="binomial")
summary(green_model3)
stargazer(green_model3, type="html", out=here("Tables", "Green3_ces19_union_immigration_int.html"))

#### Green Model 4 (working_class:immigration interaction) ####
green_model4<-glm(green~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+immigration+immigration:working_class, data=ces19phone, family="binomial")
summary(green_model4)
stargazer(green_model4, type="html", out=here("Tables", "Green4_ces19_union_workingclass_int.html"))

#### Green Model 5 (union only) ####
green_model5<-glm(green~union_both, data=ces19phone, family="binomial")
summary(green_model5)
stargazer(green_model5, type="html", out=here("Tables", "Green5_ces19_union.html"))

#### Green Model 6 (union only) ####
green_model6<-glm(green~union_both+immigration, data=ces19phone, family="binomial")
summary(green_model6)
stargazer(green_model6, type="html", out=here("Tables", "Green6_ces19_union_immigration.html"))

#--------------------------------------------------------------------------------------------------------
#### Jagmeet Singh ####
#### Singh Model 1 (basic) ####
singh_model1<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+Jagmeet_Singh, data=ces19phone, family="binomial")
summary(singh_model1)
stargazer(singh_model1, type="html", out=here("Tables", "Singh1_ces19_basic.html"))

#### Singh Model 2 (union sub-sample) ####
singh_model2<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union1+age+female+sector+degree+income+Jagmeet_Singh, data=ces19phone, family="binomial")
summary(singh_model2)
stargazer(singh_model2, type="html", out=here("Tables", "Singh2_ces19_union_subsample_basic.html"))

#### Singh Model 3 (union:Singh interaction) ####
singh_model3<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+Jagmeet_Singh+Jagmeet_Singh:union_both, data=ces19phone, family="binomial")
summary(singh_model3)
stargazer(singh_model3, type="html", out=here("Tables", "Singh3_ces19_union_Singh_int.html"))

#### Singh Model 4 (working class:Singh interaction) ####
singh_model4<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+Jagmeet_Singh+Jagmeet_Singh:working_class, data=ces19phone, family="binomial")
summary(singh_model4)
stargazer(singh_model4, type="html", out=here("Tables", "Singh4_ces19_workingclass_Singh_int.html"))

#--------------------------------------------------------------------------------------------------------
#### Environment ####
#### Environment Model 1 (basic) ####
environment_model1<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+environment, data=ces19phone, family="binomial")
summary(environment_model1)
stargazer(environment_model1, type="html", out=here("Tables", "environment1_ces19_basic.html"))

#--------------------------------------------------------------------------------------------------------
#### Age2 (0-1) ####
#### Age Model 1 (basic) ####
age_model1<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age2+female+sector+degree+income, data=ces19phone, family="binomial")
summary(age_model1)
stargazer(age_model1, type="html", out=here("Tables", "age1_ces19_basic.html"))
