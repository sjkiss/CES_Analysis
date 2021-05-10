#Logistic model for ces19phone including immigration sentiment

library(stargazer)

#Recode variables
ces19phone$ndp<-car::Recode(ces19phone$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
ces19phone$catholic<-car::Recode(ces19phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces19phone$no_religion<-car::Recode(ces19phone$religion, "0=1; 1:3=0; NA=NA")
ces19phone$working_class<-Recode(ces19phone$occupation, "5=1; else=0")
ces19phone$female<-Recode(ces19phone$male, "1=0; 0=1")

ces19phone %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces19phone
ces19phone$region2<-factor(ces19phone$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))

table(ces19phone$ndp)
table(ces19phone$catholic)
table(ces19phone$no_religion)
table(ces19phone$working_class)
table(ces19phone$region2)
table(ces19phone$female)

summary(ces19phone)

#NDP Model 1 (basic)
ndp_model1<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=ces19phone, family="binomial")
summary(ndp_model1)
stargazer(ndp_model1, type="html", out=here("Tables", "NDP1_ces19_basic.html"))

#NDP Model 2 (with immigration sentiment)
ndp_model2<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+immigration, data=ces19phone, family="binomial")
summary(ndp_model2)
stargazer(ndp_model2, type="html", out=here("Tables", "NDP2_ces19_immigration.html"))

#NDP Model 3 (union:immigration interaction)
ndp_model3<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+immigration+immigration:union_both, data=ces19phone, family="binomial")
summary(ndp_model3)
stargazer(ndp_model3, type="html", out=here("Tables", "NDP3_ces19_union_immigration_int.html"))

#NDP Model 4 (working_class:immigration interaction)
ndp_model4<-glm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income+immigration+immigration:working_class, data=ces19phone, family="binomial")
summary(ndp_model4)
stargazer(ndp_model4, type="html", out=here("Tables", "NDP4_ces19_union_workingclass_int.html"))

#NDP Model 5 (union only)
ndp_model5<-glm(ndp~union_both, data=ces19phone, family="binomial")
summary(ndp_model5)
stargazer(ndp_model5, type="html", out=here("Tables", "NDP5_ces19_union.html"))

#NDP Model 6 (union only)
ndp_model6<-glm(ndp~union_both+immigration, data=ces19phone, family="binomial")
summary(ndp_model6)
stargazer(ndp_model6, type="html", out=here("Tables", "NDP6_ces19_union_immigration.html"))

