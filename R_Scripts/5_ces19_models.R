#### Various models exploring ces19 and working class interactions 
#Run master file to load up data

library(stargazer)
library(broom)
library(nnet)

#By election
summary(ces)

#Recodes
ces19phone$working_class<-Recode(ces19phone$occupation, "4:5=1; else=0")
ces19phone$occupation2<-Recode(as.factor(ces19phone$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
ces19phone$ndp<-car::Recode(ces19phone$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
table(ces19phone$working_class)
table(ces19phone$ndp)

#Turn region into factor with East as reference case
ces19phone$region3<-Recode(as.factor(ces19phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces19phone$region3)
table(ces19phone$region3)



#### 2019 Models ####
#Model basic with controls

modelROC<-glm(ndp~region3+working_class+union_both+age+male+sector, data=ces19phone, family="binomial")

ces19phone %>% 
  filter(quebec==1)->ces.out

modelQC<-glm(ndp~working_class+union_both+age+male+sector, data=ces.out, family="binomial")
summary(modelROC)
summary(modelQC)

#M1 with leadership (Jagmeet_Singh)
model1ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Jagmeet_Singh, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model1QC<-glm(ndp~working_class+union_both+age+male+sector+Jagmeet_Singh, data=ces.out, family="binomial")
summary(model1ROC)
summary(model1QC)

#M2 with redistribution
model2ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model2QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution, data=ces.out, family="binomial")
summary(model2ROC)
summary(model2QC)

#M3 with environment
model3ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model3QC<-glm(ndp~working_class+union_both+age+male+sector+environment, data=ces.out, family="binomial")
summary(model3ROC)
summary(model3QC)

#M4 with immigration
model4ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model4QC<-glm(ndp~working_class+union_both+age+male+sector+immigration, data=ces.out, family="binomial")
summary(model4ROC)
summary(model4QC)

#Combine 5 models into one table
stargazer(modelROC, model1ROC, model2ROC, model3ROC, model4ROC, type="html", out=here("Tables", "ROC_ces19_attitudinal_variables.html"))
stargazer(modelQC, model1QC, model2QC, model3QC, model4QC, type="html", out=here("Tables", "QC_ces19_attitudinal_variables.html"))

#### Leadership interactions ####
#M5 leadership:working class interaction
model5ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Jagmeet_Singh+working_class:Jagmeet_Singh, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model5QC<-glm(ndp~working_class+union_both+age+male+sector+Jagmeet_Singh+working_class:Jagmeet_Singh, data=ces.out, family="binomial")
summary(model5ROC)
summary(model5QC)

#M6 leadership:union interaction
model6ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Jagmeet_Singh+union_both:Jagmeet_Singh, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model6QC<-glm(ndp~working_class+union_both+age+male+sector+Jagmeet_Singh+union_both:Jagmeet_Singh, data=ces.out, family="binomial")
summary(model6ROC)
summary(model6QC)

#### Redistribution interactions ####
#M7 redistribution:working class interaction
model7ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution+working_class:redistribution, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model7QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution+working_class:redistribution, data=ces.out, family="binomial")
summary(model7ROC)
summary(model7QC)

#M8 redistribution:union interaction
model8ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution+union_both:redistribution, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model8QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution+union_both:redistribution, data=ces.out, family="binomial")
summary(model8ROC)
summary(model8QC)

#### Environment interactions ####
#M9 environment:working class interaction
model9ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment+working_class:environment, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model9QC<-glm(ndp~working_class+union_both+age+male+sector+environment+working_class:environment, data=ces.out, family="binomial")
summary(model9ROC)
summary(model9QC)

#M10 environment:union interaction
model10ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment+union_both:environment, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model10QC<-glm(ndp~working_class+union_both+age+male+sector+environment+union_both:environment, data=ces.out, family="binomial")
summary(model10ROC)
summary(model10QC)

#### Immigration interactions ####
#M11 immigration:working class interaction
model11ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration+working_class:immigration, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model11QC<-glm(ndp~working_class+union_both+age+male+sector+immigration+working_class:immigration, data=ces.out, family="binomial")
summary(model11ROC)
summary(model11QC)

#M12 immigration:union interaction
model12ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration+union_both:immigration, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model12QC<-glm(ndp~working_class+union_both+age+male+sector+immigration+union_both:immigration, data=ces.out, family="binomial")
summary(model12ROC)
summary(model12QC)

#Combine interaction models into one table
stargazer(modelROC, model5ROC, model7ROC, model9ROC, model11ROC, type="html", out=here("Tables", "ROC_ces19_attitudinal_workingclass_interactions.html"))
stargazer(modelQC, model5QC, model7QC, model9QC, model11QC, type="html", out=here("Tables", "QC_ces19_attitudinal_workingclass_interactions.html"))
stargazer(modelROC, model6ROC, model8ROC, model10ROC, model12ROC, type="html", out=here("Tables", "ROC_ces19_attitudinal_union_interactions.html"))
stargazer(modelQC, model6QC, model8QC, model10QC, model12QC, type="html", out=here("Tables", "QC_ces19_attitudinal_union_interactions.html"))

