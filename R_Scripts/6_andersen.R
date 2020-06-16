#Replicating 'The class-party relationship in Canada, 1965-2004' (Andersen 2013)
#Run master file to load up data

#Instead of typing Recode everytime, we can just load the car library here
library(car)

#Create other relevant variables 
#ces$working_class<-Recode(ces$occupation, "5=1; 1:4=0; else=NA")

#ces$working_class<-Recode(ces$occupation, "4:5=1; else=0")
ces$occupation2<-Recode(ces$occupation, "4:5=1; 3=2; 2=3; 1=4; NA=NA")
ces$ndp_bloc<-Recode(ces$vote, "3:4=1; 0:2=0; 5=0; NA=NA")
ces$left_right<-Recode(ces$vote, "3=1; 2=0; else=NA")
ces$left_right2<-Recode(ces$vote, "3:4=1; 2=0; else=NA")

#new variable checks
#check for case counts 
table(ces$election, ces$ndp, useNA = "ifany")
table(ces$election, ces$liberal, useNA = "ifany")
table(ces$election, ces$conservative, useNA = "ifany")
table(ces$election, ces$occupation2, useNA = "ifany")
table(ces$election, ces$ndp_bloc, useNA = "ifany")
table(ces$election, ces$left_right, useNA = "ifany")
table(ces$election, ces$left_right2, useNA = "ifany")

#Info: Missing variable in the following elections:
#Sector 1965 and 1972
#Occupation 2000 and 2019
table(ces$election, ces$sector)
table(ces$election, ces$occupation2)

#By election
head(ces)
tail(ces)
summary(ces)
#Load broom
library(broom)

#------------------------------------------------------------------------------------------------------------
### Count cases
ces %>% 
  group_by(election) %>% 
  filter(election==1965|
           election==1968|
           election==1972|
           election==1974|
           election==1979|
           election==1980|
           election==1984|
           election==1988|
           election==1993|
           election==1997|
           election==2004) %>% 
  select(male, age, religion, degree, occupation2, region, quebec) %>% 
  summary()

##Count missing values
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  View()

#------------------------------------------------------------------------------------------------------------
library(nnet)
library(stargazer)
help(stargazer)

###Model 1 - Andersen replication (by Region) - just occupation

#we need to filter out years 2000, 2006 to 2019
ces %>% 
  group_by(election) %>% 
  filter(election==1965|
           election==1968|
           election==1972|
           election==1974|
           election==1979|
           election==1980|
           election==1984|
           election==1988|
           election==1993|
           election==1997|
           election==2004) ->ces.out

  andersen_model1<-multinom(formula = left_right ~ as.factor(occupation2)+as.factor(election), data = ces.out)
  
summary(andersen_model1)
stargazer(andersen_model1, type="text", n=TRUE )
stargazer(andersen_model1, type="html", out=here("Tables", "andersen_model1.html"))

#------------------------------------------------------------------------------------------------------------
###Model 2 - Andersen replication (by Quebec) - just occupation

ces %>% 
  filter(quebec==1)
  andersen_model2<-multinom(formula = left_right2~ as.factor(occupation2)+as.factor(election), data = ces.out)

summary(andersen_model2)
stargazer(andersen_model2, type="text")
stargazer(andersen_model2, type="html", out=here("Tables", "andersen_model2.html"))

#------------------------------------------------------------------------------------------------------------
###Model 3 - Andersen replication (by Region)

ces %>% 
  andersen_model3<-multinom(formula = left_right ~ as.factor(occupation2)+age+male+as.factor(religion)+degree+as.factor(election)+as.factor(region), data = ces.out)

summary(andersen_model3)
stargazer(andersen_model3, type="text")
stargazer(andersen_model3, type="html", out=here("Tables", "andersen_model3.html"))

#------------------------------------------------------------------------------------------------------------
###Model 4 - Andersen replication (by Quebec)

ces %>% 
  filter(quebec==1)
  andersen_model4<-multinom(formula = left_right2~ as.factor(occupation2)+age+male+as.factor(religion)+degree+as.factor(election), data = ces.out)

summary(andersen_model4)
stargazer(andersen_model4, type="text")
stargazer(andersen_model4, type="html", out=here("Tables", "andersen_model4.html"))

#------------------------------------------------------------------------------------------------------------

