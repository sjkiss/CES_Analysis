#Replicating 'The class-party relationship in Canada, 1965-2004' (Andersen 2013)
#Run master file to load up data

#Instead of typing Recode everytime, we can just load the car library here
library(car)

#Create other relevant variables 
#ces$working_class<-Recode(ces$occupation, "5=1; 1:4=0; else=NA")

#ces$working_class<-Recode(ces$occupation, "4:5=1; else=0")
table(ces$occupation)
table(ces$occupation3)
#This was your old code 
#ces$occupation2<-Recode(ces$occupation, "4:5=1; 3=2; 2=3; 1=4; NA=NA")
#But I think I'd rather do it this way; just to keep the numbers as close as possible;
#also this turns it into a factor with working class as the reference category.

#ces$occupation2<-Recode(ces$occupation, "4:5='Working_Class'; 3='Routine_Nonmanual' ;2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'),as.factor=T)
#The above didn't work for me so I am using this recode -Matt
ces$occupation2<-Recode(as.factor(ces$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
levels(ces$occupation2)
table(ces$occupation2)

#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces$occupation4<-Recode(as.factor(ces$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
levels(ces$occupation4)
table(ces$occupation4)
table(ces$occupation, ces$occupation2, ces$occupation4)

#I think Andersen has the Conservatives set as the reference category. 
ces$vote
table(as_factor(ces$vote))
ces$vote2<-Recode(as.factor(ces$vote), "0=NA; ;1='Liberal' ; 2='Conservative' ; 3:4='Left' ; 5='Green'", levels=c('Conservative', 'Liberal', 'Left', 'Green'))
table(ces$vote2)
levels(ces$vote2)

#new variable checks
#check for case counts 
table(ces$election, ces$ndp, useNA = "ifany")
table(ces$election, ces$liberal, useNA = "ifany")
table(ces$election, ces$conservative, useNA = "ifany")
table(ces$election, ces$occupation2, useNA = "ifany")

#Turn religion into factor with None as reference case
ces$religion2<-Recode(as.factor(ces$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces$religion2)
table(ces$religion2)

#Turn region into factor with East as reference case
ces$region3<-Recode(as.factor(ces$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces$region3)
table(ces$region3)

#Info: Missing variable in the following elections:
#Sector 1965 and 1972
#Occupation 2000 and 2019
table(ces$election, ces$sector)
table(ces$election, ces$occupation2)
table(ces$election, ces$occupation4)

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
  select(male, age, religion, degree, occupation2, occupation4, region, quebec) %>% 
  summary()

##Count missing values
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) 

#------------------------------------------------------------------------------------------------------------
library(nnet)
library(stargazer)
##
ces$election

###Model 1 -Partial Replication of Table 7.3 in Andersen (by Region) - just occupation and election
#we need to filter out years 2000
ces %>% 
  filter(election!=2000 & election<2006 &vote2!="Green")->ces.out
table(ces$vote2)
  #QC
  andersen1qc<-multinom(vote2 ~ occupation2+as.factor(election), data = subset(ces.out, quebec==1))
  #ROC
    andersen1roc<-multinom(vote2 ~ occupation2+as.factor(election), data = subset(ces.out, quebec!=1))

library(stargazer)
#The command add.lines adds output into the stargazer table
    #The number of observations is stored in the number of fitted values in the model
nrow(andersen1qc$fitted.values)
#nobs vector
nobs_andersen1qc<-c("N", rep(nrow(andersen1qc$fitted.values), 2))
nobs_andersen1roc<-c("N", rep(nrow(andersen1roc$fitted.values), 2))

#Check
nobs_andersen1qc
#add in 

stargazer(andersen1qc, type="html", out=here("Tables", "andersen1qc.html"), title="Multinomial Logistic Regression of Left Vote, QC", add.lines=list(nobs_andersen1qc))
stargazer(andersen1roc, type="html", out=here("Tables", "andersen1roc.html"), title="Multinomial Logistic Regression of NDP Vote, ROC", add.lines=list(nobs_andersen1roc))

#### Produce Figure 7.1 ####
#This is how Andersen does it; seems unwieldy 
#In this regard, Figure 7.1 displays fitted probabilities of
#voting by social class derived from models specifying an interaction between
#social class and time coded as a set of dummy regressors representing each year
#separately.

#First we spread the election variable to be dummies
ces %>% 
  pivot_wider(., names_from=election, values_from=election)->ces.out

#Need to make 15 separate regression models: one each with occupation and an interaction between occupation and an election dummy. Then generate predicted probabilities for each

# I think I can do it quicker; you tell me. 

#First Quebec
#For some reason ggpredict fails to produce standard errors, which I think we would like to produce margins of errors
library(ggeffects)
ces %>% 
  filter(election!=2000& election<2006 &quebec==1) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x)), 
         predicted=map(model, ggpredict))->qc_models

#Start with where the predicted values are stored
qc_models %>% 
  #unnest_wider is a new command; I just found out about it today;
  #unnest the predicted values objects
  unnest_wider(predicted) %>%
  #Now unnest occupation2
  unnest(occupation2) %>%
  #filter in only probability of voting for left
  filter(response.level!="Green") %>% 
  #PLot as line plot 
  ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In QC\nLeft Vote")

#Now ROC
ces %>% 
  filter(election!=2000& election<2006 &quebec!=1) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x))) %>% 
  mutate(predicted=map(model,ggpredict))-> roc_models
#Here we are assigning the election years to be the names of each model

#Start with where the predicted values are stored
roc_models %>% 
  #unnest_wider is a new command; I just found out about it today;
  #unnest the predicted values objects
  unnest_wider(predicted) %>%
  #Now unnest occupation2
  unnest(occupation2) %>%
  #filter in only probability of voting for left
  filter(response.level!="Green") %>% 
  #PLot as line plot 
  ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In ROC\nLeft Vote")

#### Extend Figure 7.1 to 2019 ####
#Make ROC Models to 2019
ces %>% 
  filter(election!=2000&quebec==0) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x)), 
         predicted=map(model, ggpredict))->roc_models_2019

#Start with where the predicted values are stored
roc_models_2019 %>% 
  #unnest_wider is a new command; I just found out about it today;
  #unnest the predicted values objects
  unnest_wider(predicted) %>%
  #Now unnest occupation2
  unnest(occupation2) %>%
  #filter in only probability of voting for left
  filter(response.level!="Green") %>% 
  #PLot as line plot 
  ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In ROC")+theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "class_voting_roc_2019.png"), width=8, height=3)
#Now QC 2019
ces %>% 
  filter(election!=2000 &quebec==1) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x))) %>% 
  mutate(predicted=map(model,ggpredict))-> qc_models_2019

#Start with where the predicted values are stored
qc_models_2019 %>% 
  #unnest_wider is a new command; I just found out about it today;
  #unnest the predicted values objects
  unnest_wider(predicted) %>%
  #Now unnest occupation2
  unnest(occupation2) %>%
  #filter in only probability of voting for left
  filter(response.level!="Green") %>% 
  #PLot as line plot 
  ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In QCC")+theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "class_voting_qc_2019.png"), width=8, height=3)

#----------------------------------------------------------------------------------------------------
####Model 2 - Replication of Table 7.3 in Andersen (by Region) ####
#we need to filter out years 2000

ces %>% 
  filter(election!=2000 & election<2006 &vote2!="Green")->ces.out

table(ces$vote2)
#QC
andersen2qc<-multinom(vote2 ~ as.factor(occupation2)+age+male+as.factor(religion2)+degree+as.factor(election), data = subset(ces.out, quebec==1))
#ROC
andersen2roc<-multinom(vote2 ~ as.factor(occupation2)+age+male+as.factor(religion2)+degree+as.factor(election)+as.factor(region3), data = subset(ces.out, quebec!=1))

library(stargazer)
#The command add.lines adds output into the stargazer table
#The number of observations is stored in the number of fitted values in the model
nrow(andersen2qc$fitted.values)
#nobs vector
nobs_andersen2qc<-c("N", rep(nrow(andersen2qc$fitted.values), 2))
nobs_andersen2roc<-c("N", rep(nrow(andersen2roc$fitted.values), 2))

#Check
nobs_andersen2qc
#add in 

stargazer(andersen2qc, type="html", out=here("Tables", "andersen2qc.html"), title="Multinomial Logistic Regression of Left Vote, 1965-2004, QC", add.lines=list(nobs_andersen2qc))
stargazer(andersen2roc, type="html", out=here("Tables", "andersen2roc.html"), title="Multinomial Logistic Regression of NDP Vote, 1965-2004, ROC", add.lines=list(nobs_andersen2roc))

#----------------------------------------------------------------------------------------------------
####Model 3 - Replication of Table 7.3 in Andersen (by Region) expanded to 2019 ####
#we need to filter out years 2000
ces %>% 
  filter(election!=2000 & vote2!="Green")->ces.out
table(ces$vote2)
#QC
andersen3qc<-multinom(vote2 ~ as.factor(occupation2)+age+male+as.factor(religion2)+degree+as.factor(election), data = subset(ces.out, quebec==1))
#ROC
andersen3roc<-multinom(vote2 ~ as.factor(occupation2)+age+male+as.factor(religion2)+degree+as.factor(election)+as.factor(region3), data = subset(ces.out, quebec!=1))

library(stargazer)
#The command add.lines adds output into the stargazer table
#The number of observations is stored in the number of fitted values in the model
nrow(andersen3qc$fitted.values)
#nobs vector
nobs_andersen3qc<-c("N", rep(nrow(andersen3qc$fitted.values), 2))
nobs_andersen3roc<-c("N", rep(nrow(andersen3roc$fitted.values), 2))

#Check
nobs_andersen3qc
#add in 

stargazer(andersen3qc, type="html", out=here("Tables", "andersen3qc.html"), title="Multinomial Logistic Regression of Left Vote, 1965-2019, QC", add.lines=list(nobs_andersen3qc))
stargazer(andersen3roc, type="html", out=here("Tables", "andersen3roc.html"), title="Multinomial Logistic Regression of NDP Vote, 1965-2019, ROC", add.lines=list(nobs_andersen3roc))

#----------------------------------------------------------------------------------------------------
  ####Model 4 - Extension of Table 7.3 in Andersen including Self-Employed (by Region) 1979-2019 ####

#we need to filter out years 2000 and before 1979
ces %>% 
  filter(election!=2000 & election>1974 & vote2!="Green")->ces.out
table(ces$vote2)
#QC
andersen4qc<-multinom(vote2 ~ as.factor(occupation4)+age+male+as.factor(religion2)+degree+as.factor(election), data = subset(ces.out, quebec==1))
#ROC
andersen4roc<-multinom(vote2 ~ as.factor(occupation4)+age+male+as.factor(religion2)+degree+as.factor(election)+as.factor(region3), data = subset(ces.out, quebec!=1))

library(stargazer)
#The command add.lines adds output into the stargazer table
#The number of observations is stored in the number of fitted values in the model
nrow(andersen4qc$fitted.values)
#nobs vector
nobs_andersen4qc<-c("N", rep(nrow(andersen4qc$fitted.values), 2))
nobs_andersen4roc<-c("N", rep(nrow(andersen4roc$fitted.values), 2))

#Check
nobs_andersen4qc
#add in 

stargazer(andersen4qc, type="html", out=here("Tables", "andersen4qc.html"), title="Multinomial Logistic Regression of Left Vote, 1979-2019, QC", add.lines=list(nobs_andersen4qc))
stargazer(andersen4roc, type="html", out=here("Tables", "andersen4roc.html"), title="Multinomial Logistic Regression of NDP Vote, 1979-2019, ROC", add.lines=list(nobs_andersen4roc))
