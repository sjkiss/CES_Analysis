#Replicating 'The class-party relationship in Canada, 1965-2004' (Andersen 2013)
#Run master file to load up data

#Instead of typing Recode everytime, we can just load the car library here
library(car)

#Create other relevant variables 
#ces$working_class<-Recode(ces$occupation, "5=1; 1:4=0; else=NA")

#ces$working_class<-Recode(ces$occupation, "4:5=1; else=0")
table(ces$occupation)
#This was your old code 
#ces$occupation2<-Recode(ces$occupation, "4:5=1; 3=2; 2=3; 1=4; NA=NA")
#But I think I'd rather do it this way; just to keep the numbers as close as possible;
#also this turns it into a factor with working class as the reference category.
table(ces$occupation)

ces$occupation2<-Recode(ces$occupation, "4:5='Working_Class'; 3='Routine Non_Manual' ;2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine Non_Manual'),as.factor=T)
table(ces$occupation, ces$occupation2)
#I think Andersen has the Conservatives set as the reference category. 
ces$vote
ces$vote2<-Recode(as.factor(ces$vote), "0=NA; ;1='Liberal' ; 2='Conservative' ; 3:4='Left' ; 5='Green'", levels=c('Conservative', 'Liberal', 'Left', 'Green'))
table(ces$vote2)
levels(ces$vote2)
#new variable checks
#check for case counts 
table(ces$election, ces$ndp, useNA = "ifany")
table(ces$election, ces$liberal, useNA = "ifany")
table(ces$election, ces$conservative, useNA = "ifany")
table(ces$election, ces$occupation2, useNA = "ifany")


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
  ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In QC\nLeft Vote")

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

#### Replicate Table 7.3 Exactly ####

#### Extend Table 7.3 to 2019 for ROC and Quebec ####

