#Replicating 'The class-party relationship in Canada, 1965-2004' (Andersen 2013)
#Run master file to load up data
#Instead of typing Recode everytime, we can just load the car library here
library(car)

#I think Andersen has the Conservatives set as the reference category. 
ces$vote
table(as_factor(ces$vote))

#new variable checks
#check for case counts 
table(ces$election, ces$ndp, useNA = "ifany")
table(ces$election, ces$liberal, useNA = "ifany")
table(ces$election, ces$conservative, useNA = "ifany")
table(ces$election, ces$occupation2, useNA = "ifany")

#Info: Missing variables in the following elections:
#Sector 1965 and 1972
#Occupation 2000
table(ces$election, ces$sector)
table(ces$election, ces$occupation2)
table(ces$election, ces$occupation4)
table(ces$election, ces$working_class4)
table(ces$election, ces$working_class2)

#By election
head(ces)
tail(ces)
summary(ces)

library(broom)
library(nnet)
library(stargazer)

##Count missing values
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) 

###Model 1 -Partial Replication of Table 7.3 in Andersen (by Region) - just occupation and election
#we need to filter out years 2000
ces %>% 
  filter(election!=2000 & election<2006 &vote2!="Green")->ces.out
  #QC
  andersen1qc<-multinom(vote2 ~ occupation2+as.factor(election), data = subset(ces.out, quebec==1))
  #ROC
    andersen1roc<-multinom(vote2 ~ occupation2+as.factor(election), data = subset(ces.out, quebec!=1))
    summary(andersen1qc)

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

#In this regard, Figure 7.1 displays fitted probabilities of voting by social class 
#derived from models specifying an interaction between social class and time 
#coded as a set of dummy regressors representing each year #separately.

#First Quebec
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
  filter(response.level!="Green|BQ") %>% 
  #PLot as line plot 
  ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In QC")

#Now ROC
ces %>% 
  filter(election!=2000& election<2006 &quebec!=1) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x))) %>% 
  mutate(predicted=map(model,ggpredict))-> roc_models
#Here we are assigning the election years to be the names of each model

#Start with where the predicted values are stored
roc_models %>% 
  unnest_wider(predicted) %>%
  unnest(occupation2) %>%
  filter(response.level!="Green|BQ") %>% 
  ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In ROC")+ylim(c(0,1))

#Combine ROC and QC Models

#Provide region
qc_models$region<-rep("Quebec", nrow(qc_models))
roc_models$region<-rep("Rest of Canada", nrow(roc_models))
qc_models %>% 
  bind_rows(., roc_models) %>% 
  unnest_wider(predicted) %>% 
  unnest(occupation2) %>% 
  filter(response.level!="Green") %>%
  rename(Class=x, Election=election) %>% 
  ggplot(., aes(x=Election, y=predicted, group=Class, linetype=Class))+geom_line()+facet_grid(region~response.level)->canada_class_voting_1965_2004
canada_class_voting_1965_2004
ggsave(filename=here("Plots", "canada_class_voting_1965_2004.png"), width=12, height=4)

#### Extend Figure 7.1 to 2019 ####
#Make ROC Models to 2019
ces$region
ces$religion2
ces %>% 
  filter(election!=2000&quebec==0) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x)), 
         predicted=map(model, ggpredict))->roc_models_2019

#Start with where the predicted values are stored

# roc_models_2019 %>% 
#   unnest_wider(predicted) %>%
#   unnest(occupation2) %>%
#   filter(response.level!="Green") %>% 
#   ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In ROC")+theme(axis.text.x=element_text(angle=90))

#ggsave(here("Plots", "class_voting_roc_2019.png"), width=10, height=3)
#Now QC 2019
ces %>% 
  filter(election!=2000 &quebec==1) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x)), 
         predicted=map(model, ggpredict))-> qc_models_2019

#Start with where the predicted values are stored
# qc_models_2019 %>% 
#   unnest_wider(predicted) %>%
#   unnest(occupation2) %>%
#   filter(response.level!="Green") %>% 
#   ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In QCC")+theme(axis.text.x=element_text(angle=90))
#ggsave(here("Plots", "class_voting_qc_2019.png"), width=10, height=3)

qc_models_2019$region<-rep("Quebec", nrow(qc_models_2019))
roc_models_2019$region<-rep("Rest of Canada", nrow(roc_models_2019))
qc_models_2019
qc_models_2019 %>% 
  bind_rows(roc_models_2019) %>% 
unnest_wider(predicted) %>% 
  unnest(occupation2) %>% 
  filter(response.level!="Green") %>%
  rename(Class=x, Election=election) %>% 
  ggplot(., aes(x=Election, y=predicted, group=Class, linetype=Class))+geom_line()+facet_grid(region~response.level)+scale_linetype_manual(values=c(2,3,6, 1))+theme(axis.text.x = element_text(angle = 90))
ggsave(filename=here("Plots", "canada_class_voting_1965_2019.png"), dpi=300,width=12, height=4)


#### 

####Model 2 - Replication of Table 7.3 in Andersen (by Region) ####
#we need to filter out years 2000

ces %>% 
  filter(election!=2000 & election<2006 &vote2!="Green")->ces.out

table(ces$vote2)
#QC
andersen2qc<-multinom(vote2 ~ as.factor(occupation2)+age+male+as.factor(religion2)+degree+as.factor(election), data = subset(ces.out, quebec==1))
#ROC
andersen2roc<-multinom(vote2 ~ as.factor(occupation2)+age+male+as.factor(religion2)+degree+as.factor(election)+as.factor(region3), data = subset(ces.out, quebec!=1))



# library(stargazer)
# #The command add.lines adds output into the stargazer table
# #The number of observations is stored in the number of fitted values in the model
# nrow(andersen2qc$fitted.values)
# #nobs vector
# nobs_andersen2qc<-c("N", rep(nrow(andersen2qc$fitted.values), 2))
# nobs_andersen2roc<-c("N", rep(nrow(andersen2roc$fitted.values), 2))
# 
# #Check
# nobs_andersen2qc
# #add in 
# 
# stargazer(andersen2qc, type="html", out=here("Tables", "andersen2qc.html"), title="Multinomial Logistic Regression of Left Vote, 1965-2004, QC", add.lines=list(nobs_andersen2qc))
# stargazer(andersen2roc, type="html", out=here("Tables", "andersen2roc.html"), title="Multinomial Logistic Regression of NDP Vote, 1965-2004, ROC", add.lines=list(nobs_andersen2roc))


####Model 3 - Replication of Table 7.3 in Andersen (by Region) expanded to 2019 ####
#we need to filter out years 2000
ces %>% 
  filter(election!=2000 & vote2!="Green")->ces.out
table(ces$vote2)
#QC
andersen3qc<-multinom(vote2 ~ as.factor(occupation2)+age+male+as.factor(religion2)+degree+as.factor(election), data = subset(ces.out, quebec==1))
#ROC
andersen3roc<-multinom(vote2 ~ as.factor(occupation2)+age+male+as.factor(religion2)+degree+as.factor(election)+as.factor(region3), data = subset(ces.out, quebec!=1))

stargazer(andersen3qc, andersen3roc, type="html", out=here("Tables", "andersen_replication_extension_1965_2019.html"))


#The command add.lines adds output into the stargazer table
#The number of observations is stored in the number of fitted values in the model
# nrow(andersen3qc$fitted.values)
# #nobs vector
# nobs_andersen3qc<-c("N", rep(nrow(andersen3qc$fitted.values), 2))
# nobs_andersen3roc<-c("N", rep(nrow(andersen3roc$fitted.values), 2))

# #Check
# nobs_andersen3qc
#add in 


andersen3<-list(andersen3qc, andersen3roc)
map(andersen3, function(x) rep(nrow(x$fitted.values), 3)) %>% 
  unlist()->n_obs


#Graph


####Model 4 - Extension of Table 7.3 in Andersen including Self-Employed (by Region) 1979-2019 ####
ces %>% 
  filter(election> 1974&election!=2000  & vote2!="Green")->ces.out
as.factor(ces.out$election)

#QC
#Set reference for eleciton to be 1993

#
andersen4qc<-multinom(vote2 ~ as.factor(occupation4)+age+male+as.factor(religion2)+degree+relevel(as.factor(election), ref="1993"), data = subset(ces.out, quebec==1))
#ROC
andersen4roc<-multinom(vote2 ~ as.factor(occupation4)+age+male+as.factor(religion2)+degree+as.factor(election)+as.factor(region3), data = subset(ces.out, quebec!=1))

stargazer(andersen4qc, andersen4roc, digits=2,out=here("Tables", "andersen_replication_extension_1979_2019.html"),  type="html", column.labels=c("QC", "ROC"), column.separate = c(3,2), title="Multinomial Logistic Regression of Left Vote On Class, 1979-2019", covariate.labels=c('(Social Class) Managers', '(Social Class) Professionals','(Social Class) Self-Employed', '(Social Class) Routine Non Manual', 'Age', 'Male', '(Religion) Catholic', '(Religion) Protestant', '(Religion) Other', '(Education) Degree', '1979','1980' ,'1984','1988','1997', '2004', '2006','2008','2011','2015','2019','1980','1984' ,'1988','1993','1997', '2004', '2006','2008','2011','2015','2019', '(Region) Ontario','(Region) West'),
          add.lines=list(c("N"," ", nrow(andersen4qc$fitted.values), "", nrow(andersen4roc$fitted.values), " ")), 
          single.row = T, 
          dep.var.labels=c("Right/Liberal", "Right/NDP", "Right/BQ", "Right/Liberal", "Right/NDP"), 
          star.cutoffs = c(0.05, 0.01, 0.001))

#### Check Time And Degree ####
