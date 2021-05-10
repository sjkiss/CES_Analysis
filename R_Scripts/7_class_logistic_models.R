#### Class logistic models 1979-2019 ####

#Run master file to load up data

library(stargazer)
library(broom)
library(nnet)

ces$ndp_vs_right<-Recode(ces$vote, "3=1; 2=0; else=NA")
ces$ndp_vs_liberal<-Recode(ces$vote, "3=1; 1=0; else=NA")
table(ces$ndp_vs_right)
table(ces$ndp_vs_liberal)

#CREATE WORKING CLASS DICHOTOMOUS VARIABLE; NOTE HERE ONLY EMPLOYED AND SELF-EMPLOYED PEOPLE ARE SET TO 0 OR 1; ELSE = NA
ces$working_class<-Recode(ces$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
ces$working_class2<-Recode(ces$occupation, "4:5=1; else=0")
#This collapses the two labour categories into one working class
ces$occupation2<-Recode(as.factor(ces$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces$occupation4<-Recode(as.factor(ces$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
ces$working_class3<-Recode(ces$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
ces$working_class4<-Recode(ces$occupation3, "4:5=1; else=0")

#this is the NDP vote variable
ces$ndp<-Recode(ces$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
table(ces$ndp)
table(ces$working_class, ces$election)
table(ces$working_class2, ces$election)
table(ces$working_class3, ces$election)
table(ces$working_class4)

#Let's put the Occupation variables in order
ces$occupation2<-fct_relevel(ces$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')
ces$occupation4<-fct_relevel(ces$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')
table(ces$occupation2, ces$election)
table(ces$occupation4, ces$election)

#Turn region into factor with East as reference case
ces$region3<-Recode(as.factor(ces$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces$region3)
table(ces$region3)

#Recode low income
ces$low_income<-Recode(ces$income, "1=1; 2:5=0; else=NA")
table(ces$low_income)

#Recode high moral traditionalism=authoritarian
table(ces19phone$moral_traditionalism)
ces19phone$authoritarian<-Recode(ces19phone$moral_traditionalism, "0.38:1=1; 0:3.75=0; else=NA")
table(ces19phone$authoritarian)
table(ces15phone$moral_traditionalism)
ces15phone$authoritarian<-Recode(ces15phone$moral_traditionalism, "0.38:1=1; 0:3.75=0; else=NA")
table(ces15phone$authoritarian)

#Party voting
ces19phone$bloc<-Recode(ces19phone$vote, "4=1; 0:3=0; 5=0; else=NA")
ces19phone$green<-Recode(ces19phone$vote, "5=1; 0:4=0; else=NA")
ces$other<-Recode(ces$vote, "4:5=1; 0=1; 1:3=0; else=NA")

#By election
summary(ces)

#Count missing values
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  View()

#------------------------------------------------------------------------------------------------

#M1 NDP vs Right ROC
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp_vs_right~income+degree+sector+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_vs_right_ROC_models_1

#M1 NDP vs Right QC
ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp_vs_right~income+degree+sector+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_vs_right_QC_models_1

#M1 NDP vs Liberal ROC
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp_vs_liberal~income+degree+sector+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_vs_liberal_ROC_models_1

#M1 NDP vs Liberal QC
ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp_vs_liberal~income+degree+sector+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_vs_liberal_QC_models_1

#Print models
stargazer(ndp_vs_right_ROC_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_vs_right_ROC_models_1.html"))
stargazer(ndp_vs_right_QC_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_vs_right_QC_models_1.html"))
stargazer(ndp_vs_liberal_ROC_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_vs_liberal_ROC_models_1.html"))
stargazer(ndp_vs_liberal_QC_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_vs_liberal_QC_models_1.html"))

#Plot coefficients
ndp_vs_right_ROC_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)
ggsave(here("Plots", "ROC_degree_ndp_vs_right_coefficients.png"))

ndp_vs_right_QC_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)
ggsave(here("Plots", "QC_degree_ndp_vs_right_coefficients.png"))

ndp_vs_liberal_ROC_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)
ggsave(here("Plots", "ROC_degree_ndp_vs_liberal_coefficients.png"))

ndp_vs_liberal_QC_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)
ggsave(here("Plots", "QC_degree_ndp_vs_liberal_coefficients.png"))

#------------------------------------------------------------------------------------------------
#### Redistribution models ####

table(ces$pro_redistribution, ces$election)
table(ces$redistribution, ces$election)
table(ces$working_class, ces$election)
table(ces$working_class4, ces$election)
library(knitr)
library(kableExtra)

ces93$working_class<-Recode(ces93$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
ces97$working_class<-Recode(ces97$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
ces0411$working_class04<-Recode(ces0411$occupation04, "4:5=1; 3=0; 2=0; 1=0; else=NA")
ces0411$working_class06<-Recode(ces0411$occupation06, "4:5=1; 3=0; 2=0; 1=0; else=NA")
ces0411$working_class08<-Recode(ces0411$occupation08, "4:5=1; 3=0; 2=0; 1=0; else=NA")
ces0411$working_class11<-Recode(ces0411$occupation11, "4:5=1; 3=0; 2=0; 1=0; else=NA")
ces15phone$working_class<-Recode(ces15phone$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
ces19phone$working_class<-Recode(ces19phone$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
table(ces93$working_class)
table(ces97$working_class)
table(ces0411$working_class04)
table(ces0411$working_class06)
table(ces0411$working_class08)
table(ces0411$working_class11)
table(ces15phone$working_class)
table(ces19phone$working_class)

#M1 NDP ROC
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=1988 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~income+degree+sector+union_both+working_class2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_ROC_redistribution_models_1

#M1 NDP QC
ces %>% 
  filter(quebec==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=1988 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~income+degree+sector+union_both+working_class2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_QC_redistribution_models_1

#M1 Con ROC
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=1988 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~income+degree+sector+union_both+working_class2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->conservative_ROC_redistribution_models_1

#M1 Con QC
ces %>% 
  filter(quebec==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=1988 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~income+degree+sector+union_both+working_class2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->conservative_QC_redistribution_models_1

#M2 NDP ROC WC interaction
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=1988 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~income+degree+sector+union_both+working_class2+redistribution+working_class2:redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_ROC_redistribution_models_2

#M2 NDP QC WC interaction
ces %>% 
  filter(quebec==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=1988 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~income+degree+sector+union_both+working_class2+redistribution+working_class2:redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_QC_redistribution_models_2

#M2 Con ROC WC interaction
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=1988 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~income+degree+sector+union_both+working_class2+redistribution+working_class2:redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->conservative_ROC_redistribution_models_2

#M2 Con QC WC interaction
ces %>% 
  filter(quebec==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=1988 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~income+degree+sector+union_both+working_class2+redistribution+working_class2:redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->conservative_QC_redistribution_models_2

stargazer(ndp_ROC_redistribution_models_1$model, column.labels=c("1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_ROC_redistribution_models_1.html"))
stargazer(ndp_QC_redistribution_models_1$model, column.labels=c("1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_QC_redistribution_models_1.html"))
stargazer(conservative_ROC_redistribution_models_1$model, column.labels=c("1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_ROC_redistribution_models_1.html"))
stargazer(conservative_QC_redistribution_models_1$model, column.labels=c("1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_QC_redistribution_models_1.html"))
stargazer(ndp_ROC_redistribution_models_2$model, column.labels=c("1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_ROC_redistribution_inter_models_2.html"))
stargazer(ndp_QC_redistribution_models_2$model, column.labels=c("1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_QC_redistribution_inter_models_2.html"))
stargazer(conservative_ROC_redistribution_models_2$model, column.labels=c("1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_ROC_redistribution_inter_models_2.html"))
stargazer(conservative_QC_redistribution_models_2$model, column.labels=c("1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_QC_redistribution_inter_models_2.html"))

#------------------------------------------------------------------------------------------------
#### Redistribution descriptives ####

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of respondents in ces studies")

ces %>% 
  group_by(election, working_class) %>% 
  filter(!is.na(working_class)) %>%
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of WC respondents in ces studies")

ces %>% 
  group_by(election, low_income) %>% 
  filter(!is.na(low_income)) %>%
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of low income respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(pro_redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average pro-redistribution of respondents in ces studies")

ces %>% 
  group_by(election, working_class) %>% 
  filter(!is.na(working_class)) %>%
  summarize(avg_age=mean(pro_redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average pro-redistribution of WC respondents in ces studies")

ces %>% 
  group_by(election, low_income) %>% 
  filter(!is.na(low_income)) %>%
  summarize(avg_age=mean(pro_redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average pro-redistribution of low income respondents in ces studies")

ces %>%
  filter(!is.na(redistribution)) %>%
  group_by(working_class, election) %>%
  summarize(mean_redistribution = mean(redistribution))

ces %>%
  filter(!is.na(pro_redistribution)) %>%
  group_by(working_class, election) %>%
  summarize(mean_pro_redistribution = mean(pro_redistribution))

## Share of Pro-redistribution Working Class members voting NDP
ces %>% 
  group_by(election, pro_redistribution, working_class, ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting NDP")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_NDP.png"))

## Share of Pro-redistribution Working Class members voting Conservative
ces %>% 
  group_by(election, pro_redistribution, working_class, conservative) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(conservative)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(conservative==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting Conservative")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_Conservative.png"))

## Share of Pro-redistribution Working Class members voting Liberal
ces %>% 
  group_by(election, pro_redistribution, working_class, liberal) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(liberal)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(liberal==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting Liberal")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_Liberal.png"))

# Working Class redistribution by year
ces93 %>%
  select(working_class, redistribution, pro_redistribution) %>% 
  group_by(working_class) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

ces97 %>%
  select(working_class, redistribution, pro_redistribution) %>% 
  group_by(working_class) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

ces0411 %>%
  select(working_class04, redistribution04, pro_redistribution04) %>% 
  group_by(working_class04) %>%
  summarise_at(vars(redistribution04, pro_redistribution04), mean, na.rm=T)

ces0411 %>%
  select(working_class06, redistribution06, pro_redistribution06) %>% 
  group_by(working_class06) %>%
  summarise_at(vars(redistribution06, pro_redistribution06), mean, na.rm=T)

ces0411 %>%
  select(working_class08, redistribution08, pro_redistribution08) %>% 
  group_by(working_class08) %>%
  summarise_at(vars(redistribution08, pro_redistribution08), mean, na.rm=T)

ces0411 %>%
  select(working_class11, redistribution11, pro_redistribution11) %>% 
  group_by(working_class11) %>%
  summarise_at(vars(redistribution11, pro_redistribution11), mean, na.rm=T)

ces15phone %>%
  select(working_class, redistribution, pro_redistribution) %>% 
  group_by(working_class) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

ces19phone %>%
  select(working_class, redistribution, pro_redistribution) %>% 
  group_by(working_class) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

# Working Class voting by pro-redistribution 2019
ces19phone %>%
  select(working_class, pro_redistribution, liberal, conservative, ndp, bloc, green) %>% 
  group_by(working_class, pro_redistribution) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro_redistribution_Working_Class_Vote_2019.html"))

# Working Class voting pro-redistribution by election
ces %>%
  select(election, working_class, pro_redistribution, liberal, conservative, ndp) %>% 
  group_by(election, working_class, pro_redistribution) %>%
  summarise_at(vars(liberal, conservative, ndp), mean, na.rm=T) %>% 
  as.data.frame() %>%
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro_redistribution_Working_Class_Vote_by_election.html"))

# Low Incomes voting by pro-redistribution 2019
ces19phone %>%
  select(low_income, pro_redistribution, liberal, conservative, ndp, bloc, green) %>% 
  group_by(low_income, pro_redistribution) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro_redistribution_low_income_Vote_2019.html"))

# Low Income voting pro-redistribution by election
ces %>%
  select(election, low_income, pro_redistribution, liberal, conservative, ndp) %>% 
  group_by(election, low_income, pro_redistribution) %>%
  summarise_at(vars(liberal, conservative, ndp), mean, na.rm=T) %>% 
  as.data.frame() %>%
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro_redistribution_low_income_Vote_by_election.html"))

##### Moral traditionalism comparison ####
# Working Class voting by moral traditionalism 2019
ces19phone %>%
  select(working_class, authoritarian, liberal, conservative, ndp, bloc, green) %>% 
  group_by(working_class, authoritarian) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "moral_traditionalism_Working_Class_Vote_2019.html"))

# Low Incomes voting by moral traditionalism 
ces19phone %>%
  select(low_income, authoritarian, liberal, conservative, ndp, bloc, green) %>% 
  group_by(low_income, authoritarian) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "moral_traditionalism_low_income_Vote_2019.html"))

ces15phone %>%
  select(working_class, authoritarian, liberal, conservative, ndp, bloc, green) %>% 
  group_by(working_class, authoritarian) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "moral_traditionalism_Working_Class_Vote_2015.html"))

# Low Income voting by moral traditionalism 
ces15phone %>%
  select(low_income, authoritarian, liberal, conservative, ndp, bloc, green) %>% 
  group_by(low_income, authoritarian) %>%
  summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "moral_traditionalism_low_income_Vote_2015.html"))

ces19phone %>%
  #  filter(!is.na(occupation4)) %>%
  select(authoritarian, Jagmeet_Singh, Andrew_Scheer, immigration, redistribution, environment, minorities_help, continentalism, market_liberalism, political_disaffection, ideology) %>% 
  group_by(authoritarian) %>%
  summarise_at(vars(Jagmeet_Singh, Andrew_Scheer, immigration, redistribution, environment, minorities_help, continentalism, market_liberalism, political_disaffection, ideology), mean, na.rm=T)

ces15phone %>%
  #  filter(!is.na(occupation4)) %>%
  select(authoritarian, Tom_Mulcair, Stephen_Harper, immigration, redistribution, environment, minorities_help, continentalism, market_liberalism, political_disaffection, ideology) %>% 
  group_by(authoritarian) %>%
  summarise_at(vars(Tom_Mulcair, Stephen_Harper, immigration, redistribution, environment, minorities_help, continentalism, market_liberalism, political_disaffection, ideology), mean, na.rm=T)

ces19phone %>%
  #  filter(!is.na(occupation4)) %>%
  select(pro_redistribution, Jagmeet_Singh, Andrew_Scheer, immigration, environment, minorities_help, continentalism, market_liberalism, political_disaffection, ideology) %>% 
  group_by(pro_redistribution) %>%
  summarise_at(vars(Jagmeet_Singh, Andrew_Scheer, immigration, environment, minorities_help, continentalism, market_liberalism, political_disaffection, ideology), mean, na.rm=T)

ces15phone %>%
  #  filter(!is.na(occupation4)) %>%
  select(pro_redistribution, Tom_Mulcair, Stephen_Harper, immigration, environment, minorities_help, continentalism, market_liberalism, political_disaffection, ideology) %>% 
  group_by(pro_redistribution) %>%
  summarise_at(vars(Tom_Mulcair, Stephen_Harper, immigration, environment, minorities_help, continentalism, market_liberalism, political_disaffection, ideology), mean, na.rm=T)

#------------------------------------------------------------------------------------------------
#### Working Class descriptives ####

#Share of Working class voting NDP
ces %>% 
  group_by(election, working_class, ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting NDP")
ggsave(here("Plots", "NDP_working_class_vote.png"))

#Share of Working class voting Liberal
ces %>% 
  group_by(election, working_class, liberal) %>% 
  summarize(n=n()) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(liberal)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(liberal==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Liberal")
ggsave(here("Plots", "Liberal_working_class_vote.png"))

#Share of Working class voting Conservative
ces %>% 
  group_by(election, working_class, conservative) %>% 
  summarize(n=n()) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(conservative)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(conservative==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Conservative")
ggsave(here("Plots", "Conservative_working_class_vote.png"))

#Share of Working class voting Other
ces %>% 
  group_by(election, working_class, other) %>% 
  summarize(n=n()) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(other)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(other==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Other")
ggsave(here("Plots", "Other_working_class_vote.png"))

#Party Vote Shares of Working Class
#This was your code

# ces %>% 
#   group_by(election, working_class, vote) %>% 
#   summarize(n=n()) %>% 
#   mutate(pct=n/sum(n)) %>%
#   filter(working_class==1 & (vote<4 & vote>0)) %>% 
#   ggplot(.,aes(x=as.numeric(election), y=pct))+
#   geom_point()+
#   geom_smooth(method="lm", se=F)+
#   facet_grid(~as_factor(vote))+
#   labs(title="Share of Working Class voting for political parties over time")
# ggsave(here("Plots", "Party_shares_working_class_vote.png"))

#My modifications
ces %>% 
  group_by(election, working_class, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(working_class==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Working Class voting for political parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_working_class_vote.png"))
#Percent of NDP Voters Working Class
ces %>% 
  group_by(election, vote, working_class) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(working_class==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="NDP Voter % that are Working Class")
ggsave(here("Plots", "NDP_Voters_Working_Class_Percent.png"))

#Percent of Liberal Voters Working Class
ces %>% 
  group_by(election, vote, working_class) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(working_class==1 & vote==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Liberal Voter % that are Working Class")
ggsave(here("Plots", "Lib_Voters_Working_Class_Percent.png"))

#Percent of Conservative Voters Working Class
ces %>% 
  group_by(election, vote, working_class) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(working_class==1 & vote==2) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Conservative Voter % that are Working Class")
ggsave(here("Plots", "Con_Voters_Working_Class_Percent.png"))



