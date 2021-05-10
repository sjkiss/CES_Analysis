#### Blais replication ####
#Run master file to load up data

library(stargazer)
library(broom)
library(nnet)

ces$catholic<-Recode(ces$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces$no_religion<-Recode(ces$religion, "0=1; 1:3=0; NA=NA")
ces$bloc<-Recode(ces$vote, "4=1; 0:3=0; 5=0; else=NA")
ces$green<-Recode(ces$vote, "5=1; 0:4=0; else=NA")

#CREATE WORKING CLASS DICHOTOMOUS VARIABLE; NOTE HERE ONLY EMPLOYED AND SELF-EMPLOYED PEOPLE ARE SET TO 0 OR 1; ELSE = NA
ces$working_class<-Recode(ces$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
#This collapses the two labour categories into one working class
ces$occupation2<-Recode(as.factor(ces$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces$occupation4<-Recode(as.factor(ces$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
ces$working_class3<-Recode(ces$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
ces$working_class4<-Recode(ces$occupation3, "4:5=1; else=0")
#this is the NDP vote variable
ces$ndp<-Recode(ces$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
table(ces$working_class, ces$election)
table(ces$ndp)
table(ces$working_class3, ces$election)
table(ces$working_class4)
#Let's put the working class variables in order
ces$occupation2<-fct_relevel(ces$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')
ces$occupation4<-fct_relevel(ces$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')

#Turn region into factor with East as reference case
ces$region3<-Recode(as.factor(ces$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces$region3)
table(ces$region3)

#By election
summary(ces)

#Count missing values
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  View()

#------------------------------------------------------------------------------------------------

#### M1 Blais Replication Extension ####

ces %>% 
  filter(election!=1965 & election!=1972 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete1

ces %>% 
  filter(election!=1965 & election!=1972 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete1

ces %>% 
  filter(election!=1965 & election!=1972 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete1

stargazer(ndp_models_complete1$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1968_2019_1.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete1$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1968_2019_1.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete1$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1968_2019_1.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#Join all parties and plot sector coefficients
ndp_models_complete1 %>% 
  bind_rows(., liberal_models_complete1) %>% 
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="working_class"| term=="union_both") %>% 
  mutate(term=Recode(term, "'working_class'='Working Class'; 'union_both'='Union'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "M1_blais_replication_all_parties.png"))

#------------------------------------------------------------------------------------------------

#### M2 Blais Replication Extension (without sector ####
ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete2

ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete2

ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete2

stargazer(ndp_models_complete2$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1965_2019_2.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1965-2019 without Sector", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete2$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1965_2019_2.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1965-2019 without Sector", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete2$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1965_2019_2.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1965-2019 without Sector", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#Join all parties and plot sector coefficients
ndp_models_complete2 %>% 
  bind_rows(., liberal_models_complete2) %>% 
  bind_rows(., conservative_models_complete2) %>%
  unnest(tidied) %>% 
  filter(term=="working_class"| term=="union_both") %>% 
  mutate(term=Recode(term, "'working_class'='Working Class'; 'union_both'='Union'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M2_blais_no_sector_3_parties.png"))

#------------------------------------------------------------------------------------------------

#### M3 Blais Replication Extension (with Degree and Income) ####
ces %>% 
  filter(election!=1965 & election!=1972 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete3

ces %>% 
  filter(election!=1965 & election!=1972 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete3

ces %>% 
  filter(election!=1965 & election!=1972 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete3

stargazer(ndp_models_complete3$model, 
          type="html", 
          out=here("Tables", "NDP_Models_Degree_Income_1968_2019_3.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 with Degree and Income", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete3$model, 
          type="html", 
          out=here("Tables", "liberal_Models_Degree_Income_1968_2019_3.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 with Degree and Income", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete3$model, 
          type="html", 
          out=here("Tables", "conservative_Models_Degree_Income_1968_2019_3.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 with Degree and Income", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#Join all parties and plot sector coefficients
ndp_models_complete3 %>% 
  bind_rows(., liberal_models_complete3) %>% 
  bind_rows(., conservative_models_complete3) %>%
  unnest(tidied) %>% 
  filter(term=="working_class"| term=="union_both") %>% 
  mutate(term=Recode(term, "'working_class'='Working Class'; 'union_both'='Union'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M3_blais_degree_income_3_parties.png"))

#------------------------------------------------------------------------------------------------

#### M4 Blais Replication Extension 1979-2019 with Greens ####
table(ces$election, ces$sector)

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class3+union_both+age+female+sector+degree+income, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete4

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class3+union_both+age+female+sector+degree+income, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete4

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class3+union_both+age+female+sector+degree+income, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete4

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(green~as.factor(region2)+catholic+no_religion+non_charter_language+working_class3+union_both+age+female+sector+degree+income, data=x)),
         tidied=map(model, tidy), 
         vote=rep('Green', nrow(.)))->green_models_complete4

stargazer(ndp_models_complete4$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1968_2019_4.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete4$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1968_2019_4.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete4$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1968_2019_4.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(green_models_complete4$model, 
          type="html", 
          out=here("Tables", "Green_Models_1979_2019_4.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Green Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#Join all parties and plot sector coefficients
ndp_models_complete4 %>% 
  bind_rows(., liberal_models_complete4) %>% 
  bind_rows(., conservative_models_complete4) %>%
  bind_rows(., green_models_complete4) %>%
  unnest(tidied) %>% 
  filter(term=="union_both"| term=="working_class3") %>% 
  mutate(term=Recode(term, "'union_both'='Union'; 'working_class3'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Working Class")))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  scale_color_manual(values=c("blue", "green", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
  
#save 
ggsave(here("Plots", "M4_blais_1979_2019_4_parties.png"))

#------------------------------------------------------------------------------------------------

#### M5 Blais Replication Extension Working Class sub-sample 1979-2019 with Greens ####
table(ces$election, ces$sector)

ces %>% 
  filter(working_class3==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+union_both+age+female+sector+degree+income, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete5

ces %>% 
  filter(working_class3==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+union_both+age+female+sector+degree+income, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete5

ces %>% 
  filter(working_class3==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+union_both+age+female+sector+degree+income, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete5

ces %>% 
  filter(working_class3==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(green~as.factor(region2)+catholic+no_religion+non_charter_language+union_both+age+female+sector+degree+income, data=x)),
         tidied=map(model, tidy), 
         vote=rep('Green', nrow(.)))->green_models_complete5

stargazer(ndp_models_complete5$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1979_2019_5.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete5$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1979_2019_5.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete5$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1979_2019_5.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(green_models_complete5$model, 
          type="html", 
          out=here("Tables", "Green_Models_1979_2019_5.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Green Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#Join all parties and plot sector coefficients
ndp_models_complete5 %>% 
  bind_rows(., liberal_models_complete5) %>% 
  bind_rows(., conservative_models_complete5) %>%
  bind_rows(., green_models_complete5) %>%
  unnest(tidied) %>% 
  filter(term=="union_both"| term=="working_class3") %>% 
  mutate(term=Recode(term, "'union_both'='Union'; 'working_class3'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Working Class")))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  scale_color_manual(values=c("blue", "green", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M5_blais_workingclass_subsample_4_parties.png"))

#------------------------------------------------------------------------------------------------

#### M6 Blais Replication Extension 1979-2019 with Greens & Occupation4 as factor####
table(ces$election, ces$sector)

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+as.factor(occupation4)+union_both+age+female+sector+degree+income, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete6

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+as.factor(occupation4)+union_both+age+female+sector+degree+income, data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete6

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+as.factor(occupation4)+union_both+age+female+sector+degree+income, data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete6

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(green~as.factor(region2)+catholic+no_religion+non_charter_language+as.factor(occupation4)+union_both+age+female+sector+degree+income, data=x)),
         tidied=map(model, tidy), 
         vote=rep('Green', nrow(.)))->green_models_complete6

stargazer(ndp_models_complete6$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1979_2019_6.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete6$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1979_2019_6.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete6$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1979_2019_6.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(green_models_complete6$model, 
          type="html", 
          out=here("Tables", "Green_Models_1979_2019_6.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Green Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#Join all parties and plot sector coefficients
ndp_models_complete6 %>% 
  bind_rows(., liberal_models_complete6) %>% 
  bind_rows(., conservative_models_complete6) %>%
  bind_rows(., green_models_complete6) %>%
  unnest(tidied) %>% 
  filter(term=="union_both"| term=="working_class3") %>% 
  mutate(term=Recode(term, "'union_both'='Union'; 'working_class3'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Working Class")))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  scale_color_manual(values=c("blue", "green", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M6_blais_occupation_1979_2019_4_parties.png"))
