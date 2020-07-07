#Replicating 'The public-private sector cleavage in North America (Blais et al 1990)'
#Run master file to load up data

library(tidyverse)
library(labelled)
library(here)
library(car)
library(broom)
library(stargazer)
library(ggeffects)

#Create other relevant dummy variables (Catholic, no_religion, ndp_id, low income x2, lower occupations)
ces$catholic<-Recode(ces$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces$no_religion<-Recode(ces$religion, "0=1; 1:3=0; NA=NA")
ces$ndp_id<-Recode(ces$party_id, "3=1; 0:2=0; 4:5=0; NA=NA")
ces$low_income<-Recode(ces$income, "1=1; 2:5=0; NA=NA")
ces$high_income<-Recode(ces$income, "1:4=0; 5=1; NA=NA")
ces$income_12<-Recode(ces$income, "1:2=1; 3:5=0; NA=NA")
ces$income_345<-Recode(ces$income, "3:5=1; 1:2=0; NA=NA")
ces$occupation_12<-Recode(ces$occupation, "1:2=1; 3:5=0; NA=NA")
ces$occupation_345<-Recode(ces$occupation, "3:5=1; 1:2=0; NA=NA")
#ces$working_class<-Recode(ces$occupation, "5=1; 1:4=0; else=NA")

ces$working_class<-Recode(ces$occupation, "5=1; else=0")

#new variable checks
#check for case counts 
table(ces$election, ces$ndp, useNA = "ifany")
table(ces$election, ces$liberal, useNA = "ifany")
table(ces$election, ces$conservative, useNA = "ifany")
table(ces$election, ces$catholic, useNA = "ifany")
table(ces$election, ces$no_religion, useNA = "ifany")
table(ces$election, ces$ndp_id, useNA = "ifany")
table(ces$election, ces$low_income, useNA = "ifany")
table(ces$election, ces$high_income, useNA = "ifany")
table(ces$election, ces$income_12, useNA = "ifany")
table(ces$election, ces$income_345, useNA = "ifany")
table(ces$election, ces$occupation_12, useNA = "ifany")
table(ces$election, ces$occupation_345, useNA = "ifany")
table(ces$election, ces$working_class, useNA = "ifany")
table(ces$election, ces$size, useNA = "ifany")

#Info: Missing variable in the following elections:
#Sector 1965 and 1972
#Occupation 2000 and 2019

#By election
head(ces)
tail(ces)
summary(ces)

#------------------------------------------------------------------------------------------------------------
### Count cases
ces %>% 
  group_by(election) %>% 
  filter(election==1968|
           election==1974|
           election==1979|
           election==1980|
           election==1984) %>% 
select(non_charter_language, working_class, no_religion, sector, catholic, union_both, size) %>% 
summary()

###Count missing values
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  View()

#------------------------------------------------------------------------------------------------

#### M1 Blais Replication Extension (including 2019) ####
table(ces$election, ces$sector)

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete1

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete1

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete1

stargazer(ndp_models_complete1$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1968_2019_1.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete1$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1968_2019_1.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete1$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1968_2019_1.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete1 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
#ggsave(here("Plots", "sector_ndp_1968_2019_1.png"))

liberal_models_complete1 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
#ggsave(here("Plots", "sector_liberal_1968_2019_1.png"))

conservative_models_complete1 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
#ggsave(here("Plots", "sector_conservative_1968_2019_1.png"))

#Join all parties and plot sector coefficients
ndp_models_complete1 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete1) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete1) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for sector and union
  filter(term=="sector"| term=="union_both") %>% 
  mutate(term=Recode(term, "'sector'='Sector'; 'union_both'='Union'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Public Sector on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M1_public_sector_all_parties.png"))

#------------------------------------------------------------------------------------------------

#### M2 Blais Replication Extension (including 2019)(Female:Sector interaction) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+female:sector+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete2

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+female:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete2

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+female:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete2

stargazer(ndp_models_complete2$model, 
          type="html", 
          out=here("Tables", "NDP_Models_Female_int_1968_2019_2.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 Female:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete2$model, 
          type="html", 
          out=here("Tables", "liberal_Models_Female_int_1968_2019_2.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 Female:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete2$model, 
          type="html", 
          out=here("Tables", "conservative_Models_Female_int_1968_2019_2.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 Female:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete2 %>% 
  unnest(tidied) %>% 
  filter(term=="female:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Sector:Female interaction on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "female_sector_ndp_1968_2019_2.png"))

liberal_models_complete2 %>% 
  unnest(tidied) %>% 
  filter(term=="female:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Sector:Female interaction on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "female_sector_liberal_1968_2019_2.png"))

conservative_models_complete2 %>% 
  unnest(tidied) %>% 
  filter(term=="female:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Sector:Female interaction on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "female_sector_conservative_1968_2019_2.png"))

#Join all parties and plot sector coefficients
ndp_models_complete2 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete2) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete2) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for female:sector and female
  filter(term=="female:sector"| term=="female") %>% 
  #  mutate(term=Recode(term, "'female:sector'='Female_Sector'; 'female'='Female'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Female")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Female:Sector on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M2_female_sector_all_parties.png"))

#------------------------------------------------------------------------------------------------

#### M3 Blais Replication Extension (including 2019)(no union_both) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete3

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete3

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+age+female+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete3

stargazer(ndp_models_complete3$model, 
          type="html", 
          out=here("Tables", "NDP_Models_without_union_1968_2019_3.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05),
          title="NDP Models 1968-2019 without Union", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete3$model, 
          type="html", 
          out=here("Tables", "liberal_Models_without_union_1968_2019_3.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 without Union", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete3$model, 
          type="html", 
          out=here("Tables", "conservative_Models_without_union_1968_2019_3.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 without Union", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete3 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_ndp_1968_2019_3.png"))

liberal_models_complete3 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_liberal_1968_2019_3.png"))

conservative_models_complete3 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sectoron Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_conservative_1968_2019_3.png"))

#Join all parties and plot sector coefficients
ndp_models_complete3 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete3) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete3) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for sector and working class
  filter(term=="sector"| term=="working_class") %>% 
  mutate(term=Recode(term, "'sector'='Sector'; 'working_class'='working class'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "working class")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Public Sector on PV (w/o Union)", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M3_public_sector_nounion_all_parties.png"))

#------------------------------------------------------------------------------------------------

#### M4 Blais Replication Extension (including 2019)(Working_Class:Sectorinteraction) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+working_class:sector+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete4

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+working_class:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete4

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+working_class:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete4

stargazer(ndp_models_complete4$model, 
          type="html", 
          out=here("Tables", "NDP_Models_workingclass_int_1968_2019_4.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 Working class:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete4$model, 
          type="html", 
          out=here("Tables", "liberal_Models_workingclass_int_1968_2019_4.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 Working class:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete4$model, 
          type="html", 
          out=here("Tables", "conservative_Models_workingclass_int_1968_2019_4.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 Working class:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete4 %>% 
  unnest(tidied) %>% 
  filter(term=="working_class:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Working Class:Sector interaction on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "workingclass_sector_ndp_1968_2019_4.png"))

liberal_models_complete4 %>% 
  unnest(tidied) %>% 
  filter(term=="working_class:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Working Class:Sector interaction on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "workingclass_sector_liberal_1968_2019_4.png"))

conservative_models_complete4 %>% 
  unnest(tidied) %>% 
  filter(term=="working_class:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Working Class:Sector interaction on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "workingclass_sector_conservative_1968_2019_4.png"))

#Join all parties and plot sector coefficients
ndp_models_complete4 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete4) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete4) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for sector and union
  filter(term=="working_class:sector"| term=="working_class") %>% 
  #  mutate(term=Recode(term, "'working_class:sector'='working class:Sector'; 'working_class'='working class'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "working class")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Working Class:Sector on PV", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M4_working_class_sector_all_parties.png"))

#------------------------------------------------------------------------------------------------

#### M5 Blais Replication Extension (including 2019)(Union:Sector interaction) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+union_both:sector+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete5

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+union_both:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete5

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+union_both:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete5

stargazer(ndp_models_complete5$model, 
          type="html", 
          out=here("Tables", "NDP_Models_union_int_1968_2019_5.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 Union:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete5$model, 
          type="html", 
          out=here("Tables", "liberal_Models_union_int_1968_2019_5.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 Union:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete5$model, 
          type="html", 
          out=here("Tables", "conservative_union_int_Models_1968_2019_5.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 Union:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete5 %>% 
  unnest(tidied) %>% 
  filter(term=="union_both:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of union:sector interaction on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
#ggsave(here("Plots", "union_sector_ndp_1968_2019_5.png"))

liberal_models_complete5 %>% 
  unnest(tidied) %>% 
  filter(term=="union_both:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of union:sector interaction on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
#ggsave(here("Plots", "union_sector_liberal_1968_2019_5.png"))

conservative_models_complete5 %>% 
  unnest(tidied) %>% 
  filter(term=="union_both:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of union:sector interaction on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
#ggsave(here("Plots", "union_sector_conservative_1968_2019_5.png"))

#Join all parties and plot sector coefficients
ndp_models_complete5 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete5) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete5) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for region:sector and sector
  filter(term=="union_both:sector"| term=="sector") %>% 
  #  mutate(term=Recode(term, "'union_both:sector'='Union:Sector'; 'union_both'='Union'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Union:Sector on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M5_union_sector_all_parties.png"))

#------------------------------------------------------------------------------------------------

#### M6 Blais Replication Extension (including 2019)(with Degree and Income) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete6

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete6

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete6

stargazer(ndp_models_complete6$model, 
          type="html", 
          out=here("Tables", "NDP_Models_Degree_Income_1968_2019_6.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 with Degree and Income", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete6$model, 
          type="html", 
          out=here("Tables", "liberal_Models_Degree_Income_1968_2019_6.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 with Degree and Income", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete6$model, 
          type="html", 
          out=here("Tables", "conservative_Models_Degree_Income_1968_2019_6.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 with Degree and Income", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots for Degree ####
# Set theme
theme_set(theme_bw())
ndp_models_complete6 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Degree on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "degree_ndp_1968_2019_6.png"))

liberal_models_complete6 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Degree on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "degree_liberal_1968_2019_6.png"))

conservative_models_complete6 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Degree on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "degree_conservative_1968_2019_6.png"))

#Join all parties and plot sector coefficients
ndp_models_complete6 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete6) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete6) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for sector and union
  filter(term=="degree"| term=="sector") %>% 
  mutate(term=Recode(term, "'sector'='Sector'; 'degree'='Degree'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Sector")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Degree on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M6_degree_all_parties.png"))

#### Blais Plots for Degree ####
# Set theme
theme_set(theme_bw())
ndp_models_complete6 %>% 
  unnest(tidied) %>% 
  filter(term=="income") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Income on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "income_ndp_1968_2019_6.png"))

liberal_models_complete6 %>% 
  unnest(tidied) %>% 
  filter(term=="income") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Income on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "income_liberal_1968_2019_6.png"))

conservative_models_complete6 %>% 
  unnest(tidied) %>% 
  filter(term=="income") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Income on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "income_conservative_1968_2019_6.png"))

#Join all parties and plot sector coefficients
ndp_models_complete6 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete6) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete6) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for sector and union
  filter(term=="income"| term=="sector") %>% 
  mutate(term=Recode(term, "'sector'='Sector'; 'income'='Income'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Sector")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Income on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M6_income_all_parties.png"))

#------------------------------------------------------------------------------------------------

#### M7 Blais Replication Extension (including 2019)(Degree:Sector interaction) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+degree:sector+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete7

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+degree:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete7

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+degree:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete7

stargazer(ndp_models_complete7$model, 
          type="html", 
          out=here("Tables", "NDP_Models_Degree_int_1968_2019_7.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 with Degree:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete7$model, 
          type="html", 
          out=here("Tables", "liberal_Models_Degree_int_1968_2019_7.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 with Degree:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete7$model, 
          type="html", 
          out=here("Tables", "conservative_Models_Degree_int_1968_2019_7.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 with Degree:Sector interaction", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete7 %>% 
  unnest(tidied) %>% 
  filter(term=="degree:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Degree:Sector interaction on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "income_sector_ndp_1968_2019_8.png"))

liberal_models_complete7 %>% 
  unnest(tidied) %>% 
  filter(term=="degree:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Degree:Sector interaction on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "income_sector_liberal_1968_2019_8.png"))

conservative_models_complete7 %>% 
  unnest(tidied) %>% 
  filter(term=="degree:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Degree:Sector interaction on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "degree_sector_conservative_1968_2019_7.png"))

#Join all parties and plot sector coefficients
ndp_models_complete7 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete7) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete7) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for degree:sector and degree
  filter(term=="degree:sector"| term=="degree") %>% 
  #  mutate(term=Recode(term, "'degree:sector'='Degree:Sector'; 'degree'='Degree'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "sector")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Degree:Sector on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
# ndp_models_complete1
# ndp_models_complete7
# ndp_models_complete6%>%
#   unnest(tidied)%>%
#   filter(term=="degree")->no_int
# 
# ndp_models_complete7%>%
#   unnest(tidied)%>%
#   filter(term=="degree:sector")->int
# bind_rows(no_int, int)%>%
#   ggplot(., aes(x=election, y=estimate, col=term))+geom_point()+
# int
# no_int
#save 
ggsave(here("Plots", "M7_degree_sector_all_parties.png"))

#------------------------------------------------------------------------------------------------

#### M8 Blais Replication Extension (including 2019)(Income:Sector interaction) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+income:sector+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete8

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+income:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete8

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+income:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete8

stargazer(ndp_models_complete8$model, 
          type="html", 
          out=here("Tables", "NDP_Models_income_int_1968_2019_10.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 with Income:Sector interaction",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete8$model, 
          type="html", 
          out=here("Tables", "liberal_Models_income_int_1968_2019_10.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 with Income:Sector interaction",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete8$model, 
          type="html", 
          out=here("Tables", "conservative_Models_income_int_1968_2019_10.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 with Income:Sector interaction",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete8 %>% 
  unnest(tidied) %>% 
  filter(term=="income:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Income:Sector interaction on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "income_sector_ndp_1968_2019_8.png"))

liberal_models_complete8 %>% 
  unnest(tidied) %>% 
  filter(term=="income:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Income:Sector interaction on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "income_sector_liberal_1968_2019_8.png"))

conservative_models_complete8 %>% 
  unnest(tidied) %>% 
  filter(term=="income:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Income:Sector interaction on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "income_sector_conservative_1968_2019_8.png"))

#Join all parties and plot sector coefficients
ndp_models_complete8 %>%
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete8) %>%
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete8) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>%
  #only keep the coefficients for income:sector and sector
  filter(term=="income:sector"| term=="income") %>%
  #  mutate(term=Recode(term, "'income:sector'='Income:Sector'; 'income'='Income'")) %>%
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "income")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Income:Sector on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save
ggsave(here("Plots", "M8_income_sector_all_parties.png"))

#------------------------------------------------------------------------------------------------
#### M9 Blais Replication Extension (including 2019)(Quebec only) ####
table(ces$election, ces$quebec)

ces %>% 
  #filter out elections missing key variables and keep only Quebec
  filter(quebec==1 & election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete9

ces %>% 
  #filter out elections missing key variables and keep only Quebec
  filter(quebec==1 & election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete9

ces %>% 
  #filter out elections missing key variables and keep only Quebec
  filter(quebec==1 & election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete9

ces %>% 
  #filter out elections missing key variables and keep only Quebec
  filter(quebec==1 & election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~no_religion+non_charter_language+working_class+union_both+age+female+sector+degree+income, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Bloc', nrow(.))  
  )->bloc_models_complete9

stargazer(ndp_models_complete9$model, 
          type="html", 
          out=here("Tables", "NDP_Models_Quebec_1968_2019_9.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 Quebec only",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete9$model, 
          type="html", 
          out=here("Tables", "liberal_Models_Quebec_1968_2019_9.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 Quebec only",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete9$model, 
          type="html", 
          out=here("Tables", "conservative_Models_Quebec_1968_2019_9.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 Quebec only",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(bloc_models_complete9$model, 
          type="html", 
          out=here("Tables", "bloc_Models_Quebec_1968_2019_9.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Bloc Models 1968-2019 Quebec only",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete9 %>%
  unnest(tidied) %>%
  filter(term=="sector") %>%
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector in Quebec on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_quebec_ndp_1968_2019_9.png"))

liberal_models_complete9 %>%
  unnest(tidied) %>%
  filter(term=="sector") %>%
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector in Quebec on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_quebec_liberal_1968_2019_9.png"))

conservative_models_complete9 %>%
  unnest(tidied) %>%
  filter(term=="sector") %>%
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector in Quebec on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_quebec_conservative_1968_2019_9.png"))

bloc_models_complete9 %>%
  unnest(tidied) %>%
  filter(term=="sector") %>%
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector in Quebec on Bloc Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_quebec_bloc_1968_2019_9.png"))

#Join all parties and plot sector coefficients
ndp_models_complete9 %>%
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete9) %>%
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete9) %>%
  #add conservative models to liberal models
  bind_rows(., bloc_models_complete9) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>%
  #only keep the coefficients for sector and union
  filter(term=="sector"| term=="union_both") %>%
  mutate(term=Recode(term, "'sector'='Sector'; 'union_both'='Union'")) %>%
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Sector in Quebec on PV", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("black", "blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save
ggsave(here("Plots", "M9_quebec_sector_all_parties.png"))

# #-------------------------------------------------------------------------------------------------
# Abortion variable commented out below but is on script 6_ces_out - usable once 84-93 abortion variable is added

# #### M10 Blais Replication Extension (with Abortion) ####
# table(ces$election, ces$abortion)
# 
# ces %>% 
#   #filter out elections missing key variables
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1997 & election!=2000 & election!=2004 & election!=2006 & election!=2008 & election!=2011 & election!=2015 & election!=2019) %>%
#   #Nest by election
#   nest(variables=-election) %>% 
#   #create the model variable
#   mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+abortion, data=x)),
#          #tidy that model variable 
#          tidied=map(model, tidy), 
#          #add party name variable
#          vote=rep('NDP', nrow(.)))->ndp_models_complete10
# 
# ces %>% 
#   #filter out elections missing key variables
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1997 & election!=2000 & election!=2004 & election!=2006 & election!=2008 & election!=2011 & election!=2015 & election!=2019) %>%
#   #Nest by election
#   nest(variables=-election) %>% 
#   #create the model variable
#   mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+abortion, data=x)), 
#          #tidy that model variable 
#          tidied=map(model, tidy),
#          #add party name variable
#          #this is still in the mutate function above
#          #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
#          vote=rep('Conservative', nrow(.))  
#   )->conservative_models_complete10
# 
# ces %>% 
#   #filter out elections missing key variables
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1997 & election!=2000 & election!=2004 & election!=2006 & election!=2008 & election!=2011 & election!=2015 & election!=2019) %>%
#   #Nest by election
#   nest(variables=-election) %>% 
#   #create the model variable
#   mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+abortion, data=x)), 
#          #tidy that model variable 
#          tidied=map(model, tidy),
#          #add party name variable
#          vote=rep('Liberal', nrow(.))  
#   )->liberal_models_complete10
# 
# stargazer(ndp_models_complete10$model, 
#           type="html", 
#           out=here("Tables", "NDP_Models_abortion_1984_1993_8.html"),
#           column.labels=c("1984", "1988", "1993"), 
#           #set the cutoffs for one star to be 0.05
#           star.cutoffs=c(0.05), 
#           title="NDP Models 1984-1993 with Abortion",
#           #print some notes to show when the table is constructed
#           notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# stargazer(liberal_models_complete10$model, 
#           type="html", 
#           out=here("Tables", "liberal_Models_abortion_1984_1993_8.html"),
#           column.labels=c("1984", "1988", "1993"),  
#           #set the cutoffs for one star to be 0.05
#           star.cutoffs=c(0.05), 
#           title="Liberal Models 1984-1993 with Abortion",
#           #print some notes to show when the table is constructed
#           notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# stargazer(conservative_models_complete10$model, 
#           type="html", 
#           out=here("Tables", "conservative_Models_abortion_1984_1993_8.html"),
#           column.labels=c("1984", "1988", "1993"), 
#           #set the cutoffs for one star to be 0.05
#           star.cutoffs=c(0.05), 
#           title="Conservative Models 1984-1993 with Abortion",
#           #print some notes to show when the table is constructed
#           notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# #### Blais Plots ####
# # Set theme
# theme_set(theme_bw())
# ndp_models_complete10 %>% 
#   unnest(tidied) %>% 
#   filter(term=="abortion") %>% 
#   ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Abortion on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
# ggsave(here("Plots", "abortion_ndp_1968_2019_10.png"))
# 
# liberal_models_complete10 %>% 
#   unnest(tidied) %>% 
#   filter(term=="abortion") %>% 
#   ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Abortion on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
# ggsave(here("Plots", "abortion_liberal_1968_2019_10.png"))
# 
# conservative_models_complete8 %>% 
#   unnest(tidied) %>% 
#   filter(term=="abortion") %>% 
#   ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Abortion on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
# ggsave(here("Plots", "abortion_conservative_1968_2019_10.png"))
# 
# #Join all parties and plot sector coefficients
# ndp_models_complete10 %>% 
#   #bind liberal models to ndp models
#   bind_rows(., liberal_models_complete10) %>% 
#   #add conservative models to liberal models
#   bind_rows(., conservative_models_complete10) %>%
#   #unnest the tidied models so the spread out one row for each coefficient
#   unnest(tidied) %>% 
#   #only keep the coefficients for sector and abortion
#   filter(term=="sector"| term=="abortion") %>% 
#   mutate(term=Recode(term, "'abortion'='Abortion'; 'sector'='Sector'")) %>% 
#   #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
#   #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
#   ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Sector")))+
#   #make it a point plot
#   geom_point()+
#   #add titles
#   labs(title="OLS Coefficients of Abortion on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
#   #add errorbars, width=0 so that it is just a vertical line
#   #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
#   geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
#   #Set the y-axis limits to -0.2 and 0.2
#   ylim(c(-0.2,0.2))+
#   #modify the color scale specifying the colors of the points to be blue red and orange
#   scale_color_manual(values=c("blue", "red", "orange"))+
#   #panel this by vote with one panel per party
#   facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
# 
# #save 
# ggsave(here("Plots", "M10_abortion_all_parties.png"))
# 
# #------------------------------------------------------------------------------------------------
# #### M11 Blais Replication Extension (with Abortion:Sector interaction) ####
# table(ces$election, ces$abortion)
# 
# ces %>% 
#   #filter out elections missing key variables
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1997 & election!=2000 & election!=2004 & election!=2006 & election!=2008 & election!=2011 & election!=2015 & election!=2019) %>%
#   #Nest by election
#   nest(variables=-election) %>% 
#   #create the model variable
#   mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+abortion+abortion:sector, data=x)),
#          #tidy that model variable 
#          tidied=map(model, tidy), 
#          #add party name variable
#          vote=rep('NDP', nrow(.)))->ndp_models_complete11
# 
# ces %>% 
#   #filter out elections missing key variables
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1997 & election!=2000 & election!=2004 & election!=2006 & election!=2008 & election!=2011 & election!=2015 & election!=2019) %>%
#   #Nest by election
#   nest(variables=-election) %>% 
#   #create the model variable
#   mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+abortion+abortion:sector, data=x)), 
#          #tidy that model variable 
#          tidied=map(model, tidy),
#          #add party name variable
#          #this is still in the mutate function above
#          #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
#          vote=rep('Conservative', nrow(.))  
#   )->conservative_models_complete11
# 
# ces %>% 
#   #filter out elections missing key variables
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1997 & election!=2000 & election!=2004 & election!=2006 & election!=2008 & election!=2011 & election!=2015 & election!=2019) %>%
#   #Nest by election
#   nest(variables=-election) %>% 
#   #create the model variable
#   mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector+abortion+abortion:sector, data=x)), 
#          #tidy that model variable 
#          tidied=map(model, tidy),
#          #add party name variable
#          vote=rep('Liberal', nrow(.))  
#   )->liberal_models_complete11
# 
# stargazer(ndp_models_complete11$model, 
#           type="html", 
#           out=here("Tables", "NDP_Models_abortion_int_1968_2019_11.html"),
#           column.labels=c("1984", "1988", "1993"), 
#           #set the cutoffs for one star to be 0.05
#           star.cutoffs=c(0.05), 
#           title="NDP Models 1984-1993 Abortion:Sector interaction",
#           #print some notes to show when the table is constructed
#           notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# stargazer(liberal_models_complete11$model, 
#           type="html", 
#           out=here("Tables", "liberal_Models_abortion_int_1968_2019_11.html"),
#           column.labels=c("1984", "1988", "1993"),  
#           #set the cutoffs for one star to be 0.05
#           star.cutoffs=c(0.05), 
#           title="Liberal Models 1984-1993 Abortion:Sector interaction",
#           #print some notes to show when the table is constructed
#           notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# stargazer(conservative_models_complete11$model, 
#           type="html", 
#           out=here("Tables", "conservative_Models_abortion_int_1968_2019_11.html"),
#           column.labels=c("1984", "1988", "1993"), 
#           #set the cutoffs for one star to be 0.05
#           star.cutoffs=c(0.05), 
#           title="Conservative Models 1984-1993 Abortion:Sector interaction",
#           #print some notes to show when the table is constructed
#           notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# #### Blais Plots ####
# # Set theme
# theme_set(theme_bw())
# ndp_models_complete11 %>% 
#   unnest(tidied) %>% 
#   filter(term=="abortion:sector") %>% 
#   ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Abortion:Sector on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
# ggsave(here("Plots", "abortion_sector_ndp_1968_2019_11.png"))
# 
# liberal_models_complete11 %>% 
#   unnest(tidied) %>% 
#   filter(term=="abortion:sector") %>% 
#   ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Abortion:Sector on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
# ggsave(here("Plots", "abortion_sector_liberal_1968_2019_11.png"))
# 
# conservative_models_complete11 %>% 
#   unnest(tidied) %>% 
#   filter(term=="abortion:sector") %>% 
#   ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Abortion:Sector on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
# ggsave(here("Plots", "abortion_sector_conservative_1968_2019_11.png"))
# 
# #Join all parties and plot sector coefficients
# ndp_models_complete9 %>% 
#   #bind liberal models to ndp models
#   bind_rows(., liberal_models_complete9) %>% 
#   #add conservative models to liberal models
#   bind_rows(., conservative_models_complete9) %>%
#   #unnest the tidied models so the spread out one row for each coefficient
#   unnest(tidied) %>% 
#   #only keep the coefficients for abortion:sector and sector
#   filter(term=="abortion:sector"| term=="abortion") %>% 
#   #  mutate(term=Recode(term, "'abortion:sector'='Abortion:Sector'; 'sector'='Sector'")) %>% 
#   #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
#   #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
#   ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Sector")))+
#   #make it a point plot
#   geom_point()+
#   #add titles
#   labs(title="OLS Coefficients of Abortion:Sector on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
#   #add errorbars, width=0 so that it is just a vertical line
#   #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
#   geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
#   #Set the y-axis limits to -0.2 and 0.2
#   ylim(c(-0.2,0.2))+
#   #modify the color scale specifying the colors of the points to be blue red and orange
#   scale_color_manual(values=c("blue", "red", "orange"))+
#   #panel this by vote with one panel per party
#   facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
# 
# #save 
# ggsave(here("Plots", "M11_abortion_sector_all_parties.png"))

#------------------------------------------------------------------------------------------------
#### M12 Blais Replication Extension (including 2019) - Sector sub-sample####
table(ces$election, ces$sector)

ces %>% 
  #filter out elections missing key variables and only keep Sector
  filter(sector==1 & election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete12

ces %>% 
  #filter out elections missing key variables and only keep Sector
  filter(sector==1 & election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete12

ces %>% 
  #filter out elections missing key variables and only keep Sector
  filter(sector==1 & election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete12

stargazer(ndp_models_complete12$model, 
          type="html", 
          out=here("Tables", "NDP_Models_Sector_subsample_1968_2019_12.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 Sector subsample",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete12$model, 
          type="html", 
          out=here("Tables", "liberal_Models_Sector_subsample_1968_2019_12.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 Sector subsample",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete12$model, 
          type="html", 
          out=here("Tables", "conservative_Models_Sector_subsample_1968_2019_12.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 Sector subsample",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete12 %>% 
  unnest(tidied) %>% 
  filter(term=="union_both") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Union in Public Sector on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_subsample_ndp_1968_2019_12.png"))

liberal_models_complete12 %>% 
  unnest(tidied) %>% 
  filter(term=="union_both") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Union in Public Sector on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_subsample_liberal_1968_2019_12.png"))

conservative_models_complete12 %>% 
  unnest(tidied) %>% 
  filter(term=="union_both") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Union in Public Sector on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_subsample_conservative_1968_2019_12.png"))

#Join all parties and plot union coefficients
ndp_models_complete12 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete12) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete12) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for degree and union
  filter(term=="union_both"| term=="degree") %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'union_both'='Union'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Union in Public Sector on PV", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M12_public_sector_subsample_all_parties.png"))

#-------------------------------------------------------------------------------------------------
#### M13 Blais Replication Extension (including 2019) (ROC only) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete13

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete13

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete13

stargazer(ndp_models_complete13$model, 
          type="html", 
          out=here("Tables", "NDP_Models_ROC_1968_2019_13.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 Rest of Canada", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete13$model, 
          type="html", 
          out=here("Tables", "liberal_Models_ROC_1968_2019_13.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 Rest of Canada", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete13$model, 
          type="html", 
          out=here("Tables", "conservative_Models_ROC_1968_2019_13.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 Rest of Canada", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete13 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "ROC_sector_ndp_1968_2019_13.png"))

liberal_models_complete13 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "ROC_sector_liberal_1968_2019_13.png"))

conservative_models_complete13 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "ROC_sector_conservative_1968_2019_13.png"))

#Join all parties and plot sector coefficients
ndp_models_complete13 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete13) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete13) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for sector and union
  filter(term=="sector"| term=="union_both") %>% 
  mutate(term=Recode(term, "'sector'='Sector'; 'union_both'='Union'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Public Sector Rest of Canada on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M13_public_sector_ROC_all_parties.png"))

#-------------------------------------------------------------------------------------------------
#### M14 Blais Replication Extension (including 2019) (ROC only)& no Degree or Income) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete14

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete14

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete14

stargazer(ndp_models_complete14$model, 
          type="html", 
          out=here("Tables", "NDP_Models_ROC_1968_2019_14.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 Rest of Canada", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete14$model, 
          type="html", 
          out=here("Tables", "liberal_Models_ROC_1968_2019_14.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 Rest of Canada", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete14$model, 
          type="html", 
          out=here("Tables", "conservative_Models_ROC_1968_2019_14.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 Rest of Canada", 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete14 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "ROC_sector_ndp_1968_2019_14.png"))

liberal_models_complete14 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "ROC_sector_liberal_1968_2019_14.png"))

conservative_models_complete14 %>% 
  unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "ROC_sector_conservative_1968_2019_14.png"))

#Join all parties and plot sector coefficients
ndp_models_complete14 %>% 
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete14) %>% 
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete14) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>% 
  #only keep the coefficients for sector and union
  filter(term=="sector"| term=="union_both") %>% 
  mutate(term=Recode(term, "'sector'='Sector'; 'union_both'='Union'")) %>% 
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Public Sector Rest of Canada on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save 
ggsave(here("Plots", "M14_public_sector_ROC_all_parties.png"))

#------------------------------------------------------------------------------------------------

#### M15 Blais Replication Extension (including 2019)(Quebec:Sector interaction) ####

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+quebec:sector+sector, data=x)),
         #tidy that model variable 
         tidied=map(model, tidy), 
         #add party name variable
         vote=rep('NDP', nrow(.)))->ndp_models_complete15

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+quebec:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         #this is still in the mutate function above
         #It makes the variable vote equal to the repetition rep() of the term 'conservaive', the number of rows nrow() of the dataframe that is fed to it. 
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete15

ces %>% 
  #filter out elections missing key variables
  filter(election!=1965 & election!=1972) %>%
  #Nest by election
  nest(variables=-election) %>% 
  #create the model variable
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+degree+income+quebec:sector+sector, data=x)), 
         #tidy that model variable 
         tidied=map(model, tidy),
         #add party name variable
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete15

stargazer(ndp_models_complete15$model, 
          type="html", 
          out=here("Tables", "NDP_Models_quebec_int_1968_2019_15.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="NDP Models 1968-2019 with Quebec:Sector interaction",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete15$model, 
          type="html", 
          out=here("Tables", "liberal_Models_quebec_int_1968_2019_15.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Liberal Models 1968-2019 with Quebec:Sector interaction",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete15$model, 
          type="html", 
          out=here("Tables", "conservative_Models_quebec_int_1968_2019_15.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          title="Conservative Models 1968-2019 with Quebec:Sector interaction",
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete15 %>% 
  unnest(tidied) %>% 
  filter(term=="quebec:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Quebec:Sector interaction on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "quebec_sector_ndp_1968_2019_15.png"))

liberal_models_complete15 %>% 
  unnest(tidied) %>% 
  filter(term=="quebec:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Quebec:Sector interaction on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "quebec_sector_liberal_1968_2019_15.png"))

conservative_models_complete15 %>% 
  unnest(tidied) %>% 
  filter(term=="quebec:sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Quebec:Sector interaction on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "quebec_sector_conservative_1968_2019_15.png"))

#Join all parties and plot sector coefficients
ndp_models_complete15 %>%
  #bind liberal models to ndp models
  bind_rows(., liberal_models_complete15) %>%
  #add conservative models to liberal models
  bind_rows(., conservative_models_complete15) %>%
  #unnest the tidied models so the spread out one row for each coefficient
  unnest(tidied) %>%
  #only keep the coefficients for quebec:sector and sector
  filter(term=="quebec:sector"| term=="sector") %>%
  #  mutate(term=Recode(term, "'quebec:sector'='Quebec:Sector'; 'sector'='Sector'")) %>%
  #plot x=election, y=estimate, differentiate the parties by color, and set alpha (transparency) to vary by the variable term
  #I'm setting the union_both category of term to be the reference category, this will make it transparaent. I only figured this out after running this once.
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "sector")))+
  #make it a point plot
  geom_point()+
  #add titles
  labs(title="OLS Coefficients of Quebec:Sector on Party Vote", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #add errorbars, width=0 so that it is just a vertical line
  #ymin =estimate -1.96*standard aerror, ymax = estimate+1.96* standard error
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  #Set the y-axis limits to -0.2 and 0.2
  ylim(c(-0.2,0.2))+
  #modify the color scale specifying the colors of the points to be blue red and orange
  scale_color_manual(values=c("blue", "red", "orange"))+
  #panel this by vote with one panel per party
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

#save
ggsave(here("Plots", "M15_quebec_sector_int_all_parties.png"))

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

####Vote Shares####

# Percent of NDP Voters in Public Sector
ces %>% 
  group_by(election, vote, sector) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(sector==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="NDP Voters in Public Sector")
#save
ggsave(here("Plots", "NDP_Voters_Public_Sector_Percent.png"))

# Percent of NDP Voters Working Class
ces %>% 
  group_by(election, vote, working_class) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(working_class==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="NDP Voter % that are Working Class")
#save
ggsave(here("Plots", "NDP_Voters_Working_Class_Percent.png"))

# Percent of NDP Voters Union Households
ces %>% 
  group_by(election, vote, union) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(union_both==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="NDP Voter % that are Union Households")
#save
ggsave(here("Plots", "NDP_Voters_Union_Percent.png"))

# Percent of NDP Voters Low Income
ces %>% 
  group_by(election, vote, income) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="NDP Voter % that are Low Income")
#save
ggsave(here("Plots", "NDP_Voters_Low_Income_Percent.png"))

# Percent of NDP Voters with Degree
ces %>% 
  group_by(election, vote, degree) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(degree==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="NDP Voter % with a Degree")
#save
ggsave(here("Plots", "NDP_Voters_Degree_Percent.png"))

ces %>% 
  group_by(election, vote, degree) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(degree==1 & vote==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Liberal Voter % with a Degree")
#save
ggsave(here("Plots", "Lib_Voters_Degree_Percent.png"))

ces %>% 
  group_by(election, vote, degree) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(degree==1 & vote==2) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Conservative Voter % with a Degree")
#save
ggsave(here("Plots", "Con_Voters_Degree_Percent.png"))

#-------------------------------------------------------------------------------------------------

## What Share of Private and Public Sector Union Members vote NDP
ces %>% 
  group_by(election, union_both, sector, ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(sector)==F) %>% 
  filter(is.na(union_both)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(union_both==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(sector)))+geom_col(position="dodge")+labs(title="Share of Public and Private Sector Union respondents voting NDP")
#save
ggsave(here("Plots", "Union_Voting_NDP_by_Sector.png"))

ces %>% 
  group_by(election, union_both, sector, liberal) %>% 
  summarize(n=n()) %>% 
  filter(is.na(sector)==F) %>% 
  filter(is.na(union_both)==F) %>% 
  filter(is.na(liberal)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(union_both==1) %>% 
  filter(liberal==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(sector)))+geom_col(position="dodge")+labs(title="Share of Public and Private Sector Union respondents voting Liberal")
#save
ggsave(here("Plots", "Union_Voting_Lib_by_Sector.png"))

ces %>% 
  group_by(election, union_both, sector, conservative) %>% 
  summarize(n=n()) %>% 
  filter(is.na(sector)==F) %>% 
  filter(is.na(union_both)==F) %>% 
  filter(is.na(conservative)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(union_both==1) %>% 
  filter(conservative==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(sector)))+geom_col(position="dodge")+labs(title="Share of Public and Private Sector Union respondents voting Conservative")
#save
ggsave(here("Plots", "Union_Voting_Con_by_Sector.png"))

## What Share of Private and Public Sector Degree holders vote NDP
ces %>% 
  group_by(election, degree, sector, ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(sector)==F) %>% 
  filter(is.na(degree)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(degree==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(sector)))+geom_col(position="dodge")+labs(title="Share of Public and Private Sector Degree holders voting NDP")
#save
ggsave(here("Plots", "Degree_voting_NDP_by_Sector.png"))

## What Share of Private and Public Sector Females vote NDP
ces %>% 
  group_by(election, female, sector, ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(sector)==F) %>% 
  filter(is.na(female)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(female==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(sector)))+geom_col(position="dodge")+labs(title="Share of Public and Private Sector Females voting NDP")
#save
ggsave(here("Plots", "Female_voting_NDP_by_Sector.png"))

#-------------------------------------------------------------------------------------------------

# What share of Public Sector has voted NDP
ces %>% 
  group_by(election, sector, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(sector==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Public Sector Voting NDP")
#save
ggsave(here("Plots", "Public_Sector_voting_NDP.png"))

# What share of union membership has voted NDP
ces %>% 
  group_by(election, union_both, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(union_both==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Union Members Voting NDP")
#save
ggsave(here("Plots", "Union_voting_NDP.png"))

# What share of Degree holders has voted NDP
ces %>% 
  group_by(election, degree, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(degree==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Degree holders Voting NDP")
#save
ggsave(here("Plots", "Degree_voting_NDP.png"))

# What share of Working Class has voted NDP
ces %>% 
  group_by(election, working_class, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(working_class==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Working Class Voting NDP")
#save
ggsave(here("Plots", "Working_class_voting_NDP.png"))

# What share of Low Income has voted NDP
ces %>% 
  group_by(election, income, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Low Income Voting NDP")
#save
ggsave(here("Plots", "Low_Income_voting_NDP.png"))

# What share of Females voted NDP
ces %>% 
  group_by(election, female, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(female==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Females Voting NDP")
#save
ggsave(here("Plots", "Female_voting_NDP.png"))

#-------------------------------------------------------------------------------------------------
