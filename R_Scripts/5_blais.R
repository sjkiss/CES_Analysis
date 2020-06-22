#Replicating 'The public-private sector cleavage in North America (Blais et al 1990)'
#Run master file to load up data

#Instead of typing Recode everytime, we can just load the car library here
library(car)

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
table(ces$election, ces$sector, useNA = "ifany")


#Info: Missing variable in the following elections:
#Sector 1965 and 1972
#Occupation 2000 and 2019

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
  filter(election==1968|
           election==1974|
           election==1979|
           election==1980|
           election==1984) %>% 
select(non_charter_language, working_class, no_religion, sector, catholic, union_both, size) %>% 
summary()
##Count missing values
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  View()

-------------------------------------------------------------------------------------------------------------

#### Blais replication NDP Models ####
table(ces$sector, ces$election)
ces %>% 
  group_by(election) %>% 
  filter(election==1968|
           election==1974|
           election==1979|
           election==1980|
           election==1984) %>% 
  nest(variables=-election) %>% 
  mutate(ndp=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+size+sector, data=x)),
         tidied=map(ndp, tidy))->ndp_models
table(ces$election, ces$sector)
library(stargazer)
#stargazer(blais_models$ndp, type="text")
stargazer(ndp_models$ndp, 
          #print out html file
          type="html", 
          #file path where table should be printed and file name
          out=here("Tables", "M1_Blais_replication_ndp.html"), 
          #label the model columns
          column.labels=c("1968", "1974", "1979", "1980", "1984"), 
          #set the cutoffs for one star to be 0.05
        #  star.cutoffs=c(0.05), 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
#### Blais replication Conservative models #### 
table(ces$sector, ces$election)
ces %>% 
  group_by(election) %>% 
  filter(election==1968|
           election==1974|
           election==1979|
           election==1980|
           election==1984) %>% 
  nest(variables=-election) %>% 
  mutate(con=map(variables, function(x) lm(conservative~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+size+sector, data=x)),
         tidied=map(con, tidy))->con_models

library(stargazer)
#stargazer(blais_models$con, type="text")
stargazer(con_models$con, 
          #print out html file
          type="html", 
          #file path where table should be printed and file name
          out=here("Tables", "M1_Blais_replication_con.html"), 
          #label the model columns
          column.labels=c("1968", "1974", "1979", "1980", "1984"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
#### Blais Replication Liberal Models ####
table(ces$sector, ces$election)
ces %>% 
  group_by(election) %>% 
  filter(election==1968|
           election==1974|
           election==1979|
           election==1980|
           election==1984) %>% 
  nest(variables=-election) %>% 
  mutate(liberal=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+size+sector, data=x)),
         tidied=map(liberal, tidy))->liberal_models

library(stargazer)
stargazer(liberal_models$liberal, 
          #print out html file
          type="html", 
          #file path where table should be printed and file name
          out=here("Tables", "M1_Blais_replication_liberal.html"), 
          #label the model columns
          column.labels=c("1968", "1974", "1979", "1980", "1984"), 
          #set the cutoffs for one star to be 0.05
        #  star.cutoffs=c(0.05), 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Blais Replication Extension ####
table(ces$election, ces$sector)
ces %>% 
  #filter out elections missing key variables
   filter(election!=1965 & election!=2019&election!=1972) %>%
  #Nest by election
   nest(variables=-election) %>% 
  #create the model variable
mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)),
       #tidy that model variable 
       tidied=map(model, tidy), 
       #add party name variable
       vote=rep('NDP', nrow(.)))->ndp_models_complete
ces %>% 
    #filter out elections missing key variables
   filter(election!=1965 & election!=2019&election!=1972) %>%
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
       )->conservative_models_complete

ces %>% 
    #filter out elections missing key variables
   filter(election!=1965 & election!=2019&election!=1972) %>%
      #Nest by election
   nest(variables=-election) %>% 
     #create the model variable
mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+sector, data=x)), 
                     #tidy that model variable 
       tidied=map(model, tidy),
                     #add party name variable
            vote=rep('Liberal', nrow(.))  
       )->liberal_models_complete

stargazer(ndp_models_complete$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1968_2015.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1968_2015.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))


stargazer(conservative_models_complete$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1968_2015.html"),
          column.labels=c("1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015"), 
          #set the cutoffs for one star to be 0.05
          star.cutoffs=c(0.05), 
          #print some notes to show when the table is constructed
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Check for massive drop in missing values in mid 2000s ####
ces%>% 
  filter(election==2000) %>% 
  select(no_religion, region2, catholic, sector, female, non_charter_language, union_both) %>% 
  summary()
#### Check for massive drop in missing values in mid 2000s ####
ces%>% 
  filter(election==2004) %>% 
  select(no_religion, region2, catholic, sector, female, non_charter_language, union_both, working_class) %>% 
  summary()
ces%>% 
  filter(election==2006) %>% 
  select(no_religion, region2, catholic, sector, female, non_charter_language, union_both) %>% 
  summary()
ces%>% 
  filter(election==2008) %>% 
  select(no_religion, region2, catholic, sector, female, non_charter_language, union_both) %>% 
  summary()

ces%>% 
  filter(election==2011) %>% 
  select(no_religion, region2, catholic, sector, female, non_charter_language, union_both) %>% 
  summary()

look_for(ces0411, "union")

#### Blais Plots ####
# Set theme
theme_set(theme_bw())
ndp_models_complete %>% 
unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on NDP Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
  ggsave(here("Plots", "sector_ndp_1968_2015.png"))
liberal_models_complete %>% 
unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on Liberal Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
    ggsave(here("Plots", "sector_liberal_1968_2015.png"))
conservative_models_complete %>% 
unnest(tidied) %>% 
  filter(term=="sector") %>% 
  ggplot(., aes(x=election, y=estimate))+geom_point()+labs(title="OLS Coefficients of Public Sector on Conservative Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))
ggsave(here("Plots", "sector_conservative_1968_2015.png"))
ndp_models_complete
#Join all parties and plot sector coefficients
ndp_models_complete %>% 
  bind_rows(., liberal_models_complete) %>% 
  bind_rows(., conservative_models_complete) %>% 
  unnest(tidied) %>% 
  filter(term=="sector"| term=="union_both") %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "union_both")))+geom_point()+labs(title="OLS Coefficients of Public Sector on Party Vote")+geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+ylim(c(-0.2,0.2))+scale_color_manual(values=c("blue", "red", "orange"))+facet_wrap(~vote)

ggsave(here("Plots", "public_sector_all_parties.png"))