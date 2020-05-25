#Can we use panel respondents
##load recoded ces files
load("Data/recoded_cesdata.Rdata")
library(tidyverse)
library(labelled)
library(here)
##### SPLITTING THE 1979-1980 FILE
table(ces7980$male80)
names(ces7980)
names(ces93)
tail(names(ces0411))
ces7980 %>% 
  filter(V4002==1)->ces79
ces7980 %>% 
  filter(V4008==1)->ces80
options(max.print=1500)
names(ces80)
names(ces7980)
### We have all of the demographic variables from the ces79 questions stored in the ces80 data set. 
##Show this
table(ces80$male)
### Show that they are the same for the demogrphics
table(ces80$male, ces80$male80)
table(ces80$region, ces80$region80)
##but they are different for political variables for obvious reasons. Demographics didn't change much but vote changed quite a bit.
table(ces80$vote, ces80$vote80)
##We just need to turn the variables that end with 80 into regularly named variables.
ces80 %>% 
  select(male=male80, region=region80, quebec=quebec80, age=age80, language=language80, party_id=party_id80, vote=vote80, union, union_both, degree, employment, sector, income, occupation)->ces80
names(ces80)
#####SPLITTING THE 04-11 FILE
ces0411$survey
ces0411 %>% 
  mutate(panel=case_when(
 str_detect(survey, "Panel")~1  ,
TRUE ~ 0
  ))->ces0411

#add vote variable,
table(ces0411$survey, ces0411$panel)

ces0411 %>% 
  filter(str_detect(survey, "PES04"))->ces04
ces0411 %>% 
  filter(str_detect(survey, "PES06"))->ces06
ces0411 %>% 
  filter(str_detect(survey, "PES08"))->ces08
ces0411 %>% 
  filter(str_detect(survey, "PES11"))->ces11

ces04$ndp<-Recode(ces04$vote04, "3=1; NA=NA; else=0")
ces06$ndp<-Recode(ces06$vote06, "3=1; NA=NA; else=0")
ces08$ndp<-Recode(ces08$vote08, "3=1; NA=NA; else=0")
ces11$ndp<-Recode(ces11$vote11, "3=1; NA=NA; else=0")
##are panel respondents more likely to vote NDP. 
mod04<-glm(ndp~panel, family="binomial", data=ces04)
mod06<-glm(ndp~panel, family="binomial", data=ces06)
mod08<-glm(ndp~panel, family="binomial", data=ces08)
mod11<-glm(ndp~panel, family="binomial", data=ces11)

mod.list<-list(mod04, mod06, mod08, mod11)

map(mod.list, summary)
#In 2008 slightly less likely and in 2011 more likely 

