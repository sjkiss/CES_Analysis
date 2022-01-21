##load recoded ces files
load("Data/recoded_cesdata.Rdata")

library(tidyverse)
library(labelled)
library(here)
library(car)
ces0411
ces0411$market_liberalism04

names(ces88)
names(ces93)
names(ces97)
### Checks
nrow(ces74)==2562 #TRUE
table(ces68$var323, ces68$var379)
table(ces68$var379, ces68$union_both)
table(ces74$size)
table(ces68$var379)
ces19phone$immigration
look_for(ces68, "marital")
#check for missing occupations 
ces15phone %>% 
  filter(is.na(PES15_NOC)==F&is.na(occupation)==T) %>% 
  select(PES15_NOC, occupation) %>% 
  print(n=264)

ces19web %>% 
  filter(is.na(NOC)==F&is.na(occupation)==T) %>% 
  select(NOC, occupation)

#### Check the MIP Problems have been added ####
ces19phone$mip

#### SPLITTING THE 1979-1980 FILE ####
table(ces7980$male80)
names(ces7980)
names(ces93)
tail(names(ces0411))

library(labelled)
look_for(ces74, "respondent")
look_for(ces7980, "respondent")
look_for(ces74, "respondent")
look_for(ces7980, "filter")

#Get a summary of V9 sector and V4020
ces7980 %>% 
  select(V9, sector, V4020) %>% 
  summary()
#Gt a summary of ces74$V2
ces74%>% 
  select(V2) %>% 
  summary()

#### CES74 Sector ####

###### This code section creates a ces74 data frame from the ces74-79-80 panel survey
###### It does this because sector was only asked of non-manual respondents in ces74, but it was asked of everybody in ces79
###### Blais took the responses for the 79 question for the ces74 respondents who were reinterviewed in ces79 and made it to be their 74 response. So he went backward. 
###### All our other demographic variables were created from the pure cross-sectional survey, so I didn't want to waste all that work. 
###### When we get close to fully being able to replicate Blais, we can execute this code to create ces74 Until then we keep it off. 

# table(ces7980$sector)
# table(ces74$V2)
# data("ces7980")
# ces7980 %>%
#   #Select V9, sector and panel
#   #V9 is joint ces74 and ces7990 respondent id, sector is sector variablef rom ces7980 and 
#   #V4020 is filter variable 1= ces7980 respondents who filled out ces74 surve
#   select(V9, sector, V4020) %>%
#   #inner join (return all rows from 7980 that have values in V9 that match in ces74 on V2)
#   inner_join(., ces74, by=c("V9"="V2")) ->ces74.out
# #ces74.out is a reduced ces7980 dataframe; it now only includes ces7980 respondents who respondend to ces74 survey
# tail(names(ces74.out))
# table(ces7980$V9)
# #how many respondents in ces74
# nrow(ces74.out)
# #sector.x is the sector variable from 7980; should be a lot larger than sector.y
# table(ces74.out$sector.x)
# #setor.7 is the sector variable from ces74; only asked of non-manual respondents, see note in Blais (1990)
# #should be a lot smaller than sector.x
# table(ces74.out$sector.y)
# #The technical documentation says that there are 1295 CES74 panel respondents reinterviewed in CES79
# ## 1298 is close, but not exact
# table(ces74.out$V4020)#
# #There are 3 people who are not part of the ces74-79 panel that got caught with the same respondent IDS
# ces74.out %>%
#   #Filter in respondents who have a value of 1 on the 74-79 panel filter
#   filter(V4020==1)->ces74.out
# 
# #take ces74.out
# ces74.out %>%
#   #delete sector.y which is the sector variable from the pure ces74 study
#   select(-sector.y) %>%
#   #sector sector.x which is the sector variable from ces7980 to be sector to match all the other variables
#   rename(sector=sector.x)->ces74.out
# ces74.out$sector
# nrow(ces74.out)
# #rename the whole ces74.out data frame to be ces74; old ces74 will now be gone. 
# ces74<-ces74.out
# 
# table(ces74$sector)

#Seprate ces79 and ces80 to two separate files
ces7980 %>% 
  filter(V4002==1)->ces79

ces7980 %>% 
  filter(V4008==1)->ces80

options(max.print=1500)
names(ces80)
names(ces7980)
### We have all of the demographic variables from the ces79 questions stored in the ces80 data set. 
##Show this
ces7980$male
ces7980$male80
table(ces80$male)
### Show that they are the same for the demographics
table(ces80$male, ces80$male80)
table(ces80$region, ces80$region80)
##but they are different for political variables for obvious reasons. Demographics didn't change much but vote changed quite a bit.
table(ces80$vote, ces80$vote80)
##We just need to turn the variables that end with 80 into regularly named variables.

ces80 %>% 
  select(male=male80, region=region80, quebec=quebec80, age=age80, language=language80, party_id=party_id80, vote=vote80, union, union_both, degree, employment, sector, income, occupation, occupation3, religion, non_charter_language, size, ideology, turnout, redistribution, market_liberalism, immigration_rates, traditionalism2, mip=mip80)->ces80
names(ces80)

### Filter out ces93 referendum respondents only by removing missing values from RTYPE4 (indicates ces93 respondents)
  ces93[!is.na(ces93$RTYPE4), ] -> ces93
           
#####SPLITTING THE 04-11 FILE

### STEP 1

### The next thing to do would be to split the 2004-2011 file into separate files
### I don't think we want to mess around with the panel data
### I made a survey variable when we started
table(ces0411$survey)
names(ces0411)
##This code groups by the survey variable
ces0411 %>% 
  #make gtroups
  group_by(survey) %>% 
  #summarize those gtroups by counting
  summarize(n=n()) %>% 
  #arrange the data frame in descending order
  arrange(desc(n)) %>% 
  #print all the rows
  print(n=69)

#### STEP 2 FILTERING

#Panels not added but the rest have been
####CES04 ####
 # ces0411 %>% 
 #   filter(survey=="CPS04 PES04 MBS04" | survey=="CPS04 PES04" | survey=="CPS04 PES04 MBS04 CPS06 PES06" | survey=="CPS04 PES04 CPS06 PES06" | survey=="CPS04 PES04 CPS06" | survey=="CPS04 PES04 MBS04 CPS06" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11 WBS11")->ces04

# Do not use Panel respondents
# This way returns anyone who filled out PES04 and is not a Panel respondent
# This way we get 100 extra respondents
# ces0411 %>%
#  filter(str_detect(ces0411$survey, "PES04")&str_detect(ces0411$survey, "Panel", negate=T))->ces04
#Use Panel Respondents
ces0411 %>%
 filter(str_detect(ces0411$survey, "PES04"))->ces04

# Do the union checks
table(ces0411$union04)
table(ces0411$union_both04)#
table(ces04$union_both04)
nrow(ces04)
table( as_factor(ces04$ces04_CPS_S6A), as_factor(ces04$ces04_CPS_S6B), useNA = "ifany")
table(as_factor(ces04$union_both04), as_factor(ces04$ces04_CPS_S6A), useNA = "ifany")
table(as_factor(ces04$union_both04), as_factor(ces04$ces04_CPS_S6B), useNA = "ifany")


#### CES06 ####
 # ces0411 %>%  
 #   filter(survey=="CPS06 PES06" | survey=="CPS04 PES04 MBS04 CPS06 PES06" | survey=="CPS04 PES04 CPS06 PES06" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11 WBS11")->ces06
 # nrow(ces06)
# Do not use Panel respondents
# ces0411 %>%
#  filter(str_detect(ces0411$survey, "PES06")&str_detect(ces0411$survey, "Panel", negate=T))->ces06

## Use Panel Respondents
ces0411 %>%
 filter(str_detect(ces0411$survey, "PES06"))->ces06
nrow(ces06)
#### CES08
# Do not use Panel respondents
# ces0411 %>% 
#   filter(str_detect(ces0411$survey, "PES08")&str_detect(ces0411$survey, "Panel", negate=T))->ces08
## Use Panel Respondents
### CES08
ces0411 %>% 
  filter(str_detect(ces0411$survey, "PES08"))->ces08

#### CES11 ####
# ces0411 %>% 
#   filter(survey=="New RDD_2011 CPS11 PES11" | survey=="New RDD_2011 CPS11" | survey=="New RDD_2011 CPS11 PES11 MBS11" | survey=="New RDD_2011 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11 WBS11")->ces11
# Do not use Panel respondents
# ces0411 %>% 
#   filter(str_detect(ces0411$survey, "PES11")&str_detect(ces0411$survey, "Panel", negate=T))->ces11

#Use Panel respondents
ces0411 %>% 
  filter(str_detect(ces0411$survey, "PES11"))->ces11
#### nrows of each CES study####
nrow(ces04)
nrow(ces06)
nrow(ces08)
nrow(ces11)

#Remove ces0411 for data saving
rm(ces0411)
#### STEP 3 RENAMING VARIABLES

### This is how we will rename the variables in each data frame.. removing the years. 

#### Make CES04 ####
nrow(ces04)
table(ces04$union04, useNA = "ifany")
table(ces04$union_both04, useNA = "ifany")
table(ces04$ces04_CPS_S6A, useNA = "ifany")
table(ces04$union04, ces04$union_both04, useNA = "ifany")
#After consulting the ODESI merged file, there should be 802 yes and 1251 no to the respondent question in the merged file
table(ces0411$ces04_CPS_S6A)
#but after we kept only the respondents who took part in the PES04
table(ces04$ces04_CPS_S6A) # we lose more than half. So that can't be good. 
table(ces04$union04, ces04$ces04_CPS_S6A, useNA = "ifany")
table(ces04$union_both04, ces04$ces04_CPS_S6A, useNA = "ifany")
table(ces04$union_both04, ces04$ces04_CPS_S6B, useNA = "ifany")
table(as_factor(ces04$ces04_CPS_S6A), as_factor(ces04$ces04_CPS_S6B), useNA = "ifany")

#### Rename CES 04####
ces04 %>% 
  rename(union_both=union_both04)->ces04
ces04 %>% 
  rename(union=union04)->ces04
ces04 %>% 
  rename(degree=degree04)->ces04
ces04 %>% 
  rename(region=region04)->ces04
ces04 %>% 
  rename(quebec=quebec04)->ces04
ces04 %>% 
  rename(age=age04)->ces04
ces04 %>% 
  rename(religion=religion04)->ces04
ces04 %>% 
  rename(language=language04)->ces04
ces04 %>% 
  rename(employment=employment04)->ces04
ces04 %>% 
  rename(sector=sector04)->ces04
ces04 %>% 
  rename(party_id=party_id04)->ces04
ces04 %>% 
  rename(vote=vote04)->ces04
ces04 %>% 
  rename(occupation=occupation04)->ces04
ces04 %>% 
  rename(income=income04)->ces04
ces04 %>% 
  rename(non_charter_language=non_charter_language04)->ces04
ces04 %>% 
  rename(occupation3=occupation04_3)->ces04
ces04 %>% 
  rename(redistribution=redistribution04)->ces04
ces04 %>% 
  rename(pro_redistribution=pro_redistribution04)->ces04
ces04 %>% 
  rename(market_liberalism=market_liberalism04)->ces04
ces04 %>% 
  rename(traditionalism=traditionalism04)->ces04
ces04 %>% 
  rename(traditionalism2=traditionalism204)->ces04
ces04 %>% 
  rename(immigration_rates=immigration_rates04)->ces04
ces04 %>% 
  rename(enviro=enviro04)->ces04
ces04 %>% 
  rename(death_penalty=death_penalty04)->ces04
ces04 %>% 
  rename(crime=crime04)->ces04
ces04 %>% 
  rename(gay_rights=gay_rights04)->ces04
ces04 %>% 
  rename(abortion=abortion04)->ces04
ces04 %>% 
  rename(authoritarianism=authoritarianism04)->ces04
ces04 %>% 
  rename(quebec_accom=quebec_accom04)->ces04
ces04 %>% 
  rename(religiosity=religiosity04)->ces04
ces04 %>% 
  rename(liberal_leader=liberal_leader04)->ces04
ces04 %>% 
  rename(conservative_leader=conservative_leader04)->ces04
ces04 %>% 
  rename(ndp_leader=ndp_leader04)->ces04
ces04 %>% 
  rename(bloc_leader=bloc_leader04)->ces04
#ces04 %>% 
 # rename(green_leader=green_leader04)->ces04
ces04 %>% 
  rename(liberal_rating=liberal_rating04)->ces04
ces04 %>% 
  rename(conservative_rating=conservative_rating04)->ces04
ces04 %>% 
  rename(ndp_rating=ndp_rating04)->ces04
ces04 %>% 
  rename(bloc_rating=bloc_rating04)->ces04
ces04 %>% 
  rename(green_rating=green_rating04)->ces04
ces04 %>% 
  rename(education=education04)->ces04
ces04 %>% 
  rename(personal_retrospective=personal_retrospective04)->ces04
ces04 %>% 
  rename(ideology=ideology04)->ces04
ces04 %>% 
  rename(turnout=turnout04)->ces04
ces04 %>% 
  rename(mip=mip04)->ces04
ces04 %>% 
  rename(satdem=satdem04)->ces04

table(ces04$survey, ces04$non_charter_language)

#### Rename CES06 ####
ces06 %>% 
  rename(union_both=union_both06)->ces06
ces06 %>% 
  rename(union=union06)->ces06
ces06 %>% 
  rename(degree=degree06)->ces06
ces06 %>% 
  rename(region=region06)->ces06
ces06 %>% 
  rename(quebec=quebec06)->ces06
ces06 %>% 
  rename(age=age06)->ces06
ces06 %>% 
  rename(religion=religion06)->ces06
ces06 %>% 
  rename(language=language06)->ces06
ces06 %>% 
  rename(employment=employment06)->ces06
ces06 %>% 
  rename(sector=sector06)->ces06
ces06 %>% 
  rename(vote=vote06)->ces06
ces06 %>% 
  rename(party_id=party_id06)->ces06
ces06 %>% 
  rename(occupation=occupation06)->ces06
ces06 %>% 
  rename(income=income06)->ces06
ces06 %>% 
  rename(non_charter_language=non_charter_language06)->ces06
ces06 %>% 
  rename(occupation3=occupation06_3)->ces06
ces06 %>% 
  rename(redistribution=redistribution06)->ces06
ces06 %>% 
  rename(pro_redistribution=pro_redistribution06)->ces06
ces06 %>% 
  rename(market_liberalism=market_liberalism06)->ces06
ces06 %>% 
  rename(traditionalism=traditionalism06)->ces06
ces06 %>% 
  rename(traditionalism2=traditionalism206)->ces06
ces06 %>% 
  rename(immigration_rates=immigration_rates06)->ces06
ces06 %>% 
  rename(enviro=enviro06)->ces06
ces06 %>% 
  rename(death_penalty=death_penalty06)->ces06
ces06 %>% 
  rename(crime=crime06)->ces06
ces06 %>% 
  rename(gay_rights=gay_rights06)->ces06
ces06 %>% 
  rename(abortion=abortion06)->ces06
ces06 %>% 
  rename(authoritarianism=authoritarianism06)->ces06
ces06 %>% 
  rename(quebec_accom=quebec_accom06)->ces06
ces06 %>% 
  rename(religiosity=religiosity06)->ces06
ces06 %>% 
  rename(liberal_leader=liberal_leader06)->ces06
ces06 %>% 
  rename(conservative_leader=conservative_leader06)->ces06
ces06 %>% 
  rename(ndp_leader=ndp_leader06)->ces06
ces06 %>% 
  rename(bloc_leader=bloc_leader06)->ces06
ces06 %>% 
  rename(green_leader=green_leader06)->ces06
ces06 %>% 
  rename(liberal_rating=liberal_rating06)->ces06
ces06 %>% 
  rename(conservative_rating=conservative_rating06)->ces06
ces06 %>% 
  rename(ndp_rating=ndp_rating06)->ces06
ces06 %>% 
  rename(bloc_rating=bloc_rating06)->ces06
ces06 %>% 
  rename(green_rating=green_rating06)->ces06
ces06 %>% 
  rename(education=education06)->ces06
ces06 %>% 
  rename(personal_retrospective=personal_retrospective06)->ces06
ces06 %>% 
  rename(immigration_job=immigration_job06)->ces06
ces06 %>% 
  rename(turnout=turnout06)->ces06
ces06 %>% 
  rename(mip=mip06)->ces06
ces06 %>% 
  rename(satdem=satdem06)->ces06

table(ces06$survey, ces06$non_charter_language)

#### Rename CES08 ####
ces08 %>% 
  rename(union_both=union_both08)->ces08
ces08 %>% 
  rename(union=union08)->ces08
ces08 %>% 
  rename(degree=degree08)->ces08
ces08 %>% 
  rename(region=region08)->ces08
ces08 %>% 
  rename(quebec=quebec08)->ces08
ces08 %>% 
  rename(age=age08)->ces08
ces08 %>% 
  rename(religion=religion08)->ces08
ces08 %>% 
  rename(language=language08)->ces08
ces08 %>% 
  rename(employment=employment08)->ces08
ces08 %>% 
  rename(sector=sector08)->ces08
ces08 %>% 
  rename(party_id=party_id08)->ces08
ces08 %>% 
  rename(vote=vote08)->ces08
ces08 %>% 
  rename(occupation=occupation08)->ces08
ces08 %>% 
  rename(income=income08)->ces08
ces08 %>% 
  rename(non_charter_language=non_charter_language08)->ces08
ces08 %>% 
  rename(occupation3=occupation08_3)->ces08
ces08 %>% 
  rename(redistribution=redistribution08)->ces08
ces08 %>% 
  rename(pro_redistribution=pro_redistribution08)->ces08
ces08 %>% 
  rename(market_liberalism=market_liberalism08)->ces08
ces08 %>% 
  rename(traditionalism=traditionalism08)->ces08
ces08 %>% 
  rename(traditionalism2=traditionalism208)->ces08
ces08 %>% 
  rename(immigration_rates=immigration_rates08)->ces08
ces08 %>% 
  rename(enviro=enviro08)->ces08
ces08 %>% 
  rename(death_penalty=death_penalty08)->ces08
ces08 %>% 
  rename(crime=crime08)->ces08
ces08 %>% 
  rename(gay_rights=gay_rights08)->ces08
ces08 %>% 
  rename(abortion=abortion08)->ces08
ces08 %>% 
  rename(authoritarianism=authoritarianism08)->ces08
ces08 %>% 
  rename(quebec_accom=quebec_accom08)->ces08
ces08 %>% 
  rename(religiosity=religiosity08)->ces08
ces08 %>% 
  rename(liberal_leader=liberal_leader08)->ces08
ces08 %>% 
  rename(conservative_leader=conservative_leader08)->ces08
ces08 %>% 
  rename(ndp_leader=ndp_leader08)->ces08
ces08 %>% 
  rename(bloc_leader=bloc_leader08)->ces08
ces08 %>% 
  rename(green_leader=green_leader08)->ces08
ces08 %>% 
  rename(liberal_rating=liberal_rating08)->ces08
ces08 %>% 
  rename(conservative_rating=conservative_rating08)->ces08
ces08 %>% 
  rename(ndp_rating=ndp_rating08)->ces08
ces08 %>% 
  rename(bloc_rating=bloc_rating08)->ces08
ces08 %>%  
  rename(green_rating=green_rating08)->ces08
ces08 %>% 
  rename(education=education08)->ces08
ces08 %>% 
  rename(personal_retrospective=personal_retrospective08)->ces08
ces08 %>% 
  rename(ideology=ideology08)->ces08
ces08 %>% 
  rename(immigration_job=immigration_job08)->ces08
ces08 %>% 
  rename(turnout=turnout08)->ces08
ces08 %>% 
  rename(mip=mip08)->ces08
ces08 %>% 
  rename(satdem=satdem08)->ces08

table(ces08$survey, ces08$non_charter_language)

#### Rename CES11 ####
ces11 %>% 
  rename(union_both=union_both11)->ces11
ces11 %>% 
  rename(union=union11)->ces11
ces11 %>% 
  rename(degree=degree11)->ces11
ces11 %>% 
  rename(region=region11)->ces11
ces11 %>% 
  rename(quebec=quebec11)->ces11
ces11 %>% 
  rename(age=age11)->ces11
ces11 %>% 
  rename(religion=religion11)->ces11
ces11 %>% 
  rename(language=language11)->ces11
ces11 %>% 
  rename(employment=employment11)->ces11
ces11 %>% 
  rename(sector=sector11)->ces11
ces11 %>% 
  rename(party_id=party_id11)->ces11
ces11 %>% 
  rename(vote=vote11)->ces11
ces11 %>% 
  rename(occupation=occupation11)->ces11
ces11 %>% 
  rename(income=income11)->ces11
ces11 %>% 
  rename(non_charter_language=non_charter_language11)->ces11
ces11 %>% 
  rename(occupation3=occupation11_3)->ces11
ces11 %>% 
  rename(redistribution=redistribution11)->ces11
ces11 %>% 
  rename(pro_redistribution=pro_redistribution11)->ces11
ces11 %>% 
  rename(market_liberalism=market_liberalism11)->ces11
ces11 %>% 
  rename(traditionalism=traditionalism11)->ces11
ces11 %>% 
  rename(traditionalism2=traditionalism211)->ces11
ces11 %>% 
  rename(immigration_rates=immigration_rates11)->ces11
ces11 %>% 
  rename(enviro=enviro11)->ces11
ces11 %>% 
  rename(death_penalty=death_penalty11)->ces11
ces11 %>% 
  rename(crime=crime11)->ces11
ces11 %>% 
  rename(gay_rights=gay_rights11)->ces11
ces11 %>% 
  rename(abortion=abortion11)->ces11
ces11 %>% 
  rename(authoritarianism=authoritarianism11)->ces11
ces11 %>% 
  rename(quebec_accom=quebec_accom11)->ces11
ces11 %>% 
  rename(religiosity=religiosity11)->ces11
ces11 %>% 
  rename(liberal_leader=liberal_leader11)->ces11
ces11 %>% 
  rename(conservative_leader=conservative_leader11)->ces11
ces11 %>% 
  rename(ndp_leader=ndp_leader11)->ces11
ces11 %>% 
  rename(bloc_leader=bloc_leader11)->ces11
ces11 %>% 
  rename(green_leader=green_leader11)->ces11
ces11 %>% 
  rename(liberal_rating=liberal_rating11)->ces11
ces11 %>% 
  rename(conservative_rating=conservative_rating11)->ces11
ces11 %>% 
  rename(ndp_rating=ndp_rating11)->ces11
ces11 %>% 
  rename(bloc_rating=bloc_rating11)->ces11
ces11 %>% 
  rename(green_rating=green_rating11)->ces11
ces11 %>% 
  rename(education=education11)->ces11
ces11 %>% 
  rename(personal_retrospective=personal_retrospective11)->ces11
ces11 %>% 
  rename(ideology=ideology11)->ces11
ces11 %>% 
  rename(immigration_job=immigration_job11)->ces11
ces11 %>% 
  rename(turnout=turnout11)->ces11
ces11 %>% 
  rename(mip=mip11)->ces11
ces11 %>% 
  rename(satdem=satdem11)->ces11

#### Rejoin the Files To Make CES ####

#For some years there are no variables (e.g. 1965 does not have a union variable)
#This is not actually a big deal.
#The trick is that bind_rows keeps *every* single variable, from the data frames that are bound
#If two data frames share a variable then it combines them and populates the values on the one variable from both data frames
#If one of the data frame has a variable that the other does not then it just fills the rows with missing values
#I *think* that this is the quickest way forward. 

##We are going to make a list of each survey
ces.list<-list(ces65, ces68, ces72_nov, ces74, ces79, ces80, ces84, ces88, ces93, ces97, ces00, ces04, ces06, ces08, ces11, ces15phone, ces19phone, ces21)
#WE are going to name each item in the list
names(ces.list)<-c(1965, 1968, 1972,1974, 1979,1980, 1984, 1988, 1993, 1997, 2000, 2004, 2006, 2008, 2011, 2015, 2019, 2021)

#removing election files
#Remove these only if you run into memory troubles
# rm(ces00)
# rm(ces04)
# rm(ces0411)
# rm(ces06)
# rm(ces08)
# rm(ces11)
# rm(ces15phone)
# rm(ces65)
# rm(ces68)
# rm(ces72_nov)
# rm(ces74)
# rm(ces74b)
# rm(ces79)
# rm(ces84)
# rm(ces88)
# rm(ces93)
# rm(ces97)
# rm(ces19phone)
# 
# str(ces.list)
# str(ces.list$`2019`)
# ces.list%>%
#   map(., ncol)
#bind_rows binds the rows of each element in the list together
#.id="survey"creates a new variable called "survey" and its values are the names of the list items. 

library(haven)
#Add the common variables we need from each data.frame in the combined data set here.
#common_vars<-c('male')
common_vars<-c('male', 'union_both', 'region', 'degree', 'quebec', 'age', 'religion', 'vote', 'income', 'redistribution', 'market_liberalism', 'immigration_rates', 'traditionalism2', 'turnout', 'mip')

#Start with the data frame
ces.list %>% 
  #WE have to zap the value labels (get rid of them to enable row binding)
  map(., zap_labels) %>%
  #map_df does the select function on each item in ces.list
  #It selects whatever is in common_vars, above
  #It spits out a list of data_frames
 map(., select, all_of(common_vars))%>%
  #bind_rows smushes all the data frames together, and creates a variable called election
  #The value of which come from the name of the list item
  #e.g. if a row comes from, it's value of election will be 2000
  bind_rows(., .id="election")->ces 


#Remove ces.list
# We don't need it here
rm(ces.list)

#Do a summary
summary(ces)
#Check the names
tail(names(ces))
names(ces68)

library(stringr)
#table(str_detect(names(ces0411), "survey"))
table(str_detect(names(ces00), "survey"))

names(ces)
table(ces$election, ces$male, useNA = "ifany")
table(ces$union_both)

#### Currently region is regions of English Canada only
#### quebec is dichotomous Quebec v. non-quebec
#### Create region2 which is one region variable for all of Canada
ces %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces

####Turn region2 into factor with Quebec as reference case
#### This can be changed anytime very easily 
ces$region2<-factor(ces$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces$region2)

#Turn region into factor with East as reference case
ces$region3<-Recode(as.factor(ces$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces$region3)
table(ces$region3)

##Create female variable
## Sometimes we may want to report male dichotomous variable, sometimes female. 
ces %>% 
  mutate(female=case_when(
    male==1~0,
    male==0~1
  ))->ces

library(car)
#To model party voting we need to create party vote dummy variables

#This combines the NDP and Bloc into a left-vote 
as_factor(ces$vote)
ces %>% 
  mutate(vote2=case_when(
    vote==  1~"Liberal",
    vote== 2~"Conservative",
    vote== 3~"NDP", 
    vote==  4~"BQ", 
    vote== 5~"Green",
    vote==  0~NA_character_
  ))->ces
ces$vote2<-factor(ces$vote2, levels=c("Conservative", "Liberal", "NDP", "BQ", "Green"))
#ces$vote2<-Recode(as_factor(ces$vote), "'; ;1='Liberal' ; 2='Conservative' ; 3='Left' ; 5='Green'", levels=c('Conservative', 'Liberal', 'Left', 'Green'))
table(ces$vote2, ces$election)

ces$ndp<-Recode(ces$vote, "3=1; 0:2=0; 4:6=0; NA=NA")
ces$liberal<-Recode(ces$vote, "1=1; 2:6=0; NA=NA")

ces$conservative<-Recode(ces$vote, "0:1=0; 2=1; 3:5=0; 6=1; NA=NA")
ces$bloc<-Recode(ces$vote, "4=1; 0:3=0; 5:6=0; else=NA")
ces$green<-Recode(ces$vote, "5=1; 0:4=0; 6=0; else=NA")


#Recode NDP vs Liberals/Right
ces$ndp_vs_right<-Recode(ces$vote, "3=1; 2=0; 6=0; else=NA")
ces$liberal_vs_right<-Recode(ces$vote, "1=1; 2=0; 6=0; else=NA")
ces$bloc_vs_right<-Recode(ces$vote, "4=1; 2=0; 6=0; else=NA")
ces$ndp_vs_liberal<-Recode(ces$vote, "3=1; 1=0; else=NA")
table(ces$ndp_vs_right)
table(ces$liberal_vs_right)
table(ces$bloc_vs_right)
table(ces$ndp_vs_liberal)

# Turn religion into factor with None as reference case
ces$religion2<-Recode(as.factor(ces$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces$religion2)
table(ces$religion2)

# Religion dummies
ces$catholic<-Recode(ces$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces$no_religion<-Recode(ces$religion, "0=1; 1:3=0; NA=NA")

# Reduce MIP

val_labels(ces$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, 
Taxes=9, Deficit_Debt=10, Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, 
Free_Trade=17)

ces$mip2<-Recode(ces$mip, "0=NA; 1=2; 2=2; 3=2; 4=2; 5=1; 6=1; 7=1; 8=1; 9=1; 10=1; 11=2; 12=2; 13=2; 14=2; 15=1; 16=1; 17=1")
val_labels(ces$mip2)<-c(`First Dimension`=1, `Second Dimension`=2)

ces$mip3<-Recode(ces$mip, "0=0; 1=2; 2=2; 3=0; 4=2; 5=1; 6=1; 7=1; 8=2; 9=1; 10=1; 11=0; 12=2; 13=2; 14=2; 15=2; 16=0; 17=1")
val_labels(ces$mip3)<-c(`No Dimension`=0, `First Dimension`=1, `Second Dimension`=2)

### Value labels often go missing in the creation of the ces data frame
### assign value label
#val_labels(ces$sector)<-c(Private=0, Public=1)
val_labels(ces$vote)<-c(Conservative=2, Liberal=1, NDP=3, BQ=4, Green=5, Other=0)
val_labels(ces$male)<-c(Female=0, Male=1)
val_labels(ces$union_both)<-c(None=0, Union=1)
val_labels(ces$degree)<-c(`No degree`=0, Degree=1)
val_labels(ces$region)<-c(Atlantic=1, Ontario=2, West=3)
val_labels(ces$quebec)<-c(Other=0, Quebec=1)
val_labels(ces$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#val_labels(ces$language)<-c(French=0, English=1)
#val_labels(ces$non_charter_language)<-c(Charter=0, Non_Charter=1)
#val_labels(ces$employment)<-c(Unemployed=0, Employed=1)
#val_labels(ces$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
val_labels(ces$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
val_labels(ces$redistribution)<-c(Less=0, More=1)
#val_labels(ces$education)<-c(Less=0, Same=0.5, More=1)

####
names(ces)

# #### Some occupation recodes (occupation 3 and 4 include self-employed) ####
# val_labels(ces$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
# ces$occupation2<-Recode(as.factor(ces$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
# ces$occupation2<-fct_relevel(ces$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')
# val_labels(ces$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
# 
# ces$occupation4<-Recode(as.factor(ces$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals','Self-Employed', 'Routine_Nonmanual')
# #ces$occupation4<-fct_relevel(ces$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')
# )
# 
# table(ces$occupation, ces$election)
# table(ces$occupation2, ces$election)
# table(ces$occupation3, ces$election)
# table(ces$occupation4, ces$election)
# 
# ces %>% 
#   select(occupation, occupation3, election) %>% 
#   group_by(election) %>% 
#   summarise_all(funs(sum(is.na(.))/length(.))) 
# prop.table(table(ces15phone$occupation, useNA = "ifany"))
# prop.table(table(ces19phone$occupation, useNA = "ifany"))
# 
# Working Class variables (3 and 4 include self-employed; 2 and 4 are dichotomous where everyone else is set to 0)
# ces$working_class<-Recode(ces$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
# ces$working_class2<-Recode(ces$occupation, "4:5=1; else=0")
# ces$working_class3<-Recode(ces$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
# ces$working_class4<-Recode(ces$occupation3, "4:5=1; else=0")
# table(ces$working_class, ces$election)
# table(ces$working_class2, ces$election)
# table(ces$working_class3, ces$election)
# table(ces$working_class4, ces$election)

#Create variables
ces$left<-Recode(ces$vote, "1=1; 3=1; 5=1; 0=0; 2=0; 4=0;6=0; else=NA")

table(ces$left, ces$election)
table(ces$ndp, ces$election)
ces$right<-Recode(ces$vote, "2=1; 0=0; 1=0; 3:5=0;6=1; else=NA")
table(ces$right, ces$election)
val_labels(ces$right)<-c(Right=1, Other=0)
# ces$upper_class<-Recode(ces$occupation, "1:2=1; 3:5=0; else=NA")
# table(ces$upper_class)
# ces$upper_class2<-Recode(ces$occupation3, "1:2=1; 3:6=0; else=NA")
# table(ces$upper_class2)

# #table(ces$employment, ces$election)
# 
# ces %>% 
#   mutate(`Period`=case_when(
#     election>2000~1,
#     election<2004~0
#   ))->ces

#val_labels(ces$working_class4)<-c(`Other`=0, `Working  Class`=1)

#### Set Theme ####
theme_set(theme_bw())

#This command calls the file 2_diagnostics.R
#source("R_scripts/3_recode_diagnostics.R", echo=T)
#source("R_scripts/4_make_models.R", echo=T)
#source("R_scripts/5_ces15_models.R", echo=T)
#source("R_scripts/5_ces15_block_models.R", echo=T)
#source("R_scripts/5_ces19_models.R", echo=T)
#source("R_scripts/5_ces19_block_models.R", echo=T)
#source("R_scripts/7_class_logistic_models.R", echo=T)
#source("R_scripts/8_block_recursive_models.R", echo=T)

#source("R_scripts/8_analysis_script.R", echo=T)
