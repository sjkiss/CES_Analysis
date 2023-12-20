#Install the package
#devtools::install_github("sjkiss/cesdata2")
#
library(cesdata2)

#Note all data sets are immediately available
#Have added ces21 and ces15web
library(tidyverse)
glimpse(ces21)
glimpse(ces15web)
glimpse(ces15phone)

#Split ces79 and ces80

#Seprate ces79 and ces80 to two separate files
ces7980 %>% 
  filter(V4002==1)->ces79

ces7980 %>% 
  filter(V4008==1)->ces80

#Check 
table(ces79$male, useNA = "ifany")
table(ces79$male80, useNA = "ifany")
table(ces80$male, useNA = "ifany")
table(ces80$male80, useNA = "ifany")

#WE have to rename the ces80 80 variables in order to create the full ces data frame.
#This is how we did it previously 

#ces80 %>% 
 # select(male=male80, region=region80, quebec=quebec80, age=age80, language=language80, party_id=party_id80, vote=vote80, union, union_both, degree, employment, sector, income,income_tertile, income2, occupation, occupation3, religion, non_charter_language, size, ideology, turnout, redistribution, market_liberalism, immigration_rates, traditionalism2, mip=mip80, foreign=foreign80)->ces80

#Downside is that we have to specify each specific variable.
#Can do it globally by doing it this way.

#Save into names(ces80) the results of Removing all instances
names(ces80)<-str_remove_all(
  #from the names of ces80
  names(ces80), 
  #80
  "80")
#Check what has happened
names(ces80)
#Filter the CES93
### Filter out ces93 referendum respondents only by removing missing values from RTYPE4 (indicates ces93 respondents)
ces93[!is.na(ces93$RTYPE4), ] -> ces93

#Split ces0411
#Use Panel Respondents
ces0411 %>%
  filter(str_detect(ces0411$survey, "PES04"))->ces04

ces0411 %>%
  filter(str_detect(ces0411$survey, "PES06"))->ces06

### CES08
ces0411 %>% 
  filter(str_detect(ces0411$survey, "PES08"))->ces08

#Use Panel respondents
ces0411 %>% 
  filter(str_detect(ces0411$survey, "PES11"))->ces11

#rename variables
#This is how we did it previously
#### Rename CES 04####
# ces04 %>% 
#   rename(union_both=union_both04)->ces04
# ces04 %>% 
#   rename(income2=income042)->ces04

#This works a lot easier
#### Rename Variables in CES0411 ####
names(ces04)<-str_remove_all(names(ces04), "04")
names(ces06)<-str_remove_all(names(ces06), "06")
names(ces08)<-str_remove_all(names(ces08), "08")
names(ces11)<-str_remove_all(names(ces11), "11")
#Make ces.list
##We are going to make a list of each survey
ces.list<-list(ces65, ces68, ces72_nov, ces74, ces79, ces80, ces84, ces88, ces93, ces97, ces00, ces04, ces06, ces08, ces11, ces15phone, ces15web, ces19phone,ces19web, ces21)
#We are going to name each item in the list
names(ces.list)<-c(1965, 1968, 1972, 1974, 1979, 1980, 1984, 1988, 1993, 1997, 2000, 2004, 2006, 2008, 2011,  '2015 Phone' , '2015 Web','2019 Phone', '2019 Web', 2021)

# Select common vars for adding into ces
#Add the common variables we need from each data.frame in the combined data set here.

common_vars<-c('male',
               'sector', 
               'occupation',
               'employment', 
               'union_both',
               'region', 'union',
               'degree', 
               'quebec', 
               'age', 
               'religion', 
               'vote', 
               'income',
               'redistribution',
               'market_liberalism', 
               'immigration_rates', 
               'traditionalism',
               'traditionalism2', 
               'trad1', 'trad2', 'immigration_rates',
               'market1','market2',
               'turnout', 'mip', 'occupation', 'occupation3', 'education', 'personal_retrospective', 'national_retrospective', 'vote3',
               'efficacy_external', 'efficacy_external2', 'efficacy_internal', 'political_efficacy', 'inequality', 'efficacy_rich', 'promise', 'trust', 'pol_interest', 'foreign',
               #Note that I have added two new variables to each data-set election and mode
               #This is to make it easier to differentiate ces15web from ces15phone and ces19web from ces19phone
               'non_charter_language', 'language', 'employment', 'satdem', 'satdem2', 'turnout', 'party_id', 'postgrad', 'income_tertile', 'income2', 'household', 'enviro', 'ideology', 'income_house', 'enviro_spend', "election", "mode")
#Start with the data frame
ces.list %>% 
    map(., select, any_of(common_vars))%>%
  #This is a bit different; we don't have to 
   bind_rows(.)->ces 

table(ces$election)
# Descriptives
#Convert mode to factor
ces$mode<-factor(ces$mode)

library(skimr)
#This code is just deleting a bunch of statistics that are not usefulr
my_skim<-skim_with(factor=sfl(ordered=NULL, n_unique=NULL), 
                   numeric = sfl(p0=NULL, sd=NULL,p25=NULL, p50=NULL, p75=NULL, p100=NULL,hist = NULL))

ces %>% 
  group_by(election) %>% 
  my_skim() %>% 
  write.csv("Results/cesdata2_descriptives.csv", row.names=F)


# # quebec is dichotomous Quebec v. non-quebec
# # Create region2 which is one region variable for all of Canada
# ces %>% 
#   mutate(region2=case_when(
#     region==1 ~ "Atlantic",
#     region==2 ~ "Ontario",
#     region==3 ~"West",
#     quebec==1 ~ "Quebec"
#   ))->ces
# 
# # Turn region2 into factor with Quebec as reference case
# # This can be changed anytime very easily 
# 
# ces$region2<-factor(ces$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
# levels(ces$region2)
# 
# # Turn region into factor with East as reference case
# ces$region3<-Recode(as.factor(ces$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
# levels(ces$region3)
# table(ces$region3)
# 
# ### Female
# ## Sometimes we may want to report male dichotomous variable, sometimes female. 
# ces %>% 
#   mutate(female=case_when(
#     male==1~0,
#     male==0~1
#   ))->ces
# 
# ## Party Vote Variables
# # This sets the other parties as 0 
# as_factor(ces$vote)
# ces %>% 
#   mutate(vote2=case_when(
#     vote==  1~"Liberal",
#     vote== 2~"Conservative",
#     vote== 3~"NDP", 
#     vote==  4~"BQ", 
#     vote== 5~"Green",
#     vote==  0~NA_character_
#   ))->ces
# # Refactor and set the levels
# ces$vote2<-factor(ces$vote2, levels=c("Conservative", "Liberal", "NDP", "BQ", "Green"))
# table(ces$vote2, ces$election)
# levels(ces$vote2)
# 
# # These are party dummies
# # Note that we are setting the People's Party to be conservative
# ces$ndp<-Recode(ces$vote, "3=1; 0:2=0; 4:6=0; NA=NA")
# ces$liberal<-Recode(ces$vote, "1=1; 2:6=0; NA=NA")
# ces$conservative<-Recode(ces$vote, "0:1=0; 2=1; 3:5=0; 6=1; NA=NA")
# ces$bloc<-Recode(ces$vote, "4=1; 0:3=0; 6=0; else=NA")
# ces$green<-Recode(ces$vote, "5=1; 0:4=0; 6=0; else=NA")
# 
# #Recode NDP vs Liberals/Right
# ces$ndp_vs_right<-Recode(ces$vote, "3=1; 2=0; else=NA")
# ces$liberal_vs_right<-Recode(ces$vote, "1=1; 2=0; else=NA")
# ces$bloc_vs_right<-Recode(ces$vote, "4=1; 2=0; else=NA")
# ces$ndp_vs_liberal<-Recode(ces$vote, "3=1; 1=0; else=NA")
# ces$left<-Recode(ces$vote, "1=1; 3=1; 5=1; 0=0; 2=0; 4=0; 6=0; else=NA")
# ces$right<-Recode(ces$vote, "2=1; 0=0; 1=0; 3:5=0; 6=1; else=NA")
# 
# # Turn religion into factor with None as reference case
# ces$religion2<-Recode(as.factor(ces$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
# levels(ces$religion2)
# table(ces$religion2)
# # Religion dummies
# ces$catholic<-Recode(ces$religion, "1=1; 2:3=0; 0=0; NA=NA")
# ces$no_religion<-Recode(ces$religion, "0=1; 1:3=0; NA=NA")
# 
# # Occupation(occupation 3 and 4 include self-employed as a category)
# # Occupation 2 and 4 collapse skilled and Unskilled
# ces$occupation2<-Recode(as.factor(ces$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
# ces$occupation2<-fct_relevel(ces$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')
# ces$occupation4<-Recode(as.factor(ces$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals','Self-Employed', 'Routine_Nonmanual'))
# # Working Class variables (3 and 4 include self-employed; 2 and 4 are dichotomous where everyone else is set to 0)
# ces$working_class<-Recode(ces$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
# ces$working_class2<-Recode(ces$occupation, "4:5=1; else=0")
# ces$working_class3<-Recode(ces$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
# ces$working_class4<-Recode(ces$occupation3, "4:5=1; else=0")
# 
# #Check sample sizes for occupation
# ces %>% 
#   select(occupation, occupation3, election) %>% 
#   group_by(election) %>% 
#   summarise_all(funs(sum(is.na(.))/length(.))) 
# 
# #Create Upper Class Variables variables
# 
# table(ces$right, ces$election)
# ces$upper_class<-Recode(ces$occupation, "1:2=1; 3:5=0; else=NA")
# table(ces$upper_class)
# ces$upper_class2<-Recode(ces$occupation3, "1:2=1; 3:6=0; else=NA")
# table(ces$upper_class2)
# 
# # Create Income Dummy
# 
# ces$rich<-Recode(ces$income, "4:5=1; else=0")
# ces$poor<-Recode(ces$income, "1:2=1; else=0")
# 
# # Create Time Dummies
# ces$`1965`<-Recode(ces$election, "1965=1; else=0")
# ces$`1968`<-Recode(ces$election, "1968=1; else=0")
# ces$`1972`<-Recode(ces$election, "1972=1; else=0")
# ces$`1974`<-Recode(ces$election, "1974=1; else=0")
# ces$`1979`<-Recode(ces$election, "1979=1; else=0")
# ces$`1980`<-Recode(ces$election, "1980=1; else=0")
# ces$`1984`<-Recode(ces$election, "1984=1; else=0")
# ces$`1988`<-Recode(ces$election, "1988=1; else=0")
# ces$`1993`<-Recode(ces$election, "1993=1; else=0")
# ces$`1997`<-Recode(ces$election, "1997=1; else=0")
# ces$`2000`<-Recode(ces$election, "2000=1; else=0")
# ces$`2004`<-Recode(ces$election, "2004=1; else=0")
# ces$`2006`<-Recode(ces$election, "2006=1; else=0")
# ces$`2008`<-Recode(ces$election, "2008=1; else=0")
# ces$`2011`<-Recode(ces$election, "2011=1; else=0")
# ces$`2015`<-Recode(ces$election, "2015=1; else=0")
# ces$`2019`<-Recode(ces$election, "2019=1; else=0")
# 
# # Create Period Variable
# ces %>% 
#   mutate(`Period`=case_when(
#     election>2000~1,
#     election<2004~0
#   ))->ces
# 
# ### Economic Views ###
# #Flip redistribution
# ces$redistribution_reversed<-1-ces$redistribution
# #Check for test
# 
# ces %>% 
#   group_by(election, vote2) %>% 
#   summarize(avg_redistribution=mean(redistribution, na.rm=T)) %>% 
#   print(n=100)
# names(ces)
# #Start with data frame
# ces %>% 
#   #Create new variable called economic 
#   #It is defined as the average (mean) of market1, market2 and redistribution_reviersed; missing values ignored
#   mutate(economic=rowMeans(select(., c("market1", "market2", "redistribution_reversed")), na.rm=T)) %>% 
#   #Select those variables 
#   select(market1, market2, election, redistribution_reversed,economic) %>% 
#   #Filter post 2004 to examine.
#   filter(election>2000)
# 
# #Repeat and store
# ces %>% 
#   #Create new variable called economic 
#   #It is defined as the average (mean) of market1, market2 and redistribution_reviersed; missing values ignored
#   mutate(economic=rowMeans(select(., c("market1", "market2", "redistribution_reversed")), na.rm=T)) ->ces
# 
# ### Socio-cultural Views ###
# 
# #Start with data frame
# ces %>% 
#   #Create new variable called social 
#   #It is defined as the average (mean) of trad1, trad2 and immigration; missing values ignored
#   mutate(social=rowMeans(select(., c("trad1", "trad2", "immigration_rates")), na.rm=T))  %>% 
#   #Select those variables 
#   select(trad1, trad2, election, immigration_rates, social) %>% 
#   #Filter post 2004 to examine.
#   filter(election>2000)
# 
# ces %>% 
#   #Create new variable called social 
#   #It is defined as the average (mean) of trad1, trad2 and immigration; missing values ignored
#   mutate(social=rowMeans(select(., c("trad1", "trad2", "immigration_rates")), na.rm=T))  ->ces
# 
# ### Value labels often go missing in the creation of the ces data frame
# ### assign value label
# val_labels(ces$sector)<-c(Private=0, Public=1)
# val_labels(ces$vote)<-c(Conservative=2, Liberal=1, NDP=3, BQ=4, Green=5, Other=0, PPC=6)
# val_labels(ces$male)<-c(Female=0, Male=1)
# val_labels(ces$union_both)<-c(None=0, Union=1)
# val_labels(ces$degree)<-c(`No degree`=0, Degree=1)
# val_labels(ces$region)<-c(Atlantic=1, Ontario=2, West=3)
# val_labels(ces$quebec)<-c(Other=0, Quebec=1)
# val_labels(ces$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
# val_labels(ces$language)<-c(French=0, English=1)
# val_labels(ces$non_charter_language)<-c(Charter=0, Non_Charter=1)
# val_labels(ces$employment)<-c(Unemployed=0, Employed=1)
# val_labels(ces$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
# val_labels(ces$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
# val_labels(ces$income2)<-c(Lowest=1,  Middle=2, Highest=3)
# val_labels(ces$income3)<-c(Lowest=1,  Middle=2, Highest=3)
# #val_labels(ces$redistribution)<-c(Less=0, More=1)
# val_labels(ces$education)<-c(Less=0, Same=0.5, More=1)
# val_labels(ces$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
# val_labels(ces$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
# val_labels(ces$working_class4)<-c(`Other`=0, `Working  Class`=1)
# val_labels(ces$Period)<-c(`Pre 2004`=0, `Post 2000`=1)
# val_labels(ces$right)<-c(Right=1, Other=0)
# val_labels(ces$left)<-c(Left=1, Other=0)
# val_labels(ces$rich)<-c(Rich=1, `Not Rich`=0)
# val_labels(ces$poor)<-c(Poor=1, `Not Poor`=0)
# val_labels(ces$postgrad)<-c(`Post-grad`=1, Other=0)
# #This command calls the file 2_diagnostics.R
# #source("R_scripts/3_recode_diagnostics.R", echo=T)
# #source("R_scripts/4_make_models.R", echo=T)
# #source("R_scripts/5_ces15_models.R", echo=T)
# #source("R_scripts/5_ces15_block_models.R", echo=T)
# #source("R_scripts/5_ces19_models.R", echo=T)
# #source("R_scripts/5_ces19_block_models.R", echo=T)
# #source("R_scripts/7_class_logistic_models.R", echo=T)
# #source("R_scripts/8_block_recursive_models.R", echo=T)
# 
# #source("R_scripts/8_analysis_script.R", echo=T)
# ces$satdem
# table(ces$election, ces$satdem)
# table(ces$election, ces$turnout)
# table(ces$election, ces$postgrad)
# table(ces$election, ces$union)
# 
# table(ces$income, ces$income_tertile)
# table(ces$income2, ces$income_tertile)
# ces %>% 
#   filter(income2==3&income_tertile==3) %>% 
#   select(election) #Note there are some suspicious values here
# # There are 489 people who are in the third simon quintile and the top tertile
# #I checked and they are *all* in 2004 and 2006 and they are just boundary edge cases. I used the 2004 SLID to etimate terciles for 2004, but I'm waiting on terciles
# # for the 20006 census data; this may change. But if you look at the proportions, they overwhelming majority of 3 quintiles are in the second tercile.
# 
# ces %>% 
#   filter(income2==3&income_tertile==3) %>% 
#   select(election)
# #Create tertles
# table(ces$income)
# table(ces$income2, ces$election)
# prop.table(table(ces$income_tertile, ces$election), 2)
# # ces$income2<-Recode(ces$income, "1=1; 2:4=2; 5=3")
# # ces$income3<-Recode(ces$income, "1:2=1; 3=2; 4:5=3")
# # val_labels(ces$income2)<-c("Lowest"=1, "Middle"=2, "Highest"=3)
# # val_labels(ces$income3)<-c("Lowest"=1, "Middle"=2, "Highest"=3)
# val_labels(ces$income_tertile)<-c("Lowest"=1, "Middle"=2, "Highest"=3)
# val_labels(ces$income_house)<-c("Lowest"=1, "Middle"=2, "Highest"=3)

