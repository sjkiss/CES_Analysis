#File to Recode 2004-2011 CES Data 
library(tidyverse)
library(car)
library(labelled)

#load data
data("ces0411")


####recode Gender (GENDER)####
#Gender is the same variable for all elections 2004-11

look_for(ces0411, "sex")
ces0411$male<-Recode(ces0411$GENDER, "1=1; 5=0")
val_labels(ces0411$male)<-c(Female=0, Male=1)
#checks
val_labels(ces0411$male)
table(ces0411$male)

##### Union ####
#recode Union Respondent (ces04_CPS_S6A)
ces0411$union04<-Recode(ces0411$ces04_CPS_S6A, 
                        "1=1; 5=0; else=NA")
val_labels(ces0411$union04)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union04)
table(ces0411$union04)

#recode Union Combined (ces04_CPS_S6A and ces04_CPS_S6B)
table(ces0411$ces04_CPS_S6A, ces0411$ces04_CPS_S6B, useNA = "ifany")

ces0411 %>%
  mutate(union_both04=case_when(
    ces04_CPS_S6A==1 | ces04_CPS_S6B==1 ~ 1,
    ces04_CPS_S6A==5 ~ 0,
    ces04_CPS_S6B==5 ~ 0,
    ces04_CPS_S6A==8 & ces04_CPS_S6B==8 ~ NA_real_,
    ces04_CPS_S6A==9 & ces04_CPS_S6B==9 ~ NA_real_,
    ces04_CPS_S4==1 ~ 0,
    (ces06_CPS_S6A==1 | ces06_CPS_S6B==1) & ces04_rtype1==1 ~ 1,
    ces06_CPS_S6A==5 & ces04_rtype1==1 ~ 0,
    ces06_CPS_S6B==5 & ces04_rtype1==1 ~ 0,
  ))->ces0411

table(ces0411$union_both04, useNA = "ifany")
1061+984
val_labels(ces0411$union_both04)<-c(None=0, Union=1)

#checks
val_labels(ces0411$union_both04)<-c(None=0, Union=1)
table(ces0411$union_both04)

ces0411 %>% 
  select(ces04_CPS_S6A, ces04_CPS_S6B, union_both04) %>% 
  group_by(ces04_CPS_S6A, ces04_CPS_S6B, union_both04) %>% 
  summarize(n=n())

# Some checks
table( as_factor(ces0411$ces04_CPS_S6A), as_factor(ces0411$ces04_CPS_S6B), useNA = "ifany")
table(as_factor(ces0411$union_both04), as_factor(ces0411$ces04_CPS_S6A), useNA = "ifany")
table(as_factor(ces0411$union_both04), as_factor(ces0411$ces04_CPS_S6B), useNA = "ifany")

####Education (ces04_CPS_S3)####
look_for(ces0411, "education")
ces0411$degree04<-Recode(ces0411$ces04_CPS_S3, "9:11=1; 1:8=0; else=NA")
val_labels(ces0411$degree04)<-c(nodegree=0, degree=1)
#checks
val_labels(ces0411$degree04)
table(ces0411$degree04)

#####Region (ces04_PROVINCE)####
look_for(ces0411, "province")
ces0411$region04<-Recode(ces0411$ces04_PROVINCE, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces0411$region04)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces0411$region04)
table(ces0411$region04)

#recode Quebec (ces04_PROVINCE)
look_for(ces0411, "province")
ces0411$quebec04<-Recode(ces0411$ces04_PROVINCE, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces0411$quebec04)<-c(Other=0, Quebec=1)
#checks
val_labels(ces0411$quebec04)
table(ces0411$quebec04)

#recode Age (YEARofBIRTH)
look_for(ces0411, "birth")
#provide easy to use survey variable
ces0411$survey<-as_factor(ces0411$Survey_Type04060811)
#meake easy to use year of birth variable
ces0411$yob<-Recode(ces0411$YEARofBIRTH, "9998:9999=NA")
#This checks if "04" appears in the survey variable
str_detect(ces0411$survey, "04")
#check
table(str_detect(ces0411$survey, "04"))
#pipe data frame
ces0411 %>% 
  #mutate making new variable age04
  mutate(age04=case_when(
    #if 04 appears in the values for survey then define age-4 as the result of 2004-yob
    str_detect(ces0411$survey, "04")~2004-yob
  ))-> ces0411
#check
table(ces0411$age04)

#recode Religion (ces04_CPS_S9)
look_for(ces0411, "relig")
ces0411$religion04<-Recode(ces0411$ces04_CPS_S9, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:20=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")
val_labels(ces0411$religion04)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces0411$religion04)
table(ces0411$religion04)

#recode Language (ces04_CPS_INTLANG)
look_for(ces0411, "language")
ces0411$language04<-Recode(ces0411$ces04_CPS_INTLANG, "2=0; 1=1; else=NA")
val_labels(ces0411$language04)<-c(French=0, English=1)
#checks
val_labels(ces0411$language04)
table(ces0411$language04)

#recode Non-charter Language (ces04_CPS_S17)
look_for(ces0411, "language")
ces0411$non_charter_language04<-Recode(ces0411$ces04_CPS_S17, "1:5=0; 8:64=1; 65:66=0; 95:97=1; else=NA")
val_labels(ces0411$non_charter_language04)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces0411$non_charter_language04)
table(ces0411$non_charter_language04)

#recode Employment (ces04_CPS_S4)
look_for(ces0411, "employed")
#ces0411$employment04<-Recode(ces0411$ces04_CPS_S4, "3:7=0; 1:2=1; 8:11=1; else=NA")
#ces0411$employment06<-Recode(ces0411$ces06_CPS_S4, "3:7=0; 1:2=1; 8:12=1; 13=0; 14:15=1; else=NA")

ces0411 %>%
  mutate(employment04=case_when(
    ces04_CPS_S4==1 ~ 1,
    ces04_CPS_S4==2 ~ 1,
    ces04_CPS_S4>2 & ces04_CPS_S4<8 ~ 0,
    ces04_CPS_S4>7 & ces04_CPS_S4<12 ~ 1,
    ces06_CPS_S4==1 & ces04_rtype1==1~ 1,
    ces06_CPS_S4==2 & ces04_rtype1==1~ 1,
    ces06_CPS_S4>2 & ces06_CPS_S4<8 & ces04_rtype1==1~ 0,
    ces06_CPS_S4>7 & ces06_CPS_S4<13 & ces04_rtype1==1~ 1,
    ces06_CPS_S4==13 & ces04_rtype1==1~ 0,
    ces06_CPS_S4==14 & ces04_rtype1==1~ 1,
    ces06_CPS_S4==15 & ces04_rtype1==1~ 1
  ))->ces0411

val_labels(ces0411$employment04)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces0411$employment04)
table(ces0411$employment04)

#recode Sector (ces04_CPS_S5 & ces04_CPS_S4)
look_for(ces0411, "self-employed")
ces0411 %>% 
  mutate(sector04=case_when(
    ces04_CPS_S5==5 ~1,
    ces04_CPS_S5==1 ~0,
    ces04_CPS_S5==0 ~0,
    ces04_CPS_S4==1 ~0,
    ces04_CPS_S4> 2 & ces04_CPS_S4< 12 ~ 0,
    ces04_CPS_S5==9 ~NA_real_ ,
    ces04_CPS_S5==8 ~NA_real_ ,
    ces06_CPS_S5==5 & ces04_rtype1==1~1,
    ces06_CPS_S5==1 & ces04_rtype1==1~0,
    ces06_CPS_S5==0 & ces04_rtype1==1~0,
    ces06_CPS_S4==1 & ces04_rtype1==1~0,
    ces06_CPS_S4> 2 & ces06_CPS_S4< 15 & ces04_rtype1==1~ 0,
  ))->ces0411

val_labels(ces0411$sector04)<-c(Private=0, Public=1)
#checks
val_labels(ces0411$sector04)
table(ces0411$sector04)

#recode Party ID (ces04_CPS_Q1A@3 and ces04_CPS_Q1B@3`) ***note needs `...` to recognize the variable***
look_for(ces0411, "yourself")
ces0411 %>% 
  mutate(party_id04=case_when(
    `ces04_CPS_Q1A@3`==1 | `ces04_CPS_Q1B@3`==1 ~ 1,
    `ces04_CPS_Q1A@3`==2 | `ces04_CPS_Q1B@3`==2 ~ 2,
    `ces04_CPS_Q1A@3`==3 | `ces04_CPS_Q1B@3`==3 ~ 3,
    `ces04_CPS_Q1A@3`==5 | `ces04_CPS_Q1B@3`==5 ~ 2,
    `ces04_CPS_Q1A@3`==0 | `ces04_CPS_Q1B@3`==0 ~ 0,
    `ces04_CPS_Q1A@3`==6 | `ces04_CPS_Q1B@3`==6 ~ 2,
    `ces04_CPS_Q1A@3`==8 | `ces04_CPS_Q1B@3`==8 ~ 0,
    `ces04_CPS_Q1A@3`==4 | `ces04_CPS_Q1B@3`==4 ~ 0,
  ))->ces0411

val_labels(ces0411$party_id04)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces0411$party_id04)
table(ces0411$party_id04)

#recode Vote (ces04_PES_A3@3') ***note needs `...`` to recognize the variable***
look_for(ces0411, "party did you vote")
ces0411$vote04<-Recode(ces0411$`ces04_PES_A3@3`, "1=1; 2=2; 3=3; 4=4; 8=5; 6=2; 10=0; 0=0; else=NA")
val_labels(ces0411$vote04)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$vote04)
table(ces0411$vote04)

#recode Occupation (ces04_PINPORR)
look_for(ces0411, "occupation")
look_for(ces0411, "pinporr")
#ces0411$occupation04<-Recode(ces0411$ces04_PINPORR, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
#ces0411$occupation08<-Recode(ces0411$ces08_PES_S3_NOCS, "1:1000=2; 1100:1199=1; 2100:3300=1; 4100:6300=1; 1200:1500=3; 6400:6700=3; 3400:3500=3; 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
#Panel respondents are added in from ces08 - ces04_rtype1==1 indicates that a respondent participated in the ces04 survey
#Both Pinporr and NOCS available in ces04
ces0411 %>% 
  mutate(occupation04=case_when(
    ces04_PES_SD3 >0 & ces04_PES_SD3 <1100 ~ 2,
    ces04_PINPORR==1 ~ 1,
    ces04_PINPORR==2 ~ 1,
    ces04_PINPORR==4 ~ 1,
    ces04_PINPORR==5 ~ 1,
    ces04_PINPORR==3 ~ 2,
    ces04_PINPORR==6 ~ 2,
    ces04_PINPORR==7 ~ 2,
    ces04_PINPORR==9 ~ 3,
    ces04_PINPORR==12 ~ 3,
    ces04_PINPORR==14 ~ 3,
    ces04_PINPORR==8 ~ 4,
    ces04_PINPORR==10 ~ 4,
    ces04_PINPORR==13 ~ 5,
    ces04_PINPORR==15 ~ 5,
    ces04_PINPORR==16 ~ 5,
    # ces08_PES_S3_NOCS >0 & ces08_PES_S3_NOCS <1100 & ces04_rtype1==1~ 2,
    # ces08_PES_S3_NOCS >1099 & ces08_PES_S3_NOCS <1200 & ces04_rtype1==1~ 1,
    # ces08_PES_S3_NOCS >2099 & ces08_PES_S3_NOCS <2200 & ces04_rtype1==1~ 1,
    # ces08_PES_S3_NOCS >3099 & ces08_PES_S3_NOCS <3200 & ces04_rtype1==1~ 1,
    # ces08_PES_S3_NOCS >4099 & ces08_PES_S3_NOCS <4200 & ces04_rtype1==1~ 1,
    # ces08_PES_S3_NOCS >5099 & ces08_PES_S3_NOCS <5200 & ces04_rtype1==1~ 1,
    # ces08_PES_S3_NOCS >1199 & ces08_PES_S3_NOCS <1501 & ces04_rtype1==1~ 3,
    # ces08_PES_S3_NOCS >2199 & ces08_PES_S3_NOCS <3100 & ces04_rtype1==1~ 3,
    # ces08_PES_S3_NOCS >3199 & ces08_PES_S3_NOCS <4100 & ces04_rtype1==1~ 3,
    # ces08_PES_S3_NOCS >4199 & ces08_PES_S3_NOCS <5100 & ces04_rtype1==1~ 3,
    # ces08_PES_S3_NOCS >5199 & ces08_PES_S3_NOCS <6701 & ces04_rtype1==1~ 3,
    # ces08_PES_S3_NOCS >7199 & ces08_PES_S3_NOCS <7400 & ces04_rtype1==1~ 4,
    # ces08_PES_S3_NOCS >7399 & ces08_PES_S3_NOCS <7701 & ces04_rtype1==1~ 5,
    # ces08_PES_S3_NOCS >8199 & ces08_PES_S3_NOCS <8400 & ces04_rtype1==1~ 4,
    # ces08_PES_S3_NOCS >8399 & ces08_PES_S3_NOCS <8701 & ces04_rtype1==1~ 5,
    # ces08_PES_S3_NOCS >9199 & ces08_PES_S3_NOCS <9300 & ces04_rtype1==1~ 4,
    # ces08_PES_S3_NOCS >9599 & ces08_PES_S3_NOCS <9701 & ces04_rtype1==1~ 5,
  ))->ces0411
val_labels(ces0411$occupation04)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation04)
table(ces0411$occupation04)
ces0411$ces04_CPS_S4
#recode Occupation3 as 6 class schema with self-employed (ces04_CPS_S4)
look_for(ces0411, "employ")
ces0411$occupation04_3<-ifelse(ces0411$ces04_CPS_S4==1, 6, ces0411$occupation04)
val_labels(ces0411$occupation04_3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces0411$occupation04_3)
table(ces0411$occupation04_3)

#recode Income (ces04_CPS_S18)
look_for(ces0411, "income")
#ces0411$income04<-Recode(ces0411$ces04_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:7=4; 8:10=5; else=NA")
#ces0411$income06<-Recode(ces0411$ces06_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:9=4; 10=5; else=NA")

ces0411 %>% 
  mutate(income04=case_when(
    ces04_CPS_S18==1 ~ 1,
    ces04_CPS_S18==2 ~ 2,
    ces04_CPS_S18==3 ~ 2,
    ces04_CPS_S18==4 ~ 3,
    ces04_CPS_S18==5 ~ 3,
    ces04_CPS_S18==6 ~ 4,
    ces04_CPS_S18==7 ~ 4,
    ces04_CPS_S18==8 ~ 5,
    ces04_CPS_S18==9 ~ 5,
    ces04_CPS_S18==10 ~ 5,
    # ces06_CPS_S18==1 & ces04_rtype1==1 ~ 1,
    # ces06_CPS_S18==2 & ces04_rtype1==1 ~ 2,
    # ces06_CPS_S18==3 & ces04_rtype1==1 ~ 2,
    # ces06_CPS_S18==4 & ces04_rtype1==1 ~ 3,
    # ces06_CPS_S18==5 & ces04_rtype1==1 ~ 3,
    # ces06_CPS_S18==6 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==7 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==8 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==9 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==10 & ces04_rtype1==1 ~ 5,
  ))->ces0411

val_labels(ces0411$income04)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$income04)
table(ces0411$income04)

#recode Redistribution (ces04_CPS_F6)
look_for(ces0411, "rich")
val_labels(ces0411$ces04_CPS_F6)
ces0411$redistribution04<-Recode(ces0411$ces04_CPS_F6, "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$redistribution04)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces0411$redistribution04)
table(ces0411$redistribution04)

#recode Pro-Redistribution (ces04_CPS_F6)
ces0411$pro_redistribution04<-Recode(ces0411$ces04_CPS_F6, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces0411$pro_redistribution04)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces0411$pro_redistribution04)
table(ces0411$pro_redistribution04)

#----------------------------------------------------------------------------
###Recode 2006 2nd ####

# Gender done at top

#recode Union Respondent (ces06_CPS_S6B)

ces0411$union06<-Recode(ces0411$ces06_CPS_S6A, "1=1; 5=0; else=NA")
val_labels(ces0411$union06)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union06)
table(ces0411$union06)

#recode Union Combined (ces06_CPS_S6A and ces06_CPS_S6B)
ces0411 %>% 
  mutate(union_both06=case_when(
    ces06_CPS_S6A==1 | ces06_CPS_S6B==1 ~ 1,
    ces06_CPS_S6A==5 ~ 0,
    ces06_CPS_S6B==5 ~ 0,
    ces06_CPS_S6A==8 & ces06_CPS_S6B==8 ~ NA_real_,
    ces06_CPS_S6A==9 & ces06_CPS_S6B==9 ~ NA_real_,
    ces06_CPS_S4==1 ~ 0,
    (ces04_CPS_S6A==1 | ces04_CPS_S6B==1) & ces06_RECALL==1~ 1,
    ces04_CPS_S6A==5 & ces06_RECALL==1~ 0,
    ces04_CPS_S6B==5 & ces06_RECALL==1~ 0,
    ces04_CPS_S4==1 & ces06_RECALL==1~ 0,
#    ces08_CPS_S6A==1 | ces04_CPS_S6B==1 & ces06_RECALL==1~ 1,
#    ces08_CPS_S6A==5 & ces06_RECALL==1~ 0,
#    ces08_CPS_S6B==5 & ces06_RECALL==1~ 0,
  ))->ces0411

val_labels(ces0411$union_both06)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union_both06)
table(ces0411$union_both06)
table(as_factor(ces0411$ces06_CPS_S6A), as_factor(ces0411$union_both06), useNA = "ifany")

#recode Education (ces06_CPS_S3)
look_for(ces0411, "education")
#ces0411$degree06<-Recode(ces0411$ces06_CPS_S3, "9:11=1; 1:8=0; else=NA")
table(ces0411$ces04_CPS_S3, ces0411$ces06_CPS_S3, useNA="ifany")

ces0411 %>% 
  mutate(degree06=case_when(
    ces06_CPS_S3 >0 & ces06_CPS_S3 <9 ~ 0,
    ces06_CPS_S3 >8 & ces06_CPS_S3 <12 ~ 1,
    (ces04_CPS_S3 >0 & ces04_CPS_S3 <9) & ces06_RECALL==1~ 0,
    (ces04_CPS_S3 >8 & ces04_CPS_S3 <12) & ces06_RECALL==1~ 1,
  ))->ces0411

val_labels(ces0411$degree06)<-c(nodegree=0, degree=1)
#checks
val_labels(ces0411$degree06)
table(ces0411$degree06)

#recode Region (ces06_PROVINCE)
look_for(ces0411, "province")
ces0411$region06<-Recode(ces0411$ces06_PROVINCE, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces0411$region06)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces0411$region06)
table(ces0411$region06)

#recode Quebec (ces06_PROVINCE)
look_for(ces0411, "province")
ces0411$quebec06<-Recode(ces0411$ces06_PROVINCE, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces0411$quebec06)<-c(Other=0, Quebec=1)
#checks
val_labels(ces0411$quebec06)
table(ces0411$quebec06)

#recode Age (YEARofBIRTH)
table(str_detect(ces0411$survey, "06"))
#pipe data frame
ces0411 %>% 
  #mutate making new variable age06
  mutate(age06=case_when(
    str_detect(ces0411$survey, "06")~2006-yob
  ))-> ces0411
#check
table(ces0411$age06)

#recode Religion (ces06_CPS_S9)
look_for(ces0411, "relig")
#ces0411$religion06<-Recode(ces0411$ces06_CPS_S9, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:21=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")

ces0411 %>% 
  mutate(religion06=case_when(
    ces06_CPS_S9==0 ~ 0,
    ces06_CPS_S9==1 ~ 2,
    ces06_CPS_S9==2 ~ 2,
    ces06_CPS_S9==3 ~ 3,
    ces06_CPS_S9==4 ~ 1,
    ces06_CPS_S9==5 ~ 1,
    ces06_CPS_S9==6 ~ 3,
    ces06_CPS_S9==7 ~ 2,
    ces06_CPS_S9==8 ~ 3,
    ces06_CPS_S9==9 ~ 2,
    ces06_CPS_S9==10 ~ 2,
    ces06_CPS_S9==11 ~ 3,
    ces06_CPS_S9==12 ~ 2,
    ces06_CPS_S9==13 ~ 2,
    ces06_CPS_S9==14 ~ 2,
    ces06_CPS_S9==15 ~ 3,
    ces06_CPS_S9 >15 & ces06_CPS_S9 <22 ~ 2,
    ces06_CPS_S9==97 ~ 3,
    ces04_CPS_S9==0 & ces06_RECALL==1~ 0,
    ces04_CPS_S9==1 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==2 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==3 & ces06_RECALL==1~ 3,
    ces04_CPS_S9==4 & ces06_RECALL==1~ 1,
    ces04_CPS_S9==5 & ces06_RECALL==1~ 1,
    ces04_CPS_S9==6 & ces06_RECALL==1~ 3,
    ces04_CPS_S9==7 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==8 & ces06_RECALL==1~ 3,
    ces04_CPS_S9==9 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==10 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==11 & ces06_RECALL==1~ 3,
    ces04_CPS_S9==12 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==13 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==14 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==15 & ces06_RECALL==1~ 3,
    ces04_CPS_S9 >15 & ces04_CPS_S9 <22 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==97 & ces06_RECALL==1~ 3,
  ))->ces0411

val_labels(ces0411$religion06)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces0411$religion06)
table(ces0411$religion06)

#recode Language (ces06_CPS_INTLANG)
look_for(ces0411, "language")
ces0411$language06<-Recode(ces0411$ces06_CPS_INTLANG, "2=0; 1=1; else=NA")
val_labels(ces0411$language06)<-c(French=0, English=1)
#checks
val_labels(ces0411$language06)
table(ces0411$language06)

#recode Non-charter Language (ces06_CPS_S17)
look_for(ces0411, "language")
table(ces0411$ces06_CPS_S17)
#ces0411$non_charter_language06<-Recode(ces0411$ces06_CPS_S17, "1:5=0; 8:64=1; 65:66=0; 95:97=1; else=NA")

ces0411 %>% 
  mutate(non_charter_language06=case_when(
    ces06_CPS_S17 >0 & ces06_CPS_S17 <6 ~ 0,
    ces06_CPS_S17 >7 & ces06_CPS_S17 <65 ~ 1,
    ces06_CPS_S17 >64 & ces06_CPS_S17 <67 ~ 0,
    ces06_CPS_S17 >94 & ces06_CPS_S17 <98 ~ 1,
    (ces04_CPS_S17 >0 & ces04_CPS_S17 <6) & ces06_RECALL==1~ 0,
    (ces04_CPS_S17 >7 & ces04_CPS_S17 <65) & ces06_RECALL==1~ 1,
    (ces04_CPS_S17 >64 & ces04_CPS_S17 <67) & ces06_RECALL==1~ 0,
    (ces04_CPS_S17 >94 & ces04_CPS_S17 <98) & ces06_RECALL==1~ 1,
  ))->ces0411

val_labels(ces0411$non_charter_language06)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces0411$non_charter_language06)
table(ces0411$non_charter_language06)
table(ces0411$ces06_CPS_S17, ces0411$non_charter_language06, useNA = "ifany")
table(ces0411$survey, ces0411$non_charter_language06)

#recode Employment (ces06_CPS_S4)
look_for(ces0411, "employed")
ces0411$employment06<-Recode(ces0411$ces06_CPS_S4, "3:7=0; 1:2=1; 8:12=1; 13=0; 14:15=1; else=NA")
val_labels(ces0411$employment06)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces0411$employment06)
table(ces0411$employment06)

#recode Sector (ces06_CPS_S5 & ces06_CPS_S4)
look_for(ces0411, "self-employed")
ces0411 %>% 
  mutate(sector06=case_when(
    ces06_CPS_S5==5 ~1,
    ces06_CPS_S5==1 ~0,
    ces06_CPS_S5==0 ~0,
    ces06_CPS_S4==1 ~0,
    ces06_CPS_S4> 2 & ces06_CPS_S4< 16 ~ 0,
    ces06_CPS_S5==9 ~NA_real_ ,
    ces06_CPS_S5==8 ~NA_real_ ,
  ))->ces0411

val_labels(ces0411$sector06)<-c(Private=0, Public=1)
#checks
val_labels(ces0411$sector06)
table(ces0411$sector06)

#recode Party ID (ces06_CPS_Q1A and ces06_CPS_Q1B)
look_for(ces0411, "yourself")
ces0411 %>% 
  mutate(party_id06=case_when(
    ces06_CPS_Q1A==1 | ces06_CPS_Q1B==1 ~ 1,
    ces06_CPS_Q1A==2 | ces06_CPS_Q1B==2 ~ 2,
    ces06_CPS_Q1A==3 | ces06_CPS_Q1B==3 ~ 3,
    ces06_CPS_Q1A==5 | ces06_CPS_Q1B==5 ~ 0,
    ces06_CPS_Q1A==0 | ces06_CPS_Q1B==0 ~ 0,
    ces06_CPS_Q1A==4 | ces06_CPS_Q1B==4 ~ 0,
    ces06_CPS_Q1A==9 | ces06_CPS_Q1B==9 ~ 0,
  ))->ces0411

val_labels(ces0411$party_id06)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces0411$party_id06)
table(ces0411$party_id06)

#recode Vote (ces06_PES_B4A and ces06_PES_B4B) 
look_for(ces0411, "party did you vote")
ces0411$ces06_PES_B4A
ces0411 %>% 
  mutate(vote06=case_when(
    ces06_PES_B4A==1 | ces06_PES_B4B==1 ~ 1,
    ces06_PES_B4A==2 | ces06_PES_B4B==2 ~ 2,
    ces06_PES_B4A==3 | ces06_PES_B4B==3 ~ 3,
    ces06_PES_B4A==5 | ces06_PES_B4B==5 ~ 5,
    ces06_PES_B4A==0 | ces06_PES_B4B==0 ~ 0,
    ces06_PES_B4A==4 | ces06_PES_B4B==4 ~ 4,
  ))->ces0411
table(ces0411$ces06_PES_B4A, ces0411$vote06)
val_labels(ces0411$vote06)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$vote06)
table(ces0411$vote06)

#recode Occupation (ces06_PES_SD3)
look_for(ces0411, "occupation")
look_for(ces0411, "employ")
#ces0411$occupation06<-Recode(ces0411$ces06_PES_SD3, "1:1000=2; 1100:1199=1; 2100:3300=1; 4100:6300=1; 1200:1500=3; 6400:6700=3; 3400:3500=3; 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
#ces0411$occupation04<-Recode(ces0411$ces04_PINPORR, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
#Panel respondents are added in from both ces04 and ces08 - ces06_RECALL==1 indicates that a respondent participated in the ces06 survey
ces0411 %>% 
  mutate(occupation06=case_when(
    ces06_PES_SD3 >0 & ces06_PES_SD3 <1100 ~ 2,
    ces06_PES_SD3 >1099 & ces06_PES_SD3 <1200 ~ 1,
    ces06_PES_SD3 >2099 & ces06_PES_SD3 <2200 ~ 1,
    ces06_PES_SD3 >3099 & ces06_PES_SD3 <3200 ~ 1,
    ces06_PES_SD3 >4099 & ces06_PES_SD3 <4200 ~ 1,
    ces06_PES_SD3 >5099 & ces06_PES_SD3 <5200 ~ 1,
    ces06_PES_SD3 >1199 & ces06_PES_SD3 <1501 ~ 3,
    ces06_PES_SD3 >2199 & ces06_PES_SD3 <3100 ~ 3,
    ces06_PES_SD3 >3199 & ces06_PES_SD3 <4100 ~ 3,
    ces06_PES_SD3 >4199 & ces06_PES_SD3 <5100 ~ 3,
    ces06_PES_SD3 >5199 & ces06_PES_SD3 <6701 ~ 3,
    ces06_PES_SD3 >7199 & ces06_PES_SD3 <7400 ~ 4,
    ces06_PES_SD3 >7399 & ces06_PES_SD3 <7701 ~ 5,
    ces06_PES_SD3 >8199 & ces06_PES_SD3 <8400 ~ 4,
    ces06_PES_SD3 >8399 & ces06_PES_SD3 <8701 ~ 5,
    ces06_PES_SD3 >9199 & ces06_PES_SD3 <9300 ~ 4,
    ces06_PES_SD3 >9299 & ces06_PES_SD3 <9701 ~ 5,
    (ces04_PES_SD3 >0 & ces04_PES_SD3 <1001) & ces06_RECALL==1~ 2,
    ces04_PINPORR==1 & ces06_RECALL==1~ 1,
    ces04_PINPORR==2 & ces06_RECALL==1~ 1,
    ces04_PINPORR==4 & ces06_RECALL==1~ 1,
    ces04_PINPORR==5 & ces06_RECALL==1~ 1,
    ces04_PINPORR==3 & ces06_RECALL==1~ 2,
    ces04_PINPORR==6 & ces06_RECALL==1~ 2,
    ces04_PINPORR==7 & ces06_RECALL==1~ 2,
    ces04_PINPORR==9 & ces06_RECALL==1~ 3,
    ces04_PINPORR==12 & ces06_RECALL==1~ 3,
    ces04_PINPORR==14 & ces06_RECALL==1~ 3,
    ces04_PINPORR==8 & ces06_RECALL==1~ 4,
    ces04_PINPORR==10 & ces06_RECALL==1~ 4,
    ces04_PINPORR==13 & ces06_RECALL==1~ 5,
    ces04_PINPORR==15 & ces06_RECALL==1~ 5,
    ces04_PINPORR==16 & ces06_RECALL==1~ 5,
    # ces08_PES_S3_NOCS >0 & ces08_PES_S3_NOCS <1100 & ces06_RECALL==1~ 2,
    # ces08_PES_S3_NOCS >1099 & ces08_PES_S3_NOCS <1200 & ces06_RECALL==1~ 1,
    # ces08_PES_S3_NOCS >2099 & ces08_PES_S3_NOCS <2200 & ces06_RECALL==1~ 1,
    # ces08_PES_S3_NOCS >3099 & ces08_PES_S3_NOCS <3200 & ces06_RECALL==1~ 1,
    # ces08_PES_S3_NOCS >4099 & ces08_PES_S3_NOCS <4200 & ces06_RECALL==1~ 1,
    # ces08_PES_S3_NOCS >5099 & ces08_PES_S3_NOCS <5200 & ces06_RECALL==1~ 1,
    # ces08_PES_S3_NOCS >1199 & ces08_PES_S3_NOCS <1501 & ces06_RECALL==1~ 3,
    # ces08_PES_S3_NOCS >2199 & ces08_PES_S3_NOCS <3100 & ces06_RECALL==1~ 3,
    # ces08_PES_S3_NOCS >3199 & ces08_PES_S3_NOCS <4100 & ces06_RECALL==1~ 3,
    # ces08_PES_S3_NOCS >4199 & ces08_PES_S3_NOCS <5100 & ces06_RECALL==1~ 3,
    # ces08_PES_S3_NOCS >5199 & ces08_PES_S3_NOCS <6701 & ces06_RECALL==1~ 3,
    # ces08_PES_S3_NOCS >7199 & ces08_PES_S3_NOCS <7400 & ces06_RECALL==1~ 4,
    # ces08_PES_S3_NOCS >7399 & ces08_PES_S3_NOCS <7701 & ces06_RECALL==1~ 5,
    # ces08_PES_S3_NOCS >8199 & ces08_PES_S3_NOCS <8400 & ces06_RECALL==1~ 4,
    # ces08_PES_S3_NOCS >8399 & ces08_PES_S3_NOCS <8701 & ces06_RECALL==1~ 5,
    # ces08_PES_S3_NOCS >9199 & ces08_PES_S3_NOCS <9300 & ces06_RECALL==1~ 4,
    # ces08_PES_S3_NOCS >9599 & ces08_PES_S3_NOCS <9701 & ces06_RECALL==1~ 5,
  ))->ces0411
val_labels(ces0411$occupation06)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation06)
table(ces0411$occupation06)

#recode Occupation3 as 6 class schema with self-employed (ces06_CPS_S4)
look_for(ces0411, "employ")
ces0411$occupation06_3<-ifelse(ces0411$ces06_CPS_S4==1, 6, ces0411$occupation06)
#ces0411$occupation06_3<-ifelse((ces0411$ces04_CPS_S4==1 & ces0411$ces06_RECALL==1), 6, ces0411$occupation06)
val_labels(ces0411$occupation06_3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces0411$occupation06_3)
table(ces0411$occupation06_3)

#recode Income (ces06_CPS_S18)
look_for(ces0411, "income")
#ces0411$income06<-Recode(ces0411$ces06_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:9=4; 10=5; else=NA")
#ces0411$income04<-Recode(ces0411$ces04_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:7=4; 8:10=5; else=NA")
ces0411 %>% 
  mutate(income06=case_when(
    ces06_CPS_S18==1 ~ 1,
    ces06_CPS_S18==2 ~ 2,
    ces06_CPS_S18==3 ~ 2,
    ces06_CPS_S18==4 ~ 3,
    ces06_CPS_S18==5 ~ 3,
    ces06_CPS_S18==6 ~ 4,
    ces06_CPS_S18==7 ~ 4,
    ces06_CPS_S18==8 ~ 4,
    ces06_CPS_S18==9 ~ 4,
    ces06_CPS_S18==10 ~ 5,
    ces04_CPS_S18==1 & ces06_RECALL==1 ~ 1,
    ces04_CPS_S18==2 & ces06_RECALL==1 ~ 2,
    ces04_CPS_S18==3 & ces06_RECALL==1 ~ 2,
    ces04_CPS_S18==4 & ces06_RECALL==1 ~ 3,
    ces04_CPS_S18==5 & ces06_RECALL==1 ~ 3,
    ces04_CPS_S18==6 & ces06_RECALL==1 ~ 4,
    ces04_CPS_S18==7 & ces06_RECALL==1 ~ 4,
    ces04_CPS_S18==8 & ces06_RECALL==1 ~ 5,
    ces04_CPS_S18==9 & ces06_RECALL==1 ~ 5,
    ces04_CPS_S18==10 & ces06_RECALL==1 ~ 5,
    # ces08_PES_S9A> -1 & ces08_PES_S9A < 30 & ces06_RECALL==1~ 1,
    # ces08_PES_S9A> -1 & ces08_PES_S9A < 30 & ces06_RECALL==1~ 1,
    # ces08_PES_S9A> 29 & ces08_PES_S9A < 50 & ces06_RECALL==1~ 2,
    # ces08_PES_S9A> 29 & ces08_PES_S9A < 50 & ces06_RECALL==1~ 2,
    # ces08_PES_S9A> 49 & ces08_PES_S9A < 70 & ces06_RECALL==1~ 3,
    # ces08_PES_S9A> 49 & ces08_PES_S9A < 70 & ces06_RECALL==1~ 3,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 99 & ces08_PES_S9A < 998 & ces06_RECALL==1~ 5,
    # ces08_PES_S9A> 99 & ces08_PES_S9A < 998 & ces06_RECALL==1~ 5,
  ))->ces0411

val_labels(ces0411$income06)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$income06)
table(ces0411$income06)

#recode Redistribution (ces06_CPS_F6)
look_for(ces0411, "rich")
val_labels(ces0411$ces06_CPS_F6)
ces0411$redistribution06<-Recode(ces0411$ces06_CPS_F6, "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$redistribution06)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces0411$redistribution06)
table(ces0411$redistribution06)

#recode Pro-Redistribution (ces06_CPS_F6)
ces0411$pro_redistribution06<-Recode(ces0411$ces06_CPS_F6, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces0411$pro_redistribution06)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces0411$pro_redistribution06)
table(ces0411$pro_redistribution06)

#----------------------------------------------------------------------------
####Recode 2008 3rd ####

# Gender done at top

#recode Union Respondent (ces08_CPS_S6A)
look_for(ces0411, "union")
#Make sure the union variable is coming from the respondent question
ces0411$union08<-Recode(ces0411$ces08_CPS_S6A, "1=1; 5=0; else=NA")
val_labels(ces0411$union08)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union08)
table(ces0411$union08)

#recode Union Combined (ces08_CPS_S6A and ces08_CPS_S6B)
ces0411 %>% 
  mutate(union_both08=case_when(
    ces08_CPS_S6A==1 | ces08_CPS_S6B==1 ~ 1,
    ces08_CPS_S6A==5 ~ 0,
    ces08_CPS_S6B==5 ~ 0,
    ces08_CPS_S6A==8 & ces08_CPS_S6B==8 ~ NA_real_,
    ces08_CPS_S6A==9 & ces08_CPS_S6B==9 ~ NA_real_,
    (ces06_CPS_S6A==1 | ces06_CPS_S6B==1) & ces08_CPS_REPLICATE==9999~ 1,
    (ces06_CPS_S6A==5) & ces08_CPS_REPLICATE==9999~ 0,
    (ces06_CPS_S6B==5) & ces08_CPS_REPLICATE==9999~ 0,
    (ces06_CPS_S4==1) & ces08_CPS_REPLICATE==9999~ 0,
    # (ces04_CPS_S6A==1 | ces04_CPS_S6B==1) & ces08_CPS_REPLICATE==9999~ 1,
    # (ces04_CPS_S6A==5) & ces08_CPS_REPLICATE==9999~ 0,
    # (ces04_CPS_S6B==5) & ces08_CPS_REPLICATE==9999~ 0,
  ))->ces0411

val_labels(ces0411$union_both08)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union_both08)
table(ces0411$union_both08)
table(as_factor(ces0411$ces08_CPS_S6A), as_factor(ces0411$union_both08), useNA = "ifany")

#recode Education (ces08_CPS_S3)
look_for(ces0411, "education")
#ces0411$degree08<-Recode(ces0411$ces08_CPS_S3, "9:11=1; 1:8=0; else=NA")

ces0411 %>% 
  mutate(degree08=case_when(
    ces08_CPS_S3 >0 & ces08_CPS_S3 <9 ~ 0,
    ces08_CPS_S3 >8 & ces08_CPS_S3 <12 ~ 1,
    # (ces04_CPS_S3 >0 & ces04_CPS_S3 <9) & ces08_CPS_REPLICATE==9999 ~ 0,
    # (ces04_CPS_S3 >8 & ces04_CPS_S3 <12) & ces08_CPS_REPLICATE==9999 ~ 1,
  ))->ces0411

val_labels(ces0411$degree08)<-c(nodegree=0, degree=1)
#checks
val_labels(ces0411$degree08)
table(ces0411$degree08)

#recode Region (ces08_PROVINCE)
look_for(ces0411, "province")
ces0411$region08<-Recode(ces0411$ces08_PROVINCE, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces0411$region08)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces0411$region08)
table(ces0411$region08)

#recode Quebec (ces08_PROVINCE)
look_for(ces0411, "province")
ces0411$quebec08<-Recode(ces0411$ces08_PROVINCE, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces0411$quebec08)<-c(Other=0, Quebec=1)
#checks
val_labels(ces0411$quebec08)
table(ces0411$quebec08)

#recode Age (YEARofBIRTH)
table(str_detect(ces0411$survey, "08"))
#pipe data frame
ces0411 %>% 
  #mutate making new variable age08
  mutate(age08=case_when(
    str_detect(ces0411$survey, "08")~2008-yob
    #dump in a test dataframe
  ))-> ces0411
#check
table(ces0411$age08)

#recode Religion (ces08_CPS_S9)
look_for(ces0411, "relig")
#ces0411$religion08<-Recode(ces0411$ces08_CPS_S9, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:20=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")

ces0411 %>% 
  mutate(religion08=case_when(
    ces08_CPS_S9==0 ~ 0,
    ces08_CPS_S9==1 ~ 2,
    ces08_CPS_S9==2 ~ 2,
    ces08_CPS_S9==3 ~ 3,
    ces08_CPS_S9==4 ~ 1,
    ces08_CPS_S9==5 ~ 1,
    ces08_CPS_S9==6 ~ 3,
    ces08_CPS_S9==7 ~ 2,
    ces08_CPS_S9==8 ~ 3,
    ces08_CPS_S9==9 ~ 2,
    ces08_CPS_S9==10 ~ 2,
    ces08_CPS_S9==11 ~ 3,
    ces08_CPS_S9==12 ~ 2,
    ces08_CPS_S9==13 ~ 2,
    ces08_CPS_S9==14 ~ 2,
    ces08_CPS_S9==15 ~ 3,
    ces08_CPS_S9 >15 & ces08_CPS_S9 <21 ~ 2,
    ces08_CPS_S9==97 ~ 3,
    # ces04_CPS_S9==0 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S9==1 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==2 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==3 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9==4 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S9==5 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S9==6 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9==7 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==8 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9==9 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==10 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==11 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9==12 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==13 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==14 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==15 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9 >15 & ces04_CPS_S9 <22 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==97 & ces08_CPS_REPLICATE==9999~ 3,
  ))->ces0411

val_labels(ces0411$religion08)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces0411$religion08)
table(ces0411$religion08)

#recode Language (ces08_CPS_INTLANG)
look_for(ces0411, "language")
#ces0411$language08<-Recode(ces0411$ces08_CPS_INTLANG, "2=0; 1=1; else=NA")

ces0411 %>% 
  mutate(language08=case_when(
    ces08_CPS_INTLANG==2 ~ 0,
    ces08_CPS_INTLANG==1 ~ 1,
    # ces04_CPS_INTLANG==2 & ces08_CPS_REPLICATE==9999 ~ 0,
    # ces04_CPS_INTLANG==1 & ces08_CPS_REPLICATE==9999 ~ 1,
  ))->ces0411

val_labels(ces0411$language08)<-c(French=0, English=1)
#checks
val_labels(ces0411$language08)
table(ces0411$language08)

#recode Non-charter Language (ces08_CPS_S17)
look_for(ces0411, "language")
#ces0411$non_charter_language08<-Recode(ces0411$ces08_CPS_S17, "1:5=0; 8:64=1; 65:66=0; 95:97=1; else=NA")

ces0411 %>% 
  mutate(non_charter_language08=case_when(
    ces08_CPS_S17 >0 & ces08_CPS_S17 <6 ~ 0,
    ces08_CPS_S17 >7 & ces08_CPS_S17 <65 ~ 1,
    ces08_CPS_S17 >64 & ces08_CPS_S17 <67 ~ 0,
    ces08_CPS_S17 >94 & ces08_CPS_S17 <98 ~ 1,
    # ces04_CPS_S17 >0 & ces04_CPS_S17 <6 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S17 >7 & ces04_CPS_S17 <65 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S17 >64 & ces04_CPS_S17 <67 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S17 >94 & ces04_CPS_S17 <98 & ces08_CPS_REPLICATE==9999~ 1,
  ))->ces0411

val_labels(ces0411$non_charter_language08)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces0411$non_charter_language08)
table(ces0411$non_charter_language08)
table(ces0411$survey, ces0411$non_charter_language08)

#recode Employment (ces08_CPS_S4)
look_for(ces0411, "employed")
#ces0411$employment08<-Recode(ces0411$ces08_CPS_S4, "3:7=0; 1:2=1; 8:11=1; else=NA")
#ces0411$employment06<-Recode(ces0411$ces06_CPS_S4, "3:7=0; 1:2=1; 8:12=1; 13=0; 14:15=1; else=NA")
#ces0411$employment04<-Recode(ces0411$ces04_CPS_S4, "3:7=0; 1:2=1; 8:11=1; else=NA")

ces0411 %>% 
  mutate(employment08=case_when(
    ces08_CPS_S4 >2 & ces08_CPS_S4 <8 ~ 0,
    ces08_CPS_S4 >7 & ces08_CPS_S4 <12 ~ 1,
    ces08_CPS_S4==1 ~ 1,
    ces08_CPS_S4==2 ~ 1,
    # ces06_CPS_S4 >2 & ces06_CPS_S4 <8 & ces08_CPS_REPLICATE==9999~ 0,
    # ces06_CPS_S4 >7 & ces06_CPS_S4 <13 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_CPS_S4==1 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_CPS_S4==2 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_CPS_S4==14 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_CPS_S4==15 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S4 >2 & ces04_CPS_S4 <8 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S4 >7 & ces04_CPS_S4 <13 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S4==1 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S4==2 & ces08_CPS_REPLICATE==9999~ 1,
  ))->ces0411

val_labels(ces0411$employment08)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces0411$employment08)
table(ces0411$employment08)

#recode Sector (ces08_CPS_S5 & ces08_CPS_S4)
look_for(ces0411, "self-employed")
ces0411 %>% 
  mutate(sector08=case_when(
    ces08_CPS_S5==5 ~1,
    ces08_CPS_S5==1 ~0,
    ces08_CPS_S5==0 ~0,
    ces08_CPS_S4==1 ~0,
    ces08_CPS_S4> 2 & ces08_CPS_S4< 12 ~ 0,
    ces08_CPS_S5==9 ~NA_real_ ,
    ces08_CPS_S5==8 ~NA_real_ ,
    # ces06_CPS_S5==5 & ces08_CPS_REPLICATE==9999~1,
    # ces06_CPS_S5==1 & ces08_CPS_REPLICATE==9999~0,
    # ces06_CPS_S5==0 & ces08_CPS_REPLICATE==9999~0,
    # ces06_CPS_S4==1 & ces08_CPS_REPLICATE==9999~0,
    # ces06_CPS_S4> 2 & ces06_CPS_S4< 16 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S5==5 & ces08_CPS_REPLICATE==9999~1,
    # ces04_CPS_S5==1 & ces08_CPS_REPLICATE==9999~0,
    # ces04_CPS_S5==0 & ces08_CPS_REPLICATE==9999~0,
    # ces04_CPS_S4==1 & ces08_CPS_REPLICATE==9999~0,
    # ces04_CPS_S4> 2 & ces04_CPS_S4< 12 & ces08_CPS_REPLICATE==9999~ 0,
  ))->ces0411

val_labels(ces0411$sector08)<-c(Private=0, Public=1)
#checks
val_labels(ces0411$sector08)
table(ces0411$sector08)

#recode Party ID (ces08_PES_K1)
look_for(ces0411, "yourself")
ces0411$party_id08<-Recode(ces0411$ces08_PES_K1, "1=1; 2=2; 3=3; 4:5=0; 0=0; else=NA")
val_labels(ces0411$party_id08)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces0411$party_id08)
table(ces0411$party_id08)

#recode Vote (ces08_PES_B4B) 
look_for(ces0411, "party did you vote")
ces0411$vote08<-Recode(ces0411$ces08_PES_B4B, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces0411$vote08)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$vote08)
table(ces0411$vote08)

#recode Occupation (ces08_PES_S3_NOCS)
look_for(ces0411, "occupation")
look_for(ces0411, "employ")
#ces0411$occupation08<-Recode(ces0411$ces08_PES_S3_NOCS, "1:1000=2; 1100:1199=1; 2100:3300=1; 4100:6300=1; 1200:1500=3; 6400:6700=3; 3400:3500=3; 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
#ces0411$occupation06<-Recode(ces0411$ces06_PES_SD3, "1:1000=2; 1100:1199=1; 2100:3300=1; 4100:6300=1; 1200:1500=3; 6400:6700=3; 3400:3500=3; 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
#ces0411$occupation04<-Recode(ces0411$ces04_PINPORR, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
#Panel respondents are added in from both ces04 and ces06 - ces08_CPS_REPLICATE==9999 indicates that a respondent participated in the ces08 survey
ces0411 %>% 
  mutate(occupation08=case_when(
    ces08_PES_S3_NOCS >0 & ces08_PES_S3_NOCS <1100 ~ 2,
    ces08_PES_S3_NOCS >1099 & ces08_PES_S3_NOCS <1200 ~ 1,
    ces08_PES_S3_NOCS >2099 & ces08_PES_S3_NOCS <2200 ~ 1,
    ces08_PES_S3_NOCS >3099 & ces08_PES_S3_NOCS <3200 ~ 1,
    ces08_PES_S3_NOCS >4099 & ces08_PES_S3_NOCS <4200 ~ 1,
    ces08_PES_S3_NOCS >5099 & ces08_PES_S3_NOCS <5200 ~ 1,
    ces08_PES_S3_NOCS >1199 & ces08_PES_S3_NOCS <1501 ~ 3,
    ces08_PES_S3_NOCS >2199 & ces08_PES_S3_NOCS <3100 ~ 3,
    ces08_PES_S3_NOCS >3199 & ces08_PES_S3_NOCS <4100 ~ 3,
    ces08_PES_S3_NOCS >4199 & ces08_PES_S3_NOCS <5100 ~ 3,
    ces08_PES_S3_NOCS >5199 & ces08_PES_S3_NOCS <6701 ~ 3,
    ces08_PES_S3_NOCS >7199 & ces08_PES_S3_NOCS <7400 ~ 4,
    ces08_PES_S3_NOCS >7399 & ces08_PES_S3_NOCS <7701 ~ 5,
    ces08_PES_S3_NOCS >8199 & ces08_PES_S3_NOCS <8400 ~ 4,
    ces08_PES_S3_NOCS >8399 & ces08_PES_S3_NOCS <8701 ~ 5,
    ces08_PES_S3_NOCS >9199 & ces08_PES_S3_NOCS <9300 ~ 4,
    ces08_PES_S3_NOCS >9599 & ces08_PES_S3_NOCS <9701 ~ 5,
    # ces06_PES_SD3 >0 & ces06_PES_SD3 <1100 & ces08_CPS_REPLICATE==9999~ 2,
    # ces06_PES_SD3 >1099 & ces06_PES_SD3 <1200 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_PES_SD3 >2099 & ces06_PES_SD3 <2200 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_PES_SD3 >3099 & ces06_PES_SD3 <3200 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_PES_SD3 >4099 & ces06_PES_SD3 <4200 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_PES_SD3 >5099 & ces06_PES_SD3 <5200 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_PES_SD3 >1199 & ces06_PES_SD3 <1501 & ces08_CPS_REPLICATE==9999~ 3,
    # ces06_PES_SD3 >2199 & ces06_PES_SD3 <3100 & ces08_CPS_REPLICATE==9999~ 3,
    # ces06_PES_SD3 >3199 & ces06_PES_SD3 <4100 & ces08_CPS_REPLICATE==9999~ 3,
    # ces06_PES_SD3 >4199 & ces06_PES_SD3 <5100 & ces08_CPS_REPLICATE==9999~ 3,
    # ces06_PES_SD3 >5199 & ces06_PES_SD3 <6701 & ces08_CPS_REPLICATE==9999~ 3,
    # ces06_PES_SD3 >7199 & ces06_PES_SD3 <7400 & ces08_CPS_REPLICATE==9999~ 4,
    # ces06_PES_SD3 >7399 & ces06_PES_SD3 <7701 & ces08_CPS_REPLICATE==9999~ 5,
    # ces06_PES_SD3 >8199 & ces06_PES_SD3 <8400 & ces08_CPS_REPLICATE==9999~ 4,
    # ces06_PES_SD3 >8399 & ces06_PES_SD3 <8701 & ces08_CPS_REPLICATE==9999~ 5,
    # ces06_PES_SD3 >9199 & ces06_PES_SD3 <9300 & ces08_CPS_REPLICATE==9999~ 4,
    # ces06_PES_SD3 >9299 & ces06_PES_SD3 <9701 & ces08_CPS_REPLICATE==9999~ 5,
    # ces04_PES_SD3 >0 & ces04_PES_SD3 <1001 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_PINPORR==1 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_PINPORR==2 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_PINPORR==4 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_PINPORR==5 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_PINPORR==3 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_PINPORR==6 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_PINPORR==7 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_PINPORR==9 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_PINPORR==12 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_PINPORR==14 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_PINPORR==8 & ces08_CPS_REPLICATE==9999~ 4,
    # ces04_PINPORR==10 & ces08_CPS_REPLICATE==9999~ 4,
    # ces04_PINPORR==13 & ces08_CPS_REPLICATE==9999~ 5,
    # ces04_PINPORR==15 & ces08_CPS_REPLICATE==9999~ 5,
    # ces04_PINPORR==16 & ces08_CPS_REPLICATE==9999~ 5,
  ))->ces0411
val_labels(ces0411$occupation08)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation08)
ces0411 %>% 
  filter(str_detect(survey, "PES08")) %>% 
  select(occupation08) %>% 
  group_by(occupation08) %>% 
  summarise(total=sum(occupation08), missing=sum(is.na(occupation08)))

#recode Occupation3 as 6 class schema with self-employed (ces08_CPS_S4)
look_for(ces0411, "employ")
ces0411$occupation08_3<-ifelse(ces0411$ces08_CPS_S4==1, 6, ces0411$occupation08)
val_labels(ces0411$occupation08_3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces0411$occupation08_3)
table(ces0411$occupation08_3)

#recode Income (ces08_CPS_S18A, ces08_CPS_S18B, ces08_PES_S9A, ces08_PES_S9B)
look_for(ces0411, "income")
#ces0411$income06<-Recode(ces0411$ces06_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:9=4; 10=5; else=NA")
#ces0411$income04<-Recode(ces0411$ces04_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:7=4; 8:10=5; else=NA")
ces0411 %>% 
  mutate(income08=case_when(
    ces08_CPS_S18B==1 | ces08_CPS_S18A> -1 & ces08_CPS_S18A < 30 | ces08_PES_S9B==1 | ces08_PES_S9A> -1 & ces08_PES_S9A < 30 ~ 1,
    ces08_CPS_S18B==2 | ces08_CPS_S18A> -1 & ces08_CPS_S18A < 30 | ces08_PES_S9B==2 | ces08_PES_S9A> -1 & ces08_PES_S9A < 30 ~ 1,
    ces08_CPS_S18B==3 | ces08_CPS_S18A> 29 & ces08_CPS_S18A < 50 | ces08_PES_S9B==3 | ces08_PES_S9A> 29 & ces08_PES_S9A < 50 ~ 2,
    ces08_CPS_S18B==4 | ces08_CPS_S18A> 29 & ces08_CPS_S18A < 50 | ces08_PES_S9B==4 | ces08_PES_S9A> 29 & ces08_PES_S9A < 50 ~ 2,
    ces08_CPS_S18B==5 | ces08_CPS_S18A> 49 & ces08_CPS_S18A < 70 | ces08_PES_S9B==5 | ces08_PES_S9A> 49 & ces08_PES_S9A < 70 ~ 3,
    ces08_CPS_S18B==6 | ces08_CPS_S18A> 49 & ces08_CPS_S18A < 70 | ces08_PES_S9B==6 | ces08_PES_S9A> 49 & ces08_PES_S9A < 70 ~ 3,
    ces08_CPS_S18B==7 | ces08_CPS_S18A> 69 & ces08_CPS_S18A < 100 | ces08_PES_S9B==7 | ces08_PES_S9A> 69 & ces08_PES_S9A < 100 ~ 4,
    ces08_CPS_S18B==8 | ces08_CPS_S18A> 69 & ces08_CPS_S18A < 100 | ces08_PES_S9B==8 | ces08_PES_S9A> 69 & ces08_PES_S9A < 100 ~ 4,
    ces08_CPS_S18B==9 | ces08_CPS_S18A> 69 & ces08_CPS_S18A < 100 | ces08_PES_S9B==9 | ces08_PES_S9A> 69 & ces08_PES_S9A < 100 ~ 4,
    ces08_CPS_S18B==10 | ces08_CPS_S18A> 69 & ces08_CPS_S18A < 100 | ces08_PES_S9B==10 | ces08_PES_S9A> 69 & ces08_PES_S9A < 100 ~ 4,
    ces08_CPS_S18B==11 | ces08_CPS_S18A> 99 & ces08_CPS_S18A < 998 | ces08_PES_S9B==11 | ces08_PES_S9A> 99 & ces08_PES_S9A < 998 ~ 5,
    ces08_CPS_S18B==12 | ces08_CPS_S18A> 99 & ces08_CPS_S18A < 998 | ces08_PES_S9B==12 | ces08_PES_S9A> 99 & ces08_PES_S9A < 998 ~ 5,
    # ces06_CPS_S18==1 & ces08_CPS_REPLICATE==9999 ~ 1,
    # ces06_CPS_S18==2 & ces08_CPS_REPLICATE==9999 ~ 2,
    # ces06_CPS_S18==3 & ces08_CPS_REPLICATE==9999 ~ 2,
    # ces06_CPS_S18==4 & ces08_CPS_REPLICATE==9999 ~ 3,
    # ces06_CPS_S18==5 & ces08_CPS_REPLICATE==9999 ~ 3,
    # ces06_CPS_S18==6 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces06_CPS_S18==7 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces06_CPS_S18==8 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces06_CPS_S18==9 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces06_CPS_S18==10 & ces08_CPS_REPLICATE==9999 ~ 5,
    # ces04_CPS_S18==1 & ces08_CPS_REPLICATE==9999 ~ 1,
    # ces04_CPS_S18==2 & ces08_CPS_REPLICATE==9999 ~ 2,
    # ces04_CPS_S18==3 & ces08_CPS_REPLICATE==9999 ~ 2,
    # ces04_CPS_S18==4 & ces08_CPS_REPLICATE==9999 ~ 3,
    # ces04_CPS_S18==5 & ces08_CPS_REPLICATE==9999 ~ 3,
    # ces04_CPS_S18==6 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces04_CPS_S18==7 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces04_CPS_S18==8 & ces08_CPS_REPLICATE==9999 ~ 5,
    # ces04_CPS_S18==9 & ces08_CPS_REPLICATE==9999 ~ 5,
    # ces04_CPS_S18==10 & ces08_CPS_REPLICATE==9999 ~ 5,
  ))->ces0411

val_labels(ces0411$income08)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$income08)
table(ces0411$income08)

#recode Redistribution (ces08_PES_F6)
look_for(ces0411, "rich")
val_labels(ces0411$ces08_PES_F6)
ces0411$redistribution08<-Recode(ces0411$ces08_PES_F6, "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$redistribution08)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces0411$redistribution08)
table(ces0411$redistribution08)

#recode Pro-Redistribution (ces08_PES_F6)
ces0411$pro_redistribution08<-Recode(ces0411$ces08_PES_F6, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces0411$pro_redistribution08)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces0411$pro_redistribution08)
table(ces0411$pro_redistribution08)

#----------------------------------------------------------------------------
####Recode 2011 4th ####

# Gender done at top

#recode Union Respondent (PES11_93)
look_for(ces0411, "union")
ces0411$union11<-Recode(ces0411$PES11_93, "1=1; 5=0; else=NA")
val_labels(ces0411$union11)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union11)
table(ces0411$union11)

#recode Union Combined (PES11_93 and PES11_94)
ces0411 %>% 
  mutate(union_both11=case_when(
    #If the person is in a union OR if the household is in a union, then they get a 1
    PES11_93==1 | PES11_94==1 ~ 1,
    PES11_94==5 ~ 0,
    PES11_93==5 ~ 0,
    PES11_93==8 & PES11_94==8 ~ NA_real_,
    PES11_93==9 & PES11_94==9 ~ NA_real_,
  ))->ces0411

table(as_factor(ces0411$union_both11), as_factor(ces0411$PES11_93))
table(as_factor(ces0411$union_both11), as_factor(ces0411$PES11_94))
val_labels(ces0411$union_both11)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union_both11)
table(ces0411$union_both11)

#recode Education (CPS11_79)
look_for(ces0411, "education")
ces0411$degree11<-Recode(ces0411$CPS11_79, "9:11=1; 1:8=0; else=NA")
val_labels(ces0411$degree11)<-c(nodegree=0, degree=1)
#checks
val_labels(ces0411$degree11)
table(ces0411$degree11)

#recode Region (PROVINCE11)
look_for(ces0411, "province")
ces0411$region11<-Recode(ces0411$PROVINCE11, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces0411$region11)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces0411$region11)
table(ces0411$region11)

#recode Quebec (PROVINCE11)
look_for(ces0411, "province")
ces0411$quebec11<-Recode(ces0411$PROVINCE11, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces0411$quebec11)<-c(Other=0, Quebec=1)
#checks
val_labels(ces0411$quebec11)
table(ces0411$quebec11)

#recode Age (YEARofBIRTH)
table(str_detect(ces0411$survey, "11"))
#pipe data frame
ces0411 %>% 
  #mutate making new variable age08
  mutate(age11=case_when(
    str_detect(ces0411$survey, "11")~2011-yob
    #dump in a test dataframe
  ))-> ces0411
#check
table(ces0411$age11)

#recode Religion (CPS11_80)
look_for(ces0411, "relig")
ces0411$religion11<-Recode(ces0411$CPS11_80, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:20=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")
val_labels(ces0411$religion11)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces0411$religion11)
table(ces0411$religion11)

#recode Language (CPS_INTLANG11)
look_for(ces0411, "language")
ces0411$language11<-Recode(ces0411$CPS_INTLANG11, "5=0; 1=1; else=NA")
val_labels(ces0411$language11)<-c(French=0, English=1)
#checks
val_labels(ces0411$language11)
table(ces0411$language11)

#recode Non-charter Language (CPS11_90)
look_for(ces0411, "language")
ces0411$non_charter_language11<-Recode(ces0411$CPS11_90, "0=1; 1:5=0; 8:64=1; 65=0; 95:97=1; else=NA")
val_labels(ces0411$non_charter_language11)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces0411$non_charter_language11)
table(ces0411$non_charter_language11)

#recode Employment (CPS11_91)
look_for(ces0411, "employment")
ces0411$employment11<-Recode(ces0411$CPS11_91, "3:7=0; 1:2=1; 8:11=1; else=NA")
val_labels(ces0411$employment11)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces0411$employment11)
table(ces0411$employment11)

#recode Sector (PES11_92 & CPS11_91)
look_for(ces0411, "company")
ces0411 %>% 
  mutate(sector11=case_when(
    PES11_92==5 ~1,
    PES11_92==1 ~0,
    PES11_92==0 ~0,
    CPS11_91==1 ~0,
    CPS11_91> 2 & CPS11_91< 12 ~ 0,
    PES11_92==9 ~NA_real_ ,
    PES11_92==8 ~NA_real_ ,
  ))->ces0411

val_labels(ces0411$sector11)<-c(Private=0, Public=1)
#checks
val_labels(ces0411$sector11)
table(ces0411$sector11)

#recode Party ID (PES11_59a)
look_for(ces0411, "identify")
ces0411$party_id11<-Recode(ces0411$PES11_59a, "1=1; 2=2; 3=3; 4:5=0; 0=0; else=NA")
val_labels(ces0411$party_id11)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces0411$party_id11)
table(ces0411$party_id11)

#recode Vote (PES11_6)
look_for(ces0411, "party did you vote")
ces0411$vote11<-Recode(ces0411$PES11_6, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces0411$vote11)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$vote11)
table(ces0411$vote11)

#recode Occupation (NOC_PES11)
look_for(ces0411, "occupation")
class(ces0411$NOC_PES11)
ces0411$occupation11<-Recode(as.numeric(ces0411$NOC_PES11), "1:1099=2; 1100:1199=1;
2100:2199=1; 
 3000:3199=1;
 4000:4099=1; 
 4100:4199=1;
 5100:5199=1;
 1200:1599=3; 
 2200:2299=3;
 3200:3299=3;
 3400:3500=3; 
 4200:4499=3;
 5200:5299=3;
 6200:6399=3;
 6400:6799=3; 
 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9399=4; 9400:9700=5; else=NA")
val_labels(ces0411$occupation11)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation11)
table(ces0411$occupation11)

#Show occupations of those missing responses on NOC variable
ces0411 %>% 
  select(NOC_PES11, occupation11, survey) %>% 
  group_by(occupation11) %>% 
  filter(is.na(occupation11)) %>% 
  filter(str_detect(survey, "CPS11")) %>% 
  count(NOC_PES11)

#recode Occupation3 as 6 class schema with self-employed (CPS11_91)
look_for(ces0411, "employ")
table(ces0411$occupation11, useNA = "ifany")
ces0411$CPS11_91

ces0411$occupation11_3<-ifelse(ces0411$CPS11_91==1, 6, ces0411$occupation11)
val_labels(ces0411$occupation11_3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
table(ces0411$occupation11, ces0411$occupation11_3)
#checks
val_labels(ces0411$occupation11_3)
table(ces0411$occupation11_3)

#recode Income (CPS11_92 and CPS11_93)
look_for(ces0411, "income")
ces0411 %>% 
  mutate(income11=case_when(
    CPS11_93==1 | CPS11_92> -1 & CPS11_92 < 30 ~ 1,
    CPS11_93==2 | CPS11_92> 29 & CPS11_92 < 60 ~ 2,
    CPS11_93==3 | CPS11_92> 59 & CPS11_92 < 90 ~ 3,
    CPS11_93==4 | CPS11_92> 89 & CPS11_92 < 110 ~ 4,
    CPS11_93==5 | CPS11_92> 109 & CPS11_92 < 998 ~ 5,
  ))->ces0411

val_labels(ces0411$income11)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$income11)
table(ces0411$income11)

#recode Redistribution (PES11_41)
look_for(ces0411, "rich")
val_labels(ces0411$PES11_41)
ces0411$redistribution11<-Recode(ces0411$PES11_41, "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$redistribution11)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces0411$redistribution11)
table(ces0411$redistribution11)

#recode Pro-Redistribution (PES11_41)
ces0411$pro_redistribution11<-Recode(ces0411$PES11_41, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces0411$pro_redistribution11)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces0411$pro_redistribution11)
table(ces0411$pro_redistribution11)
