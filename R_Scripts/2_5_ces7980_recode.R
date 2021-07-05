#File to Recode 1979 CES Data for 1979 election
library(cesdata)
#load data
data("ces7980")

#recode Gender (V1537)
look_for(ces7980, "sex")
table(ces7980$V1537, ces7980$V4008)
ces7980$male<-Recode(ces7980$V1537, "1=1; 2=0; 0=NA")
val_labels(ces7980$male)<-c(Female=0, Male=1)
#checks
val_labels(ces7980$male)
table(ces7980$male)

#recode Union Respondent (V1514)
look_for(ces7980, "union")
ces7980$V1514
ces7980$union<-Recode(ces7980$V1514, "1=1; 2=0; 8:9=NA")
val_labels(ces7980$union)<-c(None=0, Union=1)
#checks
val_labels(ces7980$union)
table(ces7980$union)

#recode Union Combined (V1512 and V1514)
ces7980 %>% 
  mutate(union_both=case_when(
    #If either one is union then one
    V1512==1 | V1514==1 ~ 1,
    #If both are non union then 0 
        V1512==2 & V1514==2 ~ 0,
    #if one is don't know and one is refused then missing
    V1512>7 & V1514>7 ~ NA_real_
  ))->ces7980

table(ces7980$V1512)
table(ces7980$V1514)
val_labels(ces7980$union_both)<-c(None=0, Union=1)
#checks
val_labels(ces7980$union_both)
table(ces7980$union_both)
table(ces7980$union_both, useNA = "ifany")

#recode Education (V1502)
look_for(ces7980, "school")
look_for(ces7980, "degree")
ces7980$degree<-Recode(ces7980$V1502, "0:21=0; 22:24=1; 99=NA")
val_labels(ces7980$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces7980$degree)
table(ces7980$degree)

#recode Region (V1005)
look_for(ces7980, "province")
ces7980$region<-Recode(ces7980$V1005, "0:3=1; 5=2; 6:9=3; 4=NA; 99=NA")
val_labels(ces7980$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces7980$region)
table(ces7980$region)

#recode Quebec (V1005)
look_for(ces7980, "province")
ces7980$quebec<-Recode(ces7980$V1005, "0:3=0; 5:9=0; 4=1; 99=NA")
val_labels(ces7980$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces7980$quebec)
table(ces7980$quebec)

#recode Age (V1535)
look_for(ces7980, "age")
ces7980$age<-ces7980$V1535
ces7980$age<-Recode(ces7980$V1535, "0=NA")
#check
table(ces7980$age)

#recode Religion (V1506)
look_for(ces7980, "relig")
ces7980$religion<-Recode(ces7980$V1506, "0=0; 15=0; 1=1; 2:6=2; 7:8=1; 10:14=2; 16:25=2; 99=NA; else=3")
val_labels(ces7980$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces7980$religion)
table(ces7980$religion)

#recode Language (V1020)
look_for(ces7980, "language")
ces7980$language<-Recode(ces7980$V1020, "2=0; 1=1; else=NA")
val_labels(ces7980$language)<-c(French=0, English=1)
#checks
val_labels(ces7980$language)
table(ces7980$language)

#recode Non-charter Language (V1509)
look_for(ces7980, "language")
ces7980$non_charter_language<-Recode(ces7980$V1509, "1:6=0; 7:8=1; else=NA")
val_labels(ces7980$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces7980$non_charter_language)
table(ces7980$non_charter_language)

#recode Employment (V1471)
look_for(ces7980, "employ")
look_for(ces7980, "occup")
ces7980$employment<-Recode(ces7980$V1471, "1:6=0; 11:50=1; else=NA")
val_labels(ces7980$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces7980$employment)
table(ces7980$employment)

#recode Sector (V1473 & V1471)
look_for(ces7980, "sector")
look_for(ces7980, "company")
ces7980$V1471
table(ces7980$V1471)
ces7980 %>% 
  mutate(sector=case_when(
    #teachers and nurses are added to public sector
    V1484==6925 ~ 1,
    V1484==7177 ~ 1,
    V1484==7230 ~ 1,
    #all government employees go to public sector
    V1473==13 ~ 1,
    #all non-government employees go to zero
    V1473> 0 & V1473 < 13 ~ 0,
    #all people on the margins of the labour market (e.g. retired) go to zero, as per Blais, presumably
    V1471> 0 & V1471 < 7 ~ 0,
    #Farmers go to zero
    V1471 ==50 ~ 0,
  ))->ces7980

table(ces7980$V1471, ces7980$V1473)
val_labels(ces7980$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces7980$sector)
table(ces7980$sector)
table(ces7980$sector, ces7980$V4020)

#recode Party ID (V1192)
look_for(ces7980, "federal")
ces7980$party_id<-Recode(ces7980$V1192, "1=1; 2=2; 3=3; 0=0; 4:7=0; else=NA")
val_labels(ces7980$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces7980$party_id)
table(ces7980$party_id)

#recode Vote (V1234)
look_for(ces7980, "vote")
ces7980$vote<-Recode(ces7980$V1234, "1=1; 2=2; 3=3; 4:5=0; else=NA")
val_labels(ces7980$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces7980$vote)
table(ces7980$vote)

#recode Occupation (V1471)
look_for(ces7980, "occupation")
ces7980$occupation<-Recode(ces7980$V1471, "11:12=1; 21:22=2; 30=3; 41:42=4; 43:50=5; else=NA")
val_labels(ces7980$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces7980$occupation)
table(ces7980$occupation)

#recode Occupation3 as 6 class schema with self-employed (V1477)
look_for(ces7980, "employed")
ces7980$occupation3<-ifelse(ces7980$V1477==1, 6, ces7980$occupation)
val_labels(ces7980$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces7980$occupation3)
table(ces7980$occupation3)

#recode Income (V1516)
look_for(ces7980, "income")
ces7980$income<-Recode(ces7980$V1516, "1:2=1; 3=2; 4:5=3; 6:7=4; 8=5; else=NA")
val_labels(ces7980$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces7980$income)
table(ces7980$income)

#recode Religiosity (V1507)
look_for(ces7980, "church")
ces7980$religiosity<-Recode(ces7980$V1507, "5:9=1; 4=2; 3=3; 2=4; 1=5; else=NA")
val_labels(ces7980$religiosity)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces7980$religiosity)
table(ces7980$religiosity)

#recode Liberal leader (V1261)
look_for(ces7980, "Trudeau")
ces7980$liberal_leader<-Recode(ces7980$V1261, "0=NA")
#checks
table(ces7980$liberal_leader)

#recode conservative leader (V1264)
look_for(ces7980, "Clark")

ces7980$conservative_leader<-Recode(ces7980$V1264, "0=NA")
#checks
table(ces7980$conservative_leader)

#recode NDP leader (V1267)
look_for(ces7980, "Broadbent")
ces7980$ndp_leader<-Recode(ces7980$V1267, "0=NA")
#checks
table(ces7980$ndp_leader)

#recode liberal rating (V1263)
look_for(ces7980, "therm")
ces7980$liberal_rating<-Recode(ces7980$V1263, "0=NA")
#checks
table(ces7980$liberal_rating)

#recode conservative rating (V1266)
look_for(ces7980, "therm")
ces7980$conservative_rating<-Recode(ces7980$V1266, "0=NA")
#checks
table(ces7980$conservative_rating)

#recode NDP rating (V1269)
look_for(ces7980, "therm")
ces7980$ndp_rating<-Recode(ces7980$V1269, "0=NA")
#checks
table(ces7980$ndp_rating)

#--------------------------------------------------------------------------------------------------------------------
####1980
#recode Gender (V2156)
look_for(ces7980, "sex")
ces7980$male80<-Recode(ces7980$V2156, "1=1; 2=0; 0=NA")
val_labels(ces7980$male80)<-c(Female=0, Male=1)
#checks
val_labels(ces7980$male80)
table(ces7980$male, ces7980$male80)

#recode Community Size (V1536)
look_for(ces7980, "community")
ces7980$size<-Recode(ces7980$V1536, "8:9=1; 7=2; 5:6=3; 4=4; 1:3=5; else=NA")
val_labels(ces7980$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces7980$size)
table(ces7980$size)

#No Union Respondent variable

#No Union Combined variable

#No Education variable

#recode Region (V2002)
look_for(ces7980, "province")
ces7980$region80<-Recode(ces7980$V2002, "0:3=1; 5=2; 6:9=3; 4=NA; 99=NA")
val_labels(ces7980$region80)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces7980$region80)
table(ces7980$region, ces7980$region80)

#recode Quebec (V2002)
look_for(ces7980, "province")
ces7980$quebec80<-Recode(ces7980$V2002, "0:3=0; 5:9=0; 4=1; 99=NA")
val_labels(ces7980$quebec80)<-c(Other=0, Quebec=1)
table(ces7980$quebec, ces7980$quebec80)
#checks
val_labels(ces7980$quebec80)
table(ces7980$quebec80)

#recode Age (V2155)
look_for(ces7980, "age")
ces7980$age80<-ces7980$V1535
ces7980$age80<-Recode(ces7980$V2155, "0=NA")
#check
table(ces7980$age80)

#No Religion variable

#Recode Language (V2013)
look_for(ces7980, "language")
ces7980$language80<-Recode(ces7980$V2013, "2=0; 1=1; 0=NA")
val_labels(ces7980$language80)<-c(French=0, English=1)
#checks
val_labels(ces7980$language80)
table(ces7980$language80)
table(ces7980$language, ces7980$language80)

#No Employment variable

#No Sector variable

#recode Party ID (V2043)
look_for(ces7980, "federal")
ces7980$party_id80<-Recode(ces7980$V2043, "1=1; 2=2; 3=3; 0=0; 4:7=0; else=NA")
val_labels(ces7980$party_id80)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces7980$party_id80)
table(ces7980$party_id80)
table(ces7980$party_id, ces7980$party_id80)

#recode Vote (V2062)
look_for(ces7980, "vote")
ces7980$vote80<-Recode(ces7980$V2062, "1=1; 2=2; 3=3; 4:5=0; else=NA")
val_labels(ces7980$vote80)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces7980$vote80)
table(ces7980$vote80)
table(ces7980$vote, ces7980$vote80)

#recode Liberal leader (V2080)
look_for(ces7980, "Trudeau")
ces7980$liberal_leader80<-Recode(ces7980$V2080, "0=NA")
#checks
table(ces7980$liberal_leader80)

#recode conservative leader (V2083)
look_for(ces7980, "Clark")
ces7980$conservative_leader80<-Recode(ces7980$V2083, "0=NA")
#checks
table(ces7980$conservative_leader80)

#recode NDP leader (V2086)
look_for(ces7980, "Broadbent")
ces7980$ndp_leader80<-Recode(ces7980$V2086, "0=NA")
#checks
table(ces7980$ndp_leader80)

#recode liberal rating (V2082)
look_for(ces7980, "therm")
ces7980$liberal_rating80<-Recode(ces7980$V2082, "0=NA")
#checks
table(ces7980$liberal_rating80)

#recode conservative rating (V2085)
look_for(ces7980, "therm")
ces7980$conservative_rating80<-Recode(ces7980$V2085, "0=NA")
#checks
table(ces7980$conservative_rating80)

#recode NDP rating (V2088)
look_for(ces7980, "therm")
ces7980$ndp_rating80<-Recode(ces7980$V2088, "0=NA")
#checks
table(ces7980$ndp_rating80)

##### See the script 1_master_file.R There I turned the values for the 79 variables into 1980 variables for the 1980 respondents
# No Occupation variable

# No Income variable

# No Religiosity variable
