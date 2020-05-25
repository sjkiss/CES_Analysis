#File to Recode 1979 CES Data for 1979 election

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

#recode Union Household (V1514)
look_for(ces7980, "union")
ces7980$union<-Recode(ces7980$V1514, "1=1; 2:8=0; 9=NA")
val_labels(ces7980$union)<-c(None=0, Union=1)
#checks
val_labels(ces7980$union)
table(ces7980$union)

#recode Union Combined (V1512 and V1514)
ces7980 %>% 
  mutate(union_both=case_when(
    V1512==1 | V1514==1 ~ 1,
    V1512==8 | V1512==9 ~ NA_real_,
    V1514==8 | V1514==9 ~ NA_real_,
    TRUE~ 0
  ))->ces7980

val_labels(ces7980$union_both)<-c(None=0, Union=1)
#checks
val_labels(ces7980$union_both)
table(ces7980$union_both)

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

#recode Language (V1510)
look_for(ces7980, "language")
ces7980$language<-Recode(ces7980$V1510, "2=0; 1=1; 7=0; 4=1; else=NA")
val_labels(ces7980$language)<-c(French=0, English=1)
#checks
val_labels(ces7980$language)
table(ces7980$language)

#recode Employment (V1471)
look_for(ces7980, "employ")
look_for(ces7980, "occup")
ces7980$employment<-Recode(ces7980$V1471, "1:6=0; 11:50=1; else=NA")
val_labels(ces7980$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces7980$employment)
table(ces7980$employment)

#recode Sector (V1473)
look_for(ces7980, "sector")
look_for(ces7980, "company")
ces7980$sector<-Recode(ces7980$V1473, "13=1; 1:12=0; else=NA")
val_labels(ces7980$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces7980$sector)
table(ces7980$sector)

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

#recode Occupation (V410)
look_for(ces7980, "occupation")
ces7980$occupation<-Recode(ces7980$V410, "11:12=1; 21:22=2; 30=3; 41:42=4; 43:50=5; else=NA")
val_labels(ces7980$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces7980$occupation)
table(ces7980$occupation)

#recode Income (V1516)
look_for(ces7980, "income")
ces7980$income<-Recode(ces7980$V1516, "1:2=1; 3=2; 4:5=3; 6:7=4; 8=5; else=NA")
val_labels(ces7980$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces7980$income)
table(ces7980$income)


####1980
#recode Gender (V2156)
look_for(ces7980, "sex")
ces7980$male80<-Recode(ces7980$V2156, "1=1; 2=0; 0=NA")
val_labels(ces7980$male80)<-c(Female=0, Male=1)
#checks
val_labels(ces7980$male80)
table(ces7980$male, ces7980$male80)


#No Union Household variable

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
##### See the script 1_master_file.R There I turned the values for the 79 variables into 1980 variables for the 1980 respondents
# No Occupation variable

# No Income variable
