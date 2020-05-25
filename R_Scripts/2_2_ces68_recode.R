#File to Recode 1968 CES Data

#load data
data("ces68")

#recode Gender (var401)
look_for(ces68, "sex")
ces68$male<-Recode(ces68$var401, "1=1; 2=0")
val_labels(ces68$male)<-c(Female=0, Male=1)
#checks
val_labels(ces68$male)
table(ces68$male)

#No Union Household variable

#No Union Combined variable

#recode Education (var334)
ces68$degree<-Recode(ces68$var334, "17:20=1; 25:26=1; 1:16=0; 21:24=0; 27=0; 30=NA")
val_labels(ces68$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces68$degree)
table(ces68$degree)

#recode Region (var001)
look_for(ces68, "province")
ces68$region<-Recode(ces68$var001, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces68$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces68$region)
table(ces68$region)

#recode Quebec (var001)
look_for(ces68, "province")
ces68$quebec<-Recode(ces68$var001, "0:3=0; 5:9=0; 4=1")
val_labels(ces68$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces68$quebec)
table(ces68$quebec)

#recode Age (age)
look_for(ces68, "age")
ces68$age<-ces68$age
#check
table(ces68$age)

#recode Religion (var340)
ces68$religion<-Recode(ces68$var340, "0=0; 10=0; 1=1; 2:6=2; 7:8=1; 11:19=2; 21:24=2; 26:32=2; 36:39=2; 41=2; 40=NA; else=3")
val_labels(ces68$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces68$religion)
table(ces68$religion)

#recode Language (var357)
look_for(ces68, "language")
ces68$language<-Recode(ces68$var357, "2=0; 1=1; else=NA")
val_labels(ces68$language)<-c(French=0, English=1)
#checks
val_labels(ces68$language)
table(ces68$language)

#recode Employment (var324)
look_for(ces68, "employ")
ces68$employment<-Recode(ces68$var324, "0=0; 8:9=0; 1:7=1; else=NA")
val_labels(ces68$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces68$employment)
table(ces68$employment)

#No Sector variable

#recode Party ID (var122)
look_for(ces68, "affiliation")
ces68$party_id<-Recode(ces68$var122, "1=1; 2=2; 3=3; 4:11=0; else=NA")
val_labels(ces68$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces68$party_id)
table(ces68$party_id)

#recode Vote (var180)
look_for(ces68, "vote")
ces68$vote<-Recode(ces68$var180, "2=1; 3=2; 4=3; 5:8=0; else=NA")
val_labels(ces68$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces68$vote)
table(ces68$vote)

#recode Occupation (var324)
look_for(ces68, "occupation")
ces68$occupation<-Recode(ces68$var324, "1=1; 2=2; 3:4=3; 5=4; 6:7=5; else=NA")
val_labels(ces68$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces68$occupation)
table(ces68$occupation)

#recode Income (var404)
look_for(ces68, "income")
ces68$income<-Recode(ces68$var404, "1:3=1; 4:5=2; 6:7=3; 8:9=4; 0=5; else=NA")
val_labels(ces68$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces68$income)
table(ces68$income)