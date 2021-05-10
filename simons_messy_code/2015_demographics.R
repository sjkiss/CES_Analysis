#2015 Income
data("ces15phone")
#This is not the income variable we want to use, find the categories one. 
ces15$income<-Recode(ces15phone$CPS15_92, "998:999=NA")

#2015 Gender
ces15phone$male<-Recode(ces15phone$RGENDER, "1=1; 5=0")
val_labels(ces15phone$male)<-c(male=1, female=0)


#2015 Education
look_for(ces15phone, "education")
#These are ways of just examining the variable you are dealing with. 
summary(ces15phone$CPS15_79)
table(as_factor(ces15phone$CPS15_79))
ces15phone$CPS15_79
ces15phone$degree

#Do the recode
ces15phone$degree<-Recode(ces15phone$CPS15_79, "1:8=0; 9:11=1; else=NA ")
val_labels(ces15phone$degree)<-c(degree=1, nodegree=0)
#Check
table(as_factor(ces15phone$CPS15_79), as_factor(ces15phone$degree))

#Union STatus
#double-check if this is the individual membership or the union household question; I think I have to check with Peter on this. 
ces15phone$PES15_93
ces15phone$union<-Recode(ces15phone$PES15_93, "1=1; 5=0")
#add value labels
val_labels(ces15phone$union)<-c(union=1, nounion=0)
#REgion
look_for(ces15phone, "province")
ces15phone$CPS15_PROVINCE
ces15phone$roc<-Recode(ces15phone$CPS15_PROVINCE, 
                       "10:13=0 ; 24=NA ; 35=1 ; 46:59=2 ; else=NA")
val_labels(ces15phone$roc)<-c(Atlantic=0, Ontario=1, West=2)

#Do the Quebec variable
#Note that I am making other provinces zero and don't know or refused answers NA. 
ces15phone$qc<-Recode(ces15phone$CPS15_PROVINCE, "24=1; 10:13=0; 35=0; 46:59=0; else=NA")
val_labels(ces15phone$qc)<-c(Quebec=1, ROC=0)
#Sector
look('company', labs)

#Employment Status
ces15phone$public<-Recode(ces15phone$PES15_92, "1=0 ; 5=1 ; else=NA")
val_labels(ces15phone$public)<-c(public=1, private=0)
table(as_factor(ces15phone$public), as_factor(ces15phone$PES15_92))

#Disregard this; we will get to this; it stems from a previous project. 
ces15$class15<-Recode(as.numeric(ces15$PES15_NOC), "
       0:999='Org-Prof' ;
       1110:1129='Org-Prof' ;
       2110:2199='Tech-Prof' ;
       3000:3199='Hum-Prof' ;
       4000:4199='Hum-Prof' ;
       5100:5199='Hum-Prof' ;
       1210:1259='Org-Assoc.Prof';
       2210:2289='Tech-Assoc.Prof';
       3200:3239='Hum-Assoc.Prof' ;
       4200:4299='Hum-Assoc.Prof';
       5200:5259='Hum-Assoc.Prof';
       6200:6239='Hum-Assoc.Prof';
       7200:7299='Tech-Assoc.Prof';
       8200:8299='Tech-Assoc.Prof';
       9200:9299='Tech-Assoc.Prof';
       1300:1399='Org-Assoc.Prof';
       4300:4399='Hum-Assoc.Prof' ;
       6300:6399='Hum-Assoc.Prof' ;
       7300:7399='Tech-Assoc.Prof';
       1400:1499='Org-Skilled';
       3400:3499='Hum-Skilled';
       4400:4499='Hum-Skilled';
       6400:6499='Hum-Skilled';
       7400:7499='Tech-Skilled';
       8400:8499='Tech-Skilled';
       9400:9499='Tech-Skilled';
       1510:1599='Org-Skilled';
       6500:6599='Hum-Skilled';
       7500:7599='Tech-Skilled';
       9500:9599='Tech-Skilled';
       6600:6699='Hum-Unskilled';
       7600:7699='Tech-Unskilled' ;
       8600:8699='Tech-Unskilled';
    6700:6799='Hum-Unskilled';
9600:9699='Tech-Unskilled';
5421='Hum-Skilled';
2616='Tech-Unskilled';
                      9994:9999=NA", as.factor=F)
table(ces15$class15)

ces15<-ces15 %>% 
  separate(.,class15, into=c('Skill', 'Authority'), '-', remove=F)
names(ces15)


look_for(ces15phone, "status")
#Employment Status
###This is a tough one; we will come to this. I'm not sure what we will do with this. 

ces15phone$CPS15_91

ces15phone$employed<-Recode(ces15$CPS15_91, "98:99=NA")


#Religion
ces15phone$religion<-Recode(ces15phone$CPS15_80, "4=1 ; 1=2;
       2=2 ; 9=2 ; 12=2 ;
       13=2 ;14=2 ;
       16=2 ; 18=2 ; 98:99=NA ; else=3 ; 0=0")
val_labels(ces15phone$religion)<-c(None=0,Catholic=1, Protestant=2, Other=3)
ces15phone$religion
