#Age
library(cesdata)
library(tidyverse)
library(labelled)
library(car)
data("ces0411")
look_for(ces0411, "private company")
#Economic Sector
## Please note for panel files (2004-20111 and 1974-1980) we have to be careful how we recode the new variables, we should append a two-digit marker for the year to the new variable name to differentiate. This can be easily removed later when it comes ot merging datasets to one complete time series. 
ces0411$public04<-Recode(ces0411$ces04_CPS_S5,"1=0 ; 5=1; else=NA")
ces0411$public06<-Recode(ces0411$ces06_CPS_S5,"1=0 ; 5=1; else=NA")
ces0411$public08<-Recode(ces0411$ces08_CPS_S5,"1=0 ; 5=1; else=NA")
ces0411$public11<-Recode(ces0411$PES11_92,"1=0 ; 5=1; else=NA")

look('private', ces041111.labs)
#Union Status
look_for(ces0411, "union")
data("ces93")
look_for(ces0411 ,"union")
##For now I think we will use the union household question!!!!!!!!! 
ces0411$union04<-Recode(ces0411$ces0411_CPS_S6B,"1=1; 5=0; else=NA")
ces0411$union06<-Recode(ces0411$ces0411_CPS_S6B,"1=1; 5=0; else=NA")
ces0411$union08<-Recode(ces0411$ces0411_CPS_S6B,"1=1; 5=0; else=NA")
ces0411$union11<-Recode(ces0411$PES11_94,"1=1; 5=0; else=NA")

val_labels(ces0411$union04)<-c(Union=1, Nonunion=0)
#Income
#### This is some code I used to recode income in the past, but I will let youhandle this. 
# look('income', ces041111.labs)
# ces0411$ces0411_CPS_S18
# library(car)
# ces0411$income<-Recode(ces0411$ces04_CPS_S18,"98:99=NA")
# ces0411$income<-Recode(ces0411$ces04_CPS_S18A,"998:999=NA")
# ces0411$ces0411_CPS_S18A
# ces0411$income_10<-Recode(ces0411$income, "0:19=1; 20:29=2; 30:39=3; 40:49=4; 50:59=5; 60:69=6; 70:79=7; 80:89=8; 90:99=9; 100:997=10; 998:999=NA")
# ces0411$income<-Recode(ces0411$CPS11_92,"998:999=NA")
# ces0411$income_10<-Recode(ces0411$income, "0:19=1; 20:29=2; 30:39=3; 40:49=4; 50:59=5; 60:69=6; 70:79=7; 80:89=8; 90:99=9; 100:997=10; 998:999=NA")

#Employment Status
## Hold off on employment status, I am not sure how we will deal with employment status.
ces0411$self_employed<-Recode(ces0411$ces0411_CPS_S4, "2='Working' ; 1='Self-Employed'", levels=c('Working', 'Self-Employed'))
table(ces0411$self_employed)
ces0411$self_employed<-Recode(ces0411$ces0411_CPS_S4, "2='Working'; 1='Self-Employed'" ,as.factor.result=T, levels=c('Working', 'Self-Employed'))
ces0411$self_employed<-Recode(ces0411$CPS11_91,  "2='Working'; 1='Self-Employed'" ,as.factor.result=T, levels=c('Working', 'Self-Employed'))
table(ces0411$self_employed)
table(ces0411$self_employed)
look_for(ces0411, c("highest level"))

### Degree
ces0411$degree04<-Recode(ces0411$ces04_CPS_S3, "1:8=0; 9:11=1; else=NA")
ces0411$degree06<-Recode(ces0411$ces06_CPS_S3, "1:8=0; 9:11=1; else=NA")
ces0411$degree08<-Recode(ces0411$ces08_CPS_S3, "1:8=0; 9:11=1; else=NA")
ces0411$degree11<-Recode(ces0411$CPS11_79, "1:8=0; 9:11=1; else=NA")
### You can also use val_labels to set value labels for multiple variables

val_labels(ces0411[,c('degree04', 'degree06', 'degree08', 'degree11')])<-c(Degree=1, `No degree`=0)
as_factor(ces0411$degree04)
table(as_factor(ces0411$degree04), as_factor(ces0411$ces04_CPS_S3))
table(as_factor(ces0411$degree06), as_factor(ces0411$ces06_CPS_S3))
table(as_factor(ces0411$degree08), as_factor(ces0411$ces08_CPS_S3))
table(as_factor(ces0411$degree11), as_factor(ces0411$CPS11_79))


## Religion
look_for(ces0411, "religion")
ces0411$religion
ces0411$religion04<-Recode(ces0411$ces04_CPS_S9, "4=1 ; 1=2; 2=2 ; 9=2 ; 12=2 ;13=2 ;14=2 ;16=2 ; 18=2 ; 98:99=NA ; else=3 ; 0=0")
ces0411$religion06<-Recode(ces0411$ces06_CPS_S9,"4=1 ; 1=2;2=2 ; 9=2 ; 12=2 ;13=2 ;14=2 ;16=2 ; 18=2 ; 98:99=NA ; else=3 ; 0=0")
ces0411$religion08<-Recode(ces0411$ces08_CPS_S9,"4=1 ; 1=2;2=2 ; 9=2 ; 12=2 ;13=2 ;14=2 ;16=2 ; 18=2 ; 98:99=NA ; else=3 ; 0=0")
ces0411$religion11<-Recode(ces0411$CPS11_80," 4=1 ; 1=2; 2=2 ; 9=2 ; 12=2 ; 13=2 ;14=2 ; 16=2 ; 18=2 ; 98:99=NA ; else=3 ; 0=0")

val_labels(ces0411$religion04)<-c(Catholic=1, Protestant=2, Other=3, None=4)
val_labels(ces0411$religion06)<-c(Catholic=1, Protestant=2, Other=3, None=4)
val_labels(ces0411$religion08)<-c(Catholic=1, Protestant=2, Other=3, None=4)
val_labels(ces0411$religion11)<-c(Catholic=1, Protestant=2, Other=3, None=4)


####This is some code for urban rural, but I don't think we will be suing this. disregard
ces0411$urban<-Recode(ces0411$ces0411_UARATYPE,"1='Urban' ; 2='Urban' ; 4='Urban' ; 6='Urban'; 3='Rural' ; 5='Rural'; 0=NA", levels=c('Rural', 'Urban'))
ces0411$urban<-Recode(ces0411$BLKURB_08, "0='Rural' ; 1='Urban' ; 9=NA", levels=c('Rural', 'Urban'))
ces0411$urban<-Recode(ces0411$BLKURB_11, "0='Rural' ; 1='Urban' ; 9=NA", levels=c('Rural', 'Urban'))
