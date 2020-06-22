library(tidyverse)
library(labelled)
#recode Education (ces06_CPS_S3)
look_for(ces0411, "education")
look_for(ces0411, "RDD")
# ces0411$degree06<-Recode(ces0411$ces06_CPS_S3, "9:11=1; 1:8=0; else=NA")
# val_labels(ces0411$degree06)<-c(nodegree=0, degree=1)
table(as_factor(ces0411$ces06_RECALL))
ces0411 %>% 
  mutate(degree06=case_when(
    #New RDD is 0 and degree is less than 9
   ces06_RECALL==0 &ces06_CPS_S3 <9~0,
       #New RDD is 0 and degree is less than 9
   ces06_RECALL==0 &ces06_CPS_S3 ==9~1,
          #Panel is 1 and degree0r==0
   ces06_RECALL==1 &degree04 ==0~0,
          #Panel is 1 and degree0r==1
   ces06_RECALL==1 &degree04 ==1~1
  ))
