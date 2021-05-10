#File to Recode 2019 CES WEb Data 
#Load Data
data("ces19web")
library(car)
library(haven)
#re
table(is.na(ces19web$NOC))

ces19web$occupation<-Recode(as.numeric(ces19web$NOC), "0:1099=2; 
1100:1199=1;
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
 6400:6799=3; 7200:7399=4; 
                              7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
val_labels(ces19web$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#This code checks if we missed anything
ces19web %>% 
  filter(is.na(NOC)==F&is.na(occupation)==T) %>% 
  select(NOC, occupation)

table(ces19web$occupation, useNA = "ifany")
ces19web %>% 
  mutate(occupation2=case_when(
    #If occupation is not missing then return the value for occupation
    is.na(occupation)==F ~ occupation,
    #If occupoatuib is missing and categorical occupation is 2 then return managers
    is.na(occupation)==T & pes19_occ_cat==18 ~ 2,
    is.na(occupation)==T & pes19_occ_cat==19 ~ 1,
    #This is questionable; Here we assign technician or associate professional to routine non-manual; could revisit
        is.na(occupation)==T & pes19_occ_cat==20 ~ 3,
    #Clerical Support Worker 
        is.na(occupation)==T & pes19_occ_cat==21 ~ 3,
    #Service or Sales Workers
        is.na(occupation)==T & pes19_occ_cat==22 ~ 3,
    #Skilled agricultural, forestry or fishery 
            is.na(occupation)==T & pes19_occ_cat==23 ~ 4,
    #Craft or related trades worker
            is.na(occupation)==T & pes19_occ_cat==24 ~ 4,
    #Plant Machine Operator
            is.na(occupation)==T & pes19_occ_cat==25 ~ 5,
    #Cleaner Labourer
        is.na(occupation)==T & pes19_occ_cat==26 ~ 5,
    TRUE ~ NA_real_
  ))->ces19web
table(is.na(ces19web$occupation))
table(is.na(ces19web$occupation2))
table(is.na(ces19web$NOC))



look_for(ces19web, "ethnic")
ces19web$cps19_ethnicity_41_TEXT
ces19web$cps19_ethnicity_23
ces19web %>% 
  select(contains("_ethnicity_")) %>% 
  val_labels()
  
ces19web %>% 
  mutate(vismin=case_when(
    cps19_ethnicity_23==1~1,
            cps19_ethnicity_25==1~1,
        cps19_ethnicity_32==1~1,
    TRUE~0
  ))->ces19web
ces19web$cps19_ethnicity_41_TEXT
prop.table(table(ces19web$vismin))
