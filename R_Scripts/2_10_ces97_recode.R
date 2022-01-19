#File to Recode 1997 CES Data 
#load data
data("ces97")
#### #recode Gender (cpsrgen)#### 

look_for(ces97, "gender")
ces97$male<-Recode(ces97$cpsrgen, "1=1; 5=0")
val_labels(ces97$male)<-c(Female=0, Male=1)
#checks
val_labels(ces97$male)
table(ces97$male)
#####recode Union Household (cpsm9) #### 

look_for(ces97, "union")
ces97$union<-Recode(ces97$cpsm9, "1=1; 5=0; else=NA")
val_labels(ces97$union)<-c(None=0, Union=1)
#checks
val_labels(ces97$union)
table(ces97$union)
#####Union Combined variable (identical copy of union) #### 

ces97$union_both<-ces97$union
#checks
val_labels(ces97$union_both)
table(ces97$union_both)

#### #recode Education (cpsm3)#### 
look_for(ces97, "education")
ces97$degree<-Recode(ces97$cpsm3, "9:11=1; 1:8=0; else=NA")
val_labels(ces97$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces97$degree)
table(ces97$degree)

####recode Region (province) #### 
look_for(ces97, "province")
ces97$region<-Recode(ces97$province, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces97$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces97$region)
table(ces97$region)

#### recode Quebec (province) ####
look_for(ces97, "province")
ces97$quebec<-Recode(ces97$province, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces97$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces97$quebec)
table(ces97$quebec)

####recode Age (cpsage) #### 
look_for(ces97, "age")
ces97$yob<-Recode(ces97$cpsage, "9999=NA")
ces97$age<-1997-ces97$yob
#check
table(ces97$age)

####recode Religion (cpsm10) #### 
look_for(ces97, "relig")
ces97$religion<-Recode(ces97$cpsm10, "0=0; 2=1; 1=2; 3:5=3; else=NA")
val_labels(ces97$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces97$religion)
table(ces97$religion)

#### recode Language (cpslang)#### 

look_for(ces97, "language")
ces97$language<-Recode(ces97$cpslang, "1=1; 2=0; else=NA")
val_labels(ces97$language)<-c(French=0, English=1)
#checks
val_labels(ces97$language)
table(ces97$language)
#### recode Non-charter Language (cpsm15)#### 

look_for(ces97, "language")
ces97$non_charter_language<-Recode(ces97$cpsm15, "1:5=0; 0=1; 10:27=1; else=NA")
val_labels(ces97$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces97$non_charter_language)
table(ces97$non_charter_language)
####recode Employment (cpsm4) #### 

look_for(ces97, "employment")
ces97$employment<-Recode(ces97$cpsm4, "2:7=0; 1=1; 8=1; else=NA")
val_labels(ces97$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces97$employment)
table(ces97$employment)

####recode Sector (cpsm7 & cpsm4) #### 
look_for(ces97, "firm")
ces97 %>% 
  mutate(sector=case_when(
    cpsm7==3 ~1,
    cpsm7==5 ~1,
    cpsm7==7 ~1,
    cpsm7==1 ~0,
    cpsm7==0 ~0,
    cpsm4> 1 & cpsm4< 9 ~ 0,
    cpsm7==9 ~NA_real_ ,
    cpsm7==8 ~NA_real_ ,
  ))->ces97

val_labels(ces97$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces97$sector)
table(ces97$sector)

#### recode Party ID (cpsk1 and cpsk4)#### 

look_for(ces97, "federal")
ces97 %>% 
  mutate(party_id=case_when(
    cpsk1==1 | cpsk4==1 ~ 1,
    cpsk1==2 | cpsk4==2 ~ 2,
    cpsk1==3 | cpsk4==3 ~ 3,
    cpsk1==4 | cpsk4==4 ~ 2,
    cpsk1==5 | cpsk4==5 ~ 0,
    cpsk1==6 | cpsk4==6 ~ 0,
  ))->ces97

val_labels(ces97$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces97$party_id)
table(ces97$party_id)

####recode Vote (pesa4) #### 
look_for(ces97, "vote")
ces97$vote<-Recode(ces97$pesa4, "1=1; 2=2; 3=3; 5=4; 4=2; 0=0; else=NA")
val_labels(ces97$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces97$vote)
table(ces97$vote)
#### #recode Occupation (pinporr)#### 

look_for(ces97, "occupation")
look_for(ces97, "pinporr")
ces97$occupation<-Recode(ces97$pinporr, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
val_labels(ces97$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces97$occupation)
table(ces97$occupation)
####recode Occupation3 as 6 class schema with self-employed (cpsm4) #### 

look_for(ces97, "employ")
ces97$occupation3<-ifelse(ces97$cpsm4==8, 6, ces97$occupation)
val_labels(ces97$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces97$occupation3)
table(ces97$occupation3)
#### recode Income (cpsm16 and cpsm16a)#### 

look_for(ces97, "income")
ces97 %>% 
  mutate(income=case_when(
    cpsm16a==1 | cpsm16> 0 & cpsm16 < 20 ~ 1,
    cpsm16a==2 | cpsm16> 19 & cpsm16 < 30 ~ 2,
    cpsm16a==3 | cpsm16> 29 & cpsm16 < 50 ~ 3,
    cpsm16a==4 | cpsm16> 29 & cpsm16 < 50 ~ 3,
    cpsm16a==5 | cpsm16> 49 & cpsm16 < 70 ~ 4,
    cpsm16a==6 | cpsm16> 49 & cpsm16 < 70 ~ 4,
    cpsm16a==7 | cpsm16> 69 & cpsm16 < 998 ~ 5,
    cpsm16a==8 | cpsm16> 69 & cpsm16 < 998 ~ 5,
    cpsm16a==9 | cpsm16> 69 & cpsm16 < 998 ~ 5,
    cpsm16a==10 | cpsm16> 69 & cpsm16 < 998 ~ 5,
  ))->ces97

val_labels(ces97$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces97$income)
table(ces97$income)

####recode Religiosity (pesm10b)####
look_for(ces97, "relig")
ces97$religiosity<-Recode(ces97$pesm10b, "7=1; 5=2; 8=3; 3=4; 1=5; else=NA")
val_labels(ces97$religiosity)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces97$religiosity)
table(ces97$religiosity)

#### recode Redistribution (mbsa4)#### 
look_for(ces97, "rich")
val_labels(ces97$mbsa4)
ces97$redistribution<-Recode(ces97$mbsa4, "; 1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces97$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces97$redistribution)
table(ces97$redistribution)

####recode Pro-Redistribution (mbsa4) #### 
ces97$pro_redistribution<-Recode(ces97$mbsa4, "1:2=1; 3:4=0; else=NA", as.numeric=T)
val_labels(ces97$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces97$pro_redistribution)
table(ces97$pro_redistribution)
#### recode Market Liberalism (cpsf6 and pese19)#### 

look_for(ces97, "private")
look_for(ces97, "blame")
ces97$market1<-Recode(ces97$cpsf6, "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
ces97$market2<-Recode(ces97$pese19, "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
table(ces97$market1)
table(ces97$market2)

ces97 %>% 
  rowwise() %>% 
  mutate(market_liberalism=mean(
    c_across(market1:market2)
 , na.rm=T )) ->out

out %>% 
  ungroup() %>% 
  select(c('market1', 'market2', 'market_liberalism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces97 %>% 
  rowwise() %>% 
  mutate(market_liberalism=mean(
    c_across(c('market1', 'market2')), na.rm=T  
  )) %>% 
  ungroup()->ces97

ces97 %>% 
  select(starts_with("market")) %>% 
  summary()
#Check distribution of market_liberalism
qplot(ces97$market_liberalism, geom="histogram")
table(ces97$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces97 %>% 
  select(market1, market2) %>% 
  alpha(.)
#Check correlation
ces97 %>% 
  select(market1, market2) %>% 
  cor(., use="complete.obs")

####recode Immigration (cpsj18) #### 

look_for(ces97, "imm")
ces97$immigration_rates<-Recode(ces97$cpsj18, "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
table(ces97$immigration_rates, useNA = "ifany")

#### #recode Environment (mbsa6)#### 
look_for(ces97, "env")
table(ces97$mbsa6, useNA="ifany")
ces97$enviro<-Recode(ces97$mbsa6, "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
table(ces97$enviro, useNA="ifany")

####recode Capital Punishment (pese13) #### 
look_for(ces97, "punish")
ces97$death_penalty<-Recode(ces97$pese13, "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA")
#checks
table(ces97$death_penalty)

#recode Crime (cpsa2g)
look_for(ces97, "crime")
ces97$crime<-Recode(ces97$cpsa2g, "1=1; 2=0.5; 3=0; 8=0.5; else=NA")
#checks
table(ces97$crime)

#recode Gay Rights (mbsg3)
look_for(ces97, "homo")
ces97$gay_rights<-Recode(ces97$mbsg3, "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
table(ces97$gay_rights)
#### #recode Abortion (pese5a pese5b pese5c)#### 

look_for(ces97, "abort")
ces97 %>% 
  mutate(abortion=case_when(
    pese5a==3 ~0,
    pese5a==1 ~1,
    pese5a==2 ~0.5,
    pese5a==8 ~0.5,
    pese5b==3 ~1,
    pese5b==2 ~0,
    pese5b==1 ~0.5,
    pese5b==8 ~0.5,
    pese5c==2 ~1,
    pese5c==1 ~0,
    pese5c==3 ~0.5,
    pese5c==8 ~0.5,
  ))->ces97
#checks
table(ces97$abortion)
#### #recode Lifestyle (mbsa7)#### 

look_for(ces97, "lifestyle")
ces97$lifestyles<-Recode(ces97$mbsa7, "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
table(ces97$lifestyles)
#####recode Stay Home (cpsf3) #### 

look_for(ces97, "home")
ces97$stay_home<-Recode(ces97$cpsf3, "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
table(ces97$stay_home)
#####recode Marriage Children (cpsf2) ####

look_for(ces97, "children")
ces97$marriage_children<-Recode(ces97$cpsf2, "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
table(ces97$marriage_children)
#####recode Values (mbsa9) #### 

look_for(ces97, "traditional")
ces97$values<-Recode(ces97$mbsa9, "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
table(ces97$values)
#####recode Morals (mbsa8) ####

look_for(ces97, "moral")
ces97$morals<-Recode(ces97$mbsa8, "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
table(ces97$morals)
#### Moral traditionalism#### 
#recode Moral Traditionalism (abortion, lifestyles, stay home, values, marriage childen, morals)
ces97$trad3<-ces97$abortion
ces97$trad7<-ces97$lifestyles
ces97$trad1<-ces97$stay_home
ces97$trad4<-ces97$values
ces97$trad5<-ces97$marriage_children
ces97$trad6<-ces97$morals
ces97$trad2<-ces97$gay_rights

#This code removes value labels from trad1 to trad6
#Start with data frame
ces97 %>% 
  #mutate because changing variables
  #across because we are doing something across a series of columns
  # What columns are we working on? The ones with the names generated in 
  #num_range('trad', 1:7)
  mutate(across(.cols=num_range('trad', 1:7),
                #The thing we are doing is removing value labels
                remove_val_labels))->ces97
#remove_val_labels
table(ces97$trad1)
table(ces97$trad2)
table(ces97$trad3)
table(ces97$trad4)
table(ces97$trad5)
table(ces97$trad6)
table(ces97$trad7)

ces97 %>% 
  rowwise() %>% 
  mutate(traditionalism=mean(c_across(c(trad1, trad2, trad3, trad4, trad5, trad6, trad7)) , na.rm=T )) -> out

out %>% 
  ungroup() %>% 
  select(c('trad1', 'trad2', 'trad3', 'trad4', 'trad5', 'trad6', 'trad7', 'traditionalism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces97 %>% 
  rowwise() %>% 
  mutate(traditionalism=mean(
    c_across(c('trad1', 'trad2', 'trad3', 'trad4', 'trad5', 'trad6', 'trad7')), na.rm=T 
  )) %>% 
  ungroup()->ces97

ces97 %>% 
  select(starts_with("trad")) %>% 
 head()
#Check distribution of traditionalism
qplot(ces97$traditionalism, geom="histogram")
qplot(ces97$market_liberalism, geom="histogram")

table(ces97$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces97 %>% 
  select(trad1, trad2, trad3, trad4, trad5, trad6, trad7) %>% 
  psych::alpha(.)
#Check correlation
ces97 %>% 
  select(trad1, trad2, trad3, trad4, trad5, trad6, trad7) %>% 
  cor(., use="complete.obs")


#recode Moral Traditionalism 2 (stay home & gay rights) (Left-Right)
ces97 %>% 
  rowwise() %>% 
  mutate(traditionalism2=mean(
    c_across(trad1:trad2)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('trad1', 'trad2', 'traditionalism2')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces97 %>% 
  rowwise() %>% 
  mutate(traditionalism2=mean(
    c_across(c('trad1', 'trad2')), na.rm=T  
  )) %>% 
  ungroup()->ces97

ces97 %>% 
  select(starts_with("trad")) %>% 
  summary()
#Check distribution of traditionalism
qplot(ces97$traditionalism2, geom="histogram")
table(ces97$traditionalism2, useNA="ifany")

#Calculate Cronbach's alpha
ces97 %>% 
  select(trad1, trad2) %>% 
  psych::alpha(.)

#Check correlation
ces97 %>% 
  select(trad1, trad2) %>% 
  cor(., use="complete.obs")

#just check the market and traditionalism scales for NAs
ces97 %>% 
  select(market_liberalism, traditionalism) %>% 
  str()

ggplot(ces97, aes(x=market_liberalism, y=traditionalism))+geom_point()+geom_smooth(method="lm")

#recode 2nd Dimension (stay home, immigration, gay rights, crime)
ces97$author1<-ces97$stay_home
ces97$author2<-ces97$immigration_rates
ces97$author3<-ces97$gay_rights
ces97$author4<-ces97$crime
table(ces97$author1)
table(ces97$author2)
table(ces97$author3)
table(ces97$author4)

#Remove value labels
ces97 %>% 
  mutate(across(num_range('author', 1:4), remove_val_labels))->ces97

ces97 %>% 
  rowwise() %>% 
  mutate(authoritarianism=mean(
    c_across(author1:author4)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('author1', 'author2', 'author3', 'author4', 'authoritarianism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces97 %>% 
  rowwise() %>% 
  mutate(authoritarianism=mean(
    c_across(c('author1', 'author2', 'author3', 'author4')), na.rm=T  
  )) %>% 
  ungroup()->ces97

ces97 %>% 
  select(starts_with("author")) %>% 
  summary()
#Check distribution of traditionalism
qplot(ces97$authoritarianism, geom="histogram")
table(ces97$authoritarianism, useNA="ifany")

#Calculate Cronbach's alpha
ces97 %>% 
  select(author1, author2, author3, author4) %>% 
  psych::alpha(.)

#Check correlation
ces97 %>% 
  select(author1, author2, author3, author4) %>% 
  cor(., use="complete.obs")

#recode Quebec Accommodation (cpse3a) (Left=more accom)
look_for(ces97, "quebec")
ces97$quebec_accom<-Recode(ces97$cpse3a, "2=1; 1=0; 3=0.5; 8=0.5; else=NA")
#checks
table(ces97$quebec_accom)


#recode Liberal leader (cpsd1b)
look_for(ces97, "Chretien")
ces97$liberal_leader<-Recode(ces97$cpsd1b, "0=1; 997:999=NA")
#checks
table(ces97$liberal_leader)

#recode conservative leader (cpsd1a)
look_for(ces97, "Charest")
ces97$conservative_leader<-Recode(ces97$cpsd1a, "0=1; 997:999=NA")
#checks
table(ces97$conservative_leader)

#recode NDP leader (cpsd1c)
look_for(ces97, "McDonough")
ces97$ndp_leader<-Recode(ces97$cpsd1c, "0=1; 997:999=NA")
#checks
table(ces97$ndp_leader)

#recode Bloc leader (cpsd1e)
look_for(ces97, "Duceppe")
ces97$bloc_leader<-Recode(ces97$cpsd1e, "0=1; 997:999=NA")
#checks
table(ces97$bloc_leader)

#recode liberal rating (cpsd1h)
look_for(ces97, "liberal")
ces97$liberal_rating<-Recode(ces97$cpsd1h, "0=1; 997:999=NA")
#checks
table(ces97$liberal_rating)

#recode conservative rating (cpsd1g)
look_for(ces97, "conservative")
ces97$conservative_rating<-Recode(ces97$cpsd1g, "0=1; 997:999=NA")
#checks
table(ces97$conservative_rating)

#recode NDP rating (cpsd1i)
look_for(ces97, "new democratic")
ces97$ndp_rating<-Recode(ces97$cpsd1i, "0=1; 997:999=NA")
#checks
table(ces97$ndp_rating)

#recode Bloc rating (cpsd1k)
look_for(ces97, "bloc")
ces97$bloc_rating<-Recode(ces97$cpsd1k, "0=1; 997:999=NA")
#checks
table(ces97$bloc_rating)

#recode Education (pese6f)
look_for(ces97, "edu")
ces97$education<-Recode(ces97$pese6f, "1=0; 3=0.5; 5=1; 8=0.5; else=NA")
#checks
table(ces97$education, ces97$pese6f , useNA = "ifany" )

#recode Personal Retrospective (cpsc1)
look_for(ces97, "financ")
ces97$personal_retrospective<-Recode(ces97$cpsc1, "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
val_labels(ces97$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces97$personal_retrospective)
table(ces97$personal_retrospective, ces97$cpsc1 , useNA = "ifany" )

#recode Ideology (mbsi16a)
look_for(ces97, "the left")
ces97$ideology<-Recode(ces97$mbsi16a , "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA")
val_labels(ces97$ideology)<-c(Left=0, Right=1)
#checks
val_labels(ces97$ideology)
table(ces97$ideology, ces97$mbsi16a  , useNA = "ifany")

#recode turnout (pesa2a & pesa2b)
look_for(ces97, "vote")
ces97 %>% 
  mutate(turnout=case_when(
    pesa2a==1 ~1,
    pesa2a==5 ~0,
    pesa2a==8 ~0,
    pesa2b==1 ~1,
    pesa2b==5 ~0,
    pesa2b==8 ~0,
  ))->ces97
val_labels(ces97$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces97$turnout)
table(ces97$turnout)
table(ces97$turnout, ces97$vote)

#recode Most Important Question (cpsa1)
look_for(ces97, "issue")
ces97$mip<-Recode(ces97$cpsa1, "10:16=6; 20:25=7; 26=10; 30:34=7; 35=0; 36:38=7; 40:41=10; 42:43=3; 44=13; 45=15; 
				                        46=12; 47=11; 48=0; 50:55=9; 57=8; 58=15; 59=8; 60:64=15; 65:67=4; 68:69=8; 70:71=14; 
				                        72=1; 73=14; 74=15; 75:79=2; 80:88=16; 89:90=0; 91=3; 92=0; 93=11; 94:95=0; 96-16; 97=0; else=NA")
val_labels(ces97$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10, 
                         Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Free_Trade=17)
table(ces97$mip)

# recode satisfaction with democracy (cpsb9)
look_for(ces97, "dem")
ces97$satdem<-Recode(ces97$cpsb9, "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
table(ces97$satdem, ces97$cpsb9, useNA = "ifany" )

#recode welfare (pese6b)
look_for(ces97, "welfare")
ces97$welfare<-Recode(ces97$pese6b, "1=1; 3=0.5; 8=0.5; 5=0; else=NA")
#checks
table(ces97$welfare)
table(ces97$welfare, ces97$pese6b)

