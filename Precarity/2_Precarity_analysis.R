#ces19 precarity analysis

library(stargazer)
library(broom)
library(nnet)
library(purrr)

#Recode voting
cps$ndp<-Recode(cps$vote, "3=1; 0:2=0; 4:6=0; NA=NA")
cps$liberal<-Recode(cps$vote, "1=1; 2:6=0; NA=NA")
cps$conservative<-Recode(cps$vote, "0:1=0; 2=1; 3:6=0; NA=NA")
cps$bloc<-Recode(cps$vote, "4=1; 0:3=0; 5:6=0; else=NA")
cps$green<-Recode(cps$vote, "5=1; 0:4=0; 6=0; else=NA")
cps$pparty<-Recode(cps$vote, "6=1; 0:5=0; else=NA")
table(cps$ndp)
table(cps$liberal)
table(cps$conservative)
table(cps$bloc)
table(cps$green)
table(cps$pparty)

cps$left_vs_right<-Recode(cps$vote, "1=1; 2=0; 3=1; 5=1; 6=0; else=NA")
cps$bloc_vs_right<-Recode(cps$vote, "4=1; 2=0; 6=0; else=NA")
cps$right<-Recode(cps$vote, "2=1; 6=1; 1=0; 3:5=0; 0=0; else=NA")
table(cps$left_vs_right)
table(cps$bloc_vs_right)
table(cps$right)

# Turn religion into factor with None as reference case
cps$religion2<-Recode(as.factor(cps$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(cps$religion2)
table(cps$religion2)
# Religion dummies
cps$catholic<-Recode(cps$religion, "1=1; 2:3=0; 0=0; NA=NA")
cps$no_religion<-Recode(cps$religion, "0=1; 1:3=0; NA=NA")

#### Correlations ####

#Is perception of likelihood of job loss correlated with the consequences?
cps %>% 
  select(contains("job_")) %>% 
  cor(., use="complete.obs")

cps %>% 
  select(probability, consequence) %>% 
  alpha(.)

corr<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+consequence), data=cps)
summary(corr)

## Is consequences of job loss correlated with income?
cps$income %>% 
  summary()
length(cps$income)

#Answer, the greater the income , the lower the consequences of job loss. 
cor(cps$income, cps$consequence, use="complete.obs")

## Is probability of job loss correlated with income?
cor(cps$income, cps$probability, use="complete.obs")
#Answer, the greater the income , the lower the probability of job loss. 

#### Voting for the Right ####
m1<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner), family="binomial", data=cps)
summary(m1)
m2<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology), family="binomial", data=cps)
m3<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+volatility), family="binomial", data=cps)
m4<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+volatility+probability), family="binomial", data=cps)
m5<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+volatility+probability+consequence), family="binomial", data=cps)
m6<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+volatility+probability+consequence+immigrants), family="binomial", data=cps)
m7<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+volatility+probability+consequence+immigrants+feminists), family="binomial", data=cps)
m8<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+volatility+probability+consequence+immigrants+feminists+probability:immigrants), family="binomial", data=cps)
m9<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+volatility+probability+consequence+immigrants+feminists+probability:feminists), family="binomial", data=cps)
m10<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+volatility+probability+consequence+immigrants+feminists+satisfaction_demos), family="binomial", data=cps)
m11<-glm(right~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+volatility+probability+consequence+immigrants+feminists+satisfaction_demos+probability:satisfaction_demos), family="binomial", data=cps)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, type="html", out=here("Tables", "Precarity_models.html"))

#### Precarity models ####
pm1<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner), data=cps)
summary(pm1)
pm2<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology), data=cps)
pm3<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants), data=cps)
pm4<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists), data=cps)
pm5<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities), data=cps)
pm6<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities+satisfaction_demos), data=cps)
stargazer(pm1, pm2, pm3, pm4, pm5, pm6, type="html", out=here("Tables", "Volatility_models.html"))

#### Probability models ####
prm1<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner), data=cps)
summary(pm1)
prm2<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology), data=cps)
prm3<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants), data=cps)
prm4<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists), data=cps)
prm5<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities), data=cps)
prm6<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities+satisfaction_demos), data=cps)
stargazer(prm1, prm2, prm3, prm4, prm5, prm6, type="html", out=here("Tables", "Probability_models.html"))

#### Consequence models ####
cm1<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner), data=cps)
summary(pm1)
cm2<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology), data=cps)
cm3<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants), data=cps)
cm4<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists), data=cps)
cm5<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities), data=cps)
cm6<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities+satisfaction_demos), data=cps)
stargazer(cm1, cm2, cm3, cm4, cm5, cm6, type="html", out=here("Tables", "Consequence_models.html"))


#### Descriptives ####

# Means of 3 precarity questions
cps %>%
  select(volatility, probability, consequence) %>% 
  summarise_at(vars(volatility, probability, consequence), mean, na.rm=T)

# By Vote
cps %>%
  select(vote, volatility, probability, consequence) %>% 
  group_by(vote) %>%
  summarise_at(vars(volatility, probability, consequence), mean, na.rm=T)

# By Income
cps %>%
  select(income, volatility, probability, consequence) %>% 
  group_by(income) %>%
  summarise_at(vars(volatility, probability, consequence), mean, na.rm=T)

# Probability by Party Vote
cps %>% 
  group_by(probability, vote) %>% 
  summarize(n=n()) %>% 
  filter(vote<7 & vote>0) %>% 
  mutate(pct=n/sum(n)*100) %>%
  ggplot(.,aes(x=as.numeric(probability), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  labs(title="Party vote shares by probability of job/business loss", x="Probability of Job/Biz Loss", y="Percent")
ggsave(here("Plots", "Party_shares_probability_vote.png"))

# Recode immigrants = anti_immigrants
table(cps$immigrants)
cps$anti_immigrants<-Recode(cps$immigrants, "0:0.5=0; 0.51:1=1; else=NA")
table(cps$anti_immigrants, cps$probability)

# Anti-immigrant vote by probability of job/biz loss
cps %>% 
  group_by(probability, anti_immigrants, vote) %>% 
  summarize(n=n()) %>% 
  filter(anti_immigrants==1 & (vote<7 & vote>0)) %>% 
  mutate(pct=n/sum(n)*100) %>%
  ggplot(.,aes(x=as.numeric(probability), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  labs(title="Anti-immigrant party vote shares by probability of job/business loss", x="Probability of Job/Biz Loss", y="Percent")
ggsave(here("Plots", "Party_shares_probability_immigrant_vote.png"))

# Recode feminists = anti_feminists
table(cps$feminists)
cps$anti_feminists<-Recode(cps$feminists, "0:0.5=0; 0.51:1=1; else=NA")
table(cps$anti_feminists, cps$probability)

# Anti-feminist vote by probability of job/biz loss
cps %>% 
  group_by(probability, anti_feminists, vote) %>% 
  summarize(n=n()) %>% 
  filter(anti_feminists==1 & (vote<7 & vote>0)) %>% 
  mutate(pct=n/sum(n)*100) %>%
  ggplot(.,aes(x=as.numeric(probability), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  labs(title="Anti-feminist party vote shares by probability of job/business loss", x="Probability of Job/Biz Loss", y="Percent")
ggsave(here("Plots", "Party_shares_probability_feminist_vote.png"))

# Recode authoritarian = authoritarian2
table(cps$authoritarian)
cps$authoritarian2<-Recode(cps$authoritarian, "0:0.5=0; 0.51:1=1; else=NA")
table(cps$authoritarian2, cps$probability)

# Anti-feminist vote by probability of job/biz loss
cps %>% 
  group_by(probability, authoritarian2, vote) %>% 
  summarize(n=n()) %>% 
  filter(authoritarian2==1 & (vote<7 & vote>0)) %>% 
  mutate(pct=n/sum(n)*100) %>%
  ggplot(.,aes(x=as.numeric(probability), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  labs(title="Authoritarian party vote shares by probability of job/business loss", x="Probability of Job/Biz Loss", y="Percent")
ggsave(here("Plots", "Party_shares_probability_authoritarian_vote.png"))
