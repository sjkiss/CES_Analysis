#ces19 precarity analysis
library(here)
library(stargazer)
library(broom)
library(nnet)
library(purrr)
source(here('Precarity/1_Precarity_recode.R'))


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
#### Factor analysis

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

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, type="html", out=here("Precarity", "Tables", "Precarity_models.html"))

#### Precarity models ####
pm1<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner), data=cps)
summary(pm1)
pm2<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology), data=cps)
pm3<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants), data=cps)
pm4<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists), data=cps)
pm5<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities), data=cps)
pm6<-lm(volatility~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities+satisfaction_demos), data=cps)
stargazer(pm1, pm2, pm3, pm4, pm5, pm6, type="html", out=here("Precarity", "Tables", "Volatility_models.html"))

#### Probability models ####
prm1<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner), data=cps)
summary(pm1)
prm2<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology), data=cps)
prm3<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants), data=cps)
prm4<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists), data=cps)
prm5<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities), data=cps)
prm6<-lm(probability~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities+satisfaction_demos), data=cps)
stargazer(prm1, prm2, prm3, prm4, prm5, prm6, type="html", out=here("Precarity", "Tables", "Probability_models.html"))

#### Consequence models ####
cm1<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner), data=cps)
summary(pm1)
cm2<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology), data=cps)
cm3<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants), data=cps)
cm4<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists), data=cps)
cm5<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities), data=cps)
cm6<-lm(consequence~(age+degree+as.factor(income)+male+native+as.factor(region)+homeowner+ideology+immigrants+feminists+minorities+satisfaction_demos), data=cps)
stargazer(cm1, cm2, cm3, cm4, cm5, cm6, type="html", out=here("Precarity ", "Tables", "Consequence_models.html"))


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
ggsave(here("Precarity", "Plots", "Party_shares_probability_vote.png"))

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
ggsave(here("Precarity", "Plots", "Party_shares_probability_immigrant_vote.png"))

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
ggsave(here("Precarity", "Plots", "Party_shares_probability_feminist_vote.png"))

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
ggsave(here("Precarity", "Plots", "Party_shares_probability_authoritarian_vote.png"))

#### Testing Probability of Job Loss With Years of Education
cps$probability
table(as_factor(cps$education))

library(gt)
library(knitr)
library(xtable)
library(kableExtra)


#### 
cps %>% 
  select(starts_with("precarity"))
save_kable(kable(cor(cps$education, cps$probability, use="complete.obs"),format="html" , caption="Correlation Between Years of Education And Probability of Job Loss"), file=here("Precarity", "Tables", "Correlation_years_education_probability_job_loss.html"))
lookfor(cps, "employ")
prop.table(table(as_factor(cps$cps19_employment), cps$probability),2)


# 
names(cps)
#Weight the thing
library(survey)
lookfor(cps, "weight")
cps$region<-as_factor(cps$region)
cps_des<-svydesign(ids=~0, weights=~cps19_weight_general_lfs, 
          data=subset(cps, !is.na(cps$cps19_weight_general_lfs)))
cps_des
table(cps$region2)
as_factor(cps$region)
names(cps)
m1<-svyglm(right~volatility_x+probability_x+consequence_x+
             region+
           age+
             degree+
             #male+
             income+
             homeowner+
             native+
             ideology+immigrants,
           #  as_factor(cps$region), 
           design=cps_des)

m2<-svyglm(right~volatility_x+probability_x+consequence_x+
             region+
             age+
             degree+
             male+
             income+
             homeowner+
             native+
             ideology+
             immigrants+volatility_x*immigrants,
           #  as_factor(cps$region), 
           design=cps_des)

m3<-svyglm(right~volatility_x+probability_x+consequence_x+
             region+
             age+
             degree+
             male+
             income+
             homeowner+
             native+
             ideology+
             immigrants+probability_x*immigrants,
           #  as_factor(cps$region), 
           design=cps_des)
m4<-svyglm(right~volatility_x+probability_x+consequence_x+
             region+
             age+
             degree+
             male+
             income+
             homeowner+
             native+
             ideology+
             immigrants+consequence_x*immigrants,
           #  as_factor(cps$region), 
           design=cps_des)
cps$immigrants
library(modelsummary)
modelsummary(list(m1, m2, m3, m4), stars=T)
str(m1)
library(ggeffects)

ggpredict(m2, terms=c("volatility_x", "nativism[meansd]")) 
