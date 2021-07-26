ces %>% 
  group_by(quebec,election, working_class, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(working_class==1 & (vote<5 & vote>0)) %>% 
  filter(!is.na(quebec)) %>% 
  ggplot(.,aes(x=as.factor(election), y=pct, linetype=as_factor(vote), group=as_factor(vote)))+
  geom_line()+
  scale_linetype_manual(values=c(2,3,6,1),  name="Vote")+theme(axis.text.x = element_text(angle = 90))+facet_grid(~as_factor(quebec))+
  labs(x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_working_class_vote.png"), dpi=300, width=12, height=4)

library(nnet)
library(ggeffects)
ces %>% 
  filter(election!=2000&quebec==0) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x)), 
         predicted=map(model, ggpredict))->roc_models_2019

#Start with where the predicted values are stored

# roc_models_2019 %>% 
#   unnest_wider(predicted) %>%
#   unnest(occupation2) %>%
#   filter(response.level!="Green") %>% 
#   ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In ROC")+theme(axis.text.x=element_text(angle=90))

#ggsave(here("Plots", "class_voting_roc_2019.png"), width=10, height=3)
#Now QC 2019
ces %>% 
  filter(election!=2000 &quebec==1) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x)), 
         predicted=map(model, ggpredict))-> qc_models_2019

#Start with where the predicted values are stored
# qc_models_2019 %>% 
#   unnest_wider(predicted) %>%
#   unnest(occupation2) %>%
#   filter(response.level!="Green") %>% 
#   ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In QCC")+theme(axis.text.x=element_text(angle=90))
#ggsave(here("Plots", "class_voting_qc_2019.png"), width=10, height=3)

qc_models_2019$region<-rep("Quebec", nrow(qc_models_2019))
roc_models_2019$region<-rep("Rest of Canada", nrow(roc_models_2019))
qc_models_2019
qc_models_2019 %>% 
  bind_rows(roc_models_2019) %>% 
  unnest_wider(predicted) %>% 
  unnest(occupation2) %>% 
  filter(response.level!="Green") %>%
  rename(Class=x, Election=election) %>% 
  ggplot(., aes(x=Election, y=predicted, group=Class, linetype=Class))+geom_line()+facet_grid(region~response.level)+scale_linetype_manual(values=c(2,3,6, 1))+theme(axis.text.x = element_text(angle = 90))
ggsave(filename=here("Plots", "canada_class_voting_1965_2019.png"), dpi=300,width=12, height=4)

#This removes pre-1979 elections, The Green Party and the year 2000 for multinomial logit modelling. 
ces %>% 
  filter(election> 1974&election!=2000  & vote2!="Green")->ces.out
andersen4qc<-multinom(vote2 ~ as.factor(occupation4)+age+male+as.factor(religion2)+degree+relevel(as.factor(election), ref="1993"), data = subset(ces.out, quebec==1))
#ROC
andersen4roc<-multinom(vote2 ~ as.factor(occupation4)+age+male+as.factor(religion2)+degree+as.factor(election)+as.factor(region3), data = subset(ces.out, quebec!=1))

stargazer(andersen4qc, andersen4roc, digits=2,out=here("Tables", "andersen_replication_extension_1979_2019.html"),  type="html", column.labels=c("QC", "ROC"), column.separate = c(3,2), title="Multinomial Logistic Regression of Left Vote On Class, 1979-2019", covariate.labels=c('(Social Class) Managers', '(Social Class) Professionals','(Social Class) Self-Employed', '(Social Class) Routine Non Manual', 'Age', 'Male', '(Religion) Catholic', '(Religion) Protestant', '(Religion) Other', '(Education) Degree', '1979','1980' ,'1984','1988','1997', '2004', '2006','2008','2011','2015','2019','1980','1984' ,'1988','1993','1997', '2004', '2006','2008','2011','2015','2019', '(Region) Ontario','(Region) West'),
          add.lines=list(c("N"," ", nrow(andersen4qc$fitted.values), "", nrow(andersen4roc$fitted.values), " ")), 
          single.row = T, 
          dep.var.labels=c("Right/Liberal", "Right/NDP", "Right/BQ", "Right/Liberal", "Right/NDP"), 
          star.cutoffs = c(0.05, 0.01, 0.001))

#Load broom
library(broom)
# 1979 -2019
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income+degree+union_both+as.factor(religion2)+working_class3, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete1
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income+degree+union_both+as.factor(religion2)+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete1

ces %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income+degree+union_both+as.factor(religion2)+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete1

#Join all parties and plot Degree and Class coefficients
ndp_models_complete1 %>% 
  bind_rows(., liberal_models_complete1) %>% 
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="degree"| term=="working_class3") %>% 
  mutate(term=Recode(term, "'degree'='Education'; 'working_class3'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote))+
  geom_point()+
  labs(title="OLS Coefficients of Education and Working Class on Party Vote 1979-2019", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.25,0.25))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(vote~term, switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_Degree_WC_all_parties.png"))


#### Average Scores For Working Class Versus Average ####

ces %>% 
  select(election,  working_class4, degree, redistribution, immigration_rates, market_liberalism,traditionalism2) %>%
  rename(Redistribution=redistribution, `Immigration Rates`=immigration_rates, `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2) %>% 
  mutate(Redistribution=skpersonal::revScale(Redistribution, reverse=T)) %>% 
  pivot_longer(cols=4:7) %>% 
  pivot_longer(cols=2:3, names_to="Variable", values_to="Group") %>% 
  filter(election>1984 &election!=2000) %>% 
  group_by(election, Variable, Group, name) %>% 
  summarize(average=mean(value, na.rm=T)) %>% 
  arrange(election, Variable, name, Group) %>%
  filter(!is.na(Group)) %>% 
  group_by(election, name) %>% 
  mutate(Variable=recode_factor(Variable, "degree"="Degree", "working_class4"="Class")) %>% 
  mutate(Group=case_when(
    Variable=="Degree" & Group==0 ~ "No Degree",
    Variable=="Degree"& Group== 1 ~ "Degree",
    Variable=="Class" & Group==0 ~ "Non Working Class",
    Variable=="Class" & Group==1 ~ "Working Class"
  )) %>% 
  #filter(Group!="No Degree" & Group!="Non Working Class") %>% 
  ggplot(., aes(y=election, x=average, group=Variable, col=`Group`))+geom_point()+facet_wrap(Variable~fct_relevel(name, "Immigration Rates","Moral Traditionalism", "Market Liberalism", "Redistribution"), nrow=2)+theme(axis.text.x=element_text(angle=90))+scale_y_discrete(limits=rev)+scale_color_manual(values=rep(c('grey', 'black'),2))+
  geom_vline(xintercept=0.5, linetype=2)+labs(y="Election", x="Average")
ggsave(here("Plots", "average_scores_raw_class_degree_population.png"), width=10, height=4)

#### Poooled Models

ces %>% 
  filter(election!=2000)->ces.1
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000)->ces.2
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000)->ces.3

ces$region2<-factor(ces$region2, levels=c("Ontario", "Atlantic", "Quebec", "West"))
ces$degree<-as_factor(ces$degree)
ces$male<-as_factor(ces$male)
ces$redistibution<-as.numeric(ces$redistribution)
ces$redistribution<-zap_labels(ces$redistibution)
ces$income<-as.numeric(ces$income)
ces %>% 
  select(age, male, income, region2, religion2, occupation4, redistribution, market_liberalism, immigration_rates) %>% 
  map(., class)
ces %>% 
  filter(election<1998 &election> 1984)->ces.4
ces %>% 
  filter(election>2003 )->ces.5
library(stargazer)
#Relevel region2


  
# NDP MODels
m19<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.4)
m28<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.5)
m22<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.4)
m31<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.5)
m25<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.4)
m34<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.5)



# Liberal models
m20<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.4)
m29<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.5)
m23<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.4)
m32<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.5)
m26<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.4)
m35<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.5)


#Conservative Models

m21<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.4)
m30<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.5)
m24<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.4)
m33<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.5)
m27<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.4)
m36<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.5)

stargazer(m19, m28, m22, m31, m25, m34, type="html", out=here("Tables", "NDP_Pooled_Models_2.html"), omit=c(1,2,3,4,5,6,7,8), column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'), covariate.labels=c("Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class(Routine Non-Manual)", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates", "Degree x Redistribution", "Degree x Moral Traditionalism"), dep.var.labels =c("Vote For NDP"),star.cutoffs = c(0.05, 0.01, 0.001), single.row=F, font.size="small", digits=2)
stargazer(m20, m29, m23, m32, m26, m35, type="html", out=here("Tables", "Liberal_Pooled_Models_2.html"), omit=c(1,2,3,4,5,6,7,8), column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'), covariate.labels=c("Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class(Routine Non-Manual)", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates", "Degree x Redistribution", "Degree x Moral Traditionalism"), dep.var.labels =c("Vote For Liberals"),star.cutoffs = c(0.05, 0.01, 0.001), single.row=F, font.size="small", digits=2)
stargazer(m21, m30, m24, m33, m27, m36, type="html", out=here("Tables", "Conservative_Pooled_Models_2.html"), omit=c(1,2,3,4,5,6,7,8), column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'), covariate.labels=c("Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class(Routine Non-Manual)", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates", "Degree x Redistribution", "Degree x Moral Traditionalism"), dep.var.labels =c("Vote For Right"), star.cutoffs = c(0.05, 0.01, 0.001), single.row=F, font.size="small", digits=2)

#### Generate Predicted Probabilities ####
#m22 m31 for NDP

#m23 m32

#m24 m33

## This section makes predicted probabilities from the models in 7_class_logistic
## The models are re-created above and found in the word document.
#Make a list of the models
model.list<-list(m22, m31, m23, m32, m24, m33)
#Run the ggeffects command for each. 
map(model.list, ggeffect, terms=c("redistribution", "degree")) %>% 
  #Turn each into a dataframe
  map(data.frame) ->model.list
#Provide names for each model
names(model.list)<-c(rep("NDP", 2), rep("Liberal", 2), rep("Conservative", 2)) 
#
model.list %>% 
  #Combine these things
bind_rows() %>% 
  #PRovide a variable showing the period 
  mutate(Period=rep(c(rep("1988-2000", 10), rep("2004-2019", 10)), 3)) %>% 
  #Define variables for party
  mutate(Party=c(rep("NDP", 20), rep("Liberal",20), rep("Right",20) )) %>% 
  #Plot
  ggplot(., aes(x=x, y=predicted, col=group))+geom_point()+geom_line()+facet_grid(Party~Period)

# This 
data.frame(m37effect) %>% 
  bind_rows(., m38effect) %>% 
  mutate(Period=c(rep("1988-2000", 2), rep("2004-2019", 2))) %>% 
  ggplot(., aes(x=x, y=predicted, group=1))+facet_grid(~Period)+geom_point()+ylim(c(0.3,0.8))+geom_line()
# Get Redistribution Interaction
#m22
m39<-update(m22, left~.)
#m31
m40<-update(m31, left~.)
m39effects<-ggeffect(m39, terms=c('redistribution', 'degree'))
m40effects<-ggeffect(m40, terms=c('redistribution', 'degree'))

m39effects %>% 
  as.data.frame() %>% 
  bind_rows(., m40effects) %>% 
  mutate(Period=c(rep("1988-2000", 10), rep("2004-2019",10))) %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+geom_point()+geom_line()+facet_grid(~Period)


mod1<-glm(left~region2+degree+age+male+income+religion2+occupation4+election+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution,data=ces.4, family="binomial")
mod2<-glm(left~region2+degree+age+male+income+religion2+occupation4+election+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution,data=ces.5, family="binomial")
summary(mod1)
summary(mod2)
plot(mod1)
library(ggeffects)

ces.4 %>% 
  select(region2, degree, age, male, income, religion2, occupation4, election, redistribution, market_liberalism, traditionalism2, immigration_rates)
plot(ggeffect(mod1, terms=c("redistribution", "degree")))
plot(ggeffect(mod2, terms=c("redistribution", "degree")))
summary(mod1)
