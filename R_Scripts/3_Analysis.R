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
library(stargazer)
stargazer(andersen4qc, andersen4roc, digits=2,out=here("Tables", "andersen_replication_extension_1979_2019.html"),  type="html", column.labels=c("QC", "ROC"), column.separate = c(3,2), title="Multinomial Logistic Regression of Left Vote On Class, 1979-2019", covariate.labels=c('(Social Class) Managers', '(Social Class) Professionals','(Social Class) Self-Employed', '(Social Class) Routine Non Manual', 'Age', 'Male', '(Religion) Catholic', '(Religion) Protestant', '(Religion) Other', '(Education) Degree', '1979','1980' ,'1984','1988','1997', '2004', '2006','2008','2011','2015','2019','1980','1984' ,'1988','1993','1997', '2004', '2006','2008','2011','2015','2019', '(Region) Ontario','(Region) West'),
          add.lines=list(c("N"," ", nrow(andersen4qc$fitted.values), "", nrow(andersen4roc$fitted.values), " ")), 
          single.row = T, 
          dep.var.labels=c("Right/Liberal", "Right/NDP", "Right/BQ", "Right/Liberal", "Right/NDP"), 
          star.cutoffs = c(0.05, 0.01, 0.001))

#Load broom
library(broom)
#1979 -2019
ces %>%
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income+degree+as.factor(religion2)+working_class3, data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete1
ces %>%sess
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income+degree+as.factor(religion2)+working_class3, data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete1

ces %>%
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income+degree+as.factor(religion2)+working_class3, data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete1

#Join all parties and plot Class coefficients
ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>%
  filter(term="working_class3") %>%
  mutate(term=Recode(term, "'working_class3'='Working Class'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote 1979-2019", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.3,0.3))+
  scale_color_manual(values=c("navy blue", "red", "orange"))+
  facet_grid(vote~term, switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_WC_all_parties.png"))

# 1979 -2019 without union + no degree plot (Quebec)
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~quebec+male+age+income+degree+as.factor(religion2)+working_class3, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete_QC1
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~quebec+region2+male+age+income+degree+as.factor(religion2)+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete_QC1

ces %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~quebec+male+age+income+degree+as.factor(religion2)+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete_QC1

ces %>% 
  filter(election> 1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(bloc~quebec+male+age+income+degree+as.factor(religion2)+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Bloc', nrow(.))  
  )->bloc_models_complete_QC1

# 1979 -2019 without union + no degree plot (ROC)
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~region+male+age+income+degree+as.factor(religion2)+working_class3, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete_ROC1
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~region+male+age+income+degree+as.factor(religion2)+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete_ROC1

ces %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~region+male+age+income+degree+as.factor(religion2)+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete_ROC1

#Join all parties and plot Class coefficients  by QC
ndp_models_complete_QC1 %>% 
  bind_rows(., liberal_models_complete_QC1) %>% 
  bind_rows(., conservative_models_complete_QC1) %>%
  bind_rows(., bloc_models_complete_QC1) %>%
  unnest(tidied) %>% 
  filter(term=="working_class3") %>% 
  mutate(term=Recode(term, "'working_class3'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote 1979-2019 for QC", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.3,0.3))+
  scale_color_manual(values=c("sky blue", "navy", "red", "orange"))+
  facet_grid(vote~term, switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_WC_all_parties2_QC.png"))

#Join all parties and plot Class coefficients by ROC
ndp_models_complete_ROC1 %>% 
  bind_rows(., liberal_models_complete_ROC1) %>% 
  bind_rows(., conservative_models_complete_ROC1) %>%
  unnest(tidied) %>% 
  filter(term=="working_class3") %>% 
  mutate(term=Recode(term, "'working_class3'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote 1979-2019 for ROC", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.3,0.3))+
  scale_color_manual(values=c("navy", "red", "orange"))+
  facet_grid(vote~term, switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_WC_all_parties_ROC2.png"))

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

#This was in the original 7_script, but I'm not sure we need this. 

# ces %>% 
#   filter(election!=2000)->ces.1
# ces %>% 
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000)->ces.2
# ces %>% 
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000)->ces.3

## These are some mslight modifications necessary; nothing huge.
## The one thing is to set Ontario to the reference category for the region2; it helps for predicted probabilities
ces$region2<-factor(ces$region2, levels=c("Ontario", "Atlantic", "Quebec", "West"))
ces$degree<-as_factor(ces$degree)
ces$male<-as_factor(ces$male)
ces$redistibution<-as.numeric(ces$redistribution)
ces$redistribution<-zap_labels(ces$redistibution)
ces$income<-as.numeric(ces$income)

#What we need is just 1988-2000 and 2004-2019, minus the BQ and the Greens
ces %>% 
  filter(election<1998 &election> 1984&vote2!="BQ"&vote2!="Green")->ces.1
ces %>% 
  filter(election>2003 &vote2!="BQ"&vote2!="Green")->ces.2
library(stargazer)
#Relevel region2

#### Pooled OLS Models####
# NDP Models
m19<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.1)
m28<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.2)
m22<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.1)
m31<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.2)
m25<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.1)
m34<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.2)
#NDP

# Liberal models
m20<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.1)
m29<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.2)
m23<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.1)
m32<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.2)
m26<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.1)
m35<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.2)

#Conservative Models
m21<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.1)
m30<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.2)
m24<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.1)
m33<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.2)
m27<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.1)
m36<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.2)
# 

#Storing these here to avoid having to retype. 
#"Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class (Routine Non-Manual)", 
summary(m19)
stargazer(m19, m28, m20, m29, m21, m30, type="html", omit=c(1:8), out=here("Tables", "pooled_party_vote_choice.html"),  column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'), covariate.labels=c("Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class (Routine Non-Manual)", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates"), dep.var.labels =c("NDP" ,"Liberals", "Conservatives"),star.cutoffs = c(0.05, 0.01, 0.001), single.row=F, font.size="small", digits=2)

# For Appendix (full controls displayed)
stargazer(m19, m28, m20, m29, m21, m30, type="html", out=here("Tables", "pooled_party_vote_choice_full.html"),  column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'), covariate.labels=c("Region (East", "Region (Ontario)", "Region (West)", "Age", "Male", "Religion (Catholic)", "Religion (Protestant)", "Religion (Other)", "Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class (Routine Non-Manual)", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates"), dep.var.labels =c("NDP" ,"Liberals", "Conservatives"),star.cutoffs = c(0.05, 0.01, 0.001), single.row=F, font.size="small", digits=2)

#### ###
#

interaction.models<-list(m22, m31, m25, m34, 
                         m23, m32, m26, m35, 
                         m24, m33, m27, m36)
#get length of coefficients
interaction.models %>% 
  map(., function(x) length(x$coefficients))

#names(interaction.models)<-c(rep("NDP", 2), rep("Liberals", 2), rep("Conservative", 2))
interaction.models %>% 
  map_dfr(., tidy, .id='model') %>% 
  mutate(term=recode_factor(term, "degreeDegree:redistribution"="Degree:Redistribution",
                            "degreeDegree:traditionalism2"="Degree:Traditionalism")) %>% 
  mutate(Party=c(rep("NDP", 80), rep("Liberal", 80), rep("Conservative", 80)),
                  Period=c(rep("1988-2000", 20 ), rep("2004-2019", 20), rep("1988-2000", 20 ), rep("2004-2019", 20),
         rep("1988-2000", 20 ), rep("2004-2019", 20), rep("1988-2000", 20 ), rep("2004-2019", 20),rep("1988-2000", 20 ), rep("2004-2019", 20), rep("1988-2000", 20 ), rep("2004-2019", 20))) %>% filter(str_detect(term, ":")) %>% 
  ggplot(., aes(x=Period, y=estimate, col=Period))+geom_point()+facet_grid(term~fct_relevel(Party, "NDP", "Liberal"), scales="free")+scale_color_grey(start=0.8, end=0.2) +geom_errorbar(width=0, aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))+   geom_hline(yintercept=0,  linetype=2)+theme(strip.text.y.right = element_text(angle = 0))+labs(y="Coefficient")
ggsave(filename=here("Plots", "interaction_terms.png"), width=8, height=4)


  ## The models are re-created above and found in the word document.
  #Make a list of the models
summary(m22)
summary(m31)
summary(m23)
summary(m32)
summary(m24)
summary(m33)
model.list<-list(m22, m31, m24, m33)
#Run the ggeffects command for each. 
map(model.list, ggeffect, terms=c("redistribution", "degree")) %>% 
  #Turn each into a dataframe
  map(data.frame) ->model.list
#Provide names for each model
names(model.list)<-c(rep("NDP", 2),  rep("Conservative", 2)) 
#
model.list %>% 
  #Combine these things
  bind_rows() %>% 
  #PRovide a variable showing the period 
  mutate(Period=rep(c(rep("1988-2000", 10), rep("2004-2019", 10)), 2)) %>% 
  #Define variables for party
  mutate(Party=c(rep("NDP", 20),  rep("Right",20) )) %>% 
  #Plot
  ggplot(., aes(x=x, y=predicted, col=group))+geom_point()+geom_line()+facet_grid(Party~Period)+labs(x="Redistribution", y="Predicted")+scale_color_grey(name="Degree\nStatus")


ggsave(filename=here("Plots", "degree_redistribution_interaction_ndp_conservative.png"), width=6, height=4 )
