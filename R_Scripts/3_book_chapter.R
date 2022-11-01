### this is the analysis for the book chapter
theme_set(theme_bw(base_size=20))
#### Voting Shares of Working Class
library(lubridate)
ces %>% 
  select(election, working_class, vote2) %>% 
  #filter(is.na(election))
  group_by(election, working_class, vote2) %>% 
  mutate(election=ymd(election, truncated=2L)) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=(n/sum(n))*100) %>% 
  filter(working_class==1) %>% 
  filter(vote2!="Green") %>% 
  ggplot(., aes(x=election, y=pct, linetype=fct_relevel(vote2, "Liberal"), group=vote2))+
  geom_point()+
  geom_line()+
  labs(y="Percent", x="Election", linetype="Vote")+scale_linetype_manual(values=c(1,3,5,6))+
  scale_x_date(breaks= seq.Date(from=as.Date("1965-01-01"), to=as.Date("2021-01-01"), by="5 years"), date_labels="%Y")


ggsave(here("Plots", "book_chapter_working_class_votes_for_parties.png"))

ces %>% 
  select(election, working_class, vote2) %>% 
  mutate(election=ymd(election, truncated=2L)) %>% 
  group_by(election,  vote2, working_class) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=(n/sum(n))*100) %>% 
  filter(working_class==1) 
  filter(vote2!="Green") %>% 
  ggplot(., aes(x=election, y=pct, linetype=fct_relevel(vote2, "Liberal"), shape=fct_relevel(vote2, "Liberal"),group=vote2))+
  geom_point()+
  geom_line()+
  labs(y="Percent", x="Election", linetype="Vote", shape="Vote") +
scale_x_date(breaks= seq.Date(from=as.Date("1965-01-01"), to=as.Date("2021-01-01"), by="4 years"), date_labels="%Y")
ggsave("book_chapter_parties_share_working_class_votes.png")
ces$Occupation<-Recode(ces$occupation4, "'Working_Class'='Working Class' ; 
                       'Routine_Nonmanual'='Routine Non-Manual'", levels=c(
                         "Managers", "Professionals", "Self-Employed", "Routine Non-Manual", "Working Class"
                       ))
ces %>% 
  select(election, Occupation, vote2) %>% 
  mutate(election=ymd(election, truncated=2L)) %>% 
  filter(election>1978&election!=2000&election!=2021) %>% 
  group_by(election, Occupation, vote2) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=n/sum(n)) %>% 
filter(vote2!="Green") %>% 
  filter(!is.na(Occupation)) %>% 
  ggplot(., aes(x=election, y=pct*100, linetype=Occupation, group=Occupation))+
  geom_line()+
  facet_wrap(~vote2)+
  scale_linetype_manual(values=c(2,4,6,8,1))+labs(y="Percent", x="Election", 
                                                  title="Raw Share of Class Voting Per Party") +
scale_x_date(breaks= seq.Date(from=as.Date("1980-01-01"), to=as.Date("2021-01-01"), by="4 years"), date_labels="%Y")+
  theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "book_occupation_share_per_party.png"))

ces %>% 
  select(election, Occupation, vote2) %>% 
  filter(election>1978& election!=2000& election!=2021) %>% 
  mutate(election=ymd(election, truncated=2L)) %>% 
  group_by(election, vote2, Occupation) %>% 
  summarize(n=n()) %>% 
  #filter(!is.na(vote2)) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(vote2!="Green") %>% 
  filter(!is.na(Occupation)) %>% 
  ggplot(., aes(x=election, y=pct*100, linetype=vote2, group=vote2))+
  geom_line()+
  facet_wrap(~Occupation)+scale_linetype_manual(values=c(1,2,3,4))+
  labs(y="Percent", x="Election", title="Share Of Each Party's Electorate By Class",linetype="Vote")+
  theme(axis.text.x=element_text(angle=90))+
  scale_x_date(breaks= seq.Date(from=as.Date("1980-01-01"), to=as.Date("2021-01-01"), by="5 years"), date_labels="%Y")
  
ggsave(here("Plots", "book_party_share_by_occupation.png"), width=10, height=8)

ces$region2<-factor(ces$region2, levels=c("Atlantic", "Quebec", "Ontario", "West"))
ces$income_tertile
ces$traditionalism2
ces %>% 
  filter(election>1988&Period==0)->ces.1
ces %>% 
  filter(Period==1&election<2022)->ces.2
table(ces.2$election)
table(ces.1$election)
ces$immi
ndp1<-lm(ndp~region2+male+age+income_tertile+degree+catholic+no_religion+redistribution+market_liberalism+immigration_rates+traditionalism+`1993`+`1997`+`2000`, data=ces.1)
ndp2<-lm(ndp~region2+male+age+income_tertile+degree+catholic+no_religion+redistribution+market_liberalism+immigration_rates+traditionalism+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`, data=ces.2)

lib1<-lm(liberal~region2+male+age+income_tertile+degree+catholic+no_religion+redistribution+market_liberalism+immigration_rates+traditionalism+`1993`+`1997`+`2000`, data=ces.1)
lib2<-lm(liberal~region2+male+age+income_tertile+degree+catholic+no_religion+redistribution+market_liberalism+immigration_rates+traditionalism+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`, data=ces.2)

con1<-lm(conservative~region2+male+age+income_tertile+degree+catholic+no_religion+redistribution+market_liberalism+immigration_rates+traditionalism+`1993`+`1997`+`2000`, data=ces.1)
con2<-lm(conservative~region2+male+age+income_tertile+degree+catholic+no_religion+redistribution+market_liberalism+immigration_rates+traditionalism+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`, data=ces.2)
model.list<-list(ndp1, ndp2, lib1, lib2, con1, con2)
library(stargazer)
stargazer(model.list, 
          type="html", out=here("Tables", "book_table_1_ols_pre_post.html"))
lookfor(ces19phone, "last")
lookfor(ces19phone, "vote")
ces19phone %>% 
  select(occupation4, vote2 ) %>% 
  filter(vote2)
ces19phone$past_vote
#USe ces19phone
ces19phone %>% 
  #WE need past_vote for 2015, vote for 2019 and occupation3 for the class variable that has the self-employed
  select(past_vote, vote, occupation3) %>% 
  #Convert to factors
  as_factor() %>% 
  #We are only interested in NDP voters from 2015. Which way did they go?
  filter(past_vote=="NDP") %>% 
  #WE want to know the counts of the 2015 NDP voters 2019 vote choice by occupation 
  #So we form groups by occupation3 and vote
  group_by(occupation3, vote) %>% 
  #Count
  summarize(n=n()) %>% 
  #Form percents
  #This should be the vote flows of 2015 NDP voters by occupation 
  mutate(pct=n/sum(n)) %>% 
  #WE don't want people who had no reported vote in 2019
  filter(!is.na(vote)) %>% 
  #We don't want people with other votes in 2019
  filter(vote!="Other") %>% 
  filter(occupation3=="Skilled"|occupation3=="Unskilled")
  #This cleans up the occupation variable and combines the unskilled and skilled categories
  mutate(occupation4=Recode(occupation3, "'Routine_Nonmanual'='Routine Non-Manual'; 'Skilled'='Working Class';
                            'Unskilled'='Working Class'; 
                            'Self_employed'='Self-Employed'", 
                            levels=c("Managers", "Professional", "Self-Employed", "Routine Non-Manual", "Working Class"))) %>%  
  #Graph
  mutate(vote=fct_relevel(vote, "Bloc",  "Conservative", "Green", "Liberal", "NDP")) %>% 
  ggplot(., aes(x=pct, fill=vote, y=vote))+geom_col()+facet_wrap(~occupation4)+xlim(c(0,0.6)) + 
  scale_fill_manual(values=c("grey80", "grey70", "grey60", "grey50", "black"))+theme(legend.position="none")+labs(x="Percent", y="Vote", title="2019 Votes of 2015 NDP voters, by class")
ggsave(here("Plots", "book_vote_flow.png"))

ces19phone %>% 
  select(occupation3, mip)->out19
ces15phone %>% 
  select(occupation3, mip)->out15
out15$Election<-rep("2015",nrow(out15))
out19$Election<-rep("2019", nrow(out19))
out15 %>% 
  bind_rows(out19) %>% 
  as_factor() %>% 
  mutate(occupation3=Recode(occupation3, "'Routine_Nonmanual'='Routine Non-Manual'; 'Skilled'='Working Class';
                            'Unskilled'='Working Class'; 
                            'Self_employed'='Self-Employed'", 
                            levels=c("Managers", "Professional", "Self-Employed", "Routine Non-Manual", "Working Class")) ) %>% 
mutate(Issue=Recode(mip, "'Jobs'='Jobs and Economy' ;
                    'Economy'='Jobs and Economy'"))  %>% 
  group_by(Election, occupation3, Issue) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100) %>% 
  arrange(Issue, occupation3) %>% 
  group_by(Issue, occupation3) %>% 
  mutate(Change=Percent-lag(Percent)) %>% 
  #filter(Election==2019) %>% 
  filter(str_detect(Issue, "Jobs|Environment|Immigration|Health|Energy")) %>% 
  filter(!is.na(occupation3)) %>% 
  ggplot(., aes(y=fct_relevel(occupation3, "Working Class", "Routine Non-Manual", "Self-Employed", "Professional", "Managers"), x=Percent, fill=fct_relevel(Issue, "Environment", "Energy")))+
  facet_grid(~Election)+geom_col(position="dodge")+labs(y="Class", fill="Issue")+scale_fill_grey(guide=guide_legend(reverse=T))
ggsave(here("Plots", "book_mip_change.png"))


#Do the moral traditionalism interaction
ces %>% 
  #Keep only 2015 and 2019
  filter(election==2015|election==2019) %>% 
  #Filter working_class 
 #filter(working_class4==1) %>% 
  #Select relevenat variables note no controls
  select(working_class4, market_liberalism, traditionalism2, election, conservative, ndp) %>% 
  #Nest the the data for the elections
  nest(data=-election) %>% 
  #we are mapping the model function onto the column data
  #fitting first the model ndp ~ traditionalism and market liberalism
  mutate(ndp=map(data, function(x) lm(ndp~working_class4+traditionalism2+market_liberalism+working_class4:traditionalism2, data=x)),
         #Then doing the same for consservatives
         conservative=map(data, function(x) lm(conservative~working_class4+traditionalism2+market_liberalism+working_class4:traditionalism2, data=x))) ->ndp_conservative_models
ndp_conservative_models
library(ggeffects)
#Now mapping onto 
ndp_conservative_models$ndp %>% 
  map_dfr(., ggpredict, terms=c("traditionalism2[0,0.33, 0.66,1]", "working_class4[0,1]")) %>% 
  mutate(Election=rep(c(2015, 2019), each=8), 
         Vote=rep("NDP", nrow(.))) ->ndp_effects

ndp_conservative_models$conservative %>% 
  map_dfr(., ggpredict, terms=c("traditionalism2[0,0.33, 0.66,1]", "working_class4[0,1]")) %>% 
mutate(Election=rep(c(2015, 2019), each=8), 
       Vote=rep("Conservative", nrow(.)))->conservative_effects 
conservative_effects
ndp_effects %>% 
  bind_rows(conservative_effects) %>% 
data.frame() %>% 
ggplot(., aes(x=x, y=predicted, shape=group, linetype=group))+
  geom_line()+geom_point()+facet_grid(Vote~Election)+
  scale_x_continuous(breaks=c(0,0.33, 0.66, 1), labels=c("0", "0.33","0.66", "1"))+
  labs(shape="Class", linetype="Class",x="Traditionalism", y="Predicted Probability")
ggsave(here("Plots", "book_traditionalism_class_ols.png"), width=10, height=6)

ces %>% 
  filter(election>2014 &election<2020) %>% 
  select(manage_economy)
ces15phone %>% 
  select(manage_economy, occupation3, vote) %>% 
  mutate(Election=rep(2015, nrow(.)))->out15

ces19phone %>% 
  select(manage_economy, occupation3, vote) %>% 
  mutate(Election=rep(2019, nrow(.)))->out19
out15 %>% 
  bind_rows(out19) %>% 
  as_factor() %>% 

rename(`Manage Economy`=1, Class=2, Vote=3) %>% 
  mutate(Election=factor(Election, levels=c("2019", "2015")), Class=Recode(Class, "
                      'Unskilled'='Working Class' ; 'Skilled'='Working Class' ;
                      'Self_employed'='Self-Employed'; 
                      'Routine_Nonmanual'='Routine Non-Manual'", 
                      levels=c(NA,"Working Class" , "Routine Non-Manual", "Self-Employed", "Professional", "Managers"))) %>% 
  group_by(Election, Class, Vote) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100) %>% 
  filter(Vote!="Other"&Vote!="Green"& Vote!="Bloc") %>% 
  ggplot(., aes(x=Percent, y=Class, fill=Election))+geom_col(position="dodge")+facet_grid(vars(Vote))+
  scale_fill_grey(start=0.2, end=0.8)+labs(fill="Election")+
  guides(fill=guide_legend(reverse=T))
ggsave(here("Plots", "book_valence_change.png"))

