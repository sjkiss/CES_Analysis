#### OLS Degree+Income
library(broom)
library(stargazer)
#### Party Share of Degree Holders

ces %>% 
  select(election, ndp, conservative, liberal, degree) %>% 
  pivot_longer(., cols=ndp:liberal, names_to=c('Vote'), values_to=c('Party')) %>% 
  group_by(election, degree,Vote, Party) %>% 
  summarize(n=n()) %>% 
   filter(!is.na(Party)) %>% 
   #filter(!is.na(degree)) %>% 
  mutate(Percent=n/sum(n)) %>% 
  filter(degree==1) %>% 
  filter(Party==1) %>% 
  ggplot(., aes(x=election, y=Percent, group=1))+geom_line()+facet_grid(~Vote)+labs(title="Share of Degree Voters per Party")
ggsave(here("Plots", "piketty_degree_vote_1965_2021.png"))
#Party Share of Top Income Holders and Bottom Income Holders
#### Party Share of Degree Holders

ces %>% 
  select(election, ndp, conservative, liberal, income) %>% 
  pivot_longer(., cols=ndp:liberal, names_to=c('Vote'), values_to=c('Party')) %>% 
  group_by(election, income,Vote, Party) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(Party)) %>% 
  filter(!is.na(income)) %>% 
  mutate(Percent=n/sum(n)) %>%
  filter(income==1|income==5) %>% 
  filter(Party==1) %>% 
  arrange(., Vote) %>% 
  group_by(election,  Vote) %>% 
  mutate(difference=Percent-lag(Percent, 1)) %>% 
  ggplot(., aes(x=election, y=Percent, group=Vote))+geom_line()+facet_grid(
~Vote)+labs(main="Share of Top Income Earners Minus Bottom income earners by Party")
ggsave(here("Plots", "piketty_income_vote_1965_2021.png"))

# Basic Party vote models 1965-2021
ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete1

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete1

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete1

ces %>%
  filter(election>2003) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+income+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete1

stargazer(ndp_models_complete1$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1965-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete1$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1965-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete1$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1965-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(green_models_complete1$model, 
          type="html", 
          out=here("Tables", "Green_Models_2004_2021.html"),
          column.labels=c("2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Green Models 2004-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#Join all parties and plot Degree coefficients
ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>%
  filter(term=="degree") %>%
  mutate(term=Recode(term, "'degree'='Degree'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote))+
  geom_point()+
  labs(title="OLS Coefficients of Degree holders on Party Vote 1965-2021", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  scale_color_manual(values=c("navy blue", "red", "orange"))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  facet_grid(vote~term, switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "Vote_Coefficents_degree_big3_parties.png"), dpi=300)

#Join all parties and plot Income coefficients
ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>%
  filter(term=="income") %>%
  mutate(term=Recode(term, "'income'='Income'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote))+
  geom_point()+
  labs(title="OLS Coefficients of Income on Party Vote 1965-2021", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.08,0.08))+
  scale_color_manual(values=c("navy blue", "red", "orange"))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  facet_grid(vote~term, switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "Vote_Coefficents_income_big3_parties.png"), dpi=300)

# Attitudinal party vote models 1988-2021
ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete2

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete2

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete2

ces %>%
  filter(election>2003) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete2

stargazer(ndp_models_complete2$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1988_2021_2.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete2$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1988_2021_2.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete2$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1988_2021_2.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(green_models_complete2$model, 
          type="html", 
          out=here("Tables", "Green_Models_2004_2021_2.html"),
          column.labels=c("2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Green Models 2004-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Attitudinal party vote models 1988-2021 (Degree sub-sample)
ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & degree!=0) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete3

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & degree!=0) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete3

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & degree!=0) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete3

ces %>%
  filter(election>2003 & degree!=0) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete3

stargazer(ndp_models_complete3$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1988_2021_3.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete3$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1988_2021_3.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete3$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1988_2021_3.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(green_models_complete3$model, 
          type="html", 
          out=here("Tables", "Green_Models_2004_2021_3.html"),
          column.labels=c("2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Green Models 2004-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Attitudinal party vote models 1988-2021 (No Degree-holders sub-sample)
ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & degree!=1) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete4

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & degree!=1) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete4

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & degree!=1) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete4

ces %>%
  filter(election>2003 & degree!=1) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete4

stargazer(ndp_models_complete4$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1988_2021_4.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete4$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1988_2021_4.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete4$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1988_2021_4.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(green_models_complete4$model, 
          type="html", 
          out=here("Tables", "Green_Models_2004_2021_4.html"),
          column.labels=c("2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Green Models 2004-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

##### Raw Vote
ces %>% 
  group_by(quebec, election, degree, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(degree==1 & (vote<5 & vote>0)) %>% 
  filter(!is.na(quebec)) %>% 
  ggplot(.,aes(x=as.factor(election), y=pct, linetype=as_factor(vote), group=as_factor(vote)))+
  geom_line()+
  scale_linetype_manual(values=c(2,3,6,1),  name="Vote")+theme(axis.text.x = element_text(angle = 90))+facet_grid(~as_factor(quebec))+
  labs(x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_degree_vote.png"), dpi=300, width=12, height=4)

# Party Vote Share Degree holders by Issue
ces %>% 
  select(Election=election, degree, vote,  `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2, `Redistribution`=redistribution, `Immigration Rates`=immigration_rates) %>%
  pivot_longer(cols=`Market Liberalism`:`Immigration Rates`) %>% 
  group_by(name) %>% 
  mutate(pro=case_when(
    value>0.5~ 1,
    TRUE ~ 0
  )) %>% 
  group_by(Election, degree, name, pro, vote) %>% 
  filter(Election>1984) %>% 
  filter(!is.na(vote)) %>% 
  filter(vote > 0 &vote<5) %>% 
  summarize(n=n()) %>% 
  mutate(percent=n/sum(n)) %>% 
  # filter(Election==2015) %>% 
  filter(degree==1) %>% 
  ggplot(., aes(x=Election, y=percent, fill=as_factor(vote)))+geom_col(position="dodge")+
  facet_wrap(~fct_relevel(name, "Moral Traditionalism", ))+
  #scale_fill_grey(name="Vote") 
  scale_fill_manual(values=c('red', 'darkblue', 'orange', 'lightblue' ), name="Vote")+theme(text = element_text(size = 20), axis.text.x = element_text(angle=90))  
ggsave(filename=here("Plots", "party_vote_shares_issues_degree_1988_2021.png"), width=10, height=8, dpi=300)

# Party Vote Share No-Degree holders by Issue
ces %>% 
  select(Election=election, degree, vote,  `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2, `Redistribution`=redistribution, `Immigration Rates`=immigration_rates) %>%
  pivot_longer(cols=`Market Liberalism`:`Immigration Rates`) %>% 
  group_by(name) %>% 
  mutate(pro=case_when(
    value>0.5~ 1,
    TRUE ~ 0
  )) %>% 
  group_by(Election, degree, name, pro, vote) %>% 
  filter(Election>1984) %>% 
  filter(!is.na(vote)) %>% 
  filter(vote > 0 &vote<5) %>% 
  summarize(n=n()) %>% 
  mutate(percent=n/sum(n)) %>% 
  # filter(Election==2015) %>% 
  filter(degree==0) %>% 
  ggplot(., aes(x=Election, y=percent, fill=as_factor(vote)))+geom_col(position="dodge")+
  facet_wrap(~fct_relevel(name, "Moral Traditionalism", ))+
  #scale_fill_grey(name="Vote") 
  scale_fill_manual(values=c('red', 'darkblue', 'orange', 'lightblue' ), name="Vote")+theme(text = element_text(size = 20), axis.text.x = element_text(angle=90))  
ggsave(filename=here("Plots", "party_vote_shares_no_degree_issues_1988_2021.png"), width=10, height=8, dpi=300)

#### Pooled OLS Models####

#What we need is just 1988-2000 and 2004-2021, minus the BQ and the Greens
ces %>% 
  filter(election<2001 &election> 1984&vote2!="BQ"&vote2!="Green")->ces.1
ces %>% 
  filter(election<2020 &election>2003 &vote2!="BQ"&vote2!="Green")->ces.2

# NDP Models
m19<-lm(ndp~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.1)
m28<-lm(ndp~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.2)
m22<-lm(ndp~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.1)
m31<-lm(ndp~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.2)
m25<-lm(ndp~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.1)
m34<-lm(ndp~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.2)
#NDP

# Liberal models
m20<-lm(liberal~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.1)
m29<-lm(liberal~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.2)
m23<-lm(liberal~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.1)
m32<-lm(liberal~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.2)
m26<-lm(liberal~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.1)
m35<-lm(liberal~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.2)

#Conservative Models
m21<-lm(conservative~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.1)
m30<-lm(conservative~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.2)
m24<-lm(conservative~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.1)
m33<-lm(conservative~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.2)
m27<-lm(conservative~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.1)
m36<-lm(conservative~region2+age+male+religion2+degree+income+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.2)
# 

#Storing these here to avoid having to retype. 
#"Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class (Routine Non-Manual)", 
summary(m19)
#stargazer(m19, m28, m20, m29, m21, m30, type="html", omit=c(1:8), out=here("Tables", "pooled_party_vote_choice.html"),  column.labels = c('1988-2000', '2004-2021', '1988-2000', '2004-2021', '1988-2000', '2004-2021'), covariate.labels=c("Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class (Routine Non-Manual)", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates"), dep.var.labels =c("NDP" ,"Liberals", "Conservatives"),star.cutoffs = c(0.05, 0.01, 0.001), single.row=F, font.size="small", digits=2)
stargazer(m19, m28, m20, m29, m21, m30, type="html", omit=c(1:8), out=here("Tables", "pooled_party_vote_choice2.html"),  column.labels = c('1988-2000', '2004-2019', '1988-2000', '2004-2019', '1988-2000', '2004-2019'), dep.var.labels =c("NDP" ,"Liberals", "Conservatives"),star.cutoffs = c(0.05, 0.01, 0.001), single.row=F, font.size="small", digits=2)

#### Turnout
# Party Vote Shares by Degree & Income
ces %>% 
  group_by(election, income, turnout) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==1 & turnout==0) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Low Income Abstainers")

ces %>% 
  group_by(election, income, turnout) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==5 & turnout==0) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of High Income Abstainers")

ces %>% 
  group_by(election, degree, turnout) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(degree==0 & turnout==0) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Non-Degree holding Abstainers")

ces %>% 
  group_by(election, degree, turnout) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(degree==1 & turnout==0) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Degree holding Abstainers")

# Turnout models 1965-2021
ces %>%
  filter(election!=1972) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(turnout~region2+male+age+income+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Turnout', nrow(.)))->turnout_model1

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(turnout~region2+male+age+income+degree+as.factor(religion2)+redistribution, data=x)),
         tidied=map(model, tidy),
         vote=rep('Turnout', nrow(.)))->turnout_model2

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(turnout~region2+male+age+income+degree+as.factor(religion2)+redistribution+redistribution:degree, data=x)),
         tidied=map(model, tidy),
         vote=rep('Turnout', nrow(.)))->turnout_model3

stargazer(turnout_model1$model, 
          type="html", 
          out=here("Tables", "Turnout_Models_1965_2021.html"),
          column.labels=c("1965", "1968", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Turnout Models 1965-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(turnout_model2$model, 
          type="html", 
          out=here("Tables", "Turnout_Models_redistro_1988_2021.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Turnout Models 1965-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(turnout_model3$model, 
          type="html", 
          out=here("Tables", "Turnout_Models_interaction_1988_2021.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Turnout Models 1965-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

<<<<<<< HEAD
#### Straight OLS NDP 
library(broom)
ces %>% 
 nest(-election) %>% 
  mutate(degree=map(data, function(x) lm(ndp~degree, data=x))) %>% 
  mutate(income=map(data, function(x) lm(ndp~income, data=x))) %>% 
  mutate(vote=rep('ndp', nrow(.)))->ndp_models
 

  
ces %>% 
  nest(-election) %>% 
  mutate(degree=map(data, function(x) lm(liberal~degree, data=x))) %>% 
  mutate(income=map(data, function(x) lm(liberal~income, data=x))) %>% 
  mutate(vote=rep('liberal', nrow(.))) ->liberal_models


ces %>% 
  nest(-election) %>% 
  mutate(degree=map(data, function(x) lm(conservative~degree, data=x))) %>% 
  mutate(income=map(data, function(x) lm(conservative~income, data=x))) %>% 
  mutate(vote=rep('conservative', nrow(.)))->conservative_models

ndp_models %>% 
  bind_rows(., liberal_models) %>% 
  bind_rows(., conservative_models) ->combined_models
#Get Degree Coefficients for plotting

combined_models %>% 
  mutate(degree_tidied=map(degree, tidy)) %>% 
  unnest(degree_tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=election, y=estimate, group=1))+geom_point()+facet_grid(~vote)+geom_smooth(method='lm',se=F)+labs(title="OLS Coefficients of Degree on Vote, 1965-2021")
ggsave(here("Plots", "piketty_ols_coefficients_degree_vote.png"))
combined_models %>% 
  mutate(income_tidied=map(income, tidy)) %>% 
  unnest(income_tidied) %>%
  filter(term=="income") %>% 
  ggplot(., aes(x=election, y=estimate, group=1))+geom_point()+facet_grid(~vote)+geom_smooth(method='lm', se=F)+labs(title="OLS coefficients of Income on Vote, 1965-2021")
ggsave(here("Plots", "piketty_ols_coefficients_income_vote.png"))

