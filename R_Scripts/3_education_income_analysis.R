#### OLS Degree+Income
library(broom)
library(stargazer)

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

# Attitudinal party vote models 1988-2021 (with Sector)
ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+sector, data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete5

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+sector, data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete5

ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+sector, data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete5

stargazer(ndp_models_complete5$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1988_2021_5.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete5$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1988_2021_5.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete5$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1988_2021_5.html"),
          column.labels=c("1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1988-2021", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

##### Raw Vote ####
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

#### Turnout ####
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

#### welfare models ####
# Attitudinal party vote models 1988-2021
ces88$liberal<-Recode(ces88$vote, "1=1; 0=0; 2:5=0; else=NA")
ces88$conservative<-Recode(ces88$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces88$ndp<-Recode(ces88$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces88 %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces88
ces88$region2<-factor(ces88$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces88$region2)
ces88$religion2<-Recode(as.factor(ces88$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces88$religion2)

ndp_welfare_1988<-glm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces88, family="binomial")
lib_welfare_1988<-glm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces88, family="binomial")
con_welfare_1988<-glm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces88, family="binomial")

ces93$liberal<-Recode(ces93$vote, "1=1; 0=0; 2:5=0; else=NA")
ces93$conservative<-Recode(ces93$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces93$ndp<-Recode(ces93$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces93 %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces93
ces93$region2<-factor(ces93$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces93$region2)
ces93$religion2<-Recode(as.factor(ces93$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces93$religion2)

ndp_welfare_1993<-glm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces93, family="binomial")
lib_welfare_1993<-glm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces93, family="binomial")
con_welfare_1993<-glm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces93, family="binomial")

ces97$liberal<-Recode(ces97$vote, "1=1; 0=0; 2:5=0; else=NA")
ces97$conservative<-Recode(ces97$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces97$ndp<-Recode(ces97$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces97 %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces97
ces97$region2<-factor(ces97$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces97$region2)
ces97$religion2<-Recode(as.factor(ces97$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces97$religion2)

ndp_welfare_1997<-glm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces97, family="binomial")
lib_welfare_1997<-glm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces97, family="binomial")
con_welfare_1997<-glm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces97, family="binomial")

ces00$liberal<-Recode(ces00$vote, "1=1; 0=0; 2:5=0; else=NA")
ces00$conservative<-Recode(ces00$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces00$ndp<-Recode(ces00$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces00 %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces00
ces00$region2<-factor(ces00$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces00$region2)
ces00$religion2<-Recode(as.factor(ces00$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces00$religion2)

ndp_welfare_2000<-glm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces00, family="binomial")
lib_welfare_2000<-glm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces00, family="binomial")
con_welfare_2000<-glm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces00, family="binomial")

ces04$liberal<-Recode(ces04$vote, "1=1; 0=0; 2:5=0; else=NA")
ces04$conservative<-Recode(ces04$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces04$ndp<-Recode(ces04$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces04 %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces04
ces04$region2<-factor(ces04$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces04$region2)
ces04$religion2<-Recode(as.factor(ces04$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces04$religion2)

ndp_welfare_2004<-glm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces04, family="binomial")
lib_welfare_2004<-glm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces04, family="binomial")
con_welfare_2004<-glm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces04, family="binomial")

ces06$liberal<-Recode(ces06$vote, "1=1; 0=0; 2:5=0; else=NA")
ces06$conservative<-Recode(ces06$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces06$ndp<-Recode(ces06$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces06 %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces06
ces06$region2<-factor(ces06$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces06$region2)
ces06$religion2<-Recode(as.factor(ces06$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces06$religion2)

ndp_welfare_2006<-glm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces06, family="binomial")
lib_welfare_2006<-glm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces06, family="binomial")
con_welfare_2006<-glm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces06, family="binomial")

ces08$liberal<-Recode(ces08$vote, "1=1; 0=0; 2:5=0; else=NA")
ces08$conservative<-Recode(ces08$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces08$ndp<-Recode(ces08$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces08 %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces08
ces08$region2<-factor(ces08$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces08$region2)
ces08$religion2<-Recode(as.factor(ces08$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces08$religion2)

ndp_welfare_2008<-glm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces08, family="binomial")
lib_welfare_2008<-glm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces08, family="binomial")
con_welfare_2008<-glm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces08, family="binomial")

ces11$liberal<-Recode(ces11$vote, "1=1; 0=0; 2:5=0; else=NA")
ces11$conservative<-Recode(ces11$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces11$ndp<-Recode(ces11$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces11 %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces11
ces11$region2<-factor(ces11$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces11$region2)
ces11$religion2<-Recode(as.factor(ces11$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces11$religion2)

ndp_welfare_2011<-glm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces11, family="binomial")
lib_welfare_2011<-glm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces11, family="binomial")
con_welfare_2011<-glm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces11, family="binomial")

ces15phone$liberal<-Recode(ces15phone$vote, "1=1; 0=0; 2:5=0; else=NA")
ces15phone$conservative<-Recode(ces15phone$vote, "2=1; 0:1=0; 3:5=0; else=NA")
ces15phone$ndp<-Recode(ces15phone$vote, "3=1; 0:2=0; 4:5=0; else=NA")
ces15phone %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces15phone
ces15phone$region2<-factor(ces15phone$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces15phone$region2)
ces15phone$religion2<-Recode(as.factor(ces15phone$religion), "0='None' ; 1='Catholic' ; 2='Protestant' ; 3='Other'", levels=c('None', 'Catholic', 'Protestant', 'Other'))
levels(ces15phone$religion2)

ndp_welfare_2015<-glm(ndp~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces15phone, family="binomial")
lib_welfare_2015<-glm(liberal~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces15phone, family="binomial")
con_welfare_2015<-glm(conservative~region2+male+age+income+degree+as.factor(religion2)+redistribution+market_liberalism+traditionalism2+immigration_rates+welfare, data=ces15phone, family="binomial")

stargazer(ndp_welfare_1988, ndp_welfare_1993, ndp_welfare_1997, ndp_welfare_2000, ndp_welfare_2004, ndp_welfare_2006, ndp_welfare_2008, ndp_welfare_2011, ndp_welfare_2015, type="html", out=here("Tables", "ndp_welfare_models.html"))
stargazer(lib_welfare_1988, lib_welfare_1993, lib_welfare_1997, lib_welfare_2000, lib_welfare_2004, lib_welfare_2006, lib_welfare_2008, lib_welfare_2011, lib_welfare_2015, type="html", out=here("Tables", "liberal_welfare_models.html"))
stargazer(con_welfare_1988, con_welfare_1993, con_welfare_1997, con_welfare_2000, con_welfare_2004, con_welfare_2006, con_welfare_2008, con_welfare_2011, con_welfare_2015, type="html", out=here("Tables", "conservative_welfare_models.html"))
