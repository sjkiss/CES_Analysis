#### This script is for preparing our analysis of the Piketty Hypothesis in the Canadian context
library(broom)
library(stargazer)
library(tidyverse)
#### First cut Degree and Income gap for left-right block ####

ces %>% 
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(left~region2+male+age+income+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy))->ols_block_models

ols_block_models %>% 
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income") %>% 
  mutate(Measure=Recode(term, "'degreeDegree'='Degree' ; 'income'='Income'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=Measure, group=Measure))+geom_point()+geom_line()+
  labs(x="Election", y="Estimate")
ggsave(here("Plots", "block_degree_income.png"))

#### Decompose By Party
#### Basic Party vote models 1965-2021 ####
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
#Join all parties and plot Degree coefficients
ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income") %>%
  mutate(term=Recode(term, "'degree'='Degree'; 'income'='Income'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, size=term, group=term))+
  geom_point()+facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"))+
  #scale_alpha_manual(values=c(0.4, .8))+  
  scale_size_manual(values=c(1,3))+
  geom_smooth(method="loess", size=0.5, alpha=0.2) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
  labs(title="OLS Coefficients of Degree holders and Income on Party Vote 1965-2021", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_party_income.png"))


#### Print out regression models ####
#Income as a number
stargazer(ndp_models_complete1$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1965-2021")

stargazer(liberal_models_complete1$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1965-2021")

stargazer(conservative_models_complete1$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1965-2021")


#### Show Effetc of Income with Dichotomous Variables ####
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+as_factor(rich)+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete2

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+as_factor(rich)+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete2

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+as_factor(rich)+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete2

ces %>%
  filter(election>2003) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+as_factor(rich)+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete2
#Join all parties and plot Degree coefficients

ndp_models_complete2 %>%
  bind_rows(., liberal_models_complete2) %>%
  bind_rows(., conservative_models_complete2) %>%
  unnest(tidied) %>% 
  filter(str_detect(term, "degree|rich")) %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'as_factor(rich)Rich'='Rich'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, size=term, group=term))+
  geom_point()+facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"))+
  #scale_alpha_manual(values=c(0.4, .8))+  
  scale_size_manual(values=c(1,3))+
  geom_smooth(method="loess", size=0.5, alpha=0.2) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
  labs(title="OLS Coefficients of Degree holders and being Rich on Party Vote 1965-2021", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_rich_ party.png"))

#### Print out regression models ####
stargazer(ndp_models_complete2$model, 
          type="html", 
          out=here("Tables", "NDP_Models_rich_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1965-2021")

stargazer(liberal_models_complete2$model, 
          type="html", 
          out=here("Tables", "liberal_Models_rich_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1965-2021")

stargazer(conservative_models_complete2$model, 
          type="html", 
          out=here("Tables", "conservative_Models_rich_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1965-2021")
#### Show Effetc of Income with Dichotomous Variables (Poor) ####
ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+as_factor(poor)+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete3

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+as_factor(poor)+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete3

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+as_factor(poor)+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete3

ces %>%
  filter(election>2003) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+as_factor(poor)+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete3
#Join all parties and plot Degree coefficients

ndp_models_complete3 %>%
  bind_rows(., liberal_models_complete3) %>%
  bind_rows(., conservative_models_complete3) %>%
  unnest(tidied) %>% 
  filter(str_detect(term, "degree|poor")) %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'as_factor(poor)Poor'='Poor'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, size=term, group=term))+
  geom_point()+facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"))+
  #scale_alpha_manual(values=c(0.4, .8))+  
  scale_size_manual(values=c(1,3))+
  geom_smooth(method="loess", size=0.5, alpha=0.2) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
  labs(title="OLS Coefficients of Degree holders and being Poor on Party Vote 1965-2021", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_poor_ party.png"))

#### Print out regression models ####
stargazer(ndp_models_complete3$model, 
          type="html", 
          out=here("Tables", "NDP_Models_poor_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1965-2021")

stargazer(liberal_models_complete3$model, 
          type="html", 
          out=here("Tables", "liberal_Models_poor_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1965-2021")

stargazer(conservative_models_complete3$model, 
          type="html", 
          out=here("Tables", "conservative_Models_poor_1965_2021.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1965-2021")
#### Add In Redistribution ####

ces %>% 
  nest(variables=-canada$period) %>% 
  mutate(ndp=map(variables, function(x) lm(ndp~degree+income+male+age+region+traditionalism+redistribution+degree:redistribution, data=x)),
                 liberal=map(variables, function(x) lm(liberal~degree+income+male+age+region+traditionalism+redistribution+degree:redistribution, data=x)),
                             conservative=map(variables, function(x) lm(conservative~degree+income+male+age+region+traditionalism+redistribution+degree:redistribution, data=x)))->redistribution_models
#### CMP ####

ces %>% 
  group_by(election, vote) %>% 
  filter(election>1992) %>% 
  summarize(average=mean(economic, na.rm=T)) %>% 
  filter(vote>0 & vote<5) %>% 
  ggplot(., aes(x=election, y=average, col=as_factor(vote), group=as_factor(vote)))+
  geom_line()+geom_point()+scale_color_manual(values=c('darkred', "darkblue", "orange", "cyan"))


#Download the data
cmp<-read.csv(file="https://manifesto-project.wzb.eu/down/data/2021a/datasets/MPDataset_MPDS2021a.csv")
#Get Canada
cmp %>% 
  filter(countryname=="Canada")->canada
#Define Dimension issues

canada$economic_dimension<-((log(canada$per401+.5))+(log(canada$per402+0.5))+(log(canada$per407+0.5))+(log(canada$per410+0.5))+(log(canada$per414+0.5))+(log(canada$per505+0.5))+(log(canada$per507+0.5))+(log(canada$per702+0.5)))-((log(canada$per403+0.5))+(log(canada$per404+0.5))+(log(canada$per405+0.5))+(log(canada$per406+0.5))+(log(canada$per409+0.5))+(log(canada$per412+0.5))+(log(canada$per413+0.5))+(log(canada$per415+0.5))+(log(canada$per503+0.5))+(log(canada$per504+0.5))+(log(canada$per506+0.5))+(log(canada$per701+0.5)))
canada$social_dimension<-((log(canada$per305+0.5))+(log(canada$per601+0.5))+(log(canada$per603+0.5))+(log(canada$per605+0.5))+(log(canada$per606+0.5))+(log(canada$per608+0.5)))-((log(canada$per201+0.5))+(log(canada$per202+0.5))+(log(canada$per416+0.5))+(log(canada$per501+0.5))+(log(canada$per502+0.5))+(log(canada$per602+0.5))+(log(canada$per604+0.5))+(log(canada$per607+0.5))+(log(canada$per705+0.5))+(log(canada$per706+0.5)))
canada %>% 
  filter(date>"1989-01-01" & partyname="New Democratic Party" & partyname="Liberal Party of Canada" & partyname="Conservative Party of Canada" & partyname="Progressive Conservative Party") %>% 
  pivot_longer(cols=ends_with('_dimension'), names_to=c("Dimension"), values_to=c("Score")) %>% 
  ggplot(., aes(x=date, y=Score, col=partyabbrev))+geom_point()+geom_line()+facet_wrap(~Dimension)
