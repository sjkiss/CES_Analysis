
library(broom)
library(stargazer)
library(tidyverse)

table(ces$turnout, ces$election)
table(ces$satdem, ces$election)

# Turnout percentages
ces %>% 
  select(election, income, turnout) %>% 
  group_by(election, income, turnout) %>% 
  summarize(n=n()) %>% 
  filter(income==1 & turnout==3) %>% 
  mutate(pct=n/sum(n))

ces %>% 
  filter(election<1970 & election>1975) %>%
  group_by(election, income, turnout) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==1 & turnout==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Turnout percent of Low Income")

# Plot coefficients
ces %>% 
  group_by(election) %>% 
  filter(election>1990) %>% 
  nest() %>% 
  mutate(mods=map(data, function(x) glm(turnout~satdem, data=x, family="binomial")), 
         tidied=map(mods, tidy)) -> satdem

satdem %>% 
  unnest(tidied) %>% 
  filter(term=="satdem") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of Satisfaction with Democracy on Turnout")+geom_smooth(method="loess", se=F)

# Models
ces %>%
  filter(election>1990) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(turnout~region2+male+age+income+degree+as.factor(religion2)+union_both+satdem, data=x)),
         tidied=map(model, tidy),
         vote=rep('Turnout', nrow(.)))->satdem_models_1

ces %>%
  filter(election>1990) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(turnout~region2+male+age+income+degree+as.factor(religion2)+union_both+satdem+income:satdem, data=x)),
         tidied=map(model, tidy),
         vote=rep('Turnout', nrow(.)))->satdem_models_2

stargazer(satdem_models_1$model, 
          type="html", 
          out=here("Tables", "Turnout_Models_satdem_1993_2021.1.html"),
          column.labels=c("1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Turnout 1993-2021")

stargazer(satdem_models_2$model, 
          type="html", 
          out=here("Tables", "Turnout_Models_satdem_1993_2021.2.html"),
          column.labels=c("1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019", "2021"), 
          star.cutoffs=c(0.05), 
          title="Turnout 1993-2021")

# Plot models
satdem_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="satdem") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="OLS Coefficients of Satisfaction with Democracy on Turnout")+geom_smooth(method="loess", se=F)

satdem_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="satdem"|term=="income") %>% 
  filter(election>1990)  %>% 
  mutate(Measure=Recode(term, "'satdem'='Satisfaction with Democracy' ; 'income'='Income'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=Measure, group=Measure))+
  geom_point()+
  geom_line()+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error), width=0))+
  #geom_smooth(se=F, method="lm")+
  labs(x="Election", y="Estimate")+
  scale_color_grey()+
  geom_hline(yintercept=0, linetype=2)
# geom_errorbar(width=0,aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))
ggsave(here("Plots", "satdem_income_coefficients.png"), width=8, height=6)

satdem_models_2 %>% 
  unnest(tidied) %>% 
  filter(term=="satdem:income") %>% 
  filter(election>1990)  %>% 
  mutate(Measure=Recode(term, "'satdem:income'='satdem income interaction'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=Measure, group=Measure))+
  geom_point()+
  geom_line()+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error), width=0))+
  #geom_smooth(se=F, method="lm")+
  labs(x="Election", y="Estimate")+
  scale_color_grey()+
  geom_hline(yintercept=0, linetype=2)
# geom_errorbar(width=0,aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))
ggsave(here("Plots", "satdem_income_interaction_coefficients.png"), width=8, height=6)