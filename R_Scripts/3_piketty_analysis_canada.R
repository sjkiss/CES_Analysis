#### This script is for preparing our analysis of the Piketty Hypothesis in the Canadian context
library(broom)
library(stargazer)

#### First cut Degree and Income gap for left-right block

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
  labs(title="OLS Coefficients of Degree holders on Party Vote 1965-2021", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_party_income.png"))

#### Show Effetc of Income with Dichotomous Variables
ces %>%
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

