#### This script is for preparing our analysis of the Piketty Hypothesis in the Canadian context
library(broom)
library(stargazer)
library(tidyverse)
#### First cut Degree and Income gap for left-right block ####
ces %>% 
  filter(election<1993) %>% 
  select(election, left, region, male, age, income, degree, religion2) %>% 
  group_by(election) %>% 
  summary()
ces %>% 
  filter(election<2021) %>% 
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(left~region2+male+age+income+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy))->ols_block_models

ces %>% 
 filter(election<2021) %>% 
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(left~region2+male+age+income_tertile+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy))->ols_block_models2

# ces %>% 
#   filter(election>1979&election<2021) %>% 
#   nest(variables=-election) %>%
#   mutate(model=map(variables, function(x) lm(left~region2+male+age+income_tertile+degree+as.factor(religion2), data=x)),
#          tidied=map(model, tidy))->ols_block_models3
# ces %>% 
#   filter(election>1979&election<2021) %>% 
#   nest(variables=-election) %>%
#   mutate(model=map(variables, function(x) lm(left~region2+male+age+income_tertile+degree+as.factor(religion2), data=x)),
#          tidied=map(model, tidy))->ols_block_models4
ols_block_models2 %>% 
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income_tertile") %>% 
  filter(election<2020)  %>% 
  mutate(Measure=Recode(term, "'degree'='Degree' ; 'income_tertile'='Income'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=Measure, group=Measure))+
  geom_point()+
  geom_line()+
  #geom_smooth(se=F)+
  labs(x="Election", y="Estimate")+
  scale_color_grey()+
  geom_hline(yintercept=0, linetype=2)+
  geom_errorbar(width=0,aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))
ggsave(here("Plots", "block_degree_income2.png"), width=8, height=6)

# ols_block_models3 %>% 
#   unnest(tidied) %>% 
#   filter(term=="degree"|term=="income3") %>% 
#   filter(election<2020)  %>% 
#   mutate(Measure=Recode(term, "'degree'='Degree' ; 'income3'='Income'")) %>% 
#   ggplot(., aes(x=election, y=estimate, col=Measure, group=Measure))+
#   geom_point()+
#   geom_line()+
#   #geom_smooth(se=F)+
#   labs(x="Election", y="Estimate")+
#   scale_color_grey()+
#   geom_hline(yintercept=0, linetype=2)+labs(title="Income Categories 1:2, 3 and 4:5")
# # geom_errorbar(width=0,aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))
# ggsave(here("Plots", "block_degree_income3.png"), width=8, height=6)

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
  filter(election<2021) %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'income'='Income'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, size=term, group=term))+
  geom_point()+facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"), name="Vote")+
  #scale_alpha_manual(values=c(0.4, .8))+  
  scale_size_manual(values=c(1,3), name="Coefficient")+
  geom_smooth(method="loess", size=0.5, alpha=0.2, se=F) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
 labs( alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.12,0.12))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5, linetype=2)+
  theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_party_income.png"), width=8, height=4)

#### Decompose By Party
#### Basic Party vote models 1965-2021 ####
#Income2
ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income2+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete2

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income2+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete2

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income2+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete2

ces %>%
  filter(election>2003) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+income2+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete2
#Join all parties and plot Degree coefficients
ndp_models_complete2 %>%
  bind_rows(., liberal_models_complete2) %>%
  bind_rows(., conservative_models_complete2) %>%
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income2") %>%
  filter(election<2021) %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'income2'='Income'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, size=term, group=term))+
  geom_point()+facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"), name="Vote")+
  #scale_alpha_manual(values=c(0.4, .8))+  
  scale_size_manual(values=c(1,3), name="Coefficient")+
  geom_smooth(method="loess", size=0.5, alpha=0.2, se=F) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
  labs( alpha="Variable", color="Vote", x="Election", y="Estimate", title="Income2, 1, 2:4, 5")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.12,0.12))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5, linetype=2)+
  theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_party_income2.png"), width=8, height=4)

#Income3
ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income3+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete3

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income3+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete3

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income3+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete3

ces %>%
  filter(election>2003) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+income3+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete3
#Join all parties and plot Degree coefficients
ndp_models_complete3 %>%
  bind_rows(., liberal_models_complete3) %>%
  bind_rows(., conservative_models_complete3) %>%
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income3") %>%
  filter(election<2021) %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'income3'='Income'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, size=term, group=term))+
  geom_point()+facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"), name="Vote")+
  #scale_alpha_manual(values=c(0.4, .8))+  
  scale_size_manual(values=c(1,3), name="Coefficient")+
  geom_smooth(method="loess", size=0.5, alpha=0.2, se=F) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
  labs( alpha="Variable", color="Vote", x="Election", y="Estimate", title="Income3, 1:2, 3, 4:5")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.12,0.12))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5, linetype=2)+
  theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_party_income3.png"), width=8, height=4)



