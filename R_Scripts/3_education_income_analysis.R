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

