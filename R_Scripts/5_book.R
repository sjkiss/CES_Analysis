names(ces)
library(broom)
#set theme
theme_set(theme_minimal())
#Gender
ces %>% 
  nest(-election) %>% 
  mutate(model=map(data, ~glm(ndp~female, data=., family=binomial(link="probit"))),
         tidied=map(model, tidy)) %>% 
  unnest(tidied) %>% 
  filter(term=="female") ->gender
gender %>% 
  filter(p.value<0.05)
library(ggeffects)
gender$model %>% 
  map_df(., ggpredict, terms=c('female')) %>% 
  as.data.frame() %>% 
  mutate(Election=rep(gender$election, each=2),
         difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(., aes(x=reorder(as.factor(Election), desc(as.factor(Election))), y=difference))+geom_point()+geom_errorbar(aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error), width=0))+labs(y="Effect", title="Effect of Gender on voting for NDP, 1965-2019", caption="Difference in Probability of Voting for NDP ", x="Election")+ylim(c(-0.2, 0.2))+coord_flip()
  
ces %>% 
  group_by(election, female, vote) %>% 
  summarize(n=n()) %>% 
  filter(is.na(vote)==F) %>% 
  mutate(Percent=n/sum(n)) %>% 
  filter(vote==3) %>% 
 filter(female==1|female==0) %>% 
  ungroup() %>% 
  mutate(Difference=(Percent-lag(Percent, 1))*100) %>% 
  filter(female==1) %>% 
  ggplot(., aes(y=reorder(as.factor(election), desc(as.factor(election))), x=Difference))+geom_col()+labs(title="Gender Support for NDP", caption="This plot shows the difference in support for the NDP by men subtracted from support from women. Greater than zero shows women supporting the NDP at greater rates than men.")+labs(y="Election", x="Difference (% points)")

