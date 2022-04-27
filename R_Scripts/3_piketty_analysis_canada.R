#### This script is for preparing our analysis of the Piketty Hypothesis in the Canadian context


#### First cut Degree and Income gap for left-right block

ces %>% 
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(left~region2+male+age+income+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy))->ols_block_models

ols_block_models %>% 
  unnest(tidied) %>% 
  filter(term=="degreeDegree"|term=="income") %>% 
  mutate(Measure=Recode(term, "'degreeDegree'='Degree' ; 'income'='Income'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=Measure, group=Measure))+geom_point()+geom_line()+
  labs(x="Election", y="Estimate")

#### Decompose By Party


