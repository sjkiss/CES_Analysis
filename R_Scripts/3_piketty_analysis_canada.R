#### This script is for preparing our analysis of the Piketty Hypothesis in the Canadian context
library(broom)
library(stargazer)
library(tidyverse)
#### Voting Shares
ces %>% 
  select(election, working_class, vote2) %>% 
  group_by(election, working_class, vote2) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=n/sum(n))

ces %>% 
  select(election, working_class, vote2) %>% 
  group_by(election,  vote2, working_class) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=n/sum(n))

#### First cut Degree and Income gap for left-right block ####

ces %>% 
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(left~region2+male+age+income2+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy))->ols_block_models

ols_block_models %>% 
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income2") %>% 
  filter(election<2020)  %>% 
  mutate(Measure=Recode(term, "'degree'='Degree' ; 'income2'='Income'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=Measure, group=Measure))+
  geom_point()+
  geom_line()+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error), width=0))+
  #geom_smooth(se=F, method="lm")+
  labs(x="Election", y="Estimate")+
  scale_color_grey()+
  geom_hline(yintercept=0, linetype=2)
 # geom_errorbar(width=0,aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))
ggsave(here("Plots", "block_degree_income.png"), width=8, height=6)

#### Decompose By Party
#### Basic Party vote models 1965-2021 ####
ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income2+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete1

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income2+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete1

ces %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income2+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete1

ces %>%
  filter(election>2003) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+income2+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete1
#Join all parties and plot Degree coefficients
ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
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
 labs( alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.12,0.12))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5, linetype=2)+
  theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_party_income.png"), width=8, height=4)


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
  filter(election<2020) %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'as_factor(rich)Rich'='Rich'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, size=term, group=term))+
  geom_point()+facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"))+
  #scale_alpha_manual(values=c(0.4, .8))+  
  scale_size_manual(values=c(1,3), name="Variable")+
  geom_smooth(method="loess", size=0.5, alpha=0.2, se=F) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
 labs(title="", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.18,0.18))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5)+
  theme(axis.text.x=element_text(angle=90))
  #geom_errorbar(size=1,width=0,aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))
ggsave(here("Plots", "ols_degree_rich_party.png"), width=8, height=4)

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
#### Add In redistribution_reversed ####
ces$ROC<-(ces$quebec-1)*-1
val_labels(ces$ROC)<-c(Quebec=0, ROC=1)

library(nnet)
ces %>% 
  filter(vote2!="Green") %>%
  filter(election>1988) %>% 
  filter(ROC==1) %>% 
 # select(vote2, degree, male, age, income) %>% 
  as_factor() %>% 
  nest(-election) %>% 
mutate(model=map(data, function(x) multinom(vote2~degree+male+age+income+redistribution+traditionalism+degree*redistribution, data=x)),
       tidied=map(model, tidy, conf.int=T))->degree_redistribution_models
degree_redistribution_models
library(ggeffects)
degree_redistribution_models$model %>% 
  map(., ggeffect, terms=c('redistribution [0.33,0.66]','degree')) %>% 
  bind_rows(., .id="Election") %>% 
  data.frame() %>% 
 # filter(Election==10) %>% 
  mutate(Election=Recode(Election, "1=1993; 2=1997; 3=2000; 4=2004; 5=2006; 6=2008;7=2011; 8=2015; 9=2019;10=2021 ")) %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+
  geom_line()+
  facet_grid(response.level~Election)+ylim(c(0,1))+labs(title="Predicted Probabilities of Vote Choice Degree (Not Degree)\nBased on Redistribution (ROC)")
ggsave(filename="Plots/predicted_probabilities_redistribution_degree.png",dpi=150,width=8, height=3)
# degree_redistribution_models %>% 
#   unnest(tidied) %>% 
#   filter(term=="degree:redistribution") %>% 
#   ggplot(., aes(x=election, y=estimate, col=y.level))+
#   geom_point()+
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0)+
#   scale_color_manual(values=c('darkred', 'orange'))+geom_hline(yintercept=0, linetype=2)+
#   labs(title="Multinomial coefficients of Degree and Redistribution Interaction\nRight Vote Reference Category")
# ggsave(filename="Plots/multinomial_coefficients_degree_redistribution.png")

ces %>% 
  filter(vote2!="Green") %>%
  filter(election>1988) %>% 
  filter(ROC==1) %>% 
  as_factor() %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) 
    multinom(vote2~degree+male+age+poor+redistribution+traditionalism+poor*redistribution, 
             data=x)),
         tidied=map(model, tidy, conf.int=T))->poor_redistribution_models

poor_redistribution_models$model %>% 
  map(., ggeffect, 
      terms=c('redistribution [0.33,0.66]','poor')) %>% 
  bind_rows(., .id="Election") %>% 
  data.frame() %>% 
  # filter(Election==10) %>% 
  mutate(Election=Recode(Election, "1=1993; 2=1997; 3=2000; 4=2004; 5=2006; 6=2008;7=2011; 8=2015; 9=2019;10=2021 ")) %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+
  geom_line()+
  facet_grid(response.level~Election)+
  ylim(c(0,1))+labs(title="Predicted Probabilities of Vote Choice Poor (Not Poor)\nBased on Redistribution (ROC)")
ggsave(filename="Plots/predicted_probabilities_redistribution_poor.png",dpi=150,width=8, height=3)



# poor_redistribution_models %>% 
#   unnest(tidied) %>% 
#   filter(term=="poor:redistribution") %>% 
#   ggplot(., aes(x=election, y=estimate, col=y.level))+
#   geom_point()+
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0)+
#   scale_color_manual(values=c('darkred', 'orange'))+geom_hline(yintercept=0, linetype=2)+
# labs(title="Multinomial coefficients of Lowest Quintile and Redistribution Interaction\nRight Vote Reference Category")

#ggsave(filename="Plots/multinomial_coefficients_poor_redistribution.png")


#Traditionalism 
ces %>% 
  as_factor() %>% 
  filter(vote2!="Green") %>%
  filter(election>1988) %>% 
  filter(ROC=="ROC") %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~degree+male+age+income+redistribution+traditionalism+degree*traditionalism, data=x)),
         tidied=map(model, tidy, conf.int=T))->degree_traditionalism_models

degree_traditionalism_models$model %>% 
  map(., ggeffect, 
      terms=c('traditionalism [0.33,0.66]','degree')) %>% 
  bind_rows(., .id="Election") %>% 
  data.frame() %>% 
  # filter(Election==10) %>% 
  mutate(Election=Recode(Election, "1=1993; 2=1997; 3=2000; 4=2004; 5=2006; 6=2008;7=2011; 8=2015; 9=2019;10=2021 ")) %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+
  geom_line()+
  facet_grid(response.level~Election)+
  ylim(c(0,1))+labs(title="Predicted Probabilities of Vote Choice Degree (No Degree))\nBased on Moral Traditionalism (ROC)")

ggsave(filename="Plots/predicted_probabilities_traditionalism_degree.png",dpi=150,width=8, height=3)

# degree_traditionalism_models %>% 
#   unnest(tidied) %>% 
#   filter(str_detect(term, "degree")) %>% 
#   filter(term=="degreeDegree:traditionalism") %>% 
#   ggplot(., aes(x=election, y=estimate, col=y.level))+
#   geom_point()+
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0)+
#   scale_color_manual(values=c('darkred', 'orange'))+geom_hline(yintercept=0, linetype=2)+
#   labs(title="Multinomial coefficients of Degree and traditionalism Interaction\nRight Vote Reference Category")
# 
# ggsave(filename="Plots/Multinomial_coefficients_traditionalism_degree.png")

#Traditionalism 
ces %>% 
  as_factor() %>% 
  filter(vote2!="Green") %>%
  filter(election>1988) %>% 
  filter(ROC=="ROC") %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~degree+male+age+poor+redistribution+traditionalism+poor*traditionalism, data=x)),
         tidied=map(model, tidy, conf.int=T))->poor_traditionalism_models

# poor_traditionalism_models %>% 
#   unnest(tidied) %>% 
#   filter(str_detect(term, "poor")) %>% 
#   filter(term=="poorPoor:traditionalism") %>% 
#   ggplot(., aes(x=election, y=estimate, col=y.level))+
#   geom_point()+
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0)
#   scale_color_manual(values=c('darkred', 'orange'))+geom_hline(yintercept=0, linetype=2)+
# labs(title="Multinomial coefficients of Lowest Quintile and Traditionalism Interaction\nRight Vote Reference Category")
poor_traditionalism_models$model %>% 
  map(., ggeffect, 
      terms=c('traditionalism [0.33,0.66]','poor')) %>% 
  bind_rows(., .id="Election") %>% 
  data.frame() %>% 
  # filter(Election==10) %>% 
  mutate(Election=Recode(Election, "1=1993; 2=1997; 3=2000; 4=2004; 5=2006; 6=2008;7=2011; 8=2015; 9=2019;10=2021 ")) %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+
  geom_line()+
  facet_grid(response.level~Election)+
  ylim(c(0,1))+labs(title="Predicted Probabilities of Vote Choice Poor (Not Poor)\nBased on Moral Traditionalism (ROC)")
ggsave(filename="Plots/predicted_probabilities_traditionalism_poor.png",dpi=150,width=8, height=3)

#ggsave(filename="Plots/Multinomial_coefficients_traditionalism_poor.png")

#Immigration 
ces %>% 
  as_factor() %>% 
  filter(vote2!="Green") %>%
  filter(election>1988) %>% 
  filter(ROC=="ROC") %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~degree+male+age+income+redistribution+immigration_rates+degree*immigration_rates, data=x)),
         tidied=map(model, tidy, conf.int=T))->degree_immigration_models

degree_immigration_models$model %>% 
  map(., ggeffect, 
      terms=c('immigration_rates [0.33,0.66]','degree')) %>% 
  bind_rows(., .id="Election") %>% 
  data.frame() %>% 
  # filter(Election==10) %>% 
  mutate(Election=Recode(Election, "1=1993; 2=1997; 3=2000; 4=2004; 5=2006; 6=2008;7=2011; 8=2015; 9=2019;10=2021 ")) %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+
  geom_line()+
  facet_grid(response.level~Election)+
  ylim(c(0,1))+labs(title="Predicted Probabilities of Vote Choice Degree (No Degree))\nBased on Immigration (ROC)")

ggsave(filename="Plots/predicted_probabilities_immigration_degree.png",dpi=150,width=8, height=3)

#Immgration
ces %>% 
  as_factor() %>% 
  filter(vote2!="Green") %>%
  filter(election>1988) %>% 
  filter(ROC=="ROC") %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~degree+male+age+poor+redistribution+immigration_rates+poor*immigration_rates, data=x)),
         tidied=map(model, tidy, conf.int=T))->poor_immigration_models

poor_immigration_models$model %>% 
  map(., ggeffect, 
      terms=c('immigration_rates [0.33,0.66]','poor')) %>% 
  bind_rows(., .id="Election") %>% 
  data.frame() %>% 
  # filter(Election==10) %>% 
  mutate(Election=Recode(Election, "1=1993; 2=1997; 3=2000; 4=2004; 5=2006; 6=2008;7=2011; 8=2015; 9=2019;10=2021 ")) %>% 
  ggplot(., aes(x=x, y=predicted, col=group))+
  geom_line()+
  facet_grid(response.level~Election)+
  ylim(c(0,1))+labs(title="Predicted Probabilities of Vote Choice Poor (Not Poor)\nBased on Immigration (ROC)")
ggsave(filename="Plots/predicted_probabilities_immigration_poor.png",dpi=150,width=8, height=3)

#### Find a way to show effects plot 
# 
#### Average Scores For Degree Versus Average ####
  
  ces %>% 
    select(election, degree, redistribution_reversed, immigration_rates, market_liberalism, traditionalism2) %>%
    rename(Redistribution=redistribution_reversed, `Immigration Rates`=immigration_rates, `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2) %>% 
    #  mutate(Redistribution=skpersonal::revScale(Redistribution, reverse=T)) %>% 
    pivot_longer(cols=3:6) %>% 
    pivot_longer(cols=2, names_to="Variable", values_to="Group") %>% 
    filter(election>1988&election<2020) %>% 
    group_by(election, Variable, Group, name) %>% 
    summarize(average=mean(value, na.rm=T), n=n(), sd=sd(value, na.rm=T), se=sd/sqrt(n)) %>% 
    arrange(election, Variable, name, Group) %>% 
    filter(!is.na(Group)) %>% 
    group_by(election, name) %>% 
    mutate(Variable=recode_factor(Variable, "degree"="Degree")) %>% 
    mutate(Group=case_when(
      Variable=="Degree" & Group==0 ~ "No Degree",
      Variable=="Degree" & Group==1 ~ "Degree",
    )) %>%
    #  filter(Group=="No Degree") %>% 
    ggplot(., aes(y=election, x=average, group=Variable, col=`Group`))+geom_point()+
    facet_wrap(~fct_relevel(name, "Immigration Rates","Moral Traditionalism", "Market Liberalism", "Redistribution"), nrow=2)+
    theme(axis.text.x=element_text(angle=90))+scale_y_discrete(limits=rev)+
    scale_color_manual(values=rep(c('black', 'grey'),2))+
    geom_vline(xintercept=0.5, linetype=2)+labs(y="Election", x="Average")+labs(col="Degree Status")+
    geom_errorbar(width=0,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)))
  ggsave(filename=here("Plots", "mean_attitudinal_preferences_education.png"), width=8, height=8)
  
  #### Average Scores For Income ####
  
  ces %>% 
    select(election, income, redistribution_reversed, immigration_rates, market_liberalism, traditionalism2) %>%
    rename(Redistribution=redistribution_reversed, `Immigration Rates`=immigration_rates, `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2) %>% 
    #  mutate(Redistribution=skpersonal::revScale(Redistribution, reverse=T)) %>% 
    pivot_longer(cols=3:6) %>% 
    pivot_longer(cols=2, names_to="Variable", values_to="Group") %>% 
    filter(election>1988&election<2021) %>% 
    group_by(election, Variable, Group, name) %>% 
    summarize(average=mean(value, na.rm=T), n=n(), sd=sd(value, na.rm=T), se=sd/sqrt(n)) %>% 
    arrange(election, Variable, name, Group) %>%
    filter(!is.na(Group)) %>% 
    group_by(election, name) %>% 
    rename(Income=Group) %>% 
    # mutate(Group=case_when(
    #   Variable=="Income" & Group==1 ~ "Low Income",
    #   Variable=="Income" & Group==5 ~ "High Income",
    # )) %>%
    #filter(Variable=="Income") %>%
        filter(Income==1|Income==5) %>% 
    ggplot(., aes(y=election, x=average,  col=as_factor(Income)))+geom_point()+
      facet_wrap(~fct_relevel(name, "Immigration Rates","Moral Traditionalism", "Market Liberalism", "Redistribution"), nrow=2)+theme(axis.text.x=element_text(angle=90))+scale_y_discrete(limits=rev)+scale_color_manual(values=rep(c('grey', 'black'),2))+
    geom_vline(xintercept=0.5, linetype=2)+labs(y="Election", x="Average")+
    geom_errorbar(width=0,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)))+
    labs(col="Income Quintile")
  ggsave(filename=here("Plots", "mean_attitudinal_preferences_income.png"), width=8, height=4)
 
  
ces %>% 
  select(degree, redistribution, vote2, election) %>% 
  filter(election>1988) %>% 
  as_factor() %>% 
  #mutate(Income=fct_relevel(income2, "Lowest", "Middle", "Highest")) %>% 
  rename(Degree=degree, Vote=vote2, Redistribution=redistribution, Election=election) %>% 
  #pivot_longer(., cols=c("Degree", "Income"), names_to=c("Variable"), values_to=c("Value")) %>% 
  group_by(Election, Vote, Degree) %>% 
  filter(!is.na(Vote)&Vote!="Green"& Vote!="BQ") %>% 
  #filter(!is.na(Value)) %>% 
  filter(!is.na(Degree)) %>% 
  filter(Election<2021) %>% 
  summarize(avg=mean(Redistribution, na.rm=T), n=n(), sd=sd(Redistribution, na.rm=T), se=sd/sqrt(n)) %>% 
  arrange(Election, Degree, Vote) %>% 
  ggplot(. ,aes(x=avg, y=fct_reorder(Election, desc(Election)), col=Degree))+geom_point()+facet_grid(~Vote)+
  scale_color_grey(start=0.8, end=0.2)+
  geom_errorbar(aes(xmin=avg-(1.96*se), xmax=avg+(1.96*se), width=0))+geom_vline(xintercept=0.5, linetype=2)+
  labs(y="Year") 
ggsave(filename=here("Plots", "means_degree_redistribution_party.png"), width=8, height=4)

ces %>% 
  select(income2, redistribution, vote2, election) %>% 
  filter(election>1988&election<2021) %>% 
  as_factor() %>% 
  rename(Income=income2, Vote=vote2, Redistribution=redistribution, Election=election) %>% 
  #pivot_longer(., cols=c("Degree", "Income"), names_to=c("Variable"), values_to=c("Value")) %>% 
  group_by(Election, Vote,Income) %>% 
  filter(!is.na(Vote)&Vote!="Green"& Vote!="BQ") %>% 
  #filter(!is.na(Value)) %>% 
  filter(!is.na(Income)) %>% 
  summarize(avg=mean(Redistribution, na.rm=T), n=n(), sd=sd(Redistribution, na.rm=T), se=sd/sqrt(n)) %>% 
  ggplot(. ,aes(x=avg, y=fct_reorder(Election, desc(Election)), col=Income))+geom_point()+facet_grid(~Vote)+
  scale_color_grey(start=0.8, end=0.2)+
  geom_errorbar(aes(xmin=avg-(1.96*se), xmax=avg+(1.96*se), width=0))+geom_vline(xintercept=0.5, linetype=2)+
  labs(y="Year")
ggsave(filename=here("Plots", "means_redistribution_income_party.png"), width=8, height=4)

#### Variance of Opinion inside each party ####
  
  
#### CMP ####
  #Download the data
  cmp<-read_sav(file="https://manifesto-project.wzb.eu/down/data/2021a/datasets/MPDataset_MPDS2021a.sav")
  names(cmp)
  
  #Get Canada
  
  cmp %>% 
    filter(countryname=="Canada")->canada

    #Define Dimension issues
  economic_volume<-c("per401","per402","per407","per410","per414","per505","per507","per702","per403","per404","per405","per406","per409","per412","per413","per415","per503","per504","per506","per701")
  social_volume<-c("per305","per601","per603","per605","per606","per608","per201","per202","per416","per501","per502","per602","per604","per607","per705","per706")
  canada$economic_position<-((log(canada$per401+.5))+(log(canada$per402+0.5))+(log(canada$per407+0.5))+(log(canada$per410+0.5))+(log(canada$per414+0.5))+(log(canada$per505+0.5))+(log(canada$per507+0.5))+(log(canada$per702+0.5)))-((log(canada$per403+0.5))+(log(canada$per404+0.5))+(log(canada$per405+0.5))+(log(canada$per406+0.5))+(log(canada$per409+0.5))+(log(canada$per412+0.5))+(log(canada$per413+0.5))+(log(canada$per415+0.5))+(log(canada$per503+0.5))+(log(canada$per504+0.5))+(log(canada$per506+0.5))+(log(canada$per701+0.5)))
  canada$social_position<-((log(canada$per305+0.5))+(log(canada$per601+0.5))+(log(canada$per603+0.5))+(log(canada$per605+0.5))+(log(canada$per606+0.5))+(log(canada$per608+0.5)))-((log(canada$per201+0.5))+(log(canada$per202+0.5))+(log(canada$per416+0.5))+(log(canada$per501+0.5))+(log(canada$per502+0.5))+(log(canada$per602+0.5))+(log(canada$per604+0.5))+(log(canada$per607+0.5))+(log(canada$per705+0.5))+(log(canada$per706+0.5)))
  canada %>% 
   # select(economic_volume) %>% 
    rowwise() %>% 
    mutate(economic_volume=sum(c_across(all_of(economic_volume))), 
          social_volume=sum(c_across(all_of(social_volume)))) ->canada

  #Make table of first and second dimension issues
  library(lubridate)
  canada %>% 
    # select(edate,partyname, second_dimension, first_dimension) %>% 
    #modify party names for categorization
    mutate(Party=case_when(
      str_detect(partyname, "Cooperative Commonwealth Federation")~'CCF-NDP',
      str_detect(partyname, "Democratic")~'CCF-NDP',
      str_detect(partyname, "Progressive Conservative")~'Conservative',
      str_detect(partyname, "Reform Party of Canada")~'Conservative',
      str_detect(partyname, "Canadian Reform Canadian Alliance")~'Conservative',
      str_detect(partyname, "Conservative")~'Conservative',
      str_detect(partyname, "Liberal")~'Liberal',
      str_detect(partyname, "Social Credit")~'Social Credit',
      str_detect(partyname, "Bloc")~'Bloc',
      str_detect(partyname, "Green")~'Green',
    ), 
    #modify date
    Date=ymd(edate),
    #Create ratio
  Ratio=economic_volume/social_volume)->canada
canada$Party<-factor(canada$Party, levels=c("Liberal", "Conservative","CCF-NDP", "Bloc","Green", "Social Credit"))
#make color plot
  canada %>% 
    #pivot_longer(., cols=c("first_dimension", "second_dimension")) %>% 
    ggplot(., aes(x=Date, y=Ratio, col=Party))+
   # geom_line()+
    geom_point()+
    theme_minimal()+geom_hline(yintercept=1, linetype=2)+
    geom_smooth(se=F,  method="loess")+
    scale_color_manual(values=c("darkred","blue","orange", "cyan",  "darkgreen",  "black"))+
    scale_x_date(breaks=seq.Date(from=as.Date("1945-01-01"), to=as.Date("2015-12-31"), by="10 years"), date_labels="%Y")
  
  ggsave(filename="Plots/economic_social_volume_color.png", width=8, height=6)
#Make bw plot
  canada %>% 
    #pivot_longer(., cols=c("first_dimension", "second_dimension")) %>% 
    ggplot(., aes(x=Date, y=Ratio, col=Party, shape=Party, linetype=Party, size=Party))+
#    geom_line()+
    geom_point()+
    theme_minimal()+
    geom_hline(yintercept=1, linetype=2)+
     geom_smooth(se=F,  method="loess" )+
    #scale_color_manual(values=c("cyan", "orange", "blue", "darkgreen", "darkred", "black"))+
    scale_linetype_manual(values=c(1,2,3,4,5,6))+
    scale_color_grey()+
     scale_x_date(breaks=seq.Date(from=as.Date("1945-01-01"), to=as.Date("2015-12-31"), by="10 years"), date_labels="%Y")+
    scale_size_manual(values=c(1,1,1,0.5,0.5,0.5))
  ggsave(filename="Plots/economic_social_volume_bw.png", width=8, height=6)
  

  # This section does voter policy preferences 
  # Color
ces %>% 
  pivot_longer(cols=c(economic, social), names_to=c("Dimension"), values_to=c("Score")) %>% 
  group_by(election, Dimension,vote) %>% 
  filter(election>1992 & election<2021) %>% 
  summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sd/sqrt(n)) %>% 
  filter(vote>0 & vote<5) %>% 
  mutate(Dimension=str_to_title(Dimension)) %>% 
  mutate(Election=as.Date(election, format="%Y")) %>% 
  ggplot(., aes(x=Election, 
                y=Average, 
                col=as_factor(vote),
                # linetype=as_factor(vote),
                # shape=as_factor(vote), 
                 group=as_factor(vote)))+
  geom_point()+facet_wrap(~Dimension)+
    #geom_smooth(se=F)+
  geom_line()+
    geom_errorbar(width=0, aes(ymin=Average-(1.96*se), ymax=Average+(1.96*se)))+ 
  scale_color_manual(values=c('darkred', "darkblue", "orange", "cyan"), name='Vote')+
  scale_x_date(limits=c(as.Date("1990-01-01"), as.Date("2019-12-31")) )+
      #scale_shape_discrete(name="Vote")+
    #scale_linetype_discrete(name="Vote")+
   # scale_color_grey(name="Vote")+
  theme(legend.position = "bottom")+
  geom_hline(yintercept=0.5, linetype=2)->canada_voter_policy_position_1993_2019
canada_voter_policy_position_1993_2019

ggsave(canada_voter_policy_position_1993_2019,filename="Plots/canada_voter_policy_position_1993_2019.png", width=8, height=4)

# This section does voter policy preferences 
# BW
ces %>% 
  pivot_longer(cols=c(economic, social), names_to=c("Dimension"), values_to=c("Score")) %>% 
  group_by(election, Dimension,vote) %>% 
  filter(election>1992 & election<2021) %>% 
  summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sd/sqrt(n)) %>% 
  filter(vote>0 & vote<5) %>% 
  mutate(Dimension=str_to_title(Dimension)) %>% 
  mutate(Election=as.Date(election, format="%Y")) %>% 
  ggplot(., aes(x=Election, 
                y=Average, 
                col=as_factor(vote),
                 linetype=as_factor(vote),
                 shape=as_factor(vote), 
                group=as_factor(vote)))+
  geom_point()+facet_wrap(~Dimension)+
  #geom_smooth(se=F)+
  geom_line()+
  geom_errorbar(width=0, aes(ymin=Average-(1.96*se), ymax=Average+(1.96*se)))+ 
 # scale_color_manual(values=c('darkred', "darkblue", "orange", "cyan"), name='Vote')+
  scale_x_date(limits=c(as.Date("1990-01-01"), as.Date("2019-12-31")) )+
  scale_shape_discrete(name="Vote")+
  scale_linetype_discrete(name="Vote")+
   scale_color_grey(name="Vote")+
  theme(legend.position = "bottom")+
  geom_hline(yintercept=0.5, linetype=2)->canada_voter_policy_position_1993_2019_bw
canada_voter_policy_position_1993_2019_bw

ggsave(canada_voter_policy_position_1993_2019,
       filename="Plots/canada_voter_policy_position_1993_2019_bw.png", width=8, height=4)



library(lubridate)
names(canada)
canada %>% 
  filter(Date>"1989-01-01"& 
          (partyname=="New Democratic Party" |
           partyname=="Liberal Party of Canada" |
           partyname=="Conservative Party of Canada" |
           partyname=="Progressive Conservative Party"|
             partyname=="Canadian Reform Conservative Alliance"|
          # partyname=="Reform Party of Canada"|
             partyname=="Quebec Bloc")) %>% 
  pivot_longer(cols=ends_with('_position'), 
               names_to=c("Dimension"), 
               values_to=c("Score")) %>% 
  mutate(Party=Recode(partyname, as.factor=T ,"'New Democratic Party'='NDP' ; 
  'Quebec Bloc'='BQ' ; 
  'Liberal Party of Canada'='Liberal' ; 
  'Conservative Party of Canada'='Conservative' ; 
                      'Progressive Conservative Party'='Conservative';
                      'Reform Party of Canada'='Reform' ; 'Canadian Reform Conservative Alliance'='Conservative'", 
                      levels=c("Liberal", "Conservative", "NDP", "BQ","PC")),
 Dimension=Recode(Dimension, "'economic_position'='Economic' ; 'social_position'='Social'")) %>% 
  ggplot(., aes(x=Date, y=Score, col=Party))+
  geom_point()+
  #geom_smooth(se=F)+
  geom_line()+
  facet_wrap(~Dimension)+
  scale_x_date(limits=c(as.Date("1990-01-01"), as.Date("2019-12-31")) )+
  scale_color_manual(values=c( 'darkred', 'darkblue', 'orange','cyan', 'lightblue', 'darkgreen'), name="Party")+
  #scale_linetype_discrete()+
  #scale_color_grey()+scale_shape_discrete()+
  theme(legend.position="none")->canada_party_positions_1993_2015
canada_party_positions_1993_2015
ggsave(canada_party_positions_1993_2015,filename="Plots/canada_party_positions_1993_2015.png", width=8, height=4)


#Party policy positions in bw
library(lubridate)
names(canada)
canada %>% 
  filter(Date>"1989-01-01"& 
           (partyname=="New Democratic Party" |
              partyname=="Liberal Party of Canada" |
              partyname=="Conservative Party of Canada" |
              partyname=="Progressive Conservative Party"|
              partyname=="Canadian Reform Conservative Alliance"|
              # partyname=="Reform Party of Canada"|
              partyname=="Quebec Bloc")) %>% 
  pivot_longer(cols=ends_with('_position'), 
               names_to=c("Dimension"), 
               values_to=c("Score")) %>% 
  mutate(Party=Recode(partyname, as.factor=T ,"'New Democratic Party'='NDP' ; 
  'Quebec Bloc'='BQ' ; 
  'Liberal Party of Canada'='Liberal' ; 
  'Conservative Party of Canada'='Conservative' ; 
                      'Progressive Conservative Party'='Conservative';
                      'Reform Party of Canada'='Reform' ; 'Canadian Reform Conservative Alliance'='Conservative'", 
                      levels=c("Liberal", "Conservative", "NDP", "BQ","PC")),
         Dimension=Recode(Dimension, "'economic_position'='Economic' ; 'social_position'='Social'")) %>% 
  ggplot(., aes(x=Date, y=Score, col=Party, linetype=Party, shape=Party))+
  geom_point()+
  #geom_smooth(se=F)+
  geom_line()+
  facet_wrap(~Dimension)+
  scale_x_date(limits=c(as.Date("1990-01-01"), as.Date("2019-12-31")) )+
  #scale_color_manual(values=c( 'darkred', 'darkblue', 'orange','cyan', 'lightblue', 'darkgreen'), name="Party")+
  scale_linetype_discrete()+
  scale_color_grey()+scale_shape_discrete()+
  theme(legend.position="none")->canada_party_positions_1993_2015_bw
canada_party_positions_1993_2015_bw
ggsave(canada_party_positions_1993_2015,filename="Plots/canada_party_positions_1993_2015.png", width=8, height=4)

#install.packages('cowplot')
library(cowplot)
plot_grid(canada_party_positions_1993_2015,
          canada_voter_policy_position_1993_2019, 
           ncol=1)
ggsave(filename=here("Plots", "combined_canada_voter_policy_preferences_1993_2015_color.png"), width=8, height=8)

plot_grid(canada_party_positions_1993_2015_bw,
          canada_voter_policy_position_1993_2019_bw, 
          ncol=1)
ggsave(filename=here("Plots", "combined_canada_voter_policy_preferences_1993_2015_bw.png"), width=8, height=8)

# #Define Dimension issues
# canada %>% 
#   rowwise() %>% 
#   mutate(second_dimension=sum(c_across(c(per101:per110, per201:per204, per301:per305,
#                                          per501:per503, per601:per608,per705:per706
#   ))),
#   first_dimension=sum(c_across(c(per401:per416, per504:per507, per701:per704,))))->canada




#### Pooled OLS Models by decade ####
#What we need is by decade, minus the BQ and the Greens
ces %>% 
  filter(election<1999 & election> 1989 )->ces.1
ces %>% 
  filter(election<2009 & election> 1999 )->ces.2
ces %>% 
  filter(election<2020 & election> 2009 )->ces.3
names(ces)
# NDP Models
ces$region2<-relevel(ces$region2, "Atlantic")
m1<-lm(ndp~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+`1993`+`1997`, data=ces.1)
m2<-lm(ndp~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m3<-lm(ndp~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
m10<-lm(ndp~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+degree:redistribution_reversed+`1993`+`1997`, data=ces.1)
m11<-lm(ndp~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+degree:redistribution_reversed+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m12<-lm(ndp~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+degree:redistribution_reversed+`2011`+`2015`+`2019`, data=ces.3)

# Liberal models
m4<-lm(liberal~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+`1993`+`1997`, data=ces.1)
m5<-lm(liberal~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m6<-lm(liberal~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
m13<-lm(liberal~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+degree:redistribution_reversed+`1993`+`1997`, data=ces.1)
m14<-lm(liberal~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+degree:redistribution_reversed+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m15<-lm(liberal~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+degree:redistribution_reversed+`2011`+`2015`+`2019`, data=ces.3)

#Conservative Models
m7<-lm(conservative~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+`1993`+`1997`, data=ces.1)
m8<-lm(conservative~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m9<-lm(conservative~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
m16<-lm(conservative~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+degree:redistribution_reversed+`1993`+`1997`, data=ces.1)
m17<-lm(conservative~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+degree:redistribution_reversed+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m18<-lm(conservative~region2+age+male+degree+income+religion2+redistribution_reversed+market_liberalism+traditionalism2+immigration_rates+degree:redistribution_reversed+`2011`+`2015`+`2019`, data=ces.3)
ols.models<-list(m1, m2, m3, m4, m5, m6, m7, m8, m9)

interaction.models<-list(m10, m11, m12, m13, m14, m15, m16, m17, m18)

#### Graph Degree x Redistribution interaction ####


names(interaction.models)<-c(rep("NDP", 3), rep("Liberal", 3), rep("Conservative", 3))
library(stargazer)

stargazer(ols.models, 
          out=here("Tables", "ols_models.html"),type="html",
          covariate.labels=c("Region (Quebec)",
                             "Region (Ontario)",
                             "Region (West)",
                             "Age",
                             "Sex (Male)",
                             "Education (Degree)",
                             "Income (Quintiles)",
                             "Religion (Catholic)",
                             "Religion (Protestant)",
                             "Religion (Other)",
                             "Redistribution",
                             "Market Liberalism", 
                             "Traditionalism",
                             "Immigration Rates"
                             ), 
          dep.var.labels=c("NDP", "Liberal", "Conservative"),
          omit=c(".[12][90]"), digits=2, column.labels=rep(c("1990s", "2000s", "2010s"), 3))
ols.models
stargazer(ols.models,
          out=here("Tables", "ols_models_presentation.html"),
          type="html",
          #coef = c(),
          dep.var.labels=c("NDP", "Liberal", "Conservative"),
          omit=c("region", "age", "male", "religion",".[12][90]"),
          digits=2,
          covariate.labels=c("Degree", "Income", "Redistribution", 
                             "Market Liberalism", "Traditionalism", 
                             "Immigration", "Constant"),
          column.labels=rep(c("1990s", "2000s", "2010s"), 3))


interaction.models %>% 
  map_dfr(., tidy, .id='Party') %>% 
  mutate(term=recode_factor(term, "degree:redistribution"="Degree:Redistribution")) %>% 
  mutate(Period=c(rep("1990s", 19 ), rep("2000s", 19), rep("2010s", 19 ), rep("1990s", 19),
                  rep("2000s", 19 ), rep("2010s", 19), rep("1990s", 19 ), rep("2000s", 19), rep("2010s", 19))) %>% 
filter(str_detect(term, ":")) %>% 
  ggplot(., aes(x=Period, y=estimate, col=Period))+geom_point()+facet_grid(term~fct_relevel(Party, "NDP", "Liberal"), scales="free")+scale_color_grey(start=0.8, end=0.2) +geom_errorbar(width=0, aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))+   geom_hline(yintercept=0,  linetype=2)+theme(strip.text.y.right = element_text(angle = 0))+labs(y="Coefficient")
ggsave(filename=here("Plots", "degree_redistribution_interaction_terms.png"), width=8, height=4)
