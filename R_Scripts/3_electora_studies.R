#### This script is for preparing our analysis of the Piketty Hypothesis in the Canadian context
library(broom)
library(stargazer)
library(tidyverse)
library(dplyr)

#Set theme
#### Set Theme ####
theme_set(theme_bw(base_size=18))
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
  filter(election<2021) %>% 
  mutate(model=map(variables, function(x) lm(left~region2+male+age+income_tertile+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy))->ols_block_models

ols_block_models %>% 
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income_tertile") %>% 
  filter(election<2020)  %>% 
  mutate(Measure=Recode(term, "'degree'='Degree' ; 'income_tertile'='Income'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=Measure, group=Measure))+
  geom_point(position=position_dodge(.5))+
  geom_line()+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), 
                    ymax=estimate+(1.96*std.error), width=0), position=position_dodge(.5))+
  #geom_smooth(se=F, method="lm")+
  labs(x="Election", y="Estimate")+
  scale_color_grey()+
  geom_hline(yintercept=0, linetype=2)
 # geom_errorbar(width=0,aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))
ggsave(here("Plots", "block_degree_income_with_error.png"), width=8, height=6)

#### Decompose By Party
#### Basic Party vote models 1965-2021 ####
ces$degree
ces %>%
  nest(variables=-election) %>%
  filter(election<2021) %>% 
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income_tertile+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete1

ces %>%
  filter(election<2021) %>% 
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income_tertile+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete1

ces %>%
  filter(election<2021) %>% 
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income_tertile+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete1

ces %>%
  filter(election>2003&election<2021) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+income_tertile+degree+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete1

#Join all parties and plot Degree coefficients

ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income_tertile") %>% 
  filter(election<2021) %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'income_tertile'='Income'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, size=term, group=term))+
  geom_point()+
  facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"), name="Vote")+
  #scale_fill_manual(values=c("navy blue", "red", "orange"), name="Vote")+
  #scale_alpha_manual(values=c(0.2, .8))+  
  scale_size_manual(values=c(1,3), name="Coefficient")+
  geom_smooth(method="loess", size=0.5, alpha=0.2, se=F) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
  labs(color="Vote", x="Election", y="Estimate")+
  #geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5, linetype=2)+
  theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_party_income.png"), width=8, height=4)

ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income_tertile") %>%
  filter(election<2021) %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'income_tertile'='Income'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, size=term, group=term))+
  geom_point()+
  facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"), name="Vote")+
  #scale_fill_manual(values=c("navy blue", "red", "orange"), name="Vote")+
  #scale_alpha_manual(values=c(0.2, .8))+  
  scale_size_manual(values=c(1,3), name="Coefficient")+
  geom_smooth(method="loess", size=0.5, alpha=0.2, se=F) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
  labs( alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.2,0.2))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5, linetype=2)+
  theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "ols_degree_party_income_errors.png"), width=8, height=4)

ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="degree"|term=="income_tertile") %>%
  filter(election<2021) %>% 
  mutate(term=Recode(term, "'degree'='Degree'; 'income_tertile'='Income'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote,fill=vote, size=term, group=term, alpha=term))+
  geom_point()+
  facet_grid(~vote, switch="y")+
  scale_color_manual(values=c("navy blue", "red", "orange"), name="Vote")+
  scale_fill_manual(values=c("navy blue", "red", "orange"), name="Vote")+
  scale_alpha_manual(values=c(0.2, .8))+  
  scale_size_manual(values=c(1,3), name="Coefficient")+
  geom_smooth(method="loess", size=0.5, alpha=0.2, se=F) +
  #scale_fill_manual(values=c("navy blue", "red", "orange"))+
 labs( alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_ribbon(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))+
  ylim(c(-0.2,0.2))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
  #scale_color_grey(guide="none")+
  geom_hline(yintercept=0, alpha=0.5, linetype=2)+
  theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "ols_degree_party_income_shaded.png"), width=8, height=4)

  

#### Figure 3 and 4

#### Average Scores For Degree Versus Average ####
  # 
  # ces %>% 
  #   select(election, degree, redistribution_reversed, immigration_rates, market_liberalism, traditionalism2) %>%
  #   rename(Redistribution=redistribution_reversed, `Immigration Rates`=immigration_rates, `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2) %>% 
  #   #  mutate(Redistribution=skpersonal::revScale(Redistribution, reverse=T)) %>% 
  #   pivot_longer(cols=3:6) %>% 
  #   pivot_longer(cols=2, names_to="Variable", values_to="Group") %>% 
  #   filter(election>1988&election<2020) %>% 
  #   group_by(election, Variable, Group, name) %>% 
  #   summarize(average=mean(value, na.rm=T), n=n(), sd=sd(value, na.rm=T), se=sd/sqrt(n)) %>% 
  #   arrange(election, Variable, name, Group) %>% 
  #   filter(!is.na(Group)) %>% 
  #   group_by(election, name) %>% 
  #   mutate(Variable=recode_factor(Variable, "degree"="Degree")) %>% 
  #   mutate(Group=case_when(
  #     Variable=="Degree" & Group==0 ~ "No Degree",
  #     Variable=="Degree" & Group==1 ~ "Degree",
  #   )) %>%
  #   #  filter(Group=="No Degree") %>% 
  #   ggplot(., aes(y=election, x=average, group=Variable, col=`Group`))+geom_point()+
  #   facet_wrap(~fct_relevel(name, "Immigration Rates","Moral Traditionalism", "Market Liberalism", "Redistribution"), nrow=2)+
  #   theme(axis.text.x=element_text(angle=90))+scale_y_discrete(limits=rev)+
  #   scale_color_manual(values=rep(c('black', 'grey'),2))+
  #   geom_vline(xintercept=0.5, linetype=2)+labs(y="Election", x="Average")+labs(col="Degree Status")+
  #   geom_errorbar(width=0,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)))
  # ggsave(filename=here("Plots", "mean_attitudinal_preferences_education.png"), width=8, height=8)
  # 
  # #### Average Scores For Income ####
  # 
  # ces %>% 
  #   select(election, income_tertile, redistribution_reversed, immigration_rates, market_liberalism, traditionalism2) %>%
  #   rename(Redistribution=redistribution_reversed, `Immigration Rates`=immigration_rates, `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2) %>% 
  #   #  mutate(Redistribution=skpersonal::revScale(Redistribution, reverse=T)) %>% 
  #   pivot_longer(cols=3:6) %>% 
  #   pivot_longer(cols=2, names_to="Variable", values_to="Group") %>% 
  #   filter(election>1988&election<2021) %>% 
  #   group_by(election, Variable, Group, name) %>% 
  #   summarize(average=mean(value, na.rm=T), n=n(), sd=sd(value, na.rm=T), se=sd/sqrt(n)) %>% 
  #   arrange(election, Variable, name, Group) %>%
  #   filter(!is.na(Group)) %>% 
  #   group_by(election, name) %>% 
  #   rename(Income=Group) %>% 
  #   # mutate(Group=case_when(
  #   #   Variable=="Income" & Group==1 ~ "Low Income",
  #   #   Variable=="Income" & Group==5 ~ "High Income",
  #   # )) %>%
  #   #filter(Variable=="Income") %>%
  #      # filter(Income==1|Income==5) %>% 
  #   ggplot(., aes(y=election, x=average,  col=as_factor(Income)))+geom_point()+
  #     facet_wrap(~fct_relevel(name, "Immigration Rates","Moral Traditionalism", "Market Liberalism", "Redistribution"), nrow=2)+theme(axis.text.x=element_text(angle=90))+scale_y_discrete(limits=rev)+scale_color_manual(values=rep(c('grey', 'black'),2))+
  #   geom_vline(xintercept=0.5, linetype=2)+labs(y="Election", x="Average")+
  #   geom_errorbar(width=0,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)))+
  #   labs(col="Income Quintile")
  # ggsave(filename=here("Plots", "mean_attitudinal_preferences_income.png"), width=8, height=4)
  # 
  # 
#### Complicated Figure 3 ####
# ces %>% 
#   #Select necessary variables
#   select(degree, redistribution, vote2, election) %>% 
#   #Filter only post-1988 elections
#   filter(election>1988 & election<2020) %>% 
#   #Convert everything to factor
#   as_factor() %>% 
#   #Provide some renames for pretty printing
#   rename(Degree=degree, Vote=vote2, Redistribution=redistribution, Election=election) %>% 
#   #pivot_longer(., cols=c("Degree", "Income"), names_to=c("Variable"), values_to=c("Value")) %>% 
# #Filter out some unnecessary rows
#   filter(!is.na(Vote)&Vote!="Green"& Vote!="BQ") %>% 
#   #filter(!is.na(Value)) %>% 
#   filter(!is.na(Degree)) ->
#   degree_election_vote_redistribution
# 
# #Calculate full sample mean and merge bck in
# degree_election_vote_redistribution %>% 
#   group_by(Election) %>% 
#   mutate(Total=mean(Redistribution, na.rm=T)) %>% 
#   group_by(Election, Vote) %>% 
#   summarise(Average=mean(Redistribution, na.rm=T), n=n(), sd=sd(Redistribution, na.rm=T),se=sd/sqrt(n), Total=first(Total)) %>% 
#   mutate(Difference=Average-Total) %>% 
#   mutate(Measure="Party Gap")->election_vote_redistribution_difference 

#Calculate Degree-Party Mean differences on redistribution
# ces %>% 
#   #Filter only post-1988 elections
#   filter(election>1988 & election<2020) %>% 
#   #Convert everything to factor
#   as_factor() %>% 
#   #mutate(Income=fct_relevel(income2, "Lowest", "Middle", "Highest")) %>% 
#   #Provide some renames for pretty printing
#   rename(Degree=degree, Vote=vote2, Redistribution=redistribution, Election=election) %>% 
#   #Filter out some unnecessary rows
#   filter(!is.na(Vote)&Vote!="Green"& Vote!="BQ") %>% 
#   #filter(!is.na(Value)) %>% 
#   filter(!is.na(Degree)) %>% 
# #Nest by election and Party vote choice to run the model at that level
# nest(-c(Election, Vote)) %>% 
#   #Provide new colum model which is the result of a t.test of redistribution on Degree for each
#   #Party in each election
#   mutate(model=map(data, function(x) t.test(Redistribution~factor(Degree, levels=c("Degree", "No degree")), data=x))) %>% 
#   #Now tidy the results of each t.test
#   mutate(tidied=map(model, tidy)) %>% 
#   #Unnest to turn everything into a nice neat data frame
#   unnest(tidied) %>% 
#   #Now mutate a new variable called Sig
#   mutate(Sig=case_when(
#     #which is 1 when significant
#     p.value<0.05 ~ 1,
#     #and 0 otherwise
#     p.value>0.05 ~ 0
#     #feed to plot
#   )) %>% 
#   #Set x to be estimate, y to be Elections, reordered in the right way
#   #1993 at the top
#   ggplot(., aes(x=estimate, y=fct_reorder(Election, desc(Election))))+
#   #Add points, vary size with Significance
#   geom_point(position=position_dodge(0.5))+
#   #Add a dashed vertical line 
#   geom_vline(aes(xintercept=0), linetype=2)+
#   #add errorbars
#   #the variables are stored in the data frame that is created
#   #after unnesting the variable tidied (see above)
#   #Setting width=0 is necessary to remove the two crosshairs on the errorbar
#   #position_dodge is necessary to set the party bars off from each other because the overlop
#   geom_pointrange(aes(xmin=conf.low, xmax=conf.high), width=0)+
#   #here guides turns off the legend for the size; I think we can add it in a caption 
#   guides(size = "none") +
#   theme(legend.position = "bottom")+
#   #Set labels
#   labs(x="Gap", y="Election")+
#   facet_grid(~Vote)


#ggsave(filename=here("Plots", "means_degree_party_gap.png"), width=8, height=8)
#### Complicated Figure 4 ####
# 
# ces %>% 
#   #Select necessary variables
#   select(election, redistribution, vote2, income_tertile) %>% 
#   #Filter only post-1988 elections
#   filter(election>1988 & election<2020) %>% 
#   #Convert everything to factor
#   as_factor() %>% 
#   mutate(Income=fct_relevel(income_tertile, "Lowest", "Middle", "Highest")) %>% 
#   select(-income_tertile) %>% 
#   #Provide some renames for pretty printing
#   rename(Vote=vote2, Redistribution=redistribution, Election=election) %>% 
#   #pivot_longer(., cols=c("Degree", "Income"), names_to=c("Variable"), values_to=c("Value")) %>% 
#   #Filter out some unnecessary rows
#   filter(!is.na(Vote)&Vote!="Green"& Vote!="BQ") %>% 
#   #filter(!is.na(Value)) %>% 
#   filter(!is.na(Income)) ->
#   income_election_vote_redistribution
# income_election_vote_redistribution
# 
# #Calculate party means on redistribution
# income_election_vote_redistribution %>% 
#   filter(Income!="Middle") %>% 
#   #Nest by election and Party vote choice to run the model at that level
#   nest(-c(Election, Vote)) %>% 
#   #Provide new colum model which is the result of a t.test of redistribution on Degree for each
#   #Party in each election
#   mutate(model=map(data, function(x) t.test(Redistribution~Income, data=x))) %>% 
#   #Now tidy the results of each t.test
#   mutate(tidied=map(model, tidy)) %>% 
#   #Unnest to turn everything into a nice neat data frame
#  #To inspect items model results individuall
#   #Comment out the following line 
#   unnest(tidied) %>% 
#   #and Uncomment these lines
#   # unnest(tidied) ->out
# #Conservatives 1993
# #out$model[[1]]
# #Liberals 1993
#   #out$model[[2]]
#   #NDP
#   #
#     #out$model[[3]]
#   #Now mutate a new variable called Sig
#   mutate(Sig=case_when(
#     #which is 1 when significant
#     p.value<0.05 ~ 1,
#     #and 0 otherwise
#     p.value>0.05 ~ 0
#     #feed to plot
#   )) %>% 
#   #
#   mutate(Measure=rep(c('Income gap'), nrow(.))) %>% 
#   #Set x to be estimate, y to be Elections, reordered in the right way
#   #1993 at the top
#   ggplot(., aes(x=estimate, y=fct_reorder(Election, desc(Election)), col=Vote, shape=Measure))+
#   #Add points, vary size with Significance
#   geom_point(position=position_dodge(0.5))+
#   #Set party colors
#   scale_color_manual(values=c("darkblue", "darkred", "orange"))+
#   #Add a dashed vertical line 
#   geom_vline(aes(xintercept=0), linetype=2)+
#   #add errorbars
#   #the variables are stored in the data frame that is created
#   #after unnesting the variable tidied (see above)
#   #Setting width=0 is necessary to remove the two crosshairs on the errorbar
#   #position_dodge is necessary to set the party bars off from each other because the overlop
#   geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=0,position=position_dodge(.5))+
#   #here guides turns off the legend for the size; I think we can add it in a caption 
#   guides(size = "none") +
#   theme(legend.position = "bottom")+
#   #Set labels
#   labs(x="Gap", y="Election")+
#   geom_pointrange(aes(x=Difference, y=Election, xmin=Difference-(1.96*se), xmax=Difference+(1.96*se), shape=Measure), data=election_vote_redistribution_difference, position=position_dodge(width=0.9))
# ggsave(filename=here("Plots", "means_income_party_gap.png"), width=8, height=6)
# names(ces)
#### Figure 3 Simple ####
names(ces)
ces %>%
  #Select necessary variables
  select(Degree=degree, Redistribution=redistribution, Vote=vote2, Election=election) %>%
  #Filter only post-1988 elections
  filter(Election>1988 & Election<2020) %>%
  filter(!is.na(Vote)&Vote!="Green"&Vote!="BQ") %>% 
  #Convert everything to factor
  as_factor() %>%
#Party in each election
group_by(Election, Vote, Degree) %>% 
  summarize(Average=mean(Redistribution,na.rm=T), sd=sd(Redistribution, na.rm=T), n=n(), se=sd/sqrt(n)) ->
degree_group_differences

ces %>%
  #Select necessary variables
  select(Degree=degree, Redistribution=redistribution, Vote=vote2, Election=election) %>%
  #Filter only post-1988 elections
  filter(Election>1988 & Election<2020) %>%
  filter(!is.na(Vote)&Vote!="Green"&Vote!="BQ") %>% 
  #Convert everything to factor
  as_factor() %>%
   nest(-c(Election, Vote)) %>% 
  mutate(model=map(data, function(x) t.test(Redistribution~factor(Degree, levels=c("Degree", "No degree")), data=x))) %>% 
  mutate(tidied=map(model, tidy)) %>% 
  unnest(tidied) %>% 
  right_join(., degree_group_differences, by=c("Vote", "Election")) %>% 
select(Election, Vote, p.value, Degree, se, Average) %>% 
  filter(!is.na(Degree)) %>% 
  mutate(Sig=case_when(
    p.value<0.051~1,
    p.value>0.05~0
  )) %>% 
 # pivot_longer(., cols=c("Degree", "No degree"), names_to=c("Degree"), values_to=c("Redistribution")) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(Election, desc(Election)), col=Degree))+
  facet_grid(~Vote)+
  geom_point(aes(size=as.factor(Sig)), position=position_dodge(width=0.5))+
  geom_errorbar(size=0.5,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)),width=0,position=position_dodge(width=0.5))+
  scale_size_manual(values=c(2, 3))+
  scale_color_grey()+
guides(size="none")+
  labs(y="Election")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme_bw()+
xlim(c(0.4,1))
ggsave(filename=here("Plots", "mean_degree_difference_significance.png"), height=8, width=13)
 

#### Figure 4 Simple ####
names(ces)
ces %>%
  #Select necessary variables
  select(Income=income_tertile, Redistribution=redistribution, Vote=vote2, Election=election) %>%
  #Filter only post-1988 elections
  filter(Election>1988 & Election<2020) %>%
  filter(!is.na(Vote)&Vote!="Green"&Vote!="BQ") %>% 
  filter(Income!=2) %>% 
  #Convert everything to factor
  as_factor() %>%
  #Party in each election
  group_by(Election, Vote, Income) %>% 
  summarize(Average=mean(Redistribution,na.rm=T), sd=sd(Redistribution, na.rm=T), n=n(), se=sd/sqrt(n)) ->
  income_group_differences

ces %>%
  #Select necessary variables
  select(Income=income_tertile, Redistribution=redistribution, Vote=vote2, Election=election) %>%
  #Filter only post-1988 elections
  filter(Election>1988 & Election<2020) %>%
  filter(!is.na(Vote)&Vote!="Green"&Vote!="BQ") %>% 
  filter(Income!=2) %>% 
  #Convert everything to factor
  as_factor() %>%
  nest(-c(Election, Vote)) %>% 
  mutate(model=map(data, function(x) t.test(Redistribution~factor(Income, levels=c("Lowest", "Highest")), data=x))) %>% 
  mutate(tidied=map(model, tidy)) %>% 
  unnest(tidied) %>% 
  right_join(., income_group_differences, by=c("Vote", "Election")) %>% 
  select(Election, Vote, p.value, Income, se, Average) %>% 
  filter(!is.na(Income)) %>% 
  mutate(Sig=case_when(
    p.value<0.051~1,
    p.value>0.05~0
  )) %>% 
  # pivot_longer(., cols=c("Degree", "No degree"), names_to=c("Degree"), values_to=c("Redistribution")) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(Election, desc(Election)), col=Income))+
  facet_grid(~Vote)+
  geom_point(aes(size=as.factor(Sig)), position=position_dodge(width=0.5))+
  geom_errorbar(size=0.5,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)),width=0,position=position_dodge(width=0.5))+
  scale_size_manual(values=c(2, 3))+
  scale_color_grey()+
  guides(size=F)+
  labs(y="Election")+
  geom_vline(xintercept=0.5, linetype=2)
  theme_bw()
# ces %>% 
#   select(election, redistribution, degree) %>% 
#   filter(election>2000) %>% 
#   as_factor() %>%  
#   group_by(degree, election) %>% 
#   summarize(average=mean(redistribution, na.rm = T)) %>% 
#   View()
ggsave(filename=here("Plots", "mean_income_difference_significance.png"), height=8, width=12)


#### Figure 4 by Household Size ####

ces %>%
  #Select necessary variables
  select(Income=income_house, Redistribution=redistribution, Vote=vote2, Election=election) %>%
  #Filter only post-1988 elections
  filter(Election>1988 & Election<2020) %>%
  filter(!is.na(Vote)&Vote!="Green"&Vote!="BQ") %>% 
  filter(Income!=2) %>% 
  #Convert everything to factor
  as_factor() %>%
  #Party in each election
  group_by(Election, Vote, Income) %>% 
  summarize(Average=mean(Redistribution,na.rm=T), sd=sd(Redistribution, na.rm=T), n=n(), se=sd/sqrt(n)) ->
  income_group_differences

ces %>%
  #Select necessary variables
  select(Income=income_house, Redistribution=redistribution, Vote=vote2, Election=election) %>%
  #Filter only post-1988 elections
  filter(Election>1988 & Election<2020) %>%
  filter(!is.na(Vote)&Vote!="Green"&Vote!="BQ") %>% 
  filter(Income!=2) %>% 
  #Convert everything to factor
  as_factor() %>%
  nest(-c(Election, Vote)) %>% 
  mutate(model=map(data, function(x) t.test(Redistribution~factor(Income, levels=c("Lowest", "Highest")), data=x))) %>% 
  mutate(tidied=map(model, tidy)) %>% 
  unnest(tidied) %>% 
  right_join(., income_group_differences, by=c("Vote", "Election")) %>% 
  select(Election, Vote, p.value, Income, se, Average) %>% 
  filter(!is.na(Income)) %>% 
  mutate(Sig=case_when(
    p.value<0.051~1,
    p.value>0.05~0
  )) %>% 
  # pivot_longer(., cols=c("Degree", "No degree"), names_to=c("Degree"), values_to=c("Redistribution")) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(Election, desc(Election)), col=Income))+
  facet_grid(~Vote)+
  geom_point(aes(size=as.factor(Sig)), position=position_dodge(width=0.5))+
  geom_errorbar(size=0.5,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)),width=0,position=position_dodge(width=0.5))+
  scale_size_manual(values=c(2, 3))+
  scale_color_grey()+
  guides(size=F)+
  labs(y="Election")+
  geom_vline(xintercept=0.5, linetype=2)+ 
  theme_bw()+
xlim(c(0.4,1))
# ces %>% 
#   select(election, redistribution, degree) %>% 
#   filter(election>2000) %>% 
#   as_factor() %>% 
#   group_by(degree, election) %>% 
#   summarize(average=mean(redistribution, na.rm = T)) %>% 
#   View()
ggsave(filename=here("Plots", "mean_income_household_difference_significance.png"), height=8, width=12)


# #### CMP ####
#   #Download the data
#   cmp<-read_sav(file="https://manifesto-project.wzb.eu/down/data/2021a/datasets/MPDataset_MPDS2021a.sav")
#   names(cmp)
#   
#   #Get Canada
#   
#   cmp %>% 
#     filter(countryname=="Canada")->canada
# 
#     #Define Dimension issues
#   economic_volume<-c("per401","per402","per407","per410","per414","per505","per507","per702","per403","per404","per405","per406","per409","per412","per413","per415","per503","per504","per506","per701")
#   social_volume<-c("per305","per601","per603","per605","per606","per608","per201","per202","per416","per501","per502","per602","per604","per607","per705","per706")
#   canada$economic_position<-((log(canada$per401+.5))+(log(canada$per402+0.5))+(log(canada$per407+0.5))+(log(canada$per410+0.5))+(log(canada$per414+0.5))+(log(canada$per505+0.5))+(log(canada$per507+0.5))+(log(canada$per702+0.5)))-((log(canada$per403+0.5))+(log(canada$per404+0.5))+(log(canada$per405+0.5))+(log(canada$per406+0.5))+(log(canada$per409+0.5))+(log(canada$per412+0.5))+(log(canada$per413+0.5))+(log(canada$per415+0.5))+(log(canada$per503+0.5))+(log(canada$per504+0.5))+(log(canada$per506+0.5))+(log(canada$per701+0.5)))
#   canada$social_position<-((log(canada$per305+0.5))+(log(canada$per601+0.5))+(log(canada$per603+0.5))+(log(canada$per605+0.5))+(log(canada$per606+0.5))+(log(canada$per608+0.5)))-((log(canada$per201+0.5))+(log(canada$per202+0.5))+(log(canada$per416+0.5))+(log(canada$per501+0.5))+(log(canada$per502+0.5))+(log(canada$per602+0.5))+(log(canada$per604+0.5))+(log(canada$per607+0.5))+(log(canada$per705+0.5))+(log(canada$per706+0.5)))
#   canada %>% 
#    # select(economic_volume) %>% 
#     rowwise() %>% 
#     mutate(economic_volume=sum(c_across(all_of(economic_volume))), 
#           social_volume=sum(c_across(all_of(social_volume)))) ->canada
# 
#   #Make table of first and second dimension issues
#   library(lubridate)
#   canada %>% 
#     # select(edate,partyname, second_dimension, first_dimension) %>% 
#     #modify party names for categorization
#     mutate(Party=case_when(
#       str_detect(partyname, "Cooperative Commonwealth Federation")~'CCF-NDP',
#       str_detect(partyname, "Democratic")~'CCF-NDP',
#       str_detect(partyname, "Progressive Conservative")~'Conservative',
#       str_detect(partyname, "Reform Party of Canada")~'Conservative',
#       str_detect(partyname, "Canadian Reform Canadian Alliance")~'Conservative',
#       str_detect(partyname, "Conservative")~'Conservative',
#       str_detect(partyname, "Liberal")~'Liberal',
#       str_detect(partyname, "Social Credit")~'Social Credit',
#       str_detect(partyname, "Bloc")~'Bloc',
#       str_detect(partyname, "Green")~'Green',
#     ), 
#     #modify date
#     Date=ymd(edate),
#     #Create ratio
#   Ratio=economic_volume/social_volume)->canada
# canada$Party<-factor(canada$Party, levels=c("Liberal", "Conservative","CCF-NDP", "Bloc","Green", "Social Credit"))
# #make color plot
#   canada %>% 
#     #pivot_longer(., cols=c("first_dimension", "second_dimension")) %>% 
#     ggplot(., aes(x=Date, y=Ratio, col=Party))+
#    # geom_line()+
#     geom_point()+
#     theme_minimal()+geom_hline(yintercept=1, linetype=2)+
#     geom_smooth(se=F,  method="loess")+
#     scale_color_manual(values=c("darkred","blue","orange", "cyan",  "darkgreen",  "black"))+
#     scale_x_date(breaks=seq.Date(from=as.Date("1945-01-01"), to=as.Date("2015-12-31"), by="10 years"), date_labels="%Y")
#   
#   ggsave(filename="Plots/economic_social_volume_color.png", width=8, height=6)
# #Make bw plot
#   canada %>% 
#     #pivot_longer(., cols=c("first_dimension", "second_dimension")) %>% 
#     ggplot(., aes(x=Date, y=Ratio, col=Party, shape=Party, linetype=Party, size=Party))+
# #    geom_line()+
#     geom_point()+
#     theme_minimal()+
#     geom_hline(yintercept=1, linetype=2)+
#      geom_smooth(se=F,  method="loess" )+
#     #scale_color_manual(values=c("cyan", "orange", "blue", "darkgreen", "darkred", "black"))+
#     scale_linetype_manual(values=c(1,2,3,4,5,6))+
#     scale_color_grey()+
#      scale_x_date(breaks=seq.Date(from=as.Date("1945-01-01"), to=as.Date("2015-12-31"), by="10 years"), date_labels="%Y")+
#     scale_size_manual(values=c(1,1,1,0.5,0.5,0.5))
#   ggsave(filename="Plots/economic_social_volume_bw.png", width=8, height=6)
#   
# 
#   # This section does voter policy preferences 
#   # Color
# ces %>% 
#   pivot_longer(cols=c(economic, social), names_to=c("Dimension"), values_to=c("Score")) %>% 
#   group_by(election, Dimension,vote) %>% 
#   filter(election>1992 & election<2021) %>% 
#   summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sd/sqrt(n)) %>% 
#   filter(vote>0 & vote<5) %>% 
#   mutate(Dimension=str_to_title(Dimension)) %>% 
#   mutate(Election=as.Date(election, format="%Y")) %>% 
#   ggplot(., aes(x=Election, 
#                 y=Average, 
#                 col=as_factor(vote),
#                 # linetype=as_factor(vote),
#                 # shape=as_factor(vote), 
#                  group=as_factor(vote)))+
#   geom_point()+facet_wrap(~Dimension)+
#     #geom_smooth(se=F)+
#   geom_line()+
#     geom_errorbar(width=0, aes(ymin=Average-(1.96*se), ymax=Average+(1.96*se)))+ 
#   scale_color_manual(values=c('darkred', "darkblue", "orange", "cyan"), name='Vote')+
#   scale_x_date(limits=c(as.Date("1990-01-01"), as.Date("2019-12-31")) )+
#       #scale_shape_discrete(name="Vote")+
#     #scale_linetype_discrete(name="Vote")+
#    # scale_color_grey(name="Vote")+
#   theme(legend.position = "bottom")+
#   geom_hline(yintercept=0.5, linetype=2)->canada_voter_policy_position_1993_2019
# canada_voter_policy_position_1993_2019
# 
# ggsave(canada_voter_policy_position_1993_2019,filename="Plots/canada_voter_policy_position_1993_2019.png", width=8, height=4)
# 
# # This section does voter policy preferences 
# # BW
# ces %>% 
#   pivot_longer(cols=c(economic, social), names_to=c("Dimension"), values_to=c("Score")) %>% 
#   group_by(election, Dimension,vote) %>% 
#   filter(election>1992 & election<2021) %>% 
#   summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sd/sqrt(n)) %>% 
#   filter(vote>0 & vote<5) %>% 
#   mutate(Dimension=str_to_title(Dimension)) %>% 
#   mutate(Election=as.Date(election, format="%Y")) %>% 
#   ggplot(., aes(x=Election, 
#                 y=Average, 
#                 col=as_factor(vote),
#                  linetype=as_factor(vote),
#                  shape=as_factor(vote), 
#                 group=as_factor(vote)))+
#   geom_point()+facet_wrap(~Dimension)+
#   #geom_smooth(se=F)+
#   geom_line()+
#   geom_errorbar(width=0, aes(ymin=Average-(1.96*se), ymax=Average+(1.96*se)))+ 
#  # scale_color_manual(values=c('darkred', "darkblue", "orange", "cyan"), name='Vote')+
#   scale_x_date(limits=c(as.Date("1990-01-01"), as.Date("2019-12-31")) )+
#   scale_shape_discrete(name="Vote")+
#   scale_linetype_discrete(name="Vote")+
#    scale_color_grey(name="Vote")+
#   theme(legend.position = "bottom")+
#   geom_hline(yintercept=0.5, linetype=2)->canada_voter_policy_position_1993_2019_bw
# canada_voter_policy_position_1993_2019_bw
# 
# ggsave(canada_voter_policy_position_1993_2019,
#        filename="Plots/canada_voter_policy_position_1993_2019_bw.png", width=8, height=4)
# 
# 
# 
# library(lubridate)
# names(canada)
# canada %>% 
#   filter(Date>"1989-01-01"& 
#           (partyname=="New Democratic Party" |
#            partyname=="Liberal Party of Canada" |
#            partyname=="Conservative Party of Canada" |
#            partyname=="Progressive Conservative Party"|
#              partyname=="Canadian Reform Conservative Alliance"|
#           # partyname=="Reform Party of Canada"|
#              partyname=="Quebec Bloc")) %>% 
#   pivot_longer(cols=ends_with('_position'), 
#                names_to=c("Dimension"), 
#                values_to=c("Score")) %>% 
#   mutate(Party=Recode(partyname, as.factor=T ,"'New Democratic Party'='NDP' ; 
#   'Quebec Bloc'='BQ' ; 
#   'Liberal Party of Canada'='Liberal' ; 
#   'Conservative Party of Canada'='Conservative' ; 
#                       'Progressive Conservative Party'='Conservative';
#                       'Reform Party of Canada'='Reform' ; 'Canadian Reform Conservative Alliance'='Conservative'", 
#                       levels=c("Liberal", "Conservative", "NDP", "BQ","PC")),
#  Dimension=Recode(Dimension, "'economic_position'='Economic' ; 'social_position'='Social'")) %>% 
#   ggplot(., aes(x=Date, y=Score, col=Party))+
#   geom_point()+
#   #geom_smooth(se=F)+
#   geom_line()+
#   facet_wrap(~Dimension)+
#   scale_x_date(limits=c(as.Date("1990-01-01"), as.Date("2019-12-31")) )+
#   scale_color_manual(values=c( 'darkred', 'darkblue', 'orange','cyan', 'lightblue', 'darkgreen'), name="Party")+
#   #scale_linetype_discrete()+
#   #scale_color_grey()+scale_shape_discrete()+
#   theme(legend.position="none")->canada_party_positions_1993_2015
# canada_party_positions_1993_2015
# ggsave(canada_party_positions_1993_2015,filename="Plots/canada_party_positions_1993_2015.png", width=8, height=4)
# 
# 
# #Party policy positions in bw
# library(lubridate)
# names(canada)
# canada %>% 
#   filter(Date>"1989-01-01"& 
#            (partyname=="New Democratic Party" |
#               partyname=="Liberal Party of Canada" |
#               partyname=="Conservative Party of Canada" |
#               partyname=="Progressive Conservative Party"|
#               partyname=="Canadian Reform Conservative Alliance"|
#               # partyname=="Reform Party of Canada"|
#               partyname=="Quebec Bloc")) %>% 
#   pivot_longer(cols=ends_with('_position'), 
#                names_to=c("Dimension"), 
#                values_to=c("Score")) %>% 
#   mutate(Party=Recode(partyname, as.factor=T ,"'New Democratic Party'='NDP' ; 
#   'Quebec Bloc'='BQ' ; 
#   'Liberal Party of Canada'='Liberal' ; 
#   'Conservative Party of Canada'='Conservative' ; 
#                       'Progressive Conservative Party'='Conservative';
#                       'Reform Party of Canada'='Reform' ; 'Canadian Reform Conservative Alliance'='Conservative'", 
#                       levels=c("Liberal", "Conservative", "NDP", "BQ","PC")),
#          Dimension=Recode(Dimension, "'economic_position'='Economic' ; 'social_position'='Social'")) %>% 
#   ggplot(., aes(x=Date, y=Score, col=Party, linetype=Party, shape=Party))+
#   geom_point()+
#   #geom_smooth(se=F)+
#   geom_line()+
#   facet_wrap(~Dimension)+
#   scale_x_date(limits=c(as.Date("1990-01-01"), as.Date("2019-12-31")) )+
#   #scale_color_manual(values=c( 'darkred', 'darkblue', 'orange','cyan', 'lightblue', 'darkgreen'), name="Party")+
#   scale_linetype_discrete()+
#   scale_color_grey()+scale_shape_discrete()+
#   theme(legend.position="none")->canada_party_positions_1993_2015_bw
# canada_party_positions_1993_2015_bw
# ggsave(canada_party_positions_1993_2015,filename="Plots/canada_party_positions_1993_2015.png", width=8, height=4)
# 
# #install.packages('cowplot')
# library(cowplot)
# plot_grid(canada_party_positions_1993_2015,
#           canada_voter_policy_position_1993_2019, 
#            ncol=1)
# ggsave(filename=here("Plots", "combined_canada_voter_policy_preferences_1993_2015_color.png"), width=8, height=8)
# 
# plot_grid(canada_party_positions_1993_2015_bw,
#           canada_voter_policy_position_1993_2019_bw, 
#           ncol=1)
# ggsave(filename=here("Plots", "combined_canada_voter_policy_preferences_1993_2015_bw.png"), width=8, height=8)
# 
# # #Define Dimension issues
# # canada %>% 
# #   rowwise() %>% 
# #   mutate(second_dimension=sum(c_across(c(per101:per110, per201:per204, per301:per305,
# #                                          per501:per503, per601:per608,per705:per706
# #   ))),
# #   first_dimension=sum(c_across(c(per401:per416, per504:per507, per701:per704,))))->canada




#### Pooled OLS Models by decade ####
ces$region2<-relevel(ces$region2, "Atlantic")
#What we need is by decade, minus the BQ and the Greens

ces %>% 
  filter(election<1999 & election> 1989 )->ces.1
ces %>% 
  filter(election<2009 & election> 1999 )->ces.2
ces %>% 
  filter(election<2020 & election> 2009 )->ces.3
names(ces)

# NDP Models


m1<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`1993`+`1997`, data=ces.1)
m2<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m3<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
m10<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`1993`+`1997`, data=ces.1)
m11<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m12<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2011`+`2015`+`2019`, data=ces.3)
m19<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`1993`+`1997`, data=ces.1)
m20<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m21<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2011`+`2015`+`2019`, data=ces.3)

# Liberal models
m4<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`1993`+`1997`, data=ces.1)
m5<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m6<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
m13<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`1993`+`1997`, data=ces.1)
m14<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m15<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2011`+`2015`+`2019`, data=ces.3)
m22<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`1993`+`1997`, data=ces.1)
m23<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m24<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2011`+`2015`+`2019`, data=ces.3)

#Conservative Models
m7<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`1993`+`1997`, data=ces.1)
m8<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m9<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
m16<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`1993`+`1997`, data=ces.1)
m17<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m18<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2011`+`2015`+`2019`, data=ces.3)
m25<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`1993`+`1997`, data=ces.1)
m26<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m27<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2011`+`2015`+`2019`, data=ces.3)

ols.models<-list(m1, m2, m3, m4, m5, m6, m7, m8, m9)
interaction.models<-list(m10, m11, m12, m13, m14, m15, m16, m17, m18)
interaction.models2<-list(m19, m20, m21, m22, m23, m24, m25, m26, m27)
summary(m27)
#### Stargazer Interaction models

stargazer(interaction.models, 
          out=here("Tables", "interaction_models_degree_redistribution.html"),
          ep.var.labels=c("NDP", "Liberal", "Conservative"),
          omit=c(".[12][90]"), digits=2, column.labels=rep(c("1990s", "2000s", "2010s"), 3))
stargazer(interaction.models2, 
          out=here("Tables", "interaction_models_income_redistribution.html"),
          ep.var.labels=c("NDP", "Liberal", "Conservative"),
          omit=c(".[12][90]"), digits=2, column.labels=rep(c("1990s", "2000s", "2010s"), 3))
#### Graph Degree & Income x Redistribution interactions ####

names(interaction.models)<-c(rep("NDP", 3), rep("Liberal", 3), rep("Conservative", 3))
names(interaction.models2)<-c(rep("NDP", 3), rep("Liberal", 3), rep("Conservative", 3))
library(stargazer)

stargazer(ols.models, 
          out=here("Tables", "table_1_ols_models_decade.html"),type="html",
          covariate.labels=c("Region (Quebec)",
                             "Region (Ontario)",
                             "Region (West)",
                             "Age",
                             "Sex (Male)",
                             "Education (Degree)",
                             "Income (Terciles)",
                             "Religion (Catholic)",
                             "Religion (Protestant)",
                             "Religion (Other)",
                             "Redistribution",
                             "Market Liberalism", 
                             "Traditionalism",
                             "Immigration Rates"
                             ), 
          dep.var.labels=c("NDP", "Liberal", "Conservative"),
          omit=c(".[12][90]"), digits=2, 
          column.labels=rep(c("1990s", "2000s", "2010s"), 3))

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
  ggplot(., aes(x=Period, y=estimate, col=Period))+geom_point()+facet_grid(term~fct_relevel(Party, "NDP", "Liberal"), scales="free")+scale_color_grey(start=0.8, end=0.2) +geom_errorbar(width=0, aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))+   geom_hline(yintercept=0,  linetype=2)+
  theme(strip.text.y.right = element_text(angle = 0))+labs(y="Coefficient")
ggsave(filename=here("Plots", "degree_redistribution_interaction_terms.png"), width=8, height=4)

interaction.models2 %>% 
  map_dfr(., tidy, .id='Party') %>% 
  mutate(term=recode_factor(term, "income_tertile:redistribution"="Income:Redistribution")) %>% 
  mutate(Period=c(rep("1990s", 19 ), rep("2000s", 19), rep("2010s", 19 ), rep("1990s", 19),
                  rep("2000s", 19 ), rep("2010s", 19), rep("1990s", 19 ), rep("2000s", 19), rep("2010s", 19))) %>% 
  filter(str_detect(term, ":")) %>% 
  ggplot(., aes(x=Period, y=estimate, col=Period))+geom_point()+facet_grid(term~fct_relevel(Party, "NDP", "Liberal"), scales="free")+scale_color_grey(start=0.8, end=0.2) +geom_errorbar(width=0, aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))+   geom_hline(yintercept=0,  linetype=2)+theme(strip.text.y.right = element_text(angle = 0))+labs(y="Coefficient")
ggsave(filename=here("Plots", "income_redistribution_interaction_terms.png"), width=8, height=4)

#### Table 2 Degree. ####
tab2_90_0_ndp<-lm(ndp~degree+`1993`+`1997`, data=ces.1)
tab2_90_00_ndp<-lm(ndp~income_tertile+`1993`+`1997`, data=ces.1)
tab2_90_1_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+`1993`+`1997`, data=ces.1)
tab2_90_2_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+`1993`+`1997`, data=ces.1)
tab2_90_3_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+market_liberalism+`1993`+`1997`, data=ces.1)
tab2_90_4_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+immigration_rates+`1993`+`1997`, data=ces.1)
tab2_90_5_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+traditionalism2+`1993`+`1997`, data=ces.1)
tab2_90_6_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+immigration_rates+traditionalism2+`1993`+`1997`, data=ces.1)
tab2_00_0_ndp<-lm(ndp~degree+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_00_ndp<-lm(ndp~income_tertile+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_1_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_2_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_3_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+market_liberalism+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_4_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_5_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+traditionalism2+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_6_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+immigration_rates+traditionalism2+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_10_0_ndp<-lm(ndp~degree+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_00_ndp<-lm(ndp~income_tertile+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_1_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_2_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_3_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+market_liberalism+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_4_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_5_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+traditionalism2+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_6_ndp<-lm(ndp~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+immigration_rates+traditionalism2+`2011`+`2015`+`2019`, data=ces.3)

### Liberal OLS
tab2_90_0_liberal<-lm(liberal~degree+`1993`+`1997`, data=ces.1)
tab2_90_00_liberal<-lm(liberal~income_tertile+`1993`+`1997`, data=ces.1)
tab2_90_1_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+`1993`+`1997`, data=ces.1)
tab2_90_2_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+`1993`+`1997`, data=ces.1)
tab2_90_3_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+market_liberalism+`1993`+`1997`, data=ces.1)
tab2_90_4_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+immigration_rates+`1993`+`1997`, data=ces.1)
tab2_90_5_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+traditionalism+`1993`+`1997`, data=ces.1)
tab2_90_6_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+immigration_rates+traditionalism+`1993`+`1997`, data=ces.1)
tab2_00_0_liberal<-lm(liberal~degree+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_00_liberal<-lm(liberal~income_tertile+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_1_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_2_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_3_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+market_liberalism+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_4_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_5_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+traditionalism2+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_6_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+immigration_rates+traditionalism2+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_10_0_liberal<-lm(liberal~degree+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_00_liberal<-lm(liberal~income_tertile+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_1_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_2_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_3_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+market_liberalism+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_4_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_5_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+traditionalism2+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_6_liberal<-lm(liberal~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+immigration_rates+traditionalism2+`2011`+`2015`+`2019`, data=ces.3)

###
### Conservative OLS
tab2_90_0_conservative<-lm(conservative~degree+`1993`+`1997`, data=ces.1)
tab2_90_00_conservative<-lm(conservative~income+`1993`+`1997`, data=ces.1)
tab2_90_1_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+`1993`+`1997`, data=ces.1)
tab2_90_2_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+`1993`+`1997`, data=ces.1)
tab2_90_3_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+market_liberalism+`1993`+`1997`, data=ces.1)
tab2_90_4_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+immigration_rates+`1993`+`1997`, data=ces.1)
tab2_90_5_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+traditionalism2+`1993`+`1997`, data=ces.1)
tab2_90_6_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+immigration_rates+traditionalism2+`1993`+`1997`, data=ces.1)
tab2_00_0_conservative<-lm(conservative~degree+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_00_conservative<-lm(conservative~income_tertile+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_1_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_2_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_3_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+market_liberalism+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_4_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_5_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+traditionalism2+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_00_6_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+immigration_rates+traditionalism2+`2000`+`2004`+`2006`+`2008`, data=ces.2)
tab2_10_0_conservative<-lm(conservative~degree+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_00_conservative<-lm(conservative~income_tertile+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_1_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_2_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_3_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+market_liberalism+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_4_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_5_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+traditionalism2+`2011`+`2015`+`2019`, data=ces.3)
tab2_10_6_conservative<-lm(conservative~region2+age+male+degree+income_tertile+religion2+redistribution+market_liberalism+immigration_rates+traditionalism2+`2011`+`2015`+`2019`, data=ces.3)
#Make the degree table
tab2_90_ndp<-list(tab2_90_0_ndp,tab2_90_1_ndp, tab2_90_2_ndp, tab2_90_3_ndp, tab2_90_4_ndp, tab2_90_5_ndp, tab2_90_6_ndp)
tab2_00_ndp<-list(tab2_00_0_ndp,tab2_00_1_ndp, tab2_00_2_ndp, tab2_00_3_ndp, tab2_00_4_ndp, tab2_00_5_ndp, tab2_00_6_ndp)
tab2_10_ndp<-list(tab2_10_0_ndp,tab2_10_1_ndp, tab2_10_2_ndp, tab2_10_3_ndp, tab2_10_4_ndp, tab2_10_5_ndp, tab2_10_6_ndp)
tab2_90_liberal<-list(tab2_90_0_liberal, tab2_90_1_liberal, tab2_90_2_liberal, tab2_90_3_liberal, tab2_90_4_liberal, tab2_90_5_liberal, tab2_90_6_liberal)
tab2_00_liberal<-list(tab2_00_0_liberal,tab2_00_1_liberal, tab2_00_2_liberal, tab2_00_3_liberal, tab2_00_4_liberal, tab2_00_5_liberal, tab2_90_6_liberal)
tab2_10_liberal<-list(tab2_10_0_liberal,tab2_10_1_liberal, tab2_10_2_liberal, tab2_10_3_liberal, tab2_10_4_liberal, tab2_10_5_liberal, tab2_90_6_liberal)
tab2_90_conservative<-list(tab2_90_0_conservative,tab2_90_1_conservative, tab2_90_2_conservative, tab2_90_3_conservative, tab2_90_4_conservative, tab2_90_5_conservative, tab2_90_6_conservative)
tab2_00_conservative<-list(tab2_00_0_conservative, tab2_00_1_conservative, tab2_00_2_conservative, tab2_00_3_conservative, tab2_00_4_conservative, tab2_00_5_conservative, tab2_00_6_conservative)
tab2_10_conservative<-list(tab2_10_0_conservative, tab2_10_1_conservative, tab2_10_2_conservative, tab2_10_3_conservative, tab2_10_4_conservative, tab2_10_5_conservative, tab2_10_6_conservative)
tab2_90_conservative

tab2_90_ndp %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "degree"))->tab2_col1

tab2_00_ndp %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "degree"))->tab2_col2
tab2_10_ndp %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "degree"))->tab2_col3

tab2_90_liberal %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "degree"))->tab2_col4
tab2_00_liberal %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "degree"))->tab2_col5
tab2_10_liberal %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "degree"))->tab2_col6
tab2_90_conservative %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "degree"))->tab2_col7
tab2_00_conservative %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "degree"))->tab2_col8
tab2_10_conservative %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "degree"))->tab2_col9
tab2_col9
library(gt)
cbind(tab2_col1, tab2_col2, tab2_col3, tab2_col4, tab2_col5, tab2_col6, tab2_col7, tab2_col8, tab2_col9) %>% 
data.frame() %>% 
  mutate(`Coefficients`=c("No Controls", 
                          "Demographics", 
                          "Demographics+Redistribution",
                          "Demographics+Market Liberalism", 
                          "Demographics+Immigration",
                          "Demographics+Traditionalism", 
                          "Full Model")) %>% 
  select(`Coefficients`, matches("estimate|p.value")) %>% 
  gt() %>% 
  fmt_number(.,columns=2:19, decimals=3) %>% 
  gtsave(., filename="Tables/table2_simon_degree.html")

#### Make the Income table
tab2a_90_ndp<-list(tab2_90_00_ndp,tab2_90_1_ndp, tab2_90_2_ndp, tab2_90_3_ndp, tab2_90_4_ndp, tab2_90_5_ndp, tab2_90_6_ndp)
tab2a_00_ndp<-list(tab2_00_00_ndp,tab2_00_1_ndp, tab2_00_2_ndp, tab2_00_3_ndp, tab2_00_4_ndp, tab2_00_5_ndp, tab2_00_6_ndp)
tab2a_10_ndp<-list(tab2_10_00_ndp,tab2_10_1_ndp, tab2_10_2_ndp, tab2_10_3_ndp, tab2_10_4_ndp, tab2_10_5_ndp, tab2_10_6_ndp)
tab2a_90_liberal<-list(tab2_90_00_liberal, tab2_90_1_liberal, tab2_90_2_liberal, tab2_90_3_liberal, tab2_90_4_liberal, tab2_90_5_liberal, tab2_90_6_liberal)
tab2a_00_liberal<-list(tab2_00_00_liberal,tab2_00_1_liberal, tab2_00_2_liberal, tab2_00_3_liberal, tab2_00_4_liberal, tab2_00_5_liberal, tab2_90_6_liberal)
tab2a_10_liberal<-list(tab2_10_00_liberal,tab2_10_1_liberal, tab2_10_2_liberal, tab2_10_3_liberal, tab2_10_4_liberal, tab2_10_5_liberal, tab2_90_6_liberal)
tab2a_90_conservative<-list(tab2_90_00_conservative,tab2_90_1_conservative, tab2_90_2_conservative, tab2_90_3_conservative, tab2_90_4_conservative, tab2_90_5_conservative, tab2_90_6_conservative)
tab2a_00_conservative<-list(tab2_00_00_conservative, tab2_00_1_conservative, tab2_00_2_conservative, tab2_00_3_conservative, tab2_00_4_conservative, tab2_00_5_conservative, tab2_00_6_conservative)
tab2a_10_conservative<-list(tab2_10_00_conservative, tab2_10_1_conservative, tab2_10_2_conservative, tab2_10_3_conservative, tab2_10_4_conservative, tab2_10_5_conservative, tab2_10_6_conservative)

tab2a_90_ndp %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "income"))->tab2a_col1

tab2a_00_ndp %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "income"))->tab2a_col2
tab2a_10_ndp %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "income"))->tab2a_col3

tab2a_90_liberal %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "income"))->tab2a_col4
tab2a_00_liberal %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "income"))->tab2a_col5
tab2a_10_liberal %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "income"))->tab2a_col6
tab2a_90_conservative %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "income"))->tab2a_col7
tab2a_00_conservative %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "income"))->tab2a_col8
tab2a_10_conservative %>% 
  map(., tidy) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "income"))->tab2a_col9
tab2a_col9
library(gt)
cbind(tab2a_col1, tab2a_col2, tab2a_col3, tab2a_col4, tab2a_col5, tab2a_col6, tab2a_col7, tab2a_col8, tab2a_col9) %>% 
  data.frame() %>% 
  mutate(`Coefficients`=c("No Controls", 
                          "Demographics", 
                          "Demographics+Redistribution",
                          "Demographics+Market Liberalism", 
                          "Demographics+Immigration",
                          "Demographics+Traditionalism", 
                          "Full Model")) %>% 
  select(`Coefficients`, matches("estimate|p.value")) %>% 
  gt() %>% 
  fmt_number(.,columns=2:19, decimals=3) %>% 
  gtsave(., filename="Tables/table2_simon_income.html")
  
  

#### Postgrad robustness check (postgrad substituted for degree 1988-2019)#### 


#### Postgrad robustness check  Degree and Income gap for left-right block ####
ces %>% 
  nest(variables=-election) %>%
  filter(election>1984 & election<2021) %>% 
  mutate(model=map(variables, function(x) lm(left~region2+male+age+income_tertile+postgrad+as.factor(religion2), data=x)),
         tidied=map(model, tidy))->ols_block_models2

ols_block_models2 %>% 
  unnest(tidied) %>% 
  filter(term=="postgrad"|term=="income_tertile") %>% 
  filter(election>1984 & election<2021) %>% 
  mutate(Measure=Recode(term, "'postgrad'='Degree' ; 'income_tertile'='Income'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=Measure, group=Measure))+
  geom_point(position=position_dodge(.5))+
  geom_line()+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), 
                    ymax=estimate+(1.96*std.error), width=0), position=position_dodge(.5))+

  #geom_smooth(se=F, method="lm")+
  labs(x="Election", y="Estimate")+
  scale_color_grey()+
  geom_hline(yintercept=0, linetype=2)+
  theme(legend.position="bottom")
# geom_errorbar(width=0,aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))
ggsave(here("Plots", "block_postgrad_income_with_error.png"), width=8, height=6)



#### Postgrad robustness check Basic Party vote models 1965-2021 ####
ces %>%
  nest(variables=-election) %>%
  filter(election>1984 & election<2021) %>%
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income_tertile+postgrad+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete1

ces %>%
  filter(election>1984 & election<2021) %>%  
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income_tertile+postgrad+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete1

ces %>%
  filter(election>1984 & election<2021) %>% 
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income_tertile+postgrad+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete1

ces %>%
  filter(election>1984 & election<2021) %>% 
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(green~region2+male+age+income_tertile+postgrad+as.factor(religion2), data=x)),
         tidied=map(model, tidy),
         vote=rep('Green', nrow(.))
  )->green_models_complete1
#Join all parties and plot Degree coefficients
ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="postgrad"|term=="income_tertile") %>%
  filter(election>1984 & election<2021) %>% 
  mutate(term=Recode(term, "'postgrad'='Degree'; 'income_tertile'='Income'")) %>%
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
ggsave(here("Plots", "ols_postgrad_party_income.png"), width=8, height=4)

#### Postgrad robustness check Average Scores For Degree Versus Average ####

ces %>% 
  select(election, postgrad, redistribution_reversed, immigration_rates, market_liberalism, traditionalism2) %>%
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
  mutate(Variable=recode_factor(Variable, "postgrad"="Degree")) %>% 
  mutate(Group=case_when(
    Variable=="Degree" & Group==0 ~ "No Postgrad Degree",
    Variable=="Degree" & Group==1 ~ "Postgrad Degree",
  )) %>%
  #  filter(Group=="No Degree") %>% 
  ggplot(., aes(y=election, x=average, group=Variable, col=`Group`))+geom_point()+
  facet_wrap(~fct_relevel(name, "Immigration Rates","Moral Traditionalism", "Market Liberalism", "Redistribution"), nrow=2)+
  theme(axis.text.x=element_text(angle=90))+scale_y_discrete(limits=rev)+
  scale_color_manual(values=rep(c('grey', 'black'),2))+
  geom_vline(xintercept=0.5, linetype=2)+labs(y="Election", x="Average")+labs(col="Degree Status")+
  geom_errorbar(width=0,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)))
ggsave(filename=here("Plots", "mean_attitudinal_preferences_postgrad.png"), width=8, height=8)

ces %>% 
  select(postgrad, redistribution, vote2, election) %>% 
  filter(election>1988 & election<2020) %>% 
  as_factor() %>% 
  #mutate(Income=fct_relevel(income2, "Lowest", "Middle", "Highest")) %>% 
  mutate(Degree=Recode(postgrad, "1='Post-Grad'; 0='Other'", levels=c("No Degree", "Degree"))) %>% 
  rename(Vote=vote2, Redistribution=redistribution, Election=election) %>% 
  #pivot_longer(., cols=c("postgrad", "Income"), names_to=c("Variable"), values_to=c("Value")) %>% 
  group_by(Election, Vote, postgrad) %>% 
  filter(!is.na(Vote)&Vote!="Green"& Vote!="BQ") %>% 
  #filter(!is.na(Value)) %>% 
  filter(!is.na(postgrad)) %>% 
  summarize(avg=mean(Redistribution, na.rm=T), n=n(), sd=sd(Redistribution, na.rm=T), se=sd/sqrt(n)) %>% 
  arrange(Election, postgrad, Vote) %>% 
  ggplot(. ,aes(x=avg, y=fct_reorder(Election, desc(Election)), col=as.factor(postgrad)))+
  geom_point()+
  facet_grid(~Vote)+
  scale_color_grey(start=0.8, end=0.2)+
  geom_errorbar(aes(xmin=avg-(1.96*se), xmax=avg+(1.96*se), width=0))+
  geom_vline(xintercept=0.5, linetype=2)+
  labs(y="Year", col="Postgrad")
ggsave(filename=here("Plots", "means_postgrad_redistribution_party.png"), width=8, height=8)

#What we need is by decade, minus the BQ and the Greens
ces$region2<-relevel(ces$region2, "Atlantic")
ces %>% 
  filter(election<1999 & election> 1989 )->ces.1
ces %>% 
  filter(election<2009 & election> 1999 )->ces.2
ces %>% 
  filter(election<2020 & election> 2009 )->ces.3
names(ces)

#### Pooled OLS Models by decade  - Postgrad substituted for degree####


# NDP Models
m31<-lm(ndp~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`1993`+`1997`, data=ces.1)
m32<-lm(ndp~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m33<-lm(ndp~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
m40<-lm(ndp~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`1993`+`1997`, data=ces.1)
m41<-lm(ndp~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m42<-lm(ndp~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2011`+`2015`+`2019`, data=ces.3)
m49<-lm(ndp~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`1993`+`1997`, data=ces.1)
m50<-lm(ndp~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m51<-lm(ndp~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2011`+`2015`+`2019`, data=ces.3)

# Liberal models
m34<-lm(liberal~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`1993`+`1997`, data=ces.1)
m35<-lm(liberal~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m36<-lm(liberal~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
m43<-lm(liberal~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`1993`+`1997`, data=ces.1)
m44<-lm(liberal~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m45<-lm(liberal~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2011`+`2015`+`2019`, data=ces.3)
m52<-lm(liberal~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`1993`+`1997`, data=ces.1)
m53<-lm(liberal~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m54<-lm(liberal~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2011`+`2015`+`2019`, data=ces.3)

#Conservative Models
m37<-lm(conservative~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`1993`+`1997`, data=ces.1)
m38<-lm(conservative~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m39<-lm(conservative~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+`2011`+`2015`+`2019`, data=ces.3)
m46<-lm(conservative~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`1993`+`1997`, data=ces.1)
m47<-lm(conservative~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m48<-lm(conservative~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution+`2011`+`2015`+`2019`, data=ces.3)
m55<-lm(conservative~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`1993`+`1997`, data=ces.1)
m56<-lm(conservative~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
m57<-lm(conservative~region2+age+male+postgrad+income_tertile+religion2+redistribution+market_liberalism+traditionalism2+immigration_rates+income_tertile:redistribution+`2011`+`2015`+`2019`, data=ces.3)

ols.models<-list(m31, m32, m33, m34, m35, m36, m37, m38, m39)
interaction.models<-list(m40, m41, m42, m43, m44, m45, m46, m47, m48)
interaction.models2<-list(m49, m50, m51, m52, m53, m54, m55, m56, m57)
interaction.models


#### Graph Degree & Income x Redistribution interactions ####

names(interaction.models)<-c(rep("NDP", 3), rep("Liberal", 3), rep("Conservative", 3))
names(interaction.models2)<-c(rep("NDP", 3), rep("Liberal", 3), rep("Conservative", 3))
library(stargazer)

stargazer(ols.models, 
          out=here("Tables", "ols_models.html"),type="html",
          covariate.labels=c("Region (Quebec)",
                             "Region (Ontario)",
                             "Region (West)",
                             "Age",
                             "Sex (Male)",
                             "Education (Degree)",
                             "Income (Terciles)",
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


interaction.models2 %>% 
  map_dfr(., tidy, .id='Party') %>% 
  mutate(term=recode_factor(term, "postgrad:redistribution"="Degree:Redistribution")) %>% 
  mutate(Period=c(rep("1990s", 19 ), rep("2000s", 19), rep("2010s", 19 ), rep("1990s", 19),
                  rep("2000s", 19 ), rep("2010s", 19), rep("1990s", 19 ), rep("2000s", 19), rep("2010s", 19))) %>% 
  filter(str_detect(term, ":")) %>% 
  ggplot(., aes(x=Period, y=estimate, col=Period))+geom_point()+facet_grid(term~fct_relevel(Party, "NDP", "Liberal"), scales="free")+scale_color_grey(start=0.8, end=0.2) +geom_errorbar(width=0, aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))+   geom_hline(yintercept=0,  linetype=2)+theme(strip.text.y.right = element_text(angle = 0))+labs(y="Coefficient")
ggsave(filename=here("Plots", "postgrad_redistribution_interaction_terms.png"), width=8, height=4)

#### Multinomial Robustness Check ####

library(nnet)

# Note going forward degree and male are no longer labelled
# They are proper factors
# ces$degree<-as_factor(ces$degree)
# ces$male<-as_factor(ces$male)
ces %>%
as_factor() %>% 
  nest(variables=-election) %>%
  filter(election<2021) %>% 
  mutate(model=map(variables, function(x) multinom(vote2~region2+
                                                     male+
                                                     age+
                                                     income_tertile+
                                                     degree+
                                                     religion2, data=x)), 
         tidied=map(model, tidy)) ->multinom_models

#Get predicted probabilities
# Degree
library(modelsummary)
library(marginaleffects)
#Assign names to the list of models
names(multinom_models$model)<-multinom_models$election
names(multinom_models$tidied)<-multinom_models$election
# Comparison of Predicted Probabilities for Degree
glimpse(multinom_models)

multinom_models$model %>% 
  map_df(., function(x)
    avg_comparisons(x, variables=c("income_tertile", "degree")), .id="Election" ) %>% 
  filter(group!="Green"&group!="BQ") %>% 
  filter(contrast=="Highest - Lowest"|contrast=="Degree - No degree") %>% 
  mutate(Election=as.Date(Election, "%Y")) %>%
  mutate(term=Recode(term, "'Degree'='Degree'; 
                     'Income Tercile'='income_tertile'")) %>% 
  ggplot(., aes(x=Election,y=estimate, col=group, size=term))+
  #geom_pointrange(aes(ymin=conf.low, ymax=conf.high))+
  geom_point()+
  facet_grid(~group)+
  #geom_smooth(method="loess", se=F)+
  scale_color_manual(values=c("darkblue", "darkred", "orange"))+
  theme(strip.text.y = element_text(angle=0))+
  labs(y="Delta Predicted Probability", col="Vote")+
  scale_x_date(date_labels="%Y")+
  scale_size_manual(values=c(1,3), name="Coefficient")+
  geom_smooth(method="loess", se=F, linewidth=0.5)
ggsave(filename=here("Plots/multinomial_comparison_predicted_probabilities_degree_income.png"), width=12, height=6)




#Coefficients for degree

# multinom_models$tidied %>% 
#   bind_rows(.id="Election") %>%
#   filter(term=="degreeDegree") %>% 
#   filter(y.level=="NDP"|y.level=="Liberal") %>% 
#   mutate(Election=as.Date(Election, format="%Y")) %>% 
#   ggplot(., aes(x=Election, y=estimate, group=y.level, col=y.level))+
#   geom_pointrange(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))+
#   facet_grid(~y.level)+scale_x_date(date_labels="%Y")+
#   scale_color_manual(values=c("darkred", "orange"))+
#   geom_smooth(method="loess",se=F, linewidth=1)+
#   theme(legend.position="none")+
#   labs(y="Logit", caption="Effect of degree status on vote for Liberals / NDP v. Conservative\nControlling for age, gender, region, degree status,religion and income")
# ggsave(filename=here("Plots/coefficients_multinomial_degree.png"), width=8, height=6)

# Coefficients for Income
# multinom_models$tidied %>% 
#   bind_rows(.id="Election") %>%
#   filter(term=="income_tertile") %>% 
#   filter(y.level=="NDP"|y.level=="Liberal") %>% 
#   mutate(Election=as.Date(Election, format="%Y")) %>% 
#   ggplot(., aes(x=Election, y=estimate, group=y.level, col=y.level))+
#   geom_pointrange(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))+
#   facet_grid(~y.level)+scale_x_date(date_labels="%Y")+
#   geom_smooth(method="loess",se=F)+
#   scale_color_manual(values=c("darkred", "orange"))+
#   theme(legend.position="none")+
#   labs(y="Logit", caption="Effect of income on vote for Liberals / NDP v. Conservative\nControlling for age, gender, region, degree status,religion and income")
# ggsave(filename=here("Plots/coefficients_multinomial_income.png"), width=8, height=6)

#Multinomial Models by Decade
ces$vote2

multinom_mod1<-multinom(vote2 ~ region2 + age + male + degree + income_tertile + 
                     religion2 + redistribution + market_liberalism + traditionalism2 + 
                     immigration_rates + `1993` + `1997`, data=ces.1)
multinom_mod2<-multinom(vote2 ~ region2 + age + male + degree + income_tertile + 
                     religion2 + redistribution + market_liberalism + traditionalism2 + 
                     immigration_rates + `2000` + `2004` + `2006` + `2008`, data = ces.2)
multinom_mod3<-multinom(vote2~region2 + age + male + degree + income_tertile + 
                     religion2 + redistribution + market_liberalism + traditionalism2 + 
                     immigration_rates + `2011` + `2015` + 
                     `2019`, data = ces.3)
multinom.list
multinom.list<-list(multinom_mod1, multinom_mod2, multinom_mod3)
names(multinom.list)<-c("1990s", "2000s", "2010s")
modelsummary(multinom.list, 
             shape=term~response+model,output="gt", 
             coef_map=c("region2Quebec"="Region (Quebec)",
                           "region2Ontario"="Region (Ontario)",
                           "region2West"="Region (West)",
                           "age"="Age", 
                           "male"="Gender (Male)",
                           "income_tertile"="Income (Terciles)",
                           "degree"="Education (Degree Holder)",
             "religion2Catholic"="Religion (Catholic)",
             "religion2Protestant"="Religion (Protestant)",
             "religion2Other"="Religion (Other)",
             "redistribution"="Redistribution",
             "market_liberalism"="Market Liberalism",
             "immigration_rates"="Immigration Rates",
             "traditionalism2"="Moral Traditionalism"),
             coef_omit=c("[[:digit:]]{4}"), fmt=2,gof_omit=c("BIC|AIC|RMSE"), stars=T) %>% 
  cols_hide(., columns=8:12) %>% 
  gtsave(., filename=here("Tables/multinomial_check_table_1.html"))

ces.1$degree<-as_factor(ces.1$degree)
ces.2$degree<-as_factor(ces.2$degree)
ces.3$degree<-as_factor(ces.3$degree)
multinomial_interaction_1<-multinom(vote2~region2+age+male+degree+income_tertile+religion2+
                                      redistribution+market_liberalism+traditionalism2+immigration_rates+
                                      degree:redistribution+`1993`+`1997`, data=ces.1)
multinomial_interaction_2<-multinom(vote2~region2+age+male+degree+income_tertile+religion2+
                                      redistribution+market_liberalism+traditionalism2+immigration_rates+
                                      degree:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
multinomial_interaction_3<-multinom(vote2~region2+age+male+degree+income_tertile+religion2+
                                      redistribution+market_liberalism+traditionalism2+immigration_rates+
                                      degree:redistribution+`2011`+`2015`+`2019`, data=ces.3)
multinomial_interaction_4<-multinom(vote2~region2+age+male+degree+income_tertile+religion2+
                                      redistribution+market_liberalism+traditionalism2+immigration_rates+
                                     income_tertile:redistribution+`1993`+`1997`, data=ces.1)
multinomial_interaction_5<-multinom(vote2~region2+age+male+degree+income_tertile+religion2+
                                      redistribution+market_liberalism+traditionalism2+immigration_rates+
                                      income_tertile:redistribution+`2000`+`2004`+`2006`+`2008`, data=ces.2)
multinomial_interaction_6<-multinom(vote2~region2+age+male+degree+income_tertile+religion2+
                                      redistribution+market_liberalism+traditionalism2+immigration_rates+
                                      income_tertile:redistribution+`2011`+`2015`+`2019`, data=ces.3)
multinomial_interaction_model_list<-mget(grep("multinomial_interaction", ls(), value=T))
library(marginaleffects)
modelsummary(list(multinomial_interaction_1, multinomial_interaction_2, 
                  multinomial_interaction_3),
             shape=term~response+model,output="gt",
             coef_omit=c("[[:digit:]]{4}"), 
             fmt=2,gof_omit=c("BIC|AIC|RMSE"), stars=T) %>% 
  cols_hide(., columns=8:12)
out1<-tidy(avg_slopes(multinomial_interaction_1, variables="redistribution", by="degree")) 
out2<-tidy(avg_slopes(multinomial_interaction_2, variables="redistribution", by="degree")) 
out3<-tidy(avg_slopes(multinomial_interaction_3, variables="redistribution", by="degree"))

out1 %>% 
  bind_rows(., out2, out3) %>% 
  filter(group!="BQ"&group!="Green") %>% 
<<<<<<< HEAD
  filter(degree==1) %>% 
mutate(Decade=c(rep("1990", 3), rep("2000", 3), rep("2010", 3))) %>% 
  ggplot(., aes(x=Decade, y=estimate))+
=======
  #filter(degree=="Degree") %>% 
mutate(Decade=c(rep("1990", 6), rep("2000", 6), rep("2010", 6))) %>% 
  ggplot(., aes(x=Decade, y=estimate, col=degree))+
>>>>>>> f06bee1 (Simon's Changes)
  facet_grid(~fct_relevel(group, "NDP", "Liberal"))+
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high))
ggsave(filename=here("Plots", "interaction_degree_multinomial_robustness_check.png"),width=8, height=4)

out4<-tidy(avg_slopes(multinomial_interaction_4, by="income_tertile", variables="redistribution"))
out5<-tidy(avg_slopes(multinomial_interaction_5, by="income_tertile", variables="redistribution"))
out6<-tidy(avg_slopes(multinomial_interaction_6, by="income_tertile", variables="redistribution"))
out4
out4 %>% 
  bind_rows(., out5, out6) %>% 
  filter(group!="BQ"&group!="Green")  %>% 
  as_factor() %>% 
 #filter(degree==1) 
  mutate(Decade=c(rep("1990", 9), rep("2000", 9), rep("2010", 9))) %>% 
  ggplot(., aes(x=Decade, y=estimate, col=income_tertile))+
  facet_grid(~fct_relevel(group, "NDP", "Liberal"))+
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high))
ggsave(filename=here("Plots", "interaction_income_multinomial_robustness_check.png"),width=8, height=4)

#### Factor Analysis ####
#install.packages("splithalfr")
library(splithalfr)
spearman_brown(ces.1$market1, ces.1$market2)
ces %>% 
  filter(election>1988) %>% 
  select(market1, market2) %>% 
  na.omit()->market_liberalism
spearman_brown(market_liberalism[,1], market_lbieralism[,2])
ces %>% 
  filter(election>1988) %>% 
  select(trad1, trad2) %>% 
  na.omit()->moral_traditionalism
spearman_brown(moral_traditionalism[,1], moral_traditionalism[,2])
