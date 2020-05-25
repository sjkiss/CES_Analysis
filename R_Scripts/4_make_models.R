#### Here's how we can make some basic proportions. 
##Start with the data frame and pipe
ces %>% 
  #Form groups of the variables you're trying to tabulate
  group_by(election, vote, union) %>% 
  #summarize those groups by counting the cases n()
  summarize(n=n()) %>% 
  ##In dplyr: each time you summarize, one level of the grouping variables is "peeled off"
  #In this case, the summarized dataframe is now grouped only by election and by party vote
  #Make a new variable called pct that takes each value of n and divides by the sum(n)
  #the key here is that sum(n) is calculated at the group level (election * vote)
  mutate(pct=n/sum(n)) %>%
  #we are only interested in NDP votes and union percentages
  filter(vote==3& union==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percentage of NDP Voters Who Are Union Members")

##Saving plots
#ggsave always saves the plot that was just made in the script.
#Run here() to see where here() is. 
here()
#Combine it with "Plots"
here("Plots")
ggsave(filename=here("Plots", "percent_ndp_union.png"))

#Going the other way
# What share of union membership has voted NDP
ces %>% 
  group_by(election, union, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(union==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Union Members Voting NDP")

## Let's check the private / public sector share
ces %>% 
  group_by(election, vote, sector) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(sector==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of NDP Voters in publid sector")

ces %>% 
  group_by(election, sector, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(sector==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Public Sector Employees voting NDP")

#Voting NDP by degree
ces %>% 
  group_by(election, degree, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(degree==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Degree Holders Voting NDP")

#Voting NDP by Party ID
ces %>% 
  group_by(election, party_id, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(party_id==3& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Party ID Voting NDP")

#Voting NDP by income groups
ces %>% 
  group_by(election, income, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==5& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of High Icome Voting NDP")

ces %>% 
  group_by(election, income, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==3& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Middle Income Voting NDP")

ces %>% 
  group_by(election, income, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Low Income Voting NDP")



## Let's just fit a binomial logistic regression of being a union member by degree

model1<-glm(ndp~union, data=ces, family="binomial")
summary(model1)
str(model1)
#To model the NDPO we need to create a dichotomous NDP variable 
# I would do it this way, but I am curious where you found the way that you did it. 

ces$ndp<-car::Recode(ces$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
ndp_model<-glm(ndp~union, data=ces, family="binomial")


##Let's make some models

## There are two ways to check the impact of time.
##I prefer this way
##broom is a package that aids in the reporting of models



library(broom)
ces %>% 
  group_by(election) %>% 
  filter(election!=1965 & election!=1968) %>%  
  nest() %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of data the function that follows 
  mutate(mods=map(data, function(x) glm(ndp~union, data=x, family="binomial")), 
         tidied=map(mods, tidy)) -> models
#take a look at models
head(models)
models %>% 
  unnest(tidied) %>% 
  filter(term=="union") %>% 
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by union")

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models$mods, type="text")
#Can also output models as an html file
stargazer(models$mods, type="html", out=here("Tables", "union_models.html"))

### How to add an interaction
ces %>% 
  group_by(election) %>% 
  #There are no sector variables for 1972, so we have to delete those
  filter(election!=1965 & election!=1968 & election!=1972) %>%  
  nest() %>% 
  mutate(mods=map(data, function(x) glm(ndp~union+sector+union:sector, data=x, family="binomial")), 
         tidied=map(mods, tidy)) -> interaction_models
table(ces$election, ces$sector)
#take a look at models
head(interaction_models)
#We can look at specfic models in the list columns using square brackets
interaction_models$tidied[1]

#We could also filter out the significant ones
interaction_models %>% 
  unnest(tidied) %>% 
  filter(p.value<0.05)
##
interaction_models %>% 
  unnest(tidied) %>% 
  filter(term=="union:sector") %>% 
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="Interaction Coefficients for sector and union")

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models$mods, type="text")
#Can also output models as an html file
stargazer(models$mods, type="html", out=here("Tables", "union_models.html"))
##The other way would be to turn the election variable into a series of dichotommous variables. 

## we can do this with pivot_wider
## I found this crazy code here https://stackoverflow.com/questions/35663580/using-tidyr-spread-function-to-create-columns-with-binary-value
# 
# ces %>% 
# pivot_wider(names_from="election", 
#             values_from="election", 
#             values_fill=list(election=0),
#             values_fn = list(election = ~+(as.logical(length(.))))) ->ces.wide
