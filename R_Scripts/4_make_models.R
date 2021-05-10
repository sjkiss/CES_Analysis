#Install this package
#install.packages("ggeffects")
#Load
library(ggeffects)
#### Basic Proportions ####
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

## What Share of Private and Public Sector Union Members vote NDP
ces %>% 
  group_by(election, union, sector,ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(sector)==F) %>% 
  filter(is.na(union)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(union==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(sector)))+geom_col(position="dodge")+labs(title="Share of Public and PRivate sector union respondents voting NDP")

## Let's check the private / public sector share of the NDP universe
ces %>% 
  group_by(election, vote, sector) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(sector==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of NDP Voters in public sector")

ces %>% 
  group_by(election, sector, vote) %>% 
  summarize(n=n()) %>% 
  filter(is.na(sector)==F) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(vote==3) %>% 
  ggplot(., aes(x=election, y=pct, fill=as_factor(sector)))+geom_bar(stat="identity", position="dodge")+labs(title="Percent of Public Sector Employees voting NDP")

####  Degree status
#What is the degree status of the Canadian electorate
ces %>% 
  group_by(election,degree) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
ggplot(., aes(x=election, y=pct, fill=as.factor(degree)))+geom_col()
ggsave(here("PLots", "share_degree_ces.png"))
#Voting Party by degree

#start with data frame and pipe
ces %>% 
  #form groups of interest; we want to examine the share of people voting for parties, by degree status for each election
  group_by(election, degree, vote) %>% 
  #summrize each group calculating a variable n using the function n()
  summarize(n=n()) %>% 
  #mutate the dataframe created above by calculating a new variable called pct which takes the value of each n and dividing it by the sum of n which
  mutate(pct=n/sum(n)) %>%
  #filter(degree==1& vote==3) %>% 
  #filter out non-degree holders and include only Liberal, Conservative and NDP voters
  filter(degree==1 & (vote<4 & vote>0)) %>% 
  #ggplot
  #Note that election has to be turned numeric to support the smoothing
  #x-axis is the election, y is the pct variable created above
  ggplot(.,aes(x=as.numeric(election), y=pct))+
  #add points
  geom_point()+
  #add the smooth, method=regular linear model, y on x
  #se=F so that standard error bands are not included
  geom_smooth(method="lm", se=F)+
  #make a panel for each party, use as_factor to use the party labels
  facet_grid(~as_factor(vote))+
  #Add a title 
  labs(title="Piketty Plot: Share of Degree holders voting for political parties over time")
ggsave(here("Plots", "piketty_party_vote_by_degree.png"))


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


#### Union Logistic Model #### 
## Let's just fit a binomial logistic regression of voting NDP by union membership


##broom is a package that aids in the reporting of models
## https://cran.r-project.org/web/packages/broom/vignettes/broom.html
library(broom)
#AS always start witht hte data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  filter(election!=1965 ) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest() %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(mods=map(data, function(x) glm(ndp~union, data=x, family=binomial(link="probit"))), 
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         tidied=map(mods, tidy)) -> union
#take a look at models
head(union)
summary(union$mods[[1]])
union$election

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(union$mods, type="text")
#Can also output models as an html file
stargazer(union$mods,
          type="html", 
          out=here("Tables", "union.html"), title="PRobit coefficients of union household membership on NDP vote", column.labels=c("1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"))
#as always start with the data frame and pipe
union %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union coefficients
  filter(term=="union") %>% 
  #plot
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Probit Coefficients of voting NDP vote by union")+geom_smooth(method="loess", se=F)

#we can save that plot 
ggsave(here("Plots", "union_ndp_probit_coefficients.png"))


#Let's get the predicted probabilities
#start with the list of models 
union$mods %>% 
#map_df returns a nice dataframe; ggpredict is the function that we are mapping onto each linear model
#terms is the argument that we are sending to ggpredict
  #Reading this vignette https://cran.r-project.org/web/packages/ggeffects/vignettes/practical_logisticmixedmodel.html reports that the retruned values of ggpredict on a binomial model are predicted probabilities
  #By specifying "union" we only want the probabilities for union membership
  map_df(., ggpredict, terms=c('union')) %>% 
  #We have to add an election column; so we mutate making a new variable called election. 
  #We use the rep() function which repeats union$election and specifying each=2 means it repeats each element twice
  #A person would need to play with this in order to get the right number of elements
  mutate(election=rep(union$election, each=2)) %>% 
  #We need to subtract the probability of non-union members voting NDP from the probability of voting NDP for union members
  #we use mutate to make a new variable called difference
  #It takes the value of predicted - lag(predicted)
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ##Johnston's plot stops at 2011
#  filter(election<2012) %>% 
  #we ggplot x=election, y is the predicted value; make it a piont plot
  ggplot(., aes(x=election, y=difference))+geom_point()+
  #add an errorbar
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+labs(title="Effect of Union Family Membership on NDP vote")+ylim(c(0,0.2))
ggsave(here("Plots", "predicted_probabilities_ndp_by_union.png"))

#### Union OLS #### 
#AS always start witht hte data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  filter(election!=1965 ) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest() %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(mods=map(data, function(x) lm(ndp~union, data=x)), 
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         tidied=map(mods, tidy)) -> union2
#take a look at models
head(union2)
summary(union2$mods[[1]])
union2$election

#as always start with the data frame and pipe
union2 %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union coefficients
  filter(term=="union") %>% 
  #plot
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="OLS Coefficients of voting NDP vote by union")+geom_smooth(method="loess", se=F)

#we can save that plot 
ggsave(here("Plots", "union_ndp_OLS_coefficients.png"))

#Let's get the predicted probabilities
#start with the list of models 
union2$mods %>% 
  map_df(., ggpredict, terms=c('union')) %>% 
   mutate(election=rep(union$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(., aes(x=election, y=difference))+geom_point()+
  #add an errorbar
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+labs(title="Effect of Union Family Membership on NDP vote")+ylim(c(0,0.2))
ggsave(here("Plots", "predicted_probabilities_ndp_by_union.png"))

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(union2$mods, type="text")
#Can also output models as an html file
stargazer(union2$mods,
          type="html", 
          out=here("Tables", "union2.html"), title="OLS coefficients of union household membership on NDP vote", column.labels=c("1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"))


#### Degree Logit Model #### 
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  #filter(election!=1965 ) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest() %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(mods=map(data, function(x) glm(ndp~degree, data=x, family="binomial")), 
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         tidied=map(mods, tidy)) -> degree
#take a look at models
head(degree)
degree
#as always start with the data frame and pipe
degree %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union coefficients
  filter(term=="degree") %>% 
  #plot
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)

ggsave(here("Plots", "degree_ndp_coefficients.png"))



#How to print the exponentiated
#The tidy function has a lot of useful functions
#Because we have the object models that has the models already stored, we can just run tidy on them
#As always start with the data frame
union
union %>% 
  #mutate adds a new column
  mutate(odds=map(mods, function(x) tidy(x, exponentiate=T)))->union
head(union)

#compare
union$tidied[1]
union$odds[1]
#### How to add an interaction ####
ces %>% 
  group_by(election) %>% 
  #There are no sector variables for 1972, so we have to delete those
  filter(election!=1965 & election!=1972) %>%  
  nest() %>% 
  mutate(mods=map(data, function(x) glm(ndp~union+sector+union:sector, data=x, family="binomial")), 
         tidied=map(mods, tidy)) -> union_sector
#We could also filter out the significant ones
union_sector %>% 
  unnest(tidied) %>% 
  filter(p.value<0.05)
##Plot the interaction coefficients
union_sector %>% 
  unnest(tidied) %>% 
  filter(term=="union:sector") %>% 
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="Interaction Coefficients for sector and union")

#It works on a list of models, in this case union_sector$mods
union_sector$mods %>% 
  #pass the function ggpredict, select the terms you want the probabilities for
  # set the values for other terms inthe interaction in square brackets
  map(., ggpredict,terms=c('union', 'sector [0]')) %>% 
  #bind rows together
  bind_rows() %>% 
  #now x= union value, Predicted is the predicted probability of voting NDP for union and non-union members for the private sector (because sector ==0)
  ##this function just adds an election year variable, repeating each year from union_sector (above) twice
mutate(election=rep(union_sector$election, each=2)) %>% 
  #now we can plot
  #x is election, y is predicted, turn x into a factor and color the point
  ggplot(., aes(x=election, y=predicted, col=as.factor(x)))+
  geom_point()+
  labs(title="PRedicted Probabilities of Union (1) members and non-union (0) voting NDP for Private Sector Workers")





#### How to subset groups (i.e. income) ####
ces %>% 
  filter(income==1 & election!=1965&election!=1968) %>% 
  group_by(election) %>% 
  nest() %>% 
  mutate(mods=map(data, function(x) glm(ndp~union+income, data=x, family="binomial")))->poor_models

ces %>% 
  filter(election!=1965& election!=1968) %>% 
  group_by(election) %>% 
  nest() %>% 
  #Remember income is stored as a number, need to turn it into a factor on the fly
  mutate(mods=map(data, function(x) glm(ndp~union+as.factor(income), data=x, family="binomial")))->poor_models


