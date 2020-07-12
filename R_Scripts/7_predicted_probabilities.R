
#### Predicted Probabilities Sample Code ####

#install this package if necessary
#install.package('ggeffects')
#load
library(ggeffects)

#vignettes
# https://cran.r-project.org/web/packages/ggeffects/vignettes/introduction_plotmethod.html
# https://cran.r-project.org/web/packages/ggeffects/vignettes/ggeffects.html

#------------------------------------------------------------------------------------------------
#### M1 Blais Replication Extension (including 2019) for Sector ####

#start with the table of models
ndp_models_complete1
#predict words best on the *untidied* models
ndp_models_complete1$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='sector')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='sector')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for public sector workers. 
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='sector[1]')->public_sector
#Or we could select the predicted probabilities for private sector
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='sector[0]')->private_sector
#Note how the rows change
nrow(public_sector)
nrow(private_sector)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
ndp_models_complete1$election
public_sector %>% 
  mutate(Election=ndp_models_complete1$election)->public_sector

#Here is the plotting with vertical errorbars
public_sector %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
#We still need to add in the elections, but this is a little trickier, because we need to have 1968, 1968, 1972, 1972, etc. 
out %>% 
  #we just use the rep() command with the each=2 argument
  #this repeats the argument in the parenthesis two times. 
  mutate(Election=rep(ndp_models_complete1$election, each=2))

#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete1$election, each=2)) %>% 
  #The variable x is the public_sector variable, 0 equals private sector, 1 = public sector; y is predicted and we are going to vary the color of the points by x *AS A FACTOR*. If you don't turn it into a factor, it will treat x as a continuous variable wchih will vary the appearance
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  #POint plot; adding the position_dodge argument just moves the points a little to each side for clairty, 
  geom_point(position=position_dodge(width=0.5))+
  #add errorbars, width=0, ymin and ymax to be the confidence intervals, 
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

#Other additions to this.
#We could also go hog-wild and do what Johnston did and subtract private sector probability from public sector probability. 
#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete1$election, each=2))

#Here we need to make a new variable that is the difference of each public sector value (1) and each private sector value (0)
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete1$election, each=2)) %>% 
  #the key function is lag. 
  #we mutate the data frame, creating the variable difference that is the lag(predited)
  mutate(difference=predicted-lag(predicted))

##Repeat and go further
out %>% 
  mutate(Election=rep(ndp_models_complete1$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
    #then we just filter out the private sector
    filter(x==1) %>% 
  #And plot, note we are using y=difference now
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
#We have to recalculate the confidence intervals here using hte standard error
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP for public v. private sector")
##Presto: almost no effect of sectoral voting for hte NDP in it's life. We only get it in 1980, sort of get it in 1984 (per Blais and in 2019. )
#save
ggsave(here("Plots", "M1_difference_sectoral_NDP_vote.png"))


#------------------------------------------------------------------------------------------------

#### M13 Blais Replication Extension (including 2019) (ROC only) for Sector ####

ndp_models_complete13
ndp_models_complete13$model
ndp_models_complete13$model %>% 
  map_dfr(., ggpredict, terms='sector')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
ndp_models_complete13$model %>% 
  map_dfr(., ggpredict, terms='sector[1]')->public_sector13
#Or we could select the predicted probabilities for private sector
ndp_models_complete13$model %>% 
  map_dfr(., ggpredict, terms='sector[0]')->private_sector13

nrow(public_sector13)
nrow(private_sector13)

ndp_models_complete13$election
public_sector13 %>% 
  mutate(Election=ndp_models_complete13$election)->public_sector13

#Here is the plotting with vertical errorbars
public_sector13 %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
out %>% 
  mutate(Election=rep(ndp_models_complete13$election, each=2))

#We can plot both categories
out %>% 
  mutate(Election=rep(ndp_models_complete13$election, each=2)) %>% 
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

out %>% 
  mutate(Election=rep(ndp_models_complete13$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted))

out %>% 
  mutate(Election=rep(ndp_models_complete13$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP in ROC for public v. private sector")

#save
ggsave(here("Plots", "M13_difference_sectoral_NDP_vote_ROC.png"))


#------------------------------------------------------------------------------------------------

#### M14 Blais Replication Extension (including 2019) (ROC only)& no Degree or Income) for Sector ####

ndp_models_complete14
ndp_models_complete14$model
ndp_models_complete14$model %>% 
  map_dfr(., ggpredict, terms='sector')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
ndp_models_complete14$model %>% 
  map_dfr(., ggpredict, terms='sector[1]')->public_sector14
ndp_models_complete14$model %>% 
  map_dfr(., ggpredict, terms='sector[0]')->private_sector14

nrow(public_sector14)
nrow(private_sector14)

ndp_models_complete14$election
public_sector14 %>% 
  mutate(Election=ndp_models_complete14$election)->public_sector14

#Here is the plotting with vertical errorbars
public_sector14 %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
out %>% 
  mutate(Election=rep(ndp_models_complete14$election, each=2))

#We can plot both categories
out %>% 
  mutate(Election=rep(ndp_models_complete14$election, each=2)) %>% 
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

out %>% 
  mutate(Election=rep(ndp_models_complete14$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted))

out %>% 
  mutate(Election=rep(ndp_models_complete14$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP in ROC (w/o deg & inc) for public v. private sector")


#------------------------------------------------------------------------------------------------
#### M9 Blais Replication Extension (including 2019)(Quebec only) for Sector ####

ndp_models_complete9$model
ndp_models_complete9$model %>% 
  map_dfr(., ggpredict, terms='sector')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
ndp_models_complete9$model %>% 
  map_dfr(., ggpredict, terms='sector[1]')->public_sector9
ndp_models_complete9$model %>% 
  map_dfr(., ggpredict, terms='sector[0]')->private_sector9

nrow(public_sector9)
nrow(private_sector9)

ndp_models_complete9$election
public_sector9 %>% 
  mutate(Election=ndp_models_complete9$election)->public_sector9

#Here is the plotting with vertical errorbars
public_sector9 %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

out
nrow(out)

out %>% 
  mutate(Election=rep(ndp_models_complete9$election, each=2))

#We can plot both categories
out %>% 
  mutate(Election=rep(ndp_models_complete9$election, each=2)) %>% 
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

out %>% 
  mutate(Election=rep(ndp_models_complete9$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted))

out %>% 
  mutate(Election=rep(ndp_models_complete9$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP in Quebec for public v. private sector")

#save
ggsave(here("Plots", "M9_difference_sectoral_NDP_vote_Quebec.png"))


#------------------------------------------------------------------------------------------------
#### M6 Blais Replication Extension (including 2019) - for NDP Degree ####

ndp_models_complete6
ndp_models_complete6$model
ndp_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
ndp_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[1]')->degree_holder
ndp_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[0]')->no_degree

nrow(degree_holder)
nrow(no_degree)

ndp_models_complete6$election
degree_holder %>% 
  mutate(Election=ndp_models_complete6$election)->degree_holder

#Here is the plotting with vertical errorbars
degree_holder %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high))+
  labs(title="Predicted Probability of voting NDP for degree holders") 

#save
ggsave(here("Plots", "M6_degree_NDP_vote.png"))

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)

out %>% 
  mutate(Election=rep(ndp_models_complete6$election, each=2)) %>% 
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

#We can plot both categories
out %>% 
  mutate(Election=rep(ndp_models_complete6$election, each=2))

out %>% 
  mutate(Election=rep(ndp_models_complete6$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted))

out %>% 
  mutate(Election=rep(ndp_models_complete6$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP for degree vs no degree holders")

#save
ggsave(here("Plots", "M6_difference_degree_NDP_vote.png"))

#------------------------------------------------------------------------------------------------
#### M6 Blais Replication Extension (including 2019) - for Liberals Degree ####

liberal_models_complete6
liberal_models_complete6$model
liberal_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
liberal_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[1]')->degree_holder
liberal_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[0]')->no_degree

nrow(degree_holder)
nrow(no_degree)

liberal_models_complete6$election
degree_holder %>% 
  mutate(Election=liberal_models_complete6$election)->degree_holder

#Here is the plotting with vertical errorbars
degree_holder %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high))+
  labs(title="Predicted Probability of voting Liberal for degree holders") 

#save
ggsave(here("Plots", "M6_degree_Liberal_vote.png"))

out
nrow(out)

out %>% 
  mutate(Election=rep(liberal_models_complete6$election, each=2))

#We can plot both categories
out %>% 
  mutate(Election=rep(liberal_models_complete6$election, each=2)) %>% 
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

out %>% 
  mutate(Election=rep(liberal_models_complete6$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted))

out %>% 
  mutate(Election=rep(liberal_models_complete6$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting Liberal for degree vs no degree holders")

#save
ggsave(here("Plots", "M6_difference_degree_Liberal_vote.png"))

#------------------------------------------------------------------------------------------------
#### M6 Blais Replication Extension (including 2019) - for Conservative Degree ####

conservative_models_complete6
conservative_models_complete6$model
conservative_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
conservative_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[1]')->degree_holder
conservative_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[0]')->no_degree

nrow(degree_holder)
nrow(no_degree)

conservative_models_complete6$election
degree_holder %>% 
  mutate(Election=ndp_models_complete6$election)->degree_holder

#Here is the plotting with vertical errorbars
degree_holder %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high))+
  labs(title="Predicted Probability of voting Conservative for degree holders") 

#save
ggsave(here("Plots", "M6_degree_Conservative_vote.png")) 

out
nrow(out)

#We can plot both categories
out %>% 
  mutate(Election=rep(conservative_models_complete6$election, each=2)) %>% 
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

out %>% 
  #Mutate to add election in
  mutate(Election=rep(conservative_models_complete6$election, each=2))

out %>% 
  mutate(Election=rep(conservative_models_complete6$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted))

out %>% 
  mutate(Election=rep(conservative_models_complete6$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting Conservative for degree vs no degree holders")

#save
ggsave(here("Plots", "M6_difference_degree_Conservative_vote.png"))



#------------------------------------------------------------------------------------------------
#### M1 Blais Replication Extension (including 2019) - for NDP Females ####

ndp_models_complete1
ndp_models_complete1$model
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[1]')->female
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[0]')->male

nrow(female)
nrow(male)

ndp_models_complete1$election
female %>% 
  mutate(Election=ndp_models_complete1$election)->female

#Here is the plotting with vertical errorbars
female %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

out
nrow(out)

out %>% 
  #we just use the rep() command with the each=2 argument
  #this repeats the argument in the parenthesis two times. 
  mutate(Election=rep(ndp_models_complete1$election, each=2))

#We can plot both categories
out %>% 
  mutate(Election=rep(ndp_models_complete1$election, each=2)) %>% 
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete1$election, each=2))

out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete1$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted))

##Repeat and go further
out %>% 
  mutate(Election=rep(ndp_models_complete1$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  #then we just filter out the private sector
  filter(x==1) %>% 
  #And plot, note we are using y=difference now
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  #We have to recalculate the confidence intervals here using hte standard error
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP for females vs males")

#save
ggsave(here("Plots", "M1_difference_females_NDP_vote.png"))

#------------------------------------------------------------------------------------------------
#### M1 Blais Replication Extension (including 2019) - for Liberal Females ####

liberal_models_complete1
liberal_models_complete1$model
liberal_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
liberal_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[1]')->female
liberal_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[0]')->male

nrow(female)
nrow(male)

liberal_models_complete1$election
female %>% 
  mutate(Election=liberal_models_complete1$election)->female

#Here is the plotting with vertical errorbars
female %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

out
nrow(out)

#We can plot both categories
out %>% 
  mutate(Election=rep(ndp_models_complete1$election, each=2)) %>% 
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

out %>% 
  mutate(Election=rep(liberal_models_complete1$election, each=2))

out %>% 
  mutate(Election=rep(liberal_models_complete1$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted))

out %>% 
  mutate(Election=rep(ndp_models_complete1$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting Liberal for females vs males")

#save
ggsave(here("Plots", "M1_difference_females_Liberal_vote.png"))

#-----------------------------------------------------------------------------------------------
#### M1 Blais Replication Extension (including 2019) - for Conservative Females ####

conservative_models_complete1
conservative_models_complete1$model. 
conservative_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
conservative_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[1]')->female
conservative_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[0]')->male

nrow(female)
nrow(male)

conservative_models_complete1$election
female %>% 
  mutate(Election=conservative_models_complete1$election)->female

#Here is the plotting with vertical errorbars
female %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

out
nrow(out)

#We can plot both categories
out %>% 
  mutate(Election=rep(conservative_models_complete1$election, each=2)) %>% 
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

out %>% 
  mutate(Election=rep(conservative_models_complete1$election, each=2))

out %>% 
  mutate(Election=rep(conservative_models_complete1$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted))

out %>% 
  mutate(Election=rep(conservative_models_complete1$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  filter(x==1) %>% 
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting Conservative for females vs males")

#save
ggsave(here("Plots", "M1_difference_females_Conservative_vote.png"))


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#### M7 Blais Replication Extension (including 2019)(Degree:Sector interaction) for NDP ####

# Finding Significant Interactions
# interaction between sector and degree. Two of them happen in the most recent years.
ndp_models_complete7 %>% 
  unnest(tidied) %>% 
  filter(term=="degree:sector"&p.value<0.1) 

ndp_models_complete7$model %>% 
  #use map to apply a function to each item in ndp_models_complete7$model
  #the function is ggeffect and we want to get basically all effects, so we specify sector[0, 1] (private and public ) and non-degree and degree
  map(., ggpredict, terms=c('sector', 'degree[0,1]')) %>% 
  bind_rows() %>% 
  #add election, we need to specify each=4 because there, are four combinations of effects
  mutate(election=rep(ndp_models_complete7$election, each=4),
         Sector=Recode(x, "0='Private'; 1='Public'"))  %>% 
  #Plot x=election, y=predicted probabilities, change colour based on x which is sector
ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  #create two panels by degree and non-degree holders
  facet_grid(~group)+
  #point plot
  geom_point()+
  #Set a title
  labs(title="Difference in PP of voting NDP\nfor Public Sector Workers\nDegree and Non-Degree Holders, 1968-2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M7_degree_sector_probabilities_NDP_vote.png"))
#ggsave("M7_degree_sector_probabilities_NDP_vote.png", width=10, height=5)

#-----------------------------------------------------------------------------------------------
#### M7 Blais Replication Extension (including 2019)(Degree:Sector interaction) for Liberals ####

# Finding Significant Interactions
liberal_models_complete7 %>% 
  unnest(tidied) %>%  
  filter(term=="degree:sector"& p.value< 0.05)

liberal_models_complete7$model %>% 
  map(., ggpredict, terms=c('sector', 'degree[0,1]')) %>% 
  bind_rows() %>% 
  mutate(election=rep(liberal_models_complete7$election, each=4),
         Sector=Recode(x, "0='Private'; 1='Public'"))  %>% 
  ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  facet_grid(~group)+
  geom_point()+
  labs(title="Difference in PP of voting Liberal\nfor Public Sector Workers\nDegree and Non-Degree Holders, 1968-2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M7_degree_sector_probabilities_Liberal_vote.png"))

#------------------------------------------------------------------------------------------------

#### M7 Blais Replication Extension (including 2019)(Degree:Sector interaction) for Conservatives ####

#Finding Significant Interactions
conservative_models_complete7 %>% 
  unnest(tidied) %>%  
  filter(term=="degree:sector"& p.value< 0.05)

conservative_models_complete7$model %>% 
  map(., ggpredict, terms=c('sector', 'degree[0,1]')) %>% 
  bind_rows() %>% 
  mutate(election=rep(conservative_models_complete7$election, each=4),
         Sector=Recode(x, "0='Private'; 1='Public'"))  %>% 
  ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  facet_grid(~group)+
  geom_point()+
  labs(title="Difference in PP of voting Conservative\nfor Public Sector Workers\nDegree and Non-Degree Holders, 1968-2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M7_degree_sector_probabilities_Conservative_vote.png"))



#-----------------------------------------------------------------------------------------------
#### M2 Blais Replication Extension (including 2019)(Female:Sector interaction) for NDP ####

#Finding Significant Interactions
ndp_models_complete2 %>% 
  unnest(tidied) %>% 
  filter(term=="female:sector"&p.value<0.1) 

ndp_models_complete2$model %>% 
  map(., ggpredict, terms=c('sector', 'female[0,1]')) %>% 
  bind_rows() %>% 
  mutate(election=rep(ndp_models_complete2$election, each=4),
         Sector=Recode(x, "0='Private'; 1='Public'"))  %>% 
  ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  facet_grid(~group)+
  geom_point()+
  labs(title="Difference in PP of voting NDP\nfor Public Sector Workers\nMales vs Females, 1968-2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M7_female_sector_probabilities_NDP_vote.png"))

#-----------------------------------------------------------------------------------------------
#### M2 Blais Replication Extension (including 2019)(Female:Sector interaction) for Liberals ####

#Finding Significant Interactions
liberal_models_complete2 %>% 
  unnest(tidied) %>% 
  filter(term=="female:sector"&p.value<0.1) 

liberal_models_complete2$model %>% 
  map(., ggpredict, terms=c('sector', 'female[0,1]')) %>% 
  bind_rows() %>% 
  mutate(election=rep(liberal_models_complete2$election, each=4),
         Sector=Recode(x, "0='Private'; 1='Public'"))  %>% 
  ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  facet_grid(~group)+
  geom_point()+
  labs(title="Difference in PP of voting Liberal\nfor Public Sector Workers\nMales vs Females, 1968-2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M7_female_sector_probabilities_Liberal_vote.png"))

#-----------------------------------------------------------------------------------------------
#### M2 Blais Replication Extension (including 2019)(Female:Sector interaction) for Liberals ####

#Finding Significant Interactions
conservative_models_complete2 %>% 
  unnest(tidied) %>% 
  filter(term=="female:sector"&p.value<0.1) 

conservative_models_complete2$model %>% 
  map(., ggpredict, terms=c('sector', 'female[0,1]')) %>% 
  bind_rows() %>% 
  mutate(election=rep(conservative_models_complete2$election, each=4),
         Sector=Recode(x, "0='Private'; 1='Public'"))  %>% 
  ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  facet_grid(~group)+
  geom_point()+
  labs(title="Difference in PP of voting Conservative\nfor Public Sector Workers\nMales vs Females, 1968-2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M7_female_sector_probabilities_Conservative_vote.png"))



#-----------------------------------------------------------------------------------------------
#### M15 Blais Replication Extension (including 2019)(Quebec:Sector interaction) for NDP ####

#Finding Significant Interactions
ndp_models_complete15 %>% 
  unnest(tidied) %>% 
  filter(term=="quebec:sector"&p.value<0.1) 

ndp_models_complete15$model %>% 
  map(., ggpredict, terms=c('sector', 'quebec[0,1]')) %>% 
  bind_rows() %>% 
  mutate(election=rep(ndp_models_complete15$election, each=4),
         Sector=Recode(x, "0='Private'; 1='Public'"))  %>% 
  ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  facet_grid(~group)+
  geom_point()+
  labs(title="Difference in PP of voting NDP\nfor Public Sector Workers\nQuebecois vs ROC, 1968-2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M7_quebec_sector_probabilities_NDP_vote.png"))

#-----------------------------------------------------------------------------------------------
#### M15 Blais Replication Extension (including 2019)(Quebec:Sector interaction) for NDP ####

#Finding Significant Interactions
liberal_models_complete15 %>% 
  unnest(tidied) %>% 
  filter(term=="quebec:sector"&p.value<0.1) 

liberal_models_complete15$model %>% 
  map(., ggpredict, terms=c('sector', 'quebec[0,1]')) %>% 
  bind_rows() %>% 
  mutate(election=rep(liberal_models_complete15$election, each=4),
         Sector=Recode(x, "0='Private'; 1='Public'"))  %>% 
  ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  facet_grid(~group)+
  geom_point()+
  labs(title="Difference in PP of voting Liberal\nfor Public Sector Workers\nQuebecois vs ROC, 1968-2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M7_quebec_sector_probabilities_Liberal_vote.png"))

#-----------------------------------------------------------------------------------------------
#### M15 Blais Replication Extension (including 2019)(Quebec:Sector interaction) for NDP ####

#Finding Significant Interactions
conservative_models_complete15 %>% 
  unnest(tidied) %>% 
  filter(term=="quebec:sector"&p.value<0.1) 

conservative_models_complete15$model %>% 
  map(., ggpredict, terms=c('sector', 'quebec[0,1]')) %>% 
  bind_rows() %>% 
  mutate(election=rep(conservative_models_complete15$election, each=4),
         Sector=Recode(x, "0='Private'; 1='Public'"))  %>% 
  ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  facet_grid(~group)+
  geom_point()+
  labs(title="Difference in PP of voting Conservative\nfor Public Sector Workers\nQuebecois vs ROC, 1968-2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M7_quebec_sector_probabilities_Conservative_vote.png"))

