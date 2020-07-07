
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

#So that's it. 

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

#start with the table of models
ndp_models_complete13
#predict words best on the *untidied* models
ndp_models_complete13$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
ndp_models_complete13$model %>% 
  map_dfr(., ggpredict, terms='sector')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
ndp_models_complete13$model %>% 
  map_dfr(., ggpredict, terms='sector')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for public sector workers. 
ndp_models_complete13$model %>% 
  map_dfr(., ggpredict, terms='sector[1]')->public_sector13
#Or we could select the predicted probabilities for private sector
ndp_models_complete13$model %>% 
  map_dfr(., ggpredict, terms='sector[0]')->private_sector13
#Note how the rows change
nrow(public_sector13)
nrow(private_sector13)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
ndp_models_complete13$election
public_sector13 %>% 
  mutate(Election=ndp_models_complete13$election)->public_sector13

#Here is the plotting with vertical errorbars
public_sector13 %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
#We still need to add in the elections, but this is a little trickier, because we need to have 1968, 1968, 1972, 1972, etc. 

out %>% 
  #we just use the rep() command with the each=2 argument
  #this repeats the argument in the parenthesis two times. 
  mutate(Election=rep(ndp_models_complete13$election, each=2))

#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete13$election, each=2)) %>% 
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
  mutate(Election=rep(ndp_models_complete13$election, each=2))

#Here we need to make a new variable that is the difference of each public sector value (1) and each private sector value (0)
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete13$election, each=2)) %>% 
  #the key function is lag. 
  #we mutate the data frame, creating the variable difference that is the lag(predited)
  mutate(difference=predicted-lag(predicted))

##Repeat and go further
out %>% 
  mutate(Election=rep(ndp_models_complete13$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  #then we just filter out the private sector
  filter(x==1) %>% 
  #And plot, note we are using y=difference now
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  #We have to recalculate the confidence intervals here using hte standard error
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP in ROC for public v. private sector")

#save
ggsave(here("Plots", "M13_difference_sectoral_NDP_vote_ROC.png"))

#------------------------------------------------------------------------------------------------

#### M14 Blais Replication Extension (including 2019) (ROC only)& no Degree or Income) for Sector ####

#start with the table of models
ndp_models_complete14
#predict words best on the *untidied* models
ndp_models_complete14$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
ndp_models_complete14$model %>% 
  map_dfr(., ggpredict, terms='sector')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
ndp_models_complete14$model %>% 
  map_dfr(., ggpredict, terms='sector')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for public sector workers. 
ndp_models_complete14$model %>% 
  map_dfr(., ggpredict, terms='sector[1]')->public_sector14
#Or we could select the predicted probabilities for private sector
ndp_models_complete14$model %>% 
  map_dfr(., ggpredict, terms='sector[0]')->private_sector14
#Note how the rows change
nrow(public_sector14)
nrow(private_sector14)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
ndp_models_complete14$election
public_sector14 %>% 
  mutate(Election=ndp_models_complete14$election)->public_sector14

#Here is the plotting with vertical errorbars
public_sector14 %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

#So that's it. 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
#We still need to add in the elections, but this is a little trickier, because we need to have 1968, 1968, 1972, 1972, etc. 

out %>% 
  #we just use the rep() command with the each=2 argument
  #this repeats the argument in the parenthesis two times. 
  mutate(Election=rep(ndp_models_complete14$election, each=2))

#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete14$election, each=2)) %>% 
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
  mutate(Election=rep(ndp_models_complete14$election, each=2))

#Here we need to make a new variable that is the difference of each public sector value (1) and each private sector value (0)
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete14$election, each=2)) %>% 
  #the key function is lag. 
  #we mutate the data frame, creating the variable difference that is the lag(predited)
  mutate(difference=predicted-lag(predicted))

##Repeat and go further
out %>% 
  mutate(Election=rep(ndp_models_complete14$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  #then we just filter out the private sector
  filter(x==1) %>% 
  #And plot, note we are using y=difference now
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  #We have to recalculate the confidence intervals here using hte standard error
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP in ROC (w/o deg & inc) for public v. private sector")


#------------------------------------------------------------------------------------------------
#### M9 Blais Replication Extension (including 2019)(Quebec only) for Sector ####

#start with the table of models
ndp_models_complete9
#predict words best on the *untidied* models
ndp_models_complete9$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
ndp_models_complete9$model %>% 
  map_dfr(., ggpredict, terms='sector')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
ndp_models_complete9$model %>% 
  map_dfr(., ggpredict, terms='sector')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for public sector workers. 
ndp_models_complete9$model %>% 
  map_dfr(., ggpredict, terms='sector[1]')->public_sector9
#Or we could select the predicted probabilities for private sector
ndp_models_complete9$model %>% 
  map_dfr(., ggpredict, terms='sector[0]')->private_sector9
#Note how the rows change
nrow(public_sector9)
nrow(private_sector9)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
ndp_models_complete9$election
public_sector9 %>% 
  mutate(Election=ndp_models_complete9$election)->public_sector9

#Here is the plotting with vertical errorbars
public_sector9 %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

#So that's it. 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
#We still need to add in the elections, but this is a little trickier, because we need to have 1968, 1968, 1972, 1972, etc. 

out %>% 
  #we just use the rep() command with the each=2 argument
  #this repeats the argument in the parenthesis two times. 
  mutate(Election=rep(ndp_models_complete9$election, each=2))

#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete9$election, each=2)) %>% 
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
  mutate(Election=rep(ndp_models_complete9$election, each=2))

#Here we need to make a new variable that is the difference of each public sector value (1) and each private sector value (0)
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete9$election, each=2)) %>% 
  #the key function is lag. 
  #we mutate the data frame, creating the variable difference that is the lag(predited)
  mutate(difference=predicted-lag(predicted))

##Repeat and go further
out %>% 
  mutate(Election=rep(ndp_models_complete9$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  #then we just filter out the private sector
  filter(x==1) %>% 
  #And plot, note we are using y=difference now
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  #We have to recalculate the confidence intervals here using hte standard error
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP in Quebec for public v. private sector")

#save
ggsave(here("Plots", "M9_difference_sectoral_NDP_vote_Quebec.png"))


#------------------------------------------------------------------------------------------------
#### M6 Blais Replication Extension (including 2019) - for NDP Degree ####

#start with the table of models
ndp_models_complete6
#predict words best on the *untidied* models
ndp_models_complete6$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
ndp_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
ndp_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for public sector workers. 
ndp_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[1]')->degree_holder
#Or we could select the predicted probabilities for private sector
ndp_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[0]')->no_degree
#Note how the rows change
nrow(degree_holder)
nrow(no_degree)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
ndp_models_complete6$election
degree_holder %>% 
  mutate(Election=ndp_models_complete6$election)->degree_holder

#Here is the plotting with vertical errorbars
degree_holder %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high))+
  labs(title="Predicted Probability of voting NDP for degree holders") 

#save
ggsave(here("Plots", "M6_degree_NDP_vote.png"))

#So that's it. 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
#We still need to add in the elections, but this is a little trickier, because we need to have 1968, 1968, 1972, 1972, etc. 

out %>% 
  #we just use the rep() command with the each=2 argument
  #this repeats the argument in the parenthesis two times. 
  mutate(Election=rep(ndp_models_complete6$election, each=2))

#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete6$election, each=2)) %>% 
  #The variable x is the public_sector variable, 0 equals private sector, 1 = public sector; y is predicted and we are going to vary the color of the points by x *AS A FACTOR*. If you don't turn it into a factor, it will treat x as a continuous variable wchih will vary the appearance
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  #POint plot; adding the position_dodge argument just moves the points a little to each side for clairty, 
  geom_point(position=position_dodge(width=0.5))+
  #add errorbars, width=0, ymin and ymax to be the confidence intervals, 
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

#Other additions to this.
#We could also go hog-wild and do what Johnston did and subtract degree probability from non-degree probability. 
#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete6$election, each=2))

#Here we need to make a new variable that is the difference of each degree holder (1) and no degree holder value (0)
out %>% 
  #Mutate to add election in
  mutate(Election=rep(ndp_models_complete6$election, each=2)) %>% 
  #the key function is lag. 
  #we mutate the data frame, creating the variable difference that is the lag(predited)
  mutate(difference=predicted-lag(predicted))

##Repeat and go further
out %>% 
  mutate(Election=rep(ndp_models_complete6$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  #then we just filter out the no degree people
  filter(x==1) %>% 
  #And plot, note we are using y=difference now
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  #We have to recalculate the confidence intervals here using hte standard error
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting NDP for degree vs no degree holders")

#save
ggsave(here("Plots", "M6_difference_degree_NDP_vote.png"))

#------------------------------------------------------------------------------------------------
#### M6 Blais Replication Extension (including 2019) - for Liberals Degree ####

#start with the table of models
liberal_models_complete6
#predict words best on the *untidied* models
liberal_models_complete6$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
liberal_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
liberal_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for public sector workers. 
liberal_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[1]')->degree_holder
#Or we could select the predicted probabilities for private sector
liberal_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[0]')->no_degree
#Note how the rows change
nrow(degree_holder)
nrow(no_degree)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
liberal_models_complete6$election
degree_holder %>% 
  mutate(Election=liberal_models_complete6$election)->degree_holder

#Here is the plotting with vertical errorbars
degree_holder %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high))+
  labs(title="Predicted Probability of voting Liberal for degree holders") 

#save
ggsave(here("Plots", "M6_degree_Liberal_vote.png"))

#So that's it. 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
#We still need to add in the elections, but this is a little trickier, because we need to have 1968, 1968, 1972, 1972, etc. 

out %>% 
  #we just use the rep() command with the each=2 argument
  #this repeats the argument in the parenthesis two times. 
  mutate(Election=rep(liberal_models_complete6$election, each=2))

#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(liberal_models_complete6$election, each=2)) %>% 
  #The variable x is the public_sector variable, 0 equals private sector, 1 = public sector; y is predicted and we are going to vary the color of the points by x *AS A FACTOR*. If you don't turn it into a factor, it will treat x as a continuous variable wchih will vary the appearance
  ggplot(., aes(x=Election, y=predicted, col=as.factor(x)))+
  #POint plot; adding the position_dodge argument just moves the points a little to each side for clairty, 
  geom_point(position=position_dodge(width=0.5))+
  #add errorbars, width=0, ymin and ymax to be the confidence intervals, 
  geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high), position=position_dodge(width=0.5))

#Other additions to this.
#We could also go hog-wild and do what Johnston did and subtract degree probability from non-degree probability. 
#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(liberalmodels_complete6$election, each=2))

#Here we need to make a new variable that is the difference of each degree holder (1) and no degree holder value (0)
out %>% 
  #Mutate to add election in
  mutate(Election=rep(liberal_models_complete6$election, each=2)) %>% 
  #the key function is lag. 
  #we mutate the data frame, creating the variable difference that is the lag(predited)
  mutate(difference=predicted-lag(predicted))

##Repeat and go further
out %>% 
  mutate(Election=rep(liberal_models_complete6$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  #then we just filter out the no degree people
  filter(x==1) %>% 
  #And plot, note we are using y=difference now
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  #We have to recalculate the confidence intervals here using hte standard error
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting Liberal for degree vs no degree holders")

#save
ggsave(here("Plots", "M6_difference_degree_Liberal_vote.png"))

#------------------------------------------------------------------------------------------------

#### M6 Blais Replication Extension (including 2019) - for Conservative Degree ####

#start with the table of models
conservative_models_complete6
#predict words best on the *untidied* models
conservative_models_complete6$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
conservative_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
conservative_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for public sector workers. 
conservative_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[1]')->degree_holder
#Or we could select the predicted probabilities for private sector
conservative_models_complete6$model %>% 
  map_dfr(., ggpredict, terms='degree[0]')->no_degree
#Note how the rows change
nrow(degree_holder)
nrow(no_degree)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
conservative_models_complete6$election
degree_holder %>% 
  mutate(Election=ndp_models_complete6$election)->degree_holder

#Here is the plotting with vertical errorbars
degree_holder %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high))+
  labs(title="Predicted Probability of voting Conservative for degree holders") 

#save
ggsave(here("Plots", "M6_degree_Conservative_vote.png")) 

#So that's it. 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
#We still need to add in the elections, but this is a little trickier, because we need to have 1968, 1968, 1972, 1972, etc. 

out %>% 
  #we just use the rep() command with the each=2 argument
  #this repeats the argument in the parenthesis two times. 
  mutate(Election=rep(conservative_models_complete6$election, each=2))

#We can plot both categories
out %>% 
  #Mutate to add election in
  mutate(Election=rep(conservative_models_complete6$election, each=2)) %>% 
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
  mutate(Election=rep(conservative_models_complete6$election, each=2))

#Here we need to make a new variable that is the difference of each public sector value (1) and each private sector value (0)
out %>% 
  #Mutate to add election in
  mutate(Election=rep(conservative_models_complete6$election, each=2)) %>% 
  #the key function is lag. 
  #we mutate the data frame, creating the variable difference that is the lag(predited)
  mutate(difference=predicted-lag(predicted))

##Repeat and go further
out %>% 
  mutate(Election=rep(conservative_models_complete6$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  #then we just filter out the no degree people
  filter(x==1) %>% 
  #And plot, note we are using y=difference now
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  #We have to recalculate the confidence intervals here using hte standard error
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting Conservative for degree vs no degree holders")

#save
ggsave(here("Plots", "M6_difference_degree_Conservative_vote.png"))

#------------------------------------------------------------------------------------------------
#### M1 Blais Replication Extension (including 2019) - for NDP Females ####

#start with the table of models
ndp_models_complete1
#predict words best on the *untidied* models
ndp_models_complete1$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for women
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[1]')->female
#Or we could select the predicted probabilities for men
ndp_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[0]')->male
#Note how the rows change
nrow(female)
nrow(male)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
ndp_models_complete1$election
female %>% 
  mutate(Election=ndp_models_complete1$election)->female

#Here is the plotting with vertical errorbars
female %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

#So that's it. 

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
  labs(title="Difference in PP of voting NDP for females vs males")

#save
ggsave(here("Plots", "M1_difference_females_NDP_vote.png"))

#------------------------------------------------------------------------------------------------
#### M1 Blais Replication Extension (including 2019) - for Liberal Females ####

#start with the table of models
liberal_models_complete1
#predict words best on the *untidied* models
liberal_models_complete1$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
liberal_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
liberal_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for women
liberal_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[1]')->female
#Or we could select the predicted probabilities for men
liberal_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[0]')->male
#Note how the rows change
nrow(female)
nrow(male)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
liberal_models_complete1$election
female %>% 
  mutate(Election=liberal_models_complete1$election)->female

#Here is the plotting with vertical errorbars
female %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

#So that's it. 

#We might go back and plot both the public and private sectors to show the difference between the two.
out
nrow(out)
#We still need to add in the elections, but this is a little trickier, because we need to have 1968, 1968, 1972, 1972, etc. 

out %>% 
  #we just use the rep() command with the each=2 argument
  #this repeats the argument in the parenthesis two times. 
  mutate(Election=rep(liberal_models_complete1$election, each=2))

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
  mutate(Election=rep(liberal_models_complete1$election, each=2))

#Here we need to make a new variable that is the difference of each public sector value (1) and each private sector value (0)
out %>% 
  #Mutate to add election in
  mutate(Election=rep(liberal_models_complete1$election, each=2)) %>% 
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
  labs(title="Difference in PP of voting Liberal for females vs males")

#save
ggsave(here("Plots", "M1_difference_females_Liberal_vote.png"))

#------------------------------------------------------------------------------------------------
#### M1 Blais Replication Extension (including 2019) - for Conservative Females ####

#start with the table of models
conservative_models_complete1
#predict words best on the *untidied* models
conservative_models_complete1$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
conservative_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female')
#Note that it only prints a few rows.
#If we save it, and print the whole thing, you will see the following
conservative_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female')->out
nrow(out)# There are 30 rows, 15 elections plus two predicted values, (non public sector and public sector) for each election. 
#If you want, you can specify particular values
#So for example, we could only select the predicted probabilities for women
conservative_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[1]')->female
#Or we could select the predicted probabilities for men
conservative_models_complete1$model %>% 
  map_dfr(., ggpredict, terms='female[0]')->male
#Note how the rows change
nrow(female)
nrow(male)
#The thing that we are missing is the election years. We have to create one election
#We have them stored in ndp_models_complete
conservative_models_complete1$election
female %>% 
  mutate(Election=conservative_models_complete1$election)->female

#Here is the plotting with vertical errorbars
female %>% 
  ggplot(., aes(x=Election, y=predicted))+geom_point()+geom_errorbar(width=0, aes(ymin=conf.low, ymax=conf.high)) 

#So that's it. 

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
  mutate(Election=rep(conservative_models_complete1$election, each=2)) %>% 
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
  mutate(Election=rep(conservative_models_complete1$election, each=2))

#Here we need to make a new variable that is the difference of each public sector value (1) and each private sector value (0)
out %>% 
  #Mutate to add election in
  mutate(Election=rep(conservative_models_complete1$election, each=2)) %>% 
  #the key function is lag. 
  #we mutate the data frame, creating the variable difference that is the lag(predited)
  mutate(difference=predicted-lag(predicted))

##Repeat and go further
out %>% 
  mutate(Election=rep(conservative_models_complete1$election, each=2)) %>% 
  mutate(difference=predicted-lag(predicted)) %>% 
  #then we just filter out the private sector
  filter(x==1) %>% 
  #And plot, note we are using y=difference now
  ggplot(. , aes(x=Election, y=difference))+
  geom_point()+
  #We have to recalculate the confidence intervals here using hte standard error
  geom_errorbar(width=0, aes(ymin=difference-(1.96*std.error), ymax=difference+(1.96*std.error)))+
  labs(title="Difference in PP of voting Conservative for females vs males")

#save
ggsave(here("Plots", "M1_difference_females_Conservative_vote.png"))


#### M7 Blais Replication Extension (including 2019)(Degree:Sector interaction) for NDP ####

ces %>% 
  #filter out 1965 and 1972 because we don't have sector variables
  filter(election!=1965& election!=1972) %>% 
  #nest the data frames by election year
  nest(-election) %>% 
  #fit one model per election
  #mutate adds the variable interaction model as a column
  #it contains the results of mapping onto the data variable the function lm(ndp~sector+degree+sector+sector:degree)
  #note our interaction term here is sector:degree
  mutate(model=map(data, function(x) lm(ndp~sector+degree+sector:degree, data=x))) %>% 
  mutate(tidied=map(model, tidy)) ->degree_sector


####Finding Significant Interactions ####
#We can unnest the tidied column and find interaction terms with p < 0.05
degree_sector %>% 
  unnest(tidied) %>%  
  filter(term=="sector:degree"& p.value< 0.05)## There are four elections where there is a significant interaction between sector and degree. Two of them happen in the most recent years.


#Normally, I would do something like this in ggeffects package, but I don't quite know technically what ggeffects returns. 
#start with wherever th emodels are stored
head(degree_sector)
degree_sector$model %>% 
  #use map to apply a function to each item in degree_sector$model
  #the function is ggeffect and we want to get basically all effects, so we specify sector[0, 1] (private and public ) and non-degree and degree
  map(., ggpredict, terms=c('sector[0,1]', 'degree[0,1]')) %>% 
  bind_rows() %>% 
  #add election, we need to specify each=4 because ther eare four combinations of effects
  mutate(election=rep(degree_sector$election, each=4), 
         difference=predicted-lag(predicted, n=2)) %>% 
  filter(x==1) %>% 
ggplot(., aes(x=election, y=difference, col=as.factor(group)))+geom_point()+labs(title="Difference in PP of voting NDP\nfor Public Sector Workers\nDegree and Non-Degree Holders, 1968, 2019")
#save
ggsave(here("Plots", "M7_degree_sector_probabilities_NDP_vote.png"))

#------------------------------------------------------------------------------------------------

#### M7 Blais Replication Extension (including 2019)(Degree:Sector interaction) for Liberals ####

ces %>% 
  #filter out 1965 and 1972 because we don't have sector variables
  filter(election!=1965& election!=1972) %>% 
  #nest the data frames by election year
  nest(-election) %>% 
  #fit one model per election
  #mutate adds the variable interaction model as a column
  #it contains the results of mapping onto the data variable the function lm(ndp~sector+degree+sector+sector:degree)
  #note our interaction term here is sector:degree
  mutate(model=map(data, function(x) lm(liberal~sector+degree+sector:degree, data=x))) %>% 
  mutate(tidied=map(model, tidy)) ->degree_sector
degree_sector
#Check what we have
head(degree_sector)

####Finding Significant Interactions ####
#We can unnest the tidied column and find interaction terms with p < 0.05
degree_sector %>% 
  unnest(tidied) %>%  
  filter(term=="sector:degree"& p.value< 0.05)## There are four elections where there is a significant interaction between sector and degree. Two of them happen in the most recent years.

#Normally, I would do something like this in ggeffects package, but I don't quite know technically what ggeffects returns. 
#start with wherever th emodels are stored
head(degree_sector)
degree_sector$model %>% 
  #use map to apply a function to each item in degree_sector$model
  #the function is ggeffect and we want to get basically all effects, so we specify sector[0, 1] (private and public ) and non-degree and degree
  map(., ggeffect, terms=c('sector[0,1]', 'degree[0,1]')) %>% 
  bind_rows() %>% 
  #add election, we need to specify each=4 because ther eare four combinations of effects
  mutate(election=rep(degree_sector$election, each=4)) %>% 
  ggplot(., aes(x=election, y=predicted, col=as.factor(x)))+facet_grid(~group)+geom_point()+labs(title="Marginal Effect of Degree and Sector on vote for Liberals, 1968, 2019")

#### M7 Blais Replication Extension (including 2019)(Degree:Sector interaction) ####
ces %>% 
  #filter out 1965 and 1972 because we don't have sector variables
  filter(election!=1965& election!=1972) %>% 
  #nest the data frames by election year
  nest(-election) %>% 
  #fit one model per election
  #mutate adds the variable interaction model as a column
  #it contains the results of mapping onto the data variable the function lm(ndp~sector+degree+sector+sector:degree)
  #note our interaction term here is sector:degree
  mutate(model=map(data, function(x) lm(ndp~sector+degree+sector:degree, data=x))) %>% 
  mutate(tidied=map(model, tidy)) ->degree_sector
degree_sector
#Check what we have
head(degree_sector)


####Finding Significant Interactions ####
#We can unnest the tidied column and find interaction terms with p < 0.05
degree_sector %>% 
  unnest(tidied) %>%  
  filter(term=="sector:degree"& p.value< 0.05)## There are four elections where there is a significant interaction between sector and degree. Two of them happen in the most recent years.

#Normally, I would do something like this in ggeffects package, but I don't quite know technically what ggeffects returns. 
#start with wherever th emodels are stored
degree_sector$model %>% 
  #use map to apply a function to each item in degree_sector$model
  #the function is ggeffect and we want to get basically all effects, so we specify sector[0, 1] (private and public ) and non-degree and degree
  map(., ggeffect, terms=c('sector[0,1]', 'degree[0,1]')) %>% 
  bind_rows() %>% 
  #add election, we need to specify each=4 because ther eare four combinations of effects
  mutate(election=rep(degree_sector$election, each=4)) %>% 
ggplot(., aes(x=election, y=predicted, col=as.factor(x)))+facet_grid(~group)+geom_point()+labs(title="Marginal Effect of Degree and Sector on vote for NDP, 1968, 2019")

#save
ggsave(here("Plots", "M7_degree_sector_probabilities_Liberal_vote.png"))

#------------------------------------------------------------------------------------------------

#### M7 Blais Replication Extension (including 2019)(Degree:Sector interaction) for Conservatives ####

ces %>% 
  #filter out 1965 and 1972 because we don't have sector variables
  filter(election!=1965& election!=1972) %>% 
  #nest the data frames by election year
  nest(-election) %>% 
  #fit one model per election
  #mutate adds the variable interaction model as a column
  #it contains the results of mapping onto the data variable the function lm(ndp~sector+degree+sector+sector:degree)
  #note our interaction term here is sector:degree
  mutate(model=map(data, function(x) lm(conservative~sector+degree+sector:degree, data=x))) %>% 
  mutate(tidied=map(model, tidy)) ->degree_sector
degree_sector
#Check what we have
head(degree_sector)

#map2 passes two *different* arguments to a function and applies it to each element specified 
#So, we mutate the degree_sector adding average marginal effects (we call them marginals)
#And we pass the model variable (interaction_model) to margins() and we pass the data variable (data) to margins()
degree_sector %>% 
  mutate(marginals = map2(model, data, ~ summary(margins(.x, data = .y))))->degree_sector
#
degree_sector$marginals[1]

####Finding Significant Interactions ####
#We can unnest the tidied column and find interaction terms with p < 0.05
degree_sector %>% 
  unnest(tidied) %>%  
  filter(term=="sector:degree"& p.value< 0.05)## There are four elections where there is a significant interaction between sector and degree. Two of them happen in the most recent years.

#We can plot the marginal ames
#But 
# Take the marginals variable that contains all the marginals
#save in out
degree_sector$marginals ->out
#View to see what has happened
out  
#bind them together
bind_rows(out)->out
#view to see what has happened
out
#add in election variable
out %>%
  #we take it from degree_sector (we made it above)
  #repeate degree_Sector$election, repeating each item twice (each =2)
  mutate(election=rep(degree_sector$election, each=2))->out
out
#now plot the degree and sector 
out %>% 
  ggplot(., aes(x=election, y=AME))+facet_grid(~factor)+geom_point()+geom_errorbar(width=0, aes(ymin=lower, ymax=upper))+labs(title="Average Marginal Effects of Sector and Degree on Voting for Conservatives 1968-2019")

#Normally, I would do something like this in ggeffects package, but I don't quite know technically what ggeffects returns. 
#start with wherever th emodels are stored
head(degree_sector)
degree_sector$model %>% 
  #use map to apply a function to each item in degree_sector$model
  #the function is ggeffect and we want to get basically all effects, so we specify sector[0, 1] (private and public ) and non-degree and degree
  map(., ggeffect, terms=c('sector[0,1]', 'degree[0,1]')) %>% 
  bind_rows() %>% 
  #add election, we need to specify each=4 because ther eare four combinations of effects
  mutate(election=rep(degree_sector$election, each=4)) %>% 
  ggplot(., aes(x=election, y=predicted, col=as.factor(x)))+facet_grid(~group)+geom_point()+labs(title="Marginal Effect of Degree and Sector on vote for Conservatives, 1968, 2019")


#Normally, I would do something like this in ggeffects package, but I don't quite know technically what ggeffects returns. 
#start with wherever th emodels are stored
degree_sector$model %>% 
  #use map to apply a function to each item in degree_sector$model
  #the function is ggeffect and we want to get basically all effects, so we specify sector[0, 1] (private and public ) and non-degree and degree
  map(., ggpredict, terms=c('sector[0,1]', 'degree[0,1]')) %>% 
  bind_rows() %>% 
  #add election, we need to specify each=4 because ther eare four combinations of effects
  mutate(election=rep(degree_sector$election, each=4)) %>% 
  rename(Sector=x)%>%
  ggplot(., aes(x=as.numeric(election), y=predicted, col=as.factor(Sector)))+
  facet_grid(~group)+geom_point()+labs(title="Predicted Probabilities of Degree and Sector on vote for Conservatives, 1968, 2019")+geom_smooth(method="lm", se=F)

#save
ggsave(here("Plots", "M7_degree_sector_probabilities_Conservative_vote.png"))
