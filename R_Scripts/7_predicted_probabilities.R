
#### Predicted Probabilities Sample Code ####

#install this package if necessary
#install.package('ggeffects')
#load
library(ggeffects)

#vignettes
# https://cran.r-project.org/web/packages/ggeffects/vignettes/introduction_plotmethod.html
# https://cran.r-project.org/web/packages/ggeffects/vignettes/ggeffects.html

#------------------------------------------------------------------------------------------------
#### M1 Blais Replication Extension (including 2019) ####

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

#### M13 Blais Replication Extension (including 2019) (ROC only) ####

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

#### M14 Blais Replication Extension (including 2019) (ROC only)& no Degree or Income) ####

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
#### M9 Blais Replication Extension (including 2019)(Quebec only) ####

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
#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#### Interactions ####

#Install margins package - https://www.rdocumentation.org/packages/margins/versions/0.3.23
if (!require("remotes")) {
  install.packages("remotes")
  library("remotes")
}
install_github("leeper/prediction")
install_github("leeper/margins")
library("margins")

#### M7 Blais Replication Extension (including 2019)(Degree:Sector interaction) ####

#start with the table of models
ndp_models_complete7
#predict words best on the *untidied* models
ndp_models_complete7$model
#The key function is ggpredict() and it has to applied to each version of the model
#ggpredict takes a "terms" argument that tells you what predicted values it should return a variable for. 
ndp_models_complete7$model %>% 
  map_dfr(., ggpredict, terms=c("degree", "sector"))->out








