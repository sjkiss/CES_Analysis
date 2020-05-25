##Run some diagnostics
#Start with CES  
ces %>% 
  # form groups by election year
  group_by(election) %>% 
  # summarize each variable by summing, removing any missing values
  summarize_all(sum, na.rm=T)

####So somehow, there are a lot of values scattered throughout where the recodes resulted only in cases of zero; i.e. no one is a union member in 1965. And none of the     ### Trouble-shoot the errors
#Start with CES  
ces %>% 
  # form groups by election year
  group_by(election) %>% 
  # summarize each variable by summing the number of values athat are missing (is.na(.))
  summarize_all(funs(sum(is.na(.))))

#Check the union_both variable
##Get sample sizes by election
ces %>% 
  group_by(election) %>% 
  summarize(n=n())

##Check the proportions
## By degree
#always start with the data frame and pipe
ces %>% 
  #from groups by election and degree status
  group_by(election, degree) %>% 
  #summarize those groups making a new variable called n by counting n() the cases in each group
  summarize(n=n()) %>% 
  #mutate, make a new variable called pct by taking n and dividing by the sum of n (sum of each group)
  mutate(pct=n/sum(n)) %>% 
  #filter so that it only includes the degree holders
  filter(degree==1) %>% 
  #plot making election the x-axis, the y the percent. geom_col is basically a bar graph where you have specified the value to be the height of the bars; in this case the percentage
  ggplot(., aes(x=election, y=pct))+geom_col()+labs(title="Percentage of degree holders in CES studies")

ces %>% 
  group_by(election, union) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(union==1) %>% 
  #Percentage of union members in CES Studies
  ggplot(., aes(x=election, y=pct))+geom_col()+labs(title="Percentage of union members in CES Studies")


ces %>% 
  group_by(election, quebec) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(quebec==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_col()+labs(title="Percentage of Quebecers  in CES Studies")

ces %>% 
  group_by(election, male) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(male==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_col()+labs(title="Percentage of men in CES Studies")

ces %>% 
  group_by(election, sector) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(sector==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_col()+labs(title="Percentage of public sector employees in CES Studies")

ces %>% 
  group_by(election, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  ##filter such that vote is NDP 
  filter(vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_col()+labs(title="NDP vote reported in CES studies")

##Check mean age
ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(age, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average age of respondents in ces studies")

