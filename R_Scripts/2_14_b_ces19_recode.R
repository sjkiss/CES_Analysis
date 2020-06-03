##load recoded ces files
load("Data/recoded_cesdata.Rdata")
library(here)
library(labelled)
library(tidyverse)
#recode Occupation (p52) ***To be recoded later***
look_for(ces19phone, "occupation")
#Encoding(ces19phone$p52)<-"UTF-8"
ces19phone %>% 
  select(p52) %>% 
  mutate(p52=stringr::str_to_lower(p52)) %>% 
  group_by(p52) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  distinct() %>% 
write.csv(., file="Data/unique_occupations.csv", fileEncoding = "UTF-8")

