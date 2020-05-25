library(cesdata)
data("ces19phone")
data("ces19web")
library(labelled)
look_for(ces19phone, "occupation")
library(here)
library(tidyverse)
ces19phone %>% 
  group_by(p52) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  write_csv(., path=here("Results", "occupations.csv"))
