library(tidyverse)
library(labelled)
#just check the 04s
ces0411 %>%
  mutate(union_both04=case_when(
    ces04_CPS_S6A==1 | ces04_CPS_S6B==1 ~ 1,
    ces04_CPS_S6A==5 ~ 0,
    ces04_CPS_S6B==5 ~ 0,
    TRUE ~ NA_real_
  #   ces04_CPS_S6A==8 & ces04_CPS_S6B==8 ~ NA_real_,
  #   ces04_CPS_S6A==9 & ces04_CPS_S6B==9 ~ NA_real_,
  #   ces06_CPS_S6A==1 | ces06_CPS_S6B==1 & ces04_rtype1==1 ~ 1,
  #   ces06_CPS_S6A==5 & ces04_rtype1==1 ~ 0,
  #   ces06_CPS_S6B==5 & ces04_rtype1==1 ~ 0,
  ))->ces0411

look_for(ces0411, "employ")
ces0411 %>% 
  filter(str_detect(ces0411$survey, "PES04")) %>% 
  select(ces04_CPS_S4, ces04_CPS_S6A, ces04_CPS_S6B) %>% 
  group_by(ces04_CPS_S4, ces04_CPS_S6A,ces04_CPS_S6B) %>% 
  summarize(n=n()) %>% 
  print(n=33)
ces0411 %>% 
  select(ces04_rtype1, ces04_CPS_S4, ces06_CPS_S6A, ces06_CPS_S6B) %>% 
  filter(str_detect(ces0411$survey, "PES06")) %>% 
  group_by(ces04_rtype1, ces04_CPS_S4, ces06_CPS_S6A, ces06_CPS_S6B) %>% 
  summarize(n=n()) %>% 
  print(n=54)
ces04_