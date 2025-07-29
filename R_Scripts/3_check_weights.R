data("ces21")

library(srvyr)
ces21 %>% 
  filter(!is.na(pes21_weight_general_restricted)) %>% 
as_survey_design(., weights=pes21_weight_general_restricted)->ces21_des
ces21$occupation3
ces21$occupation
ces21_des %>% 
  group_by(occupation3, vote) %>% 
  filter(vote!=0) %>% 
  filter(!is.na(occupation3)) %>% 
  summarize(pct=survey_prop(), 
            N=survey_total(), 
            n=unweighted(n())) %>%
  mutate(unweighted_pct=n/sum(n)) %>% as_factor() %>% view()

