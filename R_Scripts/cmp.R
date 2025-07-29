
#install.packages('manifestoR')
library(manifestoR)
library(here)
library(tidyverse)
# mp_setapikey(key='6d3c2dbd29b1633085cc2089f99128d6')

#Download the data
cmp<-read.csv(file="https://manifesto-project.wzb.eu/down/data/2021a/datasets/MPDataset_MPDS2021a.csv")
#Get Canada
cmp %>% 
  filter(countryname=="Canada")->canada
#Define Dimension issues
canada %>% 
  rowwise() %>% 
  mutate(second_dimension=sum(c_across(c(per101:per110, per201:per204, per301:per305,
                                         per501:per503, per601:per608,per705:per706
                                         ))),
         first_dimension=sum(c_across(c(per401:per416, per504:per507, per701:per704,))))->canada

library(lubridate)
canada %>% 
 # select(edate,partyname, second_dimension, first_dimension) %>% 
  #modify party names for categorization
  mutate(Party=case_when(
    str_detect(partyname, "Cooperative Commonwealth Federation")~'CCF-NDP',
    str_detect(partyname, "Democratic")~'CCF-NDP',
    str_detect(partyname, "Progressive Conservative")~'Conservative',
    str_detect(partyname, "Reform Party of Canada")~'Conservative',
    str_detect(partyname, "Canadian Reform Canadian Alliance")~'Conservative',
    str_detect(partyname, "Conservative")~'Conservative',
    str_detect(partyname, "Liberal")~'Liberal',
    str_detect(partyname, "Social Credit")~'Social Credit',
    str_detect(partyname, "Bloc")~'Bloc',
    str_detect(partyname, "Green")~'Green',
  ), 
#modify date
    Date=dmy(edate), 
#Create ratio
  ratio=first_dimension/second_dimension)->canada

canada %>% 
  #pivot_longer(., cols=c("first_dimension", "second_dimension")) %>% 
  ggplot(., aes(x=Date, y=ratio, col=Party))+geom_line()+
  theme_minimal()+geom_hline(yintercept=1, linetype=2)+
  scale_color_manual(values=c("cyan", "orange", "blue", "darkgreen", "darkred", "black"))+
  labs(title="Ratio of First Dimension to Second Dimension Proportions in Canadian platforms, 1965-2015", caption="A ratio of 1 indicates the platform mentioned first and second dimension issues equally.\nA ratio less than one suggests the platform prioritized second dimension issues.\n A ratio above one suggests the platform privileged first dimension issues.")
ggsave(filename=here("Plots/cmp_first_second_ration.png"))

