
#install.packages('manifestoR')
library(manifestoR)
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
  mutate(second_dimension=sum(c_across(c(per104:per107, per109, per501, per503, per601:per605, 
                                         per607:per608, per705))),
         first_dimension=sum(c_across(c(per401:per407, per409, per412:per416, per504:per505,))))->canada

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

#Define Dimension issues
canada %>% 
  rowwise() %>% 
  mutate(culture=sum(c_across(c(per104, per109, per601, per603, per605, per608, - per105:per107, per501, per503, per602, per604, per607, per705))),
         economic=sum(c_across(c(per401, per402, per407, per409, per414, per505, - per403:per406, per409, per412, per413, per415, per416, per504))))->canada

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
    str_detect(partyname, "Bloc")~'Bloc',
    str_detect(partyname, "Green")~'Green',
  ), 
  #modify date
  Date=dmy(edate), 
  #Create ratio
  ratio=left_economic/right_economic)->canada

canada %>% 
  ggplot(., aes(x=Date, y=ratio, col=Party))+geom_line()+
  theme_minimal()+geom_hline(yintercept=1, linetype=2)+
  scale_color_manual(values=c("cyan", "orange", "blue", "darkgreen", "darkred"))+
  labs(title="Economic left-right position in Canadian platforms, 1965-2015")
