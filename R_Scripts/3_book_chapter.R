### this is the analysis for the book chapter

#### Voting Shares of Working Class
ces %>% 
  select(election, working_class, vote2) %>% 
  group_by(election, working_class, vote2) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=(n/sum(n))*100) %>% 
  filter(working_class==1) %>% 
  filter(vote2!="Green") %>% 
  ggplot(., aes(x=election, y=pct, linetype=fct_relevel(vote2, "Liberal"), group=vote2))+
  geom_point()+
  geom_line()+
  labs(y="Percent", x="Election")+scale_linetype_manual(values=c(1,3,5,6))
ggsave(here("Plots", "book_chapter_working_class_votes_for_parties.png"))

ces %>% 
  select(election, working_class, vote2) %>% 
  group_by(election,  vote2, working_class) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=(n/sum(n))*100) %>% 
  filter(working_class==1) %>% 
  filter(vote2!="Green") %>% 
  ggplot(., aes(x=election, y=pct, linetype=fct_relevel(vote2, "Liberal"), shape=fct_relevel(vote2, "Liberal"),group=vote2))+
  geom_point()+
  geom_line()+
  labs(y="Percent", x="Election", linetype="Vote", shape="Vote")
ggsave("book_chapter_parties_share_working_class_votes.png")
ces$Occupation<-Recode(ces$occupation4, "'Working_Class'='Working Class' ; 
                       'Routine_Nonmanual'='Routine Non-Manual'", levels=c(
                         "Managers", "Professionals", "Self-Employed", "Routine Non-Manual", "Working Class"
                       ))
ces %>% 
  select(election, Occupation, vote2) %>% 
  filter(election>1978) %>% 
  group_by(election, Occupation, vote2) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=n/sum(n)) %>% 
filter(vote2!="Green") %>% 
  filter(!is.na(Occupation)) %>% 
  ggplot(., aes(x=election, y=pct*100, linetype=Occupation, group=Occupation))+
  geom_line()+
  facet_wrap(~vote2)+
  scale_linetype_manual(values=c(2,4,6,8,1))+labs(y="Percent", x="Election", 
                                                  title="Raw Share of Class Voting Per Party")
ggsave(here("Plots", "book_occupation_share_per_party.png"))

ces %>% 
  select(election, Occupation, vote2) %>% 
  filter(election>1978) %>% 
  group_by(election, vote2, Occupation) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(vote2!="Green") %>% 
  filter(!is.na(Occupation)) %>% 
  ggplot(., aes(x=election, y=pct*100, linetype=vote2, group=vote2))+
  geom_line()+
  facet_wrap(~Occupation)+scale_linetype_manual(values=c(1,2,3,4))+
  labs(y="Percent", x="Election", title="Share Of Each Party's Electorate By Class",linetype="Vote")+theme(axis.text.x=element_text(angle=90))
ggsave(here("Plots", "book_party_share_by_occupation.png"), width=8, height=4)


