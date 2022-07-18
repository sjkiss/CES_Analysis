#census income quintiles master script
source("income_quintiles/income_quintiles_1971.R")
source("income_quintiles/income_quintiles_1981.R")
source("income_quintiles/income_quintiles_1986.R")
source("income_quintiles/income_quintiles_1991.R")
source("income_quintiles/income_quintiles_1996.R")
#This is a check
quintile_average_1971 %>% 
  bind_rows(., quintile_average_1981) %>% 
  bind_rows(., quintile_average_1986) %>% 
  bind_rows(., quintile_average_2001)

#print out the boundaries
df71<-data.frame(Year=rep(1971, 4),boundary= quintiles_1971, quintile=c(seq(1,4,1)))
df81<-data.frame(Year=rep(1981, 4),boundary= quintiles_1981, quintile=c(seq(1,4,1)))
df86<-data.frame(Year=rep(1986, 4),boundary= quintiles_1986, quintile=c(seq(1,4,1)))
df91<-data.frame(Year=rep(1991, 4),boundary= quintiles_1991, quintile=c(seq(1,4,1)))
df96<-data.frame(Year=rep(1996, 4),boundary= quintiles_1996, quintile=c(seq(1,4,1)))
df01<-data.frame(Year=rep(2001, 4), boundary=quintiles_2001, quintile=c(seq(1,4,1)))
bind_rows(df71, df81) %>% 
  bind_rows(., df86) %>% 
  bind_rows(., df91) %>% 
  bind_rows(., df96) %>% 
  bind_rows(., df01) %>% 
  write.csv(., file=here("Results", "quintile_boundaries.csv"))

list.files()
ls()
ls %>% 
starts_with('quintile')

quintile_average_1971
quintile_average_1971 %>% 
bind_rows(., quintile_average_1981) %>%
  bind_rows(., quintile_average_1986) %>% 
  bind_rows(., quintile_average_1991) %>% 
  bind_rows(., quintile_average_1996) %>% 
  bind_rows(., quintile_average_2001) %>% 
  filter(., is.na(quintile)==F) %>% 
  mutate(quintile=Recode(quintile, "5=1; 4=2; 3=3; 2=4; 1=5")) %>% 
  ggplot(., aes(x=year, y=avg, group=quintile))+geom_line(aes(linetype=quintile))+labs(title="Average Real Total Household Income By Quintile, Canada, 1971-2001", y="Average", x="Year")+theme_bw()->income_inequality
income_inequality %>% 
  ggsave(., filename=here("Plots", "average_income_by_quintile.png"))

#Read in from the Statistics Canada Quintile Boundary file 

