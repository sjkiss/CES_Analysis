#### Class logistic models 1965-2019 ####

#Run master file to load up data
library(stargazer)
library(broom)
library(nnet)

# Recode leaders and party ratings to (0-1)
ces$liberal_ratings<-(ces$liberal_rating /100)
ces$conservative_ratings<-(ces$conservative_rating /100)
ces$ndp_ratings<-(ces$ndp_rating /100)
ces$bloc_ratings<-(ces$bloc_rating /100)
ces$green_ratings<-(ces$green_rating /100)
ces$liberal_leaders<-(ces$liberal_leader /100)
ces$conservative_leaders<-(ces$conservative_leader /100)
ces$ndp_leaders<-(ces$ndp_leader /100)
ces$bloc_leaders<-(ces$bloc_leader /100)
ces$green_leaders<-(ces$green_leader /100)
table(ces$liberal_leaders)
table(ces$conservative_leaders)
table(ces$ndp_leaders, ces$election)


#This combines the NDP and Bloc into a left-vote 
ces %>%
  mutate(vote2=case_when(
    vote==1~"Liberal",
    vote==2~"Conservative",
    vote==3~"NDP", 
    vote==4~"BQ", 
    vote==5~"Green",
    vote==0~NA_character_
  ))->ces
ces$vote2<-factor(ces$vote2, levels=c("Conservative", "Liberal", "NDP", "BQ", "Green"))
#ces$vote2<-Recode(as_factor(ces$vote), "'; ;1='Liberal' ; 2='Conservative' ; 3='Left' ; 5='Green'", levels=c('Conservative', 'Liberal', 'Left', 'Green'))
table(ces$vote2, ces$election)
levels(ces$vote2)


# Recode Vote3 for big 3 parties
ces$vote3<-Recode(ces$vote, "2=1; 1=2; 3=3; else=NA")
ces$vote3<-Recode(as.factor(ces$vote3), "2='Liberal' ; 1='Conservative' ; 3='NDP'", levels=c('Conservative', 'Liberal', 'NDP'))
levels(ces$vote3)
table(ces$vote3, ces$election)

# Recode a Left vs Right Vote (NDP/Liberal=1 ; Con=0)
ces$left_vs_right<-Recode(ces$vote, "1=1; 2=0; 3=1; else=NA")
val_labels(ces$left_vs_right)<-c(Left=1, Right=0)
table(ces$left_vs_right, ces$election)

# Recode a Left vs Right Vote (Green/NDP/Liberal=1 ; Con=0)
ces$left_vs_right2<-Recode(ces$vote, "1=1; 2=0; 3=1; 5=1; else=NA")
table(ces$left_vs_right2, ces$election)

# Recode market vs moral variables as dummies
#Recode market liberalism = pro_market
table(ces$market_liberalism, ces$election)
ces$pro_market<-Recode(ces$market_liberalism, "0:0.375=0; 0.625:1=1; else=NA")
table(ces$pro_market, ces$election)

#Recode moral traditionalism = authoritarian
table(ces$traditionalism, ces$election)
ces$authoritarian<-Recode(ces$traditionalism, "0:0.49=0; 0.51:1=1; else=NA")
table(ces$authoritarian, ces$election)

#Recode immigration rates = anti_immigrant
table(ces$immigration_rates, ces$election)
ces$anti_immigrant<-Recode(ces$immigration_rates, "0=0; 1=1; else=NA")
table(ces$anti_immigrant, ces$election)

#Recode enviro = anti_enviro
table(ces$enviro, ces$election)
ces$anti_enviro<-Recode(ces$enviro, "0:0.25=0; 0.75:1=1; else=NA")
table(ces$anti_enviro, ces$election)

#Recode gay_rights = anti_gay
table(ces$gay_rights, ces$election)
ces$anti_gay<-Recode(ces$gay_rights, "0:0.25=0; 0.75:1=1; else=NA")
table(ces$anti_gay, ces$election)

#Recode abortion = anti_abortion
table(ces$abortion, ces$election)
ces$anti_abortion<-Recode(ces$abortion, "0:0.25=0; 0.75:1=1; else=NA")
table(ces$anti_abortion, ces$election)

#Recode crime = tough_crime
table(ces$crime, ces$election)
ces$tough_crime<-Recode(ces$crime, "0:0.46=0; 0.54:1=1; else=NA")
table(ces$tough_crime, ces$election)

#Recode death_penalty = capital_punishment
table(ces$death_penalty, ces$election)
ces$capital_punishment<-Recode(ces$death_penalty, "0:0.25=0; 0.75:1=1; else=NA")
table(ces$capital_punishment, ces$election)

#Recode redistribution = anti-redistribution
table(ces$redistribution, ces$election)
ces$anti_redistribution<-Recode(ces$redistribution, "0:0.5=1; 0.75:1=0; else=NA")
table(ces$anti_redistribution, ces$election)

#Recode traditionalism2 = authoritarian2
table(ces$traditionalism2, ces$election)
ces$authoritarian2<-Recode(ces$traditionalism2, "0:0.375=0; 0.625:1=1; else=NA")
table(ces$authoritarian2, ces$election)

#Recode quebec accom = quebec_accommodation
table(ces$quebec_accom, ces$election)
ces$quebec_accommodation<-Recode(ces$quebec_accom, "0:0.25=0; 0.75:1=1; else=NA")
table(ces$quebec_accommodation, ces$election)

#By election
summary(ces)

#Count missing values
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  View()


#### Basic party models ####

# NDP 1965-2019
ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_models_1

stargazer(ndp_models_1$model, type="html", out=here("Tables", "NDP_Models_1965_2019_1.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="NDP Models 1965-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Liberal 1965-2019
ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=x)),
         tidied=map(model, tidy), vote=rep('Liberal', nrow(.)))->liberal_models_1

stargazer(liberal_models_1$model, type="html", out=here("Tables", "liberal_Models_1965_2019_1.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Liberal Models 1965-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Conservative 1965-2019
ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=x)),
         tidied=map(model, tidy), vote=rep('Conservative', nrow(.)))->conservative_models_1

stargazer(conservative_models_1$model, type="html", out=here("Tables", "conservative_Models_1965_2019_1.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Conservative Models 1965-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# NDP 1979-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_models_2

stargazer(ndp_models_2$model, type="html", out=here("Tables", "NDP_Models_1979_2019_1.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="NDP Models 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Liberal 1979-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=x)),
         tidied=map(model, tidy), vote=rep('Liberal', nrow(.)))->liberal_models_2

stargazer(liberal_models_2$model, type="html", out=here("Tables", "liberal_Models_1979_2019_1.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Liberal Models 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Conservative 1979-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=x)),
         tidied=map(model, tidy), vote=rep('Conservative', nrow(.)))->conservative_models_2

stargazer(conservative_models_2$model, type="html", out=here("Tables", "conservative_Models_1979_2019_1.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Conservative Models 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# # NDP 1979-2019 + Leadership
# 
# ces %>% 
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
#   nest(variables=-election) %>% 
#   mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+ndp_leaders+liberal_leaders+conservative_leaders, data=x)),
#          tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_models_6
# 
# stargazer(ndp_models_6$model, type="html", out=here("Tables", "NDP_Models_1979_2019_leadership_1.html"),
#           column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
#           star.cutoffs=c(0.05), title="NDP Models leadership 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# # Liberal 1979-2019 + Leadership
# ces %>% 
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
#   nest(variables=-election) %>% 
#   mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+ndp_leaders+liberal_leaders+conservative_leaders, data=x)),
#          tidied=map(model, tidy), vote=rep('Liberal', nrow(.)))->liberal_models_6
# 
# stargazer(liberal_models_6$model, type="html", out=here("Tables", "liberal_Models_1979_2019_leadership_1.html"),
#           column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
#           star.cutoffs=c(0.05), title="Liberal Models leadership 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# # Conservative 1979-2019 + Leadership
# ces %>% 
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
#   nest(variables=-election) %>% 
#   mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+ndp_leaders+liberal_leaders+conservative_leaders, data=x)),
#          tidied=map(model, tidy), vote=rep('Conservative', nrow(.)))->conservative_models_6
# 
# stargazer(conservative_models_6$model, type="html", out=here("Tables", "conservative_Models_1979_2019__leadership_1.html"),
#           column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
#           star.cutoffs=c(0.05), title="Conservative Models leadership 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Market vs Moral party models ####

# NDP 1988-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_models_3

stargazer(ndp_models_3$model, type="html", out=here("Tables", "NDP_Models_1988_2019_1.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="NDP Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Liberal 1988-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy), vote=rep('Liberal', nrow(.)))->liberal_models_3

stargazer(liberal_models_3$model, type="html", out=here("Tables", "liberal_Models_1988_2019_1.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Liberal Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Conservative 1988-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=x)),
         tidied=map(model, tidy), vote=rep('Conservative', nrow(.)))->conservative_models_3

stargazer(conservative_models_3$model, type="html", out=here("Tables", "conservative_Models_1988_2019_1.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Conservative Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# NDP 1988-2019 degree x redistro
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_models_4

stargazer(ndp_models_4$model, type="html", out=here("Tables", "NDP_Models_1988_2019_2.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="NDP Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Liberal 1988-2019 degree x redistro
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=x)),
         tidied=map(model, tidy), vote=rep('Liberal', nrow(.)))->liberal_models_4

stargazer(liberal_models_4$model, type="html", out=here("Tables", "liberal_Models_1988_2019_2.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Liberal Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Conservative 1988-2019 degree x redistro
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=x)),
         tidied=map(model, tidy), vote=rep('Conservative', nrow(.)))->conservative_models_4

stargazer(conservative_models_4$model, type="html", out=here("Tables", "conservative_Models_1988_2019_2.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Conservative Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# NDP 1988-2019 degree x redistro
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_models_5

stargazer(ndp_models_5$model, type="html", out=here("Tables", "NDP_Models_1988_2019_3.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="NDP Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Liberal 1988-2019 degree x redistro
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=x)),
         tidied=map(model, tidy), vote=rep('Liberal', nrow(.)))->liberal_models_5

stargazer(liberal_models_5$model, type="html", out=here("Tables", "liberal_Models_1988_2019_3.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Liberal Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Conservative 1988-2019 degree x redistro
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=x)),
         tidied=map(model, tidy), vote=rep('Conservative', nrow(.)))->conservative_models_5

stargazer(conservative_models_5$model, type="html", out=here("Tables", "conservative_Models_1988_2019_3.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Conservative Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# NDP 1979-2019 without Occupation
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2), data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_models_6

stargazer(ndp_models_6$model, type="html", out=here("Tables", "NDP_Models_1979_2019_2.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="NDP Models 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Liberal 1979-2019 without Occupation
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2), data=x)),
         tidied=map(model, tidy), vote=rep('Liberal', nrow(.)))->liberal_models_6

stargazer(liberal_models_6$model, type="html", out=here("Tables", "liberal_Models_1979_2019_2.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Liberal Models 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Conservative 1979-2019 without Occupation
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2), data=x)),
         tidied=map(model, tidy), vote=rep('Conservative', nrow(.)))->conservative_models_6

stargazer(conservative_models_6$model, type="html", out=here("Tables", "conservative_Models_1979_2019_2.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2000", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Conservative Models 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Left vs Right Models ####
# # NDP vs Right 1965-2019
# ces %>% 
#   filter(election!=2000) %>%
#   nest(variables=-election) %>% 
#   mutate(model=map(variables, function(x) lm(ndp_vs_right~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=x)),
#          tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_vs_right_models_1
# 
# stargazer(ndp_vs_right_models_1$model, type="html", out=here("Tables", "NDP_vs_right_Models_1965_2019_1.html"),
#           column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
#           star.cutoffs=c(0.05), title="NDP vs Right Models 1965-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# # NDP vs Liberals 1965-2019
# ces %>% 
#   filter(election!=2000) %>%
#   nest(variables=-election) %>% 
#   mutate(model=map(variables, function(x) lm(ndp_vs_liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=x)),
#          tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_vs_liberal_models_1
# 
# stargazer(ndp_vs_liberal_models_1$model, type="html", out=here("Tables", "NDP_vs_liberal_Models_1965_2019_1.html"),
#           column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
#           star.cutoffs=c(0.05), title="NDP vs Right Models 1965-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# # NDP vs Right 1979-2019
# ces %>% 
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
#   nest(variables=-election) %>% 
#   mutate(model=map(variables, function(x) lm(ndp_vs_right~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=x)),
#          tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_vs_right_models_2
# 
# stargazer(ndp_vs_right_models_2$model, type="html", out=here("Tables", "NDP_vs_right_Models_1979_2019_1.html"),
#           column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
#           star.cutoffs=c(0.05), title="NDP vs Right Models 1965-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# # NDP vs Liberals 1979-2019
# ces %>% 
#   filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
#   nest(variables=-election) %>% 
#   mutate(model=map(variables, function(x) lm(ndp_vs_liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=x)),
#          tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->ndp_vs_liberal_models_2
# 
# stargazer(ndp_vs_liberal_models_2$model, type="html", out=here("Tables", "NDP_vs_liberal_Models_1979_2019_1.html"),
#           column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
#           star.cutoffs=c(0.05), title="NDP vs Right Models 1965-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))
# 
# #Plot coefficients
# ndp_vs_right_ROC_models_1 %>% 
#   unnest(tidied) %>% 
#   filter(term=="degree"|term=="income"|term=="sector"|term=="occupation4Working_Class") %>% 
#   ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)+facet_wrap(~term)
# ggsave(here("Plots", "ROC_degree_ndp_vs_right_coefficients.png"))
# 
# ndp_vs_right_QC_models_1 %>% 
#   unnest(tidied) %>% 
#   filter(term=="degree"|term=="income"|term=="sector"|term=="occupation4Working_Class") %>%   ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree, QC")+geom_smooth(method="loess", se=F)+facet_wrap(~term)
# ggsave(here("Plots", "QC_degree_ndp_vs_right_coefficients.png"))


# Left vs Right 1965-2019
ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(left_vs_right~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=x)),
         tidied=map(model, tidy), vote=rep('Left', nrow(.)))->left_vs_right_models_1

stargazer(left_vs_right_models_1$model, type="html", out=here("Tables", "Left_vs_Right_Models_1965_2019_1.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Left vs Right Models 1965-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Left vs Right 1979-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(left_vs_right~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=x)),
         tidied=map(model, tidy), vote=rep('Left', nrow(.)))->left_vs_right_models_2

stargazer(left_vs_right_models_2$model, type="html", out=here("Tables", "Left_vs_Right_Models_1979_2019_1.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Left vs Right Models 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Left vs Right 1988-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(left_vs_right~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2, data=x)),
         tidied=map(model, tidy), vote=rep('Left', nrow(.)))->left_vs_right_models_3

stargazer(left_vs_right_models_3$model, type="html", out=here("Tables", "Left_vs_Right_Models_1988_2019_1.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Left vs Right Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Left vs Right 1988-2019 w Greens
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(left_vs_right2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2, data=x)),
         tidied=map(model, tidy), vote=rep('Left', nrow(.)))->left_vs_right_models_4

stargazer(left_vs_right_models_4$model, type="html", out=here("Tables", "Left_vs_Right_Models_1988_2019_2.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Left vs Right Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Left vs Right 1988-2019 degree x redistro
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(left_vs_right~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+degree:redistribution, data=x)),
         tidied=map(model, tidy), vote=rep('Left', nrow(.)))->left_vs_right_models_4

stargazer(left_vs_right_models_4$model, type="html", out=here("Tables", "Left_vs_Right_interaction_Models_1988_2019_1.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Left vs Right Interaction Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# Left vs Right 1988-2019 degree x moral trad
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(left_vs_right~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+degree:traditionalism2, data=x)),
         tidied=map(model, tidy), vote=rep('Left', nrow(.)))->left_vs_right_models_5

stargazer(left_vs_right_models_5$model, type="html", out=here("Tables", "Left_vs_Right_interaction_Models_1988_2019_2.html"),
          column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), title="Left vs Right Interaction Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#### Pooled Models ####

ces %>% 
  filter(election!=2000)->ces.1
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000)->ces.2
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000)->ces.3
ces %>% 
  filter(election<1998)->ces.4
ces %>% 
  filter(election>2003)->ces.5

# 1965-2019
m1<-lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=ces.1, family="binomial")
m2<-lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=ces.1, family="binomial")
m3<-lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=ces.1, family="binomial")

# 1979-2019
m4<-lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=ces.2, family="binomial")
m5<-lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=ces.2, family="binomial")
m6<-lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=ces.2, family="binomial")

# 1988-2019
m7<-lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=ces.3, family="binomial")
m8<-lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=ces.3, family="binomial")
m9<-lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=ces.3, family="binomial")

#Pre-2004 Degree

m37<-lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=ces.4, family="binomial")
m38<-lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=ces.4, family="binomial")
m39<-lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=ces.4, family="binomial")

#Post-2004 Degree

m40<-lm(ndp~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=ces.5, family="binomial")
m41<-lm(liberal~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=ces.5, family="binomial")
m42<-lm(conservative~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=ces.5, family="binomial")

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, type="html", out=here("Tables", "Pooled_models.html"))
stargazer(m37, m38, m39, m40, m41, m42, type="html", out=here("Tables", "Pooled_models_degree.html"), column.labels = c('1965-1997', '1965-1997', '1965-1997', '2004-2019','2004-2019', '2004-2019'))

# 1988-2019 attitudinal models
m10<-lm(ndp~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.3, family="binomial")
m11<-lm(liberal~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.3, family="binomial")
m12<-lm(conservative~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.3, family="binomial")

m13<-lm(ndp~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.3, family="binomial")
m14<-lm(liberal~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.3, family="binomial")
m15<-lm(conservative~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.3, family="binomial")

m16<-lm(ndp~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.3, family="binomial")
m17<-lm(liberal~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.3, family="binomial")
m18<-lm(conservative~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.3, family="binomial")

stargazer(m10, m11, m12, m13, m14, m15, m16, m17, m18, type="html", out=here("Tables", "Pooled_attitudinal_models_1988_2019.html"))

m19<-lm(ndp~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.4, family="binomial")
m20<-lm(liberal~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.4, family="binomial")
m21<-lm(conservative~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.4, family="binomial")

m22<-lm(ndp~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.4, family="binomial")
m23<-lm(liberal~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.4, family="binomial")
m24<-lm(conservative~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.4, family="binomial")

m25<-lm(ndp~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.4, family="binomial")
m26<-lm(liberal~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.4, family="binomial")
m27<-lm(conservative~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.4, family="binomial")

stargazer(m19, m20, m21, m22, m23, m24, m25, m26, m27, type="html", out=here("Tables", "Pooled_attitudinal_models_1988_1997.html"))

m28<-lm(ndp~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.5, family="binomial")
m29<-lm(liberal~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.5, family="binomial")
m30<-lm(conservative~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.5, family="binomial")

m31<-lm(ndp~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.5, family="binomial")
m32<-lm(liberal~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.5, family="binomial")
m33<-lm(conservative~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.5, family="binomial")

m34<-lm(ndp~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.5, family="binomial")
m35<-lm(liberal~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.5, family="binomial")
m36<-lm(conservative~as.factor(region2)+age+male+as.factor(religion2)+degree+income+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.5, family="binomial")

stargazer(m29, m29, m30, m31, m32, m33, m34, m35, m36, type="html", out=here("Tables", "Pooled_attitudinal_models_2004_2019.html"))

stargazer(m19, m28, m22, m31, m25, m34, type="html", out=here("Tables", "NDP_Pooled_Models.html"), column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'))
stargazer(m20, m29, m23, m32, m26, m35, type="html", out=here("Tables", "Liberal_Pooled_Models.html"), column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'))
stargazer(m21, m30, m24, m33, m27, m36, type="html", out=here("Tables", "Conservative_Pooled_Models.html"), column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'))

#Without controls displaying
stargazer(m19, m28, m22, m31, m25, m34, type="html", out=here("Tables", "NDP_Pooled_Models_2.html"), omit=c(1,2,3,4,5,6,7,8), column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'))
stargazer(m20, m29, m23, m32, m26, m35, type="html", out=here("Tables", "Liberal_Pooled_Models_2.html"), omit=c(1,2,3,4,5,6,7,8), column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'))
stargazer(m21, m30, m24, m33, m27, m36, type="html", out=here("Tables", "Conservative_Pooled_Models_2.html"), omit=c(1,2,3,4,5,6,7,8), column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'))

#### Multinomial Models (Big 3 parties) ####
# 1965-2019
ces %>% 
  filter(election!=2000 & vote2!="Green" & vote2!="BQ")->ces.out
multinom1<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2)+as.factor(election), data = subset(ces.out))


multinom1<-list(multinom1)
map(multinom1, function(x) rep(nrow(x$fitted.values), 3)) %>% 
  unlist()->n_obs
stargazer(multinom1, digits=2, single.row=T, out=here("Tables", "multinom1_1965_2019.html"), type="html", title="Multinomial Logistic Regression of Party Vote On Class, 1965-2019", add.lines=list(c("N", n_obs)))

# Class interactions
ces %>% 
  filter(election!=2000 & vote2!="Green" & vote2!="BQ")->ces.out
multinom2<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2)+as.numeric(election)+as.factor(occupation2):as.numeric(election), data = subset(ces.out))
multinom3<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2)+as.numeric(election)+income:as.numeric(election), data = subset(ces.out))
multinom4<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2)+as.numeric(election)+degree:as.numeric(election), data = subset(ces.out))

multinoms1<-list(multinom1, multinom2, multinom3, multinom4)
map(multinoms1, function(x) rep(nrow(x$fitted.values), 3)) %>% 
  unlist()->n_obs
stargazer(multinoms1, digits=2, single.row=T, out=here("Tables", "multinoms_1965_2019.html"), type="html", title="Multinomial Logistic Regression of Party Vote On Class, 1965-2019", add.lines=list(c("N", n_obs)))

# 1979-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 &  election!=2000 & vote2!="Green" & vote2!="BQ")->ces.out
multinom5<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+as.factor(election), data = subset(ces.out))

multinom5<-list(multinom5)
map(multinoms5, function(x) rep(nrow(x$fitted.values), 3)) %>% 
  unlist()->n_obs
stargazer(multinom2, digits=2, single.row=T, out=here("Tables", "multinom2_1979_2019.html"), type="html", title="Multinomial Logistic Regression of Party Vote On Class, 1979-2019", add.lines=list(c("N", n_obs)))

# Class interactions
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 &election!=2000 & vote2!="Green" & vote2!="BQ")->ces.out
multinom6<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+as.numeric(election)+as.factor(occupation4):as.numeric(election), data = subset(ces.out))
multinom7<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+as.numeric(election)+income:as.numeric(election), data = subset(ces.out))
multinom8<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+as.numeric(election)+degree:as.numeric(election), data = subset(ces.out))

multinoms2<-list(multinom5, multinom6, multinom7, multinom8)
map(multinoms2, function(x) rep(nrow(x$fitted.values), 3)) %>% 
  unlist()->n_obs
stargazer(multinoms2, digits=2, single.row=T, out=here("Tables", "multinoms_1979_2019.html"), type="html", title="Multinomial Logistic Regression of Party Vote On Class, 1979-2019", add.lines=list(c("N", n_obs)))

# 1988-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 &  election!=1979 & election!=1980 & election!=1984 & election!=2000 & vote2!="Green" & vote2!="BQ")->ces.out
multinom9<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+as.factor(election)+redistribution+market_liberalism+traditionalism2, data = subset(ces.out))
multinom10<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+as.factor(election)+redistribution+market_liberalism+traditionalism2+degree:redistribution, data = subset(ces.out))
multinom11<-multinom(vote2~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+as.factor(election)+redistribution+market_liberalism+traditionalism2+degree:traditionalism2, data = subset(ces.out))

multinom9<-list(multinom9, multinom10, multinom11)
map(multinom9, function(x) rep(nrow(x$fitted.values), 3)) %>% 
  unlist()->n_obs
stargazer(multinom9, digits=2, single.row=T, out=here("Tables", "multinom1_1988_2019.html"), type="html", title="Multinomial Logistic Regression of Party Vote On Class, 1988-2019", add.lines=list(c("N", n_obs)))

# By election 1965-2019
ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) multinom(vote3~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation2), data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->multinom_models_12

stargazer(multinom_models_12$model, type="html", out=here("Tables", "Multinom_Models_1965_2019_1.html"),
          column.labels=c("1965", "1965", "1968", "1968", "1972", "1972", "1974", "1974", "1979", "1979", "1980", "1980", "1984", "1984", "1988", "1988", "1993", "1993", "1997", "1997", "2004", "2004", "2006", "2006", "2008", "2008", "2011", "2011", "2015", "2015", "2019", "2019"), 
          star.cutoffs=c(0.05), title="Multinom Models 1965-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# By election 1979-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 &  election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) multinom(vote3~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4), data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->multinom_models_13

stargazer(multinom_models_13$model, type="html", out=here("Tables", "Multinom_Models_1979_2019_1.html"),
          column.labels=c("1979", "1979", "1980", "1980", "1984", "1984", "1988", "1988", "1993", "1993", "1997", "1997", "2004", "2004", "2006", "2006", "2008", "2008", "2011", "2011", "2015", "2015", "2019", "2019"), 
          star.cutoffs=c(0.05), title="Multinom Models 1979-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# By election 1988-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 &  election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) multinom(vote3~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2, data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->multinom_models_14

stargazer(multinom_models_14$model, type="html", out=here("Tables", "Multinom_Models_1988_2019_1.html"),
          column.labels=c("1988", "1988", "1993", "1993", "1997", "1997", "2004", "2004", "2006", "2006", "2008", "2008", "2011", "2011", "2015", "2015", "2019", "2019"), 
          star.cutoffs=c(0.05), title="Multinom Models 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# By election 1988-2019 degree x redistro
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 &  election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) multinom(vote3~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+degree:redistribution, data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->multinom_models_15

stargazer(multinom_models_15$model, type="html", out=here("Tables", "Multinom_Models_1988_2019_2.html"),
          column.labels=c("1988", "1988", "1993", "1993", "1997", "1997", "2004", "2004", "2006", "2006", "2008", "2008", "2011", "2011", "2015", "2015", "2019", "2019"), 
          star.cutoffs=c(0.05), title="Multinom Models degree redistro interaction 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

# By election 1988-2019 degree x trad
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 &  election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) multinom(vote3~as.factor(region2)+age+male+degree+income+as.factor(religion2)+as.factor(occupation4)+redistribution+market_liberalism+traditionalism2+degree:traditionalism2, data=x)),
         tidied=map(model, tidy), vote=rep('NDP', nrow(.)))->multinom_models_16

stargazer(multinom_models_16$model, type="html", out=here("Tables", "Multinom_Models_1988_2019_3.html"),
          column.labels=c("1988", "1988", "1993", "1993", "1997", "1997", "2004", "2004", "2006", "2006", "2008", "2008", "2011", "2011", "2015", "2015", "2019", "2019"), 
          star.cutoffs=c(0.05), title="Multinom Models degree trad interaction 1988-2019", notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))


#### Redistribution models ####

table(ces$pro_redistribution, ces$election)
table(ces$redistribution, ces$election)
table(ces$working_class, ces$election)
table(ces$working_class4, ces$election)
library(knitr)
library(kableExtra)

# ces93$working_class<-Recode(ces93$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
# ces97$working_class<-Recode(ces97$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
# ces0411$working_class04<-Recode(ces0411$occupation04, "4:5=1; 3=0; 2=0; 1=0; else=NA")
# ces0411$working_class06<-Recode(ces0411$occupation06, "4:5=1; 3=0; 2=0; 1=0; else=NA")
# ces0411$working_class08<-Recode(ces0411$occupation08, "4:5=1; 3=0; 2=0; 1=0; else=NA")
# ces0411$working_class11<-Recode(ces0411$occupation11, "4:5=1; 3=0; 2=0; 1=0; else=NA")
# ces15phone$working_class<-Recode(ces15phone$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
# ces19phone$working_class<-Recode(ces19phone$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
table(ces93$working_class)
table(ces97$working_class)
table(ces0411$working_class04)
table(ces0411$working_class06)
table(ces0411$working_class08)
table(ces0411$working_class11)
table(ces15phone$working_class)
table(ces19phone$working_class)

#M1 NDP ROC
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~income+degree+sector+union_both+working_class2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_ROC_redistribution_models_1

#M1 NDP QC
ces %>% 
  filter(quebec==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~income+degree+sector+union_both+working_class2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_QC_redistribution_models_1

#M1 Con ROC
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~income+degree+sector+union_both+working_class2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->conservative_ROC_redistribution_models_1

#M1 Con QC
ces %>% 
  filter(quebec==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~income+degree+sector+union_both+working_class2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->conservative_QC_redistribution_models_1

#M2 NDP ROC WC interaction
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~income+degree+sector+union_both+working_class2+redistribution+working_class2:redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_ROC_redistribution_models_2

#M2 NDP QC WC interaction
ces %>% 
  filter(quebec==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~income+degree+sector+union_both+working_class2+redistribution+working_class2:redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_QC_redistribution_models_2

#M2 Con ROC WC interaction
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~income+degree+sector+union_both+working_class2+redistribution+working_class2:redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->conservative_ROC_redistribution_models_2

#M2 Con QC WC interaction
ces %>% 
  filter(quebec==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~income+degree+sector+union_both+working_class2+redistribution+working_class2:redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->conservative_QC_redistribution_models_2

stargazer(ndp_ROC_redistribution_models_1$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_ROC_redistribution_models_1.html"))
stargazer(ndp_QC_redistribution_models_1$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_QC_redistribution_models_1.html"))
stargazer(conservative_ROC_redistribution_models_1$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_ROC_redistribution_models_1.html"))
stargazer(conservative_QC_redistribution_models_1$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_QC_redistribution_models_1.html"))
stargazer(ndp_ROC_redistribution_models_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_ROC_redistribution_inter_models_2.html"))
stargazer(ndp_QC_redistribution_models_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_QC_redistribution_inter_models_2.html"))
stargazer(conservative_ROC_redistribution_models_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_ROC_redistribution_inter_models_2.html"))
stargazer(conservative_QC_redistribution_models_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_QC_redistribution_inter_models_2.html"))


#### Redistribution descriptives ####

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of respondents in ces studies")

ces %>% 
  group_by(election, working_class) %>% 
  filter(!is.na(working_class)) %>%
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(pro_redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average pro-redistribution of respondents in ces studies")

ces %>% 
  group_by(election, working_class) %>% 
  filter(!is.na(working_class)) %>%
  summarize(avg_age=mean(pro_redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average pro-redistribution of WC respondents in ces studies")

ces %>%
  filter(!is.na(redistribution)) %>%
  group_by(working_class, election) %>%
  summarize(mean_redistribution = mean(redistribution))

ces %>%
  filter(!is.na(pro_redistribution)) %>%
  group_by(working_class, election) %>%
  summarize(mean_pro_redistribution = mean(pro_redistribution))

## Share of Pro-redistribution Working Class members voting NDP
ces %>% 
  group_by(election, pro_redistribution, working_class, ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting NDP")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_NDP.png"))

## Share of Pro-redistribution Working Class members voting Conservative
ces %>% 
  group_by(election, pro_redistribution, working_class, conservative) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(conservative)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(conservative==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting Conservative")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_Conservative.png"))

## Share of Pro-redistribution Working Class members voting Liberal
ces %>% 
  group_by(election, pro_redistribution, working_class, liberal) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(liberal)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(liberal==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting Liberal")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_Liberal.png"))

# Working Class redistribution by year
ces88 %>%
  select(working_class, redistribution, pro_redistribution) %>% 
  group_by(working_class) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

ces93 %>%
  select(working_class, redistribution, pro_redistribution) %>% 
  group_by(working_class) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

ces97 %>%
  select(working_class, redistribution, pro_redistribution) %>% 
  group_by(working_class) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

ces0411 %>%
  select(working_class04, redistribution04, pro_redistribution04) %>% 
  group_by(working_class04) %>%
  summarise_at(vars(redistribution04, pro_redistribution04), mean, na.rm=T)

ces0411 %>%
  select(working_class06, redistribution06, pro_redistribution06) %>% 
  group_by(working_class06) %>%
  summarise_at(vars(redistribution06, pro_redistribution06), mean, na.rm=T)

ces0411 %>%
  select(working_class08, redistribution08, pro_redistribution08) %>% 
  group_by(working_class08) %>%
  summarise_at(vars(redistribution08, pro_redistribution08), mean, na.rm=T)

ces0411 %>%
  select(working_class11, redistribution11, pro_redistribution11) %>% 
  group_by(working_class11) %>%
  summarise_at(vars(redistribution11, pro_redistribution11), mean, na.rm=T)

ces15phone %>%
  select(working_class, redistribution, pro_redistribution) %>% 
  group_by(working_class) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

ces19phone %>%
  select(working_class, redistribution, pro_redistribution) %>% 
  group_by(working_class) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

# Working Class voting pro-redistribution by election
ces %>%
  select(election, working_class, pro_redistribution, liberal, conservative, ndp) %>% 
  group_by(election, working_class, pro_redistribution) %>%
  summarise_at(vars(liberal, conservative, ndp), mean, na.rm=T) %>% 
  as.data.frame() %>%
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro_redistribution_Working_Class_Vote_by_election.html"))

#### Working Class descriptives ####

#Share of Working class voting NDP
ces %>% 
  group_by(election, working_class, ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting NDP")
ggsave(here("Plots", "NDP_working_class_vote.png"))

#Share of Working class voting Liberal
ces %>% 
  group_by(election, working_class, liberal) %>% 
  summarize(n=n()) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(liberal)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(liberal==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Liberal")
ggsave(here("Plots", "Liberal_working_class_vote.png"))

#Share of Working class voting Conservative
ces %>% 
  group_by(election, working_class, conservative) %>% 
  summarize(n=n()) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(conservative)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(conservative==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Conservative")
ggsave(here("Plots", "Conservative_working_class_vote.png"))

#Share of Working class voting Other
ces %>% 
  group_by(election, working_class, other) %>% 
  summarize(n=n()) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(other)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(other==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Other")
ggsave(here("Plots", "Other_working_class_vote.png"))

#Party Vote Shares of Working Class
#This was your code

# ces %>% 
#   group_by(election, working_class, vote) %>% 
#   summarize(n=n()) %>% 
#   mutate(pct=n/sum(n)) %>%
#   filter(working_class==1 & (vote<4 & vote>0)) %>% 
#   ggplot(.,aes(x=as.numeric(election), y=pct))+
#   geom_point()+
#   geom_smooth(method="lm", se=F)+
#   facet_grid(~as_factor(vote))+
#   labs(title="Share of Working Class voting for political parties over time")
# ggsave(here("Plots", "Party_shares_working_class_vote.png"))

#My modifications
ces %>% 
  group_by(election, working_class, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(working_class==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Working Class voting for political parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_working_class_vote.png"))

#Percent of NDP Voters Working Class
ces %>% 
  group_by(election, vote, working_class) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(working_class==1 & vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="NDP Voter % that are Working Class")
ggsave(here("Plots", "NDP_Voters_Working_Class_Percent.png"))

#Percent of Liberal Voters Working Class
ces %>% 
  group_by(election, vote, working_class) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(working_class==1 & vote==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Liberal Voter % that are Working Class")
ggsave(here("Plots", "Lib_Voters_Working_Class_Percent.png"))

#Percent of Conservative Voters Working Class
ces %>% 
  group_by(election, vote, working_class) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(working_class==1 & vote==2) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Conservative Voter % that are Working Class")
ggsave(here("Plots", "Con_Voters_Working_Class_Percent.png"))

#### Market vs Moral descriptive graphs ####

#Redistribution
ces %>% 
  group_by(election, pro_redistribution, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(pro_redistribution==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Pro Redistribution voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_pro_redistribution_vote.png"))

ces %>% 
  group_by(election, pro_redistribution, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(pro_redistribution==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Pro Redistribution Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_pro_redistribution_WC_vote.png"))

ces %>% 
  group_by(election, anti_redistribution, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_redistribution==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti Redistribution voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_redistribution_vote.png"))

ces %>% 
  group_by(election, anti_redistribution, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_redistribution==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti Redistribution Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_redistribution_WC_vote.png"))

#Market liberalism
ces %>% 
  group_by(election, pro_market, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(pro_market==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Pro Market voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_pro_market_vote.png"))

ces %>% 
  group_by(election, pro_market, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(pro_market==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Pro Market Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_pro_market_WC_vote.png"))

#Traditionalism
ces %>% 
  group_by(election, authoritarian, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(authoritarian==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Authoritarian voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_authoritarian_vote.png"))

ces %>% 
  group_by(election, authoritarian, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(authoritarian==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Authoritarian Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_authoritarian_WC_vote.png"))

#Immigration
ces %>% 
  group_by(election, anti_immigrant, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_immigrant==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti-Immigrant voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_immigrant_vote.png"))

ces %>% 
  group_by(election, anti_immigrant, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_immigrant==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti-Immigrant Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_immigrant_WC_vote.png"))

#Environment
ces %>% 
  group_by(election, anti_enviro, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_enviro==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Jobs over Environment voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_enviro_vote.png"))

ces %>% 
  group_by(election, anti_enviro, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_enviro==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Jobs over Environment Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_enviro_WC_vote.png"))

#Gay Rights
ces %>% 
  group_by(election, anti_gay, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_gay==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti-Gay voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_gay_vote.png"))

ces %>% 
  group_by(election, anti_gay, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_gay==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti-Gay Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_gay_WC_vote.png"))

#Abortion
ces %>% 
  group_by(election, anti_abortion, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_abortion==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti-Abortion voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_abortion_vote.png"))

ces %>% 
  group_by(election, anti_abortion, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(anti_abortion==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti-Abortion Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_anti_abortion_WC_vote.png"))

#Crime
ces %>% 
  group_by(election, tough_crime, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(tough_crime==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Tough on Crime voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_tough_crime_vote.png"))

ces %>% 
  group_by(election, tough_crime, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(tough_crime==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Tough on Crime Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_tough_crime_WC_vote.png"))

#Death Penalty
ces %>% 
  group_by(election, capital_punishment, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(capital_punishment==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Capital Punishment voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_capital_punishment_vote.png"))

ces %>% 
  group_by(election, capital_punishment, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(capital_punishment==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Capital Punishment Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_capital_punishment_WC_vote.png"))

#Authoritarianism (gender roles, immigration, gay rights, crime)
ces %>% 
  group_by(election, authoritarian2, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(authoritarian2==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Authoritarian2 voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_authoritarian2_vote.png"))

ces %>% 
  group_by(election, authoritarian2, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(authoritarian2==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Authoritarian2 Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_authoritarian2_WC_vote.png"))

#Quebec Accommodation  
ces %>% 
  group_by(election, quebec_accommodation, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(quebec_accommodation==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti-Quebec Accommodation voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_quebec_accom_vote.png"))

ces %>% 
  group_by(election, quebec_accommodation, working_class3, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>%
  filter(quebec_accommodation==1 & working_class3==1 & (vote<4 & vote>0)) %>% 
  ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
  labs(title="Share of Anti-Quebec Accommodation Working Class voting for parties over time", x="Year", y="Percent")
ggsave(here("Plots", "Party_shares_quebec_accom_WC_vote.png"))


#### Variable descriptives - mean ratings ####

#By class entire sample
ces %>%
  select(occupation4, market_liberalism, traditionalism, immigration_rates, gay_rights, abortion, enviro, crime, death_penalty, redistribution, traditionalism2) %>%
  pivot_longer(-occupation4,values_to=c("Score"), names_to=c("Variable")) %>% 
  group_by(occupation4, Variable) %>% 
  summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sqrt(sd)/n) %>% 
  ggplot(., aes(x=Variable, y=Average, col=occupation4))+geom_jitter()

#By variable (first whole pop then working class)
ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>%
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(market_liberalism, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average market liberalism of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>% 
  summarize(avg_age=mean(market_liberalism, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average market liberalism of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(traditionalism, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average traditionalism of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>% 
  summarize(avg_age=mean(traditionalism, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average traditionalism of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(immigration_rates, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average Immigration preference of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>% 
  summarize(avg_age=mean(immigration_rates, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average Immigration preference WC of respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(enviro, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average Environment preference of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>%
  summarize(avg_age=mean(enviro, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average Environment preference of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(crime, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average crime preference of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>%
  summarize(avg_age=mean(crime, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average crime preference of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(gay_rights, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average gay rights preference of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>%
  summarize(avg_age=mean(gay_rights, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average gay rights preference of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(abortion, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average abortion preference of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>%
  summarize(avg_age=mean(abortion, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average abortion preference of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(death_penalty, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average death penalty preference of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>%
  summarize(avg_age=mean(death_penalty, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average death penalty preference of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(authoritarianism, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average authoritarian preference of respondents in ces studies")

ces %>% 
  group_by(election, working_class3) %>% 
  filter(working_class3==1) %>%
  summarize(avg_age=mean(authoritarianism, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average authoritarian preference of WC respondents in ces studies")


#### Natural vs Unnatural voting ####

#Create variables
ces$left<-Recode(ces$vote, "1=1; 3=1; 5=1; 0=0; 2=0; 4=0; else=NA")

table(ces$left, ces$election)
table(ces$ndp, ces$election)
ces$right<-Recode(ces$vote, "2=1; 0=0; 1=0; 3:5=0; else=NA")
table(ces$right, ces$election)
val_labels(ces$right)<-c(Right=1, Other=0)
ces$upper_class<-Recode(ces$occupation, "1:2=1; 3:5=0; else=NA")
table(ces$upper_class)
ces$upper_class2<-Recode(ces$occupation3, "1:2=1; 3:6=0; else=NA")
table(ces$upper_class2)

table(ces$employment, ces$election)

#Using working_class (6 categories)
#recode Natural voting
ces %>% 
  mutate(natural=case_when(
#    vote==1 & working_class3==1 ~1,
    vote==3 & working_class3==1 ~1,
    vote==5 & working_class3==1 ~1,
    vote==2 & upper_class==1 ~1,
  ))->ces
val_labels(ces$natural)<-c(natural=1)
#checks
val_labels(ces$natural)
table(ces$natural)

#recode Unnatural voting
ces %>% 
  mutate(unnatural=case_when(
    vote==2 & working_class3==1 ~1,
#    vote==1 & upper_class==1 ~1,
    vote==3 & upper_class==1 ~1,
    vote==5 & upper_class==1 ~1,
  ))->ces
val_labels(ces$unnatural)<-c(unnatural=1)
#checks
val_labels(ces$unnatural)
table(ces$unnatural)

#Unnatural Model 1
ces %>% 
  filter(unnatural==1 & quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel1_ROC

ces %>% 
  filter(unnatural==1 & quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel1_QC

ces %>% 
  filter(unnatural==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel1

#Unnatural Model 2
ces %>% 
  filter(unnatural==1 & quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel2_ROC

ces %>% 
  filter(unnatural==1 & quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel2_QC

ces %>% 
  filter(unnatural==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel2

#Unnatural Model 3
ces %>% 
  filter(unnatural==1 & quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income+redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel3_ROC

ces %>% 
  filter(unnatural==1 & quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income+redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel3_QC

ces %>% 
  filter(unnatural==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income+redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel3

#Print models
stargazer(unnatmodel1_ROC$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model1_ROC.html"))
stargazer(unnatmodel1_QC$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model1_QC.html"))
stargazer(unnatmodel1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model1.html"))
stargazer(unnatmodel2_ROC$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model2_ROC.html"))
stargazer(unnatmodel2_QC$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model2_QC.html"))
stargazer(unnatmodel2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model2.html"))
stargazer(unnatmodel3_ROC$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model3_ROC.html"))
stargazer(unnatmodel3_QC$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model3_QC.html"))

stargazer(unnatmodel3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model3.html"), covariate.labels=c("Employment", "Degree", "Income", "Redistribution", "Moral Traditionalism"))

#Natural Model 1
ces %>% 
  filter(natural==1 & quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel1_ROC

ces %>% 
  filter(natural==1 & quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel1_QC

ces %>% 
  filter(natural==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel1

#Natural Model 2
ces %>% 
  filter(natural==1 & quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel2_ROC

ces %>% 
  filter(natural==1 & quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel2_QC

ces %>% 
  filter(natural==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel2

#Natural Model 3
ces %>% 
  filter(natural==1 & quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income+redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel3_ROC

ces %>% 
  filter(natural==1 & quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income+redistribution+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel3_QC

ces %>% 
  filter(natural==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income+redistribution+traditionalism2, data=x, family="binomial", )),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel3

#Print models
stargazer(natmodel1_ROC$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model1_ROC.html"))
stargazer(natmodel1_QC$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model1_QC.html"))
stargazer(natmodel1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model1.html"))
stargazer(natmodel2_ROC$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model2_ROC.html"))
stargazer(natmodel2_QC$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model2_QC.html"))
stargazer(natmodel2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model2.html"))
stargazer(natmodel3_ROC$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model3_ROC.html"))
stargazer(natmodel3_QC$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model3_QC.html"))
stargazer(natmodel3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model3.html"),covariate.labels=c("Employment", "Degree", "Income", "Redistribution", "Moral Traditionalism"))

#Same model 3's but with Market Liberalism & Immigration added
ces %>% 
  filter(unnatural==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income+redistribution+traditionalism2+market_liberalism+immigration_rates, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->unnatmodel4

ces %>% 
  filter(natural==1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(left~employment+degree+income+redistribution+traditionalism2+market_liberalism+immigration_rates, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Left', nrow(.)))->natmodel4


map(natmodel4$model, PseudoR2) %>% 
  unlist() %>% 
  bind_cols(natmodel4, .)->natmodel4
  natmodel4
natmodel4<-rename(natmodel4, "McFadden"=6)
stargazer(unnatmodel4$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model4.html"), covariate.labels=c("Employment", "Degree", "Income", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates"), add.lines=list("PseudoR2"=c("PseudoR2", round(unnatmodel4$McFadden,2))), dep.var.caption=c("Cultural Voting"), dep.var.labels = c("Vote NDP"), single.row = T, digits=2)
stargazer(natmodel4$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model4.html"), covariate.labels=c("Employment", "Degree", "Income", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates"), add.lines=list("PseudoR2"=c("PseudoR2", round(natmodel4$McFadden,2))), dep.var.labels = c("Vote NDP"), dep.var.caption=c("Class Voting"), single.row=T, digits=2)
natmodel4
detach(package:DescTools)


stargazer(unnatmodel4$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "unnatural_model4.html"))
stargazer(natmodel4$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "natural_model4.html"))


#### Class Voting ####
#M1 NDP class - basic
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region3)+male+age+income+degree+union_both+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_ROC_1

ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~male+age+income+degree+union_both+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_QC_1

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region2)+quebec+male+age+income+degree+union_both+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_1

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region2)+quebec+male+age+income+degree+union_both+working_class3, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_WC_1

#M2 NDP class - market & moral added
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region3)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_ROC_2

ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_QC_2

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region2)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_2

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_WC_2

#M3 NDP class - full
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region3)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_ROC_3

ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+immigration_rates, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_QC_3

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region2)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_3

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_WC_3

#Quebec Accommodation
ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000 & election!=2019) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime+quebec_accom, data=x, family="binomial")),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_class_models_accom_3

#Print models
stargazer(ndp_class_models_ROC_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_ROC_1.html"))
stargazer(ndp_class_models_QC_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_QC_1.html"))
stargazer(ndp_class_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_1.html"))
stargazer(ndp_class_models_WC_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_WC_1.html"))
stargazer(ndp_class_models_ROC_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_ROC_2.html"))
stargazer(ndp_class_models_QC_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_QC_2.html"))
stargazer(ndp_class_models_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_2.html"))
stargazer(ndp_class_models_WC_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_WC_2.html"))
stargazer(ndp_class_models_ROC_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_ROC_3.html"))
stargazer(ndp_class_models_QC_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_QC_3.html"))
stargazer(ndp_class_models_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_3.html"))
stargazer(ndp_class_models_WC_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_WC_3.html"))
stargazer(ndp_class_models_accom_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015"), type="html", out=here("Tables", "NDP_class_models_accom_3.html"))

#M1 Conservative class - basic
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~as.factor(region3)+male+age+income+degree+union_both+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_ROC_1

ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~male+age+income+degree+union_both+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_QC_1

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~as.factor(region2)+male+age+income+degree+union_both+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_1

#M2 Conservative class - market & trad added
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~as.factor(region3)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_ROC_2

ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_QC_2

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~as.factor(region2)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_2

#M3 Conservative class - full
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~as.factor(region3)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_ROC_3

ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+immigration_rates, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_QC_3

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~as.factor(region2)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_3

#Quebec Accommodation
ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000 & election!=2019) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) glm(conservative~as.factor(region2)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime+quebec_accom, data=x, family="binomial")),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.)))->con_class_models_accom_3

#Print models
stargazer(con_class_models_ROC_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_ROC_1.html"))
stargazer(con_class_models_QC_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_QC_1.html"))
stargazer(con_class_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_1.html"))
stargazer(con_class_models_ROC_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_ROC_2.html"))
stargazer(con_class_models_QC_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_QC_2.html"))
stargazer(con_class_models_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_2.html"))
stargazer(con_class_models_ROC_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_ROC_3.html"))
stargazer(con_class_models_QC_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_QC_3.html"))
stargazer(con_class_models_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_3.html"))
stargazer(con_class_models_accom_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015"), type="html", out=here("Tables", "Con_class_models_accom_3.html"))

#M1 Liberal class - basic
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~as.factor(region3)+male+age+income+degree+union_both+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_ROC_1

ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~male+age+income+degree+union_both+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_QC_1

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~as.factor(region2)+male+age+income+degree+union_both+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_1

#M2 Liberal class - market & trad added
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~as.factor(region3)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_ROC_2

ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_QC_2

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~as.factor(region2)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_2

#M3 Liberal class - full
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~as.factor(region3)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_ROC_3

ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+immigration_rates, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_QC_3

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~as.factor(region2)+male+age+income+degree+union_both+occupation4+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_3

#Quebec Accommodation
ces %>%
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000 & election!=2019) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) glm(liberal~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2+redistribution+immigration_rates+enviro+crime+quebec_accom, data=x, family="binomial")),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.)))->liberal_class_models_accom_3

#Print models
stargazer(liberal_class_models_ROC_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_ROC_1.html"))
stargazer(liberal_class_models_QC_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_QC_1.html"))
stargazer(liberal_class_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_1.html"))
stargazer(liberal_class_models_ROC_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_ROC_2.html"))
stargazer(liberal_class_models_QC_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_QC_2.html"))
stargazer(liberal_class_models_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_2.html"))
stargazer(liberal_class_models_ROC_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_ROC_3.html"))
stargazer(liberal_class_models_QC_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_QC_3.html"))
stargazer(liberal_class_models_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_3.html"))
stargazer(liberal_class_models_accom_3$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015"), type="html", out=here("Tables", "liberal_class_models_accom_3.html"))

#### Interactions ####

#NDP
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2+working_class3:traditionalism2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_inter_1

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2+working_class3:redistribution+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_class_models_inter_2

#Conservative
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2+working_class3:traditionalism2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_inter_1

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(conservative~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2+working_class3:redistribution+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Conservative', nrow(.)))->con_class_models_inter_2

#Liberal
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2+working_class3:traditionalism2+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_inter_1

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=1979 & election!=1980 & election!=1984 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(liberal~as.factor(region2)+male+age+income+degree+union_both+working_class3+market_liberalism+traditionalism2+working_class3:redistribution+redistribution, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('Liberal', nrow(.)))->liberal_class_models_inter_2

#Print models
stargazer(ndp_class_models_inter_1$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_inter_1.html"))
stargazer(ndp_class_models_inter_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_class_models_inter_2.html"))
stargazer(con_class_models_inter_1$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_inter_1.html"))
stargazer(con_class_models_inter_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "Con_class_models_inter_2.html"))
stargazer(liberal_class_models_inter_1$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_inter_1.html"))
stargazer(liberal_class_models_inter_2$model, column.labels=c("1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "liberal_class_models_inter_2.html"))

names(ces)
#### 
names(ces)
table(ces$occupation4)
ces$vote


#### Party Vote Coefficients of Working Class and Union ####

# 1965-2019
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+male+age+income+degree+union_both+as.factor(religion2)+working_class3, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete1

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+male+age+income+degree+union_both+as.factor(religion2)+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete1

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+male+age+income+degree+union_both+as.factor(religion2)+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete1

stargazer(ndp_models_complete1$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1979_2019_1.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete1$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1979_2019_1.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete1$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1979_2019_1.html"),
          column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#Join all parties and plot class and Union coefficients
ndp_models_complete1 %>% 
  bind_rows(., liberal_models_complete1) %>% 
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="working_class3"| term=="union_both") %>% 
  mutate(term=Recode(term, "'working_class3'='Working Class'; 'union_both'='Union'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Union")))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class and Union on Party Vote 1979-2019", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.25,0.25))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_WC_Union_all_parties.png"))

#Join all parties and plot degree and income coefficients
ndp_models_complete1 %>% 
  bind_rows(., liberal_models_complete1) %>% 
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="degree"| term=="income") %>% 
  mutate(term=Recode(term, "'degree'='Education'; 'income'='Income'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Income")))+
  geom_point()+
  labs(title="OLS Coefficients of Education and Income on Party Vote 1979-2019", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.25,0.25))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_Degree_Income_all_parties.png"))

#Join all parties and plot Degree and Class coefficients
ndp_models_complete1 %>% 
  bind_rows(., liberal_models_complete1) %>% 
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="degree"| term=="working_class3") %>% 
  mutate(term=Recode(term, "'degree'='Education'; 'working_class3'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Working Class")))+
  geom_point()+
  labs(title="OLS Coefficients of Education and Working Class on Party Vote 1979-2019", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.25,0.25))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_Degree_WC_all_parties.png"))

# 1965-2019
ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~as.factor(region2)+male+age+income+degree+union_both+as.factor(religion2)+working_class, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete2

ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~as.factor(region2)+male+age+income+degree+union_both+as.factor(religion2)+working_class, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete2

ces %>% 
  filter(election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~as.factor(region2)+male+age+income+degree+union_both+as.factor(religion2)+working_class, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete2

stargazer(ndp_models_complete2$model, 
          type="html", 
          out=here("Tables", "NDP_Models_1965_2019_1.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="NDP Models 1979-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(liberal_models_complete2$model, 
          type="html", 
          out=here("Tables", "liberal_Models_1965_2019_1.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Liberal Models 1965-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

stargazer(conservative_models_complete2$model, 
          type="html", 
          out=here("Tables", "conservative_Models_1965_2019_1.html"),
          column.labels=c("1965", "1968", "1972", "1974", "1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), 
          star.cutoffs=c(0.05), 
          title="Conservative Models 1965-2019", 
          notes=paste("Printed on", as.character(Sys.time()), "by", Sys.getenv("USERNAME")))

#Join all parties and plot degree and income coefficients
ndp_models_complete2 %>% 
  bind_rows(., liberal_models_complete2) %>% 
  bind_rows(., conservative_models_complete2) %>%
  unnest(tidied) %>% 
  filter(term=="degree"| term=="income") %>% 
  mutate(term=Recode(term, "'degree'='Education'; 'income'='Income'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Income")))+
  geom_point()+
  labs(title="OLS Coefficients of Education and Income on Party Vote 1965-2019", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.25,0.25))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_Degree_Income_all_parties2.png"))

#Join all parties and plot Degree and Class coefficients
ndp_models_complete2 %>% 
  bind_rows(., liberal_models_complete2) %>% 
  bind_rows(., conservative_models_complete2) %>%
  unnest(tidied) %>% 
  filter(term=="degree"| term=="working_class") %>% 
  mutate(term=Recode(term, "'degree'='Education'; 'working_class'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote, alpha=fct_relevel(term, "Working Class")))+
  geom_point()+
  labs(title="OLS Coefficients of Education and Working Class on Party Vote 1965-2019", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.25,0.25))+
  scale_color_manual(values=c("blue", "red", "orange"))+
  facet_grid(rows=vars(vote), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_Degree_WC_all_parties2.png"))

#### Descriptives Combined For All Classes ####
ces %>% 
  select(election, occupation4,  vote, crime, market_liberalism,traditionalism2, authoritarianism, quebec_accom, crime) %>% 
  pivot_longer(cols=crime:quebec_accom) %>% 
  group_by(name) %>% 
  mutate(pro=case_when(
    value>0.5~ 1,
    TRUE ~ 0
  )) %>% 
  group_by(election, name, pro, vote) %>% 
  filter(election>1984) %>% 
  filter(!is.na(vote)) %>% 
  filter(vote > 0 &vote<5) %>% 
summarize(n=n()) %>% 
  mutate(percent=n/sum(n)) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(vote)))+geom_col(position="dodge")+facet_wrap(~name)+scale_fill_manual(values=c('red', 'darkblue', 'orange', 'lightblue' ))
#### 

ces %>% 
  select(election, occupation4, vote, crime, market_liberalism, traditionalism2, redistribution, immigration_rates, quebec_accom, enviro, crime) %>% 
  pivot_longer(cols=crime:enviro) %>% 
  group_by(name) %>% 
  mutate(pro=case_when(
    value>0.5~ 1,
    TRUE ~ 0
  )) %>% 
  group_by(election, name, pro, vote) %>% 
  filter(election>1984 &election!=2000) %>% 
  filter(!is.na(vote)) %>% 
  filter(vote > 0 &vote<5) %>% 
  filter(occupation4=="Working_Class") %>% 
  summarize(n=n()) %>% 
  mutate(percent=n/sum(n)) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(vote)))+geom_col(position="dodge")+facet_wrap(~name)+scale_fill_manual(values=c('red', 'darkblue', 'orange', 'lightblue' ))


#### Average Scores For Working Class Versus Average ####

ces %>% 
  select(election,  working_class4, vote, crime, redistribution, immigration_rates, market_liberalism,traditionalism2, enviro) %>%
  rename(`Crime`=`crime`,Redistribution=redistribution, `Immigration Rates`=immigration_rates, `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2, Environmentalism=enviro) %>% 
  mutate(Redistribution=skpersonal::revScale(Redistribution, reverse=T)) %>% 
  pivot_longer(cols=4:9) %>% 
  #filter(election>1984 &election!=2000) %>% 
  group_by(election, working_class4, name) %>% 
  summarize(average=mean(value, na.rm=T)) %>% 
  arrange(election, name, working_class4) %>%
  group_by(election, name) %>%
  mutate(difference=average-lag(average)) ->average_views_scores
table(ces$election, ces$working_class4)

average_views_scores %>% 
  filter(working_class4==1& election> 1984) %>%
  ggplot(., aes(y=election, x=difference))+geom_point()+facet_grid(~fct_relevel(name, "Crime", "Immigration Rates", "Moral Traditionalism", "Market Liberalism", "Redistribution"))+theme(axis.text.x=element_text(angle=90))+labs(caption="Score above 0 means working class has more conservative views than the rest of thep opulation.")+scale_y_discrete(limits=rev)+xlim(c(-0.15,0.15))+geom_vline(xintercept=0, linetype=2)
average_views_scores %>% 
  ungroup() %>% 
  mutate(working_class4=recode_factor(working_class4, "0"="Other", "1"="Working Class")) %>% 
  rename(`Working Class`=working_class4) %>%
  filter(election>1984 &election!=2000) %>% 
  ggplot(., aes(y=election, x=average, col=`Working Class`))+geom_point()+facet_wrap(~fct_relevel(name, "Crime", "Immigration Rates", "Moral Traditionalism", "Market Liberalism", "Redistribution"), nrow=2)+theme(axis.text.x=element_text(angle=90))+scale_y_discrete(limits=rev)+
  geom_vline(xintercept=0.5, linetype=2)+scale_color_grey(start=0.8, end=0.2, name="Class")+labs(y="Election", x="Average")
ggsave(here("Plots", "average_scores_raw_class_population.png"), width=6, height=4)


table(ces$election, ces$quebec_accommodation)
table(ces$election, ces$quebec_accom)
names(ces)
ces %>% 
  select(election, vote2, degree, income) %>% 
  group_by(election, degree, vote2) %>%  
  filter(!is.na(degree)&!is.na(vote2)) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) 

