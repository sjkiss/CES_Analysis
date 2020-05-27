#Run master file to load up data

#To model the NDP vote we need to create a dichotomous NDP variable 
ces$ndp<-car::Recode(ces$vote, "3=1; 0:2=0; 4:5=0; NA=NA")

#Filter party_ID as NDP
ces$party_id_ndp<-car::Recode(ces$party_id, "3=1; 0:2=0; 4:5=0; NA=NA")

# Linear models for NDP
# Model 1 with Quebec
ndp_model1<-lm(ndp~union+degree+quebec+age+religion+language+employment+sector+occupation+income, data=ces)
summary(ndp_model1)

# Model 2 with Regions
ndp_model2<-lm(ndp~union+degree+region+quebec+age+religion+language+employment+sector+occupation+income+party_id_ndp, data=ces)
summary(ndp_model2)

# Model 3 with Regions + NDP Party_ID
ndp_model3<-lm(ndp~union+degree+region+quebec+age+religion+language+employment+sector+occupation+income, data=ces)
summary(ndp_model3)

# Model 4 basic
ndp_model4<-lm(ndp~union+degree+age+employment+language+income, data=ces)
summary(ndp_model4)

----------------------------------------------------

#???
# Model 5
ces %>% 
  filter(income==1 & election!=1965&election!=1968) %>% 
  group_by(election) %>%
ndp_model5<-lm(ndp~union+degree+age+religion+language+income, data=ces)
summary(ndp_model5)
  
  
#???  
# Create a table of 3 models
ndp_model1<-tidy(lm(ndp~union+degree+quebec+age+religion+language+employment+sector+occupation+income, data=ces))
ndp_model2<-tidy(lm(ndp~union+degree+region+quebec+age+religion+language+employment+sector+occupation+income+party_id_ndp, data=ces))
ndp_model3<-tidy(lm(ndp~union+degree+region+quebec+age+religion+language+employment+sector+occupation+income, data=ces))

all_models <- rbind_list(
  ndp_model1 %>% mutate(ndp_model1 = 1),
  ndp_model1 %>% mutate(ndp_model1 = 2),
  ndp_model1 %>% mutate(ndp_model1 = 3))

all_models

write.table(all_models, file = "allmodels.txt", sep = ",", quote = FALSE, row.names = F)

summary(models)

#???
#Filter by election
ces$e1972<-car::Recode(ces$election, "1972=1; else=NA")

#By election
ndp_model1972<-lm(ndp~union+degree+quebec+age+religion+language+employment+sector+occupation+income+e1972, data=ces)

