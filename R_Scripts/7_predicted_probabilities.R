#### Predicted Probabilities Sample Code ####

#install this package if necessary
#install.package('ggeffects')
#load
library(ggeffects)

#vignettes
# https://cran.r-project.org/web/packages/ggeffects/vignettes/introduction_plotmethod.html
# https://cran.r-project.org/web/packages/ggeffects/vignettes/ggeffects.html

#------------------------------------------------------------------------------------------------

#### NDP Model 3 (union:immigration interaction) ####

# Finding Significant Interactions
# interaction between sector and degree. Two of them happen in the most recent years.
ndp_model3 %>% 
  filter(term=="immigration:union_both"&p.value<0.1) 

ndp_models_complete7$model %>% 
  #use map to apply a function to each item in ndp_models_complete7$model
  #the function is ggeffect and we want to get basically all effects, so we specify union[0, 1] (private and public ) and non-degree and degree
  map(., ggpredict, terms=c('immigration', 'union_both[0,1]')) %>% 
  bind_rows() %>% 
  #add election, we need to specify each=4 because there, are four combinations of effects
  mutate(election=rep(ndp_models_complete7$election, each=4),
         Sector=as_factor(x)) %>% 
  #Plot x=election, y=predicted probabilities, change colour based on x which is sector
  ggplot(., aes(x=as.numeric(election), y=predicted, col=Sector))+
  #create two panels by degree and non-degree holders
  facet_grid(~group)+
  #point plot
  geom_point()+
  #Set a title
  labs(title="Difference in PP of voting NDP\nfor Immigration Sentiment\nUnion and Non-Union Households, 2019")+
  geom_smooth(method="loess", se=F)

#save
ggsave(here("Plots", "M3_union_immigration_probabilities_NDP_vote.png"))
