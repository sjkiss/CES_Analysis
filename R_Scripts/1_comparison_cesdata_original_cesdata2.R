#This scripts generates comparisons descriptive statistics from original cesdata
# produced on cesdata_test with descriptive statistics produced on cesdata2 branch
# On those branches, particularly cesdata2, changes must be committed and pushed to those branches
#Read in CESdata original
cesdata_original<-read.csv(file="https://raw.githubusercontent.com/sjkiss/CES_Analysis/cesdata_test/Results/cesdata_original_descriptives_by_election.csv")
#Read in CESdata2 results
cesdata2_results<-read.csv(file="https://raw.githubusercontent.com/sjkiss/CES_Analysis/cesdata2/Results/cesdata2_descriptives_grouped_by_election.csv")
names(cesdata_original)
names(cesdata2_results)
#Merge cesdata_original descriptives
cesdata_original %>% 
  #With cesdata2 descriptives
  left_join(., cesdata2_results, by=c("skim_variable", "election"))->full
#Subtract n_missing of 1 from the other
full$n_missing_discrepancy<-full$n_missing.x-full$n_missing.y
#Subtract mean of 1 from the other
full$mean_discrepancy<-full$numeric.mean.x-full$numeric.mean.y
#Examine anything with values greater than 0. 
full %>% 
  filter(n_missing_discrepancy>0) %>% 
  View()

full %>% 
  filter(skim_variable=="duty")
#Find variables that have yet to be recoded in cesdata2

full %>% 
  filter(is.na(n_missing.y)) %>% 
  View()
