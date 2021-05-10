library(cesdata)
library(labelled)
# data("ces19phone")
data('ces15phone')
library(tidyverse)
library(stringr)
#convert q7 to mip
ces19phone$mip<-tolower(ces19phone$q7)
#### Read in Molly's MIP file ####
library(openxlsx)
library(here)
mip<-read.xlsx(here("Data", "mip.xlsx"))

ces19phone %>% 
  mutate(q7out=case_when(
    
  ))
# #convert mip to mip_cat
# ces19phone %>% 
#   mutate(mip_cat=case_when(
#     str_detect(mip, "climate") ~ "environment",
#         str_detect(mip, "environ") ~ "environment",
#             str_detect(mip, "^changement climatique") ~ "environment",
#     str_detect(mip, "health") ~ "health care",
#        str_detect(mip, "tax") ~ "taxes",
#            str_detect(mip, "^impot") ~ "taxes",
#                str_detect(mip, "^impôt") ~ "taxes",
#                str_detect(mip, "santé") ~ "health care",
#                    str_detect(mip, "crim[ei]") ~ "crime",
#                        str_detect(mip, "^job") ~ "jobs",
#                            str_detect(mip, "deficit") ~ "debt_deficit",
#                                str_detect(mip, "debt") ~ "debt_deficit",
#                                 #   str_detect(mip, "trudeau") ~ "leadership",
#                                    str_detect(mip, "ethics") ~ "ethics",
#                                        str_detect(mip, "immigr") ~ "immigration",
#                                            str_detect(mip, "pipeline") ~ "pipeline",
#                                                str_detect(mip, "ecologie") ~ "environment",
#                                                    str_detect(mip, "economy") ~ "economy",
#                                                    str_detect(mip, "économie") ~ "economy",
#                                                        str_detect(mip, "abortion") ~ "moral",
#                                                        str_detect(mip, "accountability") ~ "ethics",
# str_detect(mip, "affordability") ~ "cost_of_living",
# str_detect(mip, "change of government") ~ "change of government",
# str_detect(mip, "housing") ~ "housing",
# str_detect(mip, "honest") ~ "ethics",
# str_detect(mip, "work") ~ "jobs",
# str_detect(mip, "corruption") ~ "ethics",
# str_detect(mip, "oil") ~ "energy",
# str_detect(mip, "seniors") ~ "seniors",
# str_detect(mip, "aboriginal") ~ "first nations",
# str_detect(mip, "inequality") ~ "equality",
#     TRUE ~ NA_character_
#   )) -> ces19phone
# 
# ces19phone$mip
# 
# ces19phone %>% 
# distinct(mip, .keep_all=T) %>% 
#   select(mip, mip_cat) %>% 
#   group_by(mip, mip_cat) %>% 
#   summarize(n=n()) %>% 
#   arrange(desc(n)) %>% 
#   filter(is.na(mip_cat)) 
# 
# 
# 
# 
# 
# 
# 
# 
# library(labelled)
# 
#   ces19phone %>% 
#   mutate(mip_enviro=case_when(
#     mip_cat=='environment'~ 1,
#     TRUE ~ 0
#   ))->ces19phone
#

# ces19phone %>% 
# distinct(mip, .keep_all=T) %>% 
#   select(mip, mip_cat)-> mip19
# 
# mip19$mip_number<-rep("", nrow(mip19))
# library(openxlsx)
# 
# val_labels(ces15phone$CPS15_1) %>% 
#   data.frame()->mip15
# mip15
# #Create the workbook
# mip<-createWorkbook()
# #addWorksheet(mip, sheetName="Examples_2015")
# addWorksheet(mip, sheetName="To_Be_Coded_2019")
# writeData(mip, sheet="To_Be_Coded_2019", mip19)
# writeData(mip,startCol = 4, startRow = 1, sheet="To_Be_Coded_2019", mip15)
# saveWorkbook(mip, file="Results/mip.xlsx", overwrite=T)
# 
# 
