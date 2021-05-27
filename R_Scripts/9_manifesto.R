#install.packages('manifestoR')
#Load the library
library(manifestoR)

#This is my apikey, but you can use it. 
mp_setapikey(key='6d3c2dbd29b1633085cc2089f99128d6')

#Link to workflow documentation is here
# https://cran.r-project.org/web/packages/manifestoR/vignettes/manifestoRworkflow.pdf

mpds <- mp_maindataset()
canada <- subset(mpds, countryname == "Canada" & edate > as.Date("1980-01-01"))

names(canada)
print(head(names(canada)))
mp_describe_code("504")
doc <- canada[["62320_201510"]]
mp_scale(canada, scalingfun = logit_rile)
mp_scale(canada, scalingfun = rile)

#--------------------------------------------------------------------------------------------

#Bakker-Hobolot economic dimension logit
logit_scale <- function(data)
  scale_logit(data, pos = c("per401"), c("per402"), c("per407"), c("per410"), c("per414"), c("per507"), c("per509"), c("per702"), neg = c("per403"), c("per404"), c("per405"), c("per406"), c("per409"), c("per412"), c("per413"), c("per415"), c("per503"), c("per504"), c("per506"), c("per701"))
mp_scale(canada, scalingfun = logit_scale)

#Bakker-Hobolt economic dimension
custom_scale <- function(data)
  data$per401 + data$per402 + data$per407 + data$per410 + data$per414 + data$per505 + data$per507 + data$per702 - data$per403 - data$per404 - data$per405 - data$per406 - data$per409 - data$per412 - data$per413 - data$per415 - data$per503 - data$per504 - data$per506 - data$per701
mp_scale(canada, scalingfun = custom_scale)

# Labour issues
custom_scale <- function(data)
  data$per702 - data$per701
mp_scale(canada, scalingfun = custom_scale)

custom_scale <- function(data)
  data$per401 + data$per402 + data$per407 + data$per410 + data$per414 + data$per505 + data$per507 + data$per702 - data$per403 - data$per404 - data$per405 - data$per406 - data$per409 - data$per412 - data$per413 - data$per415 - data$per503 - data$per504 - data$per506 - data$per701
mp_bootstrap(data,
             fun = custom_scale,
             statistics = list(var, 0.025, 0.975))

