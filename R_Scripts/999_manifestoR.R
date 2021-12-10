#install.packages('manifestoR')
#Load the library
library(manifestoR)

#This is my apikey, but you can use it. 
mp_setapikey(key='6d3c2dbd29b1633085cc2089f99128d6')

#Link to workflow documentation is here
# https://cran.r-project.org/web/packages/manifestoR/vignettes/manifestoRworkflow.pdf

canada <- mp_corpus(countryname == "Canada" & edate > as.Date("1980-01-01"))

