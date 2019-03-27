##############################################################
## A simplified RUM fit to the IE_Otter data
###############################################################

library(FLBEIA)
library(tidyverse)

set.seed(111)

if(length(grep("coilin", getwd())) > 0){
    load(file.path("~", "Dropbox", "FLeetDyn_FLBEIA", "model", "fleets", "fleets.RData"))
}else{
    load(file.path(".", "model", "fleets", "fleets.RData"))
}

setwd('C:/use/Dropbox/FLeetDyn_FLBEIA')

inputs <- dir('C:/use/Dropbox/FLeetDyn_FLBEIA/model/model_inputs')
for(fl in inputs) load(file.path('C:/use/Dropbox/FLeetDyn_FLBEIA/model/model_inputs', fl))


fleets.ctrl[["IE_Otter"]][['effort.model']]   <- 'SMFB_ES'
fleets.ctrl[["IE_Otter"]][['effshare.model']] <-  'gravity.flbeia'
fleets.ctrl[["IE_Otter"]][['gravity.model']]   <-  'revenue'
fleets.ctrl[["IE_Otter"]][['tradition']]   <-  0

set.seed(3.14159)

for(mt in names(fleets[['IE_Otter']]@metiers)) fleets[["IE_Otter"]]@metiers[[mt]]@vcost[] <- runif(1,0.1,0.5)

#debug(FLBEIA:::mlogit.flbeia)
main.ctrl[[1]][2] <- 2020

test1 <- FLBEIA(biols, SRs, BDs = NULL, fleets, covars = NULL, 
  indices = NULL, advice, main.ctrl, biols.ctrl, fleets.ctrl, 
  covars.ctrl = NULL, obs.ctrl, assess.ctrl, advice.ctrl) 





fleets.ctrl[["IE_Otter"]][['effort.model']]   <- 'SMFB_ES'
fleets.ctrl[["IE_Otter"]][['effshare.model']] <-  'gravity.flbeia'
fleets.ctrl[["IE_Otter"]][['gravity.model']]   <-  'profit'
fleets.ctrl[["IE_Otter"]][['tradition']]   <-  0

#debug(FLBEIA:::mlogit.flbeia)
main.ctrl[[1]][2] <- 2020

test2 <- FLBEIA(biols, SRs, BDs = NULL, fleets, covars = NULL, 
                indices = NULL, advice, main.ctrl, biols.ctrl, fleets.ctrl, 
                covars.ctrl = NULL, obs.ctrl, assess.ctrl, advice.ctrl) 


fleets.ctrl[["IE_Otter"]][['effort.model']]   <- 'SMFB_ES'
fleets.ctrl[["IE_Otter"]][['effshare.model']] <-  'gravity.flbeia'
fleets.ctrl[["IE_Otter"]][['gravity.model']]   <-  'revenue'
fleets.ctrl[["IE_Otter"]][['tradition']]   <-  0.3

#debug(FLBEIA:::mlogit.flbeia)
main.ctrl[[1]][2] <- 2020

test3 <- FLBEIA(biols, SRs, BDs = NULL, fleets, covars = NULL, 
                indices = NULL, advice, main.ctrl, biols.ctrl, fleets.ctrl, 
                covars.ctrl = NULL, obs.ctrl, assess.ctrl, advice.ctrl) 



