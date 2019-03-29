##############################################################
## A simplified RUM fit to the IE_Otter data
###############################################################

library(FLBEIA)
library(tidyverse)
library(nnet)

set.seed(111)

if(length(grep("coilin", getwd())) > 0){
    load(file.path("~", "Dropbox", "FLeetDyn_FLBEIA", "model", "fleets", "fleets.RData"))
}else{
    load(file.path(".", "model", "fleets", "fleets.RData"))
}

setwd('C:/use/Dropbox/FLeetDyn_FLBEIA')

load(file = file.path(".", "code", "Markov_fit.RData"))
#load(file = file.path("..", "tests", "RUM_model.RData"))


inputs <- dir('C:/use/Dropbox/FLeetDyn_FLBEIA/model/model_inputs')
#inputs <- dir(file.path("..","model", 'model_inputs'))


for(fl in inputs) load(file.path('C:/use/Dropbox/FLeetDyn_FLBEIA/model/model_inputs', fl))
#for(fl in inputs) load(file.path("..", "model", 'model_inputs', fl))

fleets.ctrl[["IE_Otter"]][['effort.model']]   <- 'SMFB_ES'
fleets.ctrl[["IE_Otter"]][['effshare.model']] <-  'Markov.flbeia'
fleets.ctrl[["IE_Otter"]][['Markov.model']]   <-  Markov_fit

debug(FLBEIA:::mlogit.flbeia)

test <- FLBEIA(biols, SRs, BDs = NULL, fleets, covars = NULL, 
  indices = NULL, advice, main.ctrl, biols.ctrl, fleets.ctrl, 
  covars.ctrl = NULL, obs.ctrl, assess.ctrl, advice.ctrl) 






