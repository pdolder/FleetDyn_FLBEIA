###################################################
## Testing basic model
###################################################

library(FLBEIA)

## Load in all files
lapply(list.files(file.path("..", "model_inputs"), full.names = TRUE), load, .GlobalEnv)

## Test RUM fit
load(file = file.path("..", "..","tests", "RUM_model.RData"))

## Change effort share model
fleets.ctrl[["IE_Otter"]][['effort.model']]   <- 'SMFB_ES'
fleets.ctrl[["IE_Otter"]][['effshare.model']] <-  'mlogit.flbeia'
fleets.ctrl[["IE_Otter"]][['mlogit.model']]   <-  RUM_model_fit


## Simulate closure in metier I from 2026 for IE_Otter
## this is done by setting effshare to 0, reallocating to other metier
## and setting catch.q in metier I to zero

close.yr <- ac(2021:main.ctrl[["sim.years"]][["final"]])
close.met <- "F"

## Effort share in metier I, and reassign to 0
ef.i <- fleets[["IE_Otter"]]@metiers[[close.met]]@effshare[,close.yr]
fleets[["IE_Otter"]]@metiers[[close.met]]@effshare[,close.yr] <- 0

## For all other metier, recalculate proportionately
mets <- fleets[["IE_Otter"]]@metiers@names
mets <- mets[!mets == close.met]

for(m in mets) {
  fleets[["IE_Otter"]]@metiers[[m]]@effshare[,close.yr] <-  fleets[["IE_Otter"]]@metiers[[m]]@effshare[,close.yr] + 
    (ef.i * (fleets[["IE_Otter"]]@metiers[[m]]@effshare[,close.yr] / (1- ef.i) ))
}

## catch.q to 0

for(i in catchNames(fleets[["IE_Otter"]]@metiers[[close.met]])) {
  fleets[["IE_Otter"]]@metiers[[close.met]]@catches[[i]]@catch.q[,close.yr,,1:4] <- 0
}


SC3 <- FLBEIA(biols = biols, 
	      SRs = SRs, 
	      BDs = BDs, 
	      fleets=fleets, 
	      covars =NULL, 
              indices = NULL, 
	      advice = advice, 
	      main.ctrl = main.ctrl, 
              biols.ctrl = biols.ctrl, 
	      fleets.ctrl = fleets.ctrl, 
              covars.ctrl =covars.ctrl, 
	      obs.ctrl = obs.ctrl, 
              assess.ctrl = assess.ctrl, 
	      advice.ctrl = advice.ctrl) 

save(SC3, file = file.path("..", "outputs", "RUM_Model.RData"))

rm(list=ls())


