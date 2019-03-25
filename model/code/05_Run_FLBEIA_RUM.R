F##################################################
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


SC3 <- FLBEIA(biols = biols, 
	      SRs = SRs, 
	      BDs = NULL, 
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


