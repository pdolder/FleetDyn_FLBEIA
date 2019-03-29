###################################################
## Testing basic model
###################################################

library(FLBEIA)

## Load in all files
lapply(list.files(file.path("..", "model_inputs"), full.names = TRUE), load, .GlobalEnv)

## Change the effort share model

fleets.ctrl[['IE_Otter']][['effort.model']]   <- 'SMFB_ES'
fleets.ctrl[['IE_Otter']][['effshare.model']] <- 'gravity.flbeia'
fleets.ctrl[['IE_Otter']][['gravity.model']] <- 'revenue'  ## profit and tradition now also optionsi
## Note, tradition is a fraction of effort that comes from past effort share,
## e.g. 0.2


SC2 <- FLBEIA(biols = biols, 
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

save(SC2, file = file.path("..", "outputs", "Gravity_Model.RData"))

rm(list=ls())
