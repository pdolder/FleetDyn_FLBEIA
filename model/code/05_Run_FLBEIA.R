###################################################
## Testing basic model
###################################################

library(FLBEIA)

## Load in all files
lapply(list.files(file.path("..", "model_inputs"), full.names = TRUE), load, .GlobalEnv)

SC1 <- FLBEIA(biols = biols, 
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

save(SC1, file = file.path("..", "outputs", "BaseModel.RData"))

names(SC1)


library(ggplotFL)
plot(SC1[["stocks"]][["HAD"]])

fbar(SC1[["stocks"]][["HAD"]])
SC1[["stocks"]][["COD"]]@landings 

plotFLBiols(SC1$biols)
plotFLFleets(SC1$fleets)


## recruitment is very low/zero for stocks - but not in the n's in the biols...
