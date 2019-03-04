###################################################
## FLFleet object for effort model testing
##
###################################################

library(FLBEIA)

## Load FCube fleet object
load(file.path("..", "FCube", "fleets", "fleets.RData"))

## To condition we need the biomass in each season
# C/((B^b) * (E^a))

load(file.path("..", "biols", "biols.RData"))

biomass <- lapply(biols, function(x) {
apply(x@wt * x@n, c(1,2,4),sum)
})

## Create fleets with 4 seasons

## We just need to condition a single fleet - IE_Otter>10m
## The rest of the fleets should be the residual catch for the stocks

## We will use the spatial information as proportional catches


