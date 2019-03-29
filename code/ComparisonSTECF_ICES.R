library(tidyverse)
library(FLBEIA)

stecf_C <- read.csv(file.path("..", "data", "landings_and_discards_data.csv"))
stecf_E <- read.csv(file.path("..", "data", "effort_data.csv"))

load(file.path("..", "model", "FCube", "fleets", "fleets.RData"))

ices_E <- as.data.frame(fl1@effort + fl2@effort)

stecf_E <- stecf_E %>% group_by(year) %>%
	summarise(Effort = sum(measure.calculation)/1000) %>% 
	as.data.frame()

## Compare effort totals
stecf_E$ices <- ices_E$data[match(stecf_E$year, ices_E$year)]

stecf_E$Effort / stecf_E$ices  ## Only ~ 50 % - so we double the effort values 2004 - 2015 
## For 2003 - 2014

## Compare landings totals - e.g. cod

cod_L1 <- lapply(fl1@metiers, function(x)  { 
		if("COD-CS" %in% x@catches@names) { 	
		x@catches[["COD-CS"]]@landings } else
FLQuant(0, dim=c(1,3,1,1,1,1))
})  

cod_L2 <- lapply(fl2@metiers, function(x)  { 
		if("COD-CS" %in% x@catches@names) { 	
		x@catches[["COD-CS"]]@landings } else
FLQuant(0, dim=c(1,3,1,1,1,1))
})  

cod_L1 <- Reduce("+", cod_L1)
cod_L2 <- Reduce("+", cod_L2)

cod_L1 + cod_L2
filter(stecf_C, species == "COD", Measure.Names == "landings", year %in% 2014:2016)
## Bit different!!

## Let's just take this as "correct" for now, and add on the additional catch
## and effort to fleet 2 (doesn't matter as ultimately aggregated)




