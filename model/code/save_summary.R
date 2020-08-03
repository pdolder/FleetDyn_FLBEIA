
# Comparison of effort shares
###############

library(FLBEIA)
library(tidyverse)
library(ggplotFL)

load(file.path("..", "outputs", "Base_Model.RData"))
load(file.path("..", "outputs", "Gravity_Model.RData"))
load(file.path("..", "outputs", "Gravity_Model_Trad.RData"))
load(file.path("..", "outputs", "RUM_Model.RData"))
load(file.path("..", "outputs", "Markov_Model.RData"))

base          <- SC1
gravity       <- SC2
rum           <- SC3
markov        <- SC4
gravity_trad  <- SC5 
rm(SC1, SC2, SC3, SC4, SC5)

maxyr <- range(base$biols[["COD"]])[["maxyear"]]

## biological summary data

bio <- rbind(
      cbind(sc = "base", bioSumQ(as.data.frame(bioSum(base, long = FALSE, years = ac(2016:maxyr))), prob = c(0.05, 0.25, 0.5, 0.75, 0.95))),
cbind(sc = "gravity", bioSumQ(as.data.frame(bioSum(gravity, long = FALSE, years = ac(2016:maxyr))),prob = c(0.05, 0.25, 0.5, 0.75, 0.95))),
cbind(sc = "rum", bioSumQ(as.data.frame(bioSum(rum, long = FALSE, years = ac(2016:maxyr))),prob = c(0.05, 0.25, 0.5, 0.75, 0.95))),
cbind(sc = "markov", bioSumQ(as.data.frame(bioSum(markov, long = FALSE, years = ac(2016:maxyr))),prob = c(0.05, 0.25, 0.5, 0.75, 0.95))),
cbind(sc = "gravity_trad", bioSumQ(as.data.frame(bioSum(gravity_trad, long = FALSE, years = ac(2016:maxyr))), prob = c(0.05, 0.25, 0.5, 0.75,0.95)))
)


## Total effort in each scenario
effort <- rbind(cbind(scenario = "base", as.data.frame(base$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "gravity", as.data.frame(gravity$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "rum", as.data.frame(rum$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "markov", as.data.frame(markov$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "gravity_trad", as.data.frame(gravity_trad$fleets[["IE_Otter"]]@effort))
)

effort <- effort %>% group_by(scenario, year, season) %>% summarise(q05 = quantile(data, prob = 0.05, na.rm = T), 
							    q25 = quantile(data, 0.25,na.rm = T),
							    q50 = quantile(data, 0.50,na.rm = T), 
							    q75 = quantile(data, 0.75,na.rm = T),
							    q95 = quantile(data, 0.95, na.rm = T))


## Effort shares

effort_share <- rbind(cbind(scenario = "base", do.call(rbind, lapply(base$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))),
	     cbind(scenario = "gravity", do.call(rbind, lapply(gravity$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))),
	     cbind(scenario = "rum", do.call(rbind, lapply(rum$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))),
	     cbind(scenario = "markov", do.call(rbind, lapply(markov$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))), 
	     cbind(scenario = "gravity_trad", do.call(rbind, lapply(gravity_trad$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare)))))
	     )

effort_share <- effort_share %>% group_by(scenario, metier, year, season) %>%
	summarise(q05 = quantile(data, prob = 0.05, na.rm = T), 
							    q25 = quantile(data, 0.25,na.rm = T),
							    q50 = quantile(data, 0.50,na.rm = T), 
							    q75 = quantile(data, 0.75,na.rm = T),
							    q95 = quantile(data, 0.95, na.rm = T))


## Catch by IE_Otter fleet....

scenarios <- c("base","gravity", "gravity_trad", "rum", "markov")

ie_otter <- lapply(scenarios, function(s) {
 sc    <- lapply(get(s)[["fleets"]][["IE_Otter"]]@metiers, function(x) {
       catch <- lapply(x@catches, function(x2) cbind(stock = x2@name,as.data.frame(x2@landings + x2@discards)))
       catch_all <- cbind(metier = x@name, bind_rows(catch))
       return(catch_all)
       })
 return(cbind(scenario = s, bind_rows(sc)))
       })

ie_otter <- bind_rows(ie_otter)

ie_otter_summary <- ie_otter %>% group_by(scenario, stock, year, iter) %>% 
	summarise(data = sum(data, na.rm = T)) %>%
	group_by(scenario, stock, year) %>%
		summarise(q05 = quantile(data, prob = 0.05, na.rm = T), 
							    q25 = quantile(data, 0.25,na.rm = T),
							    q50 = quantile(data, 0.50,na.rm = T), 
							    q75 = quantile(data, 0.75,na.rm = T),
							    q95 = quantile(data, 0.95, na.rm = T))

## Advice summary

advice <- rbind(
      advSumQ(as.data.frame(advSum(base, long = FALSE, years = ac(2016:maxyr)), scenario = "base"), prob = c(0.05, 0.25, 0.5, 0.75, 0.95)),
      advSumQ(as.data.frame(advSum(gravity, long = FALSE, years = ac(2016:maxyr)), scenario = "gravity"), prob = c(0.05, 0.25, 0.5, 0.75, 0.95)),
      advSumQ(as.data.frame(advSum(rum, long = FALSE, years = ac(2016:maxyr)), scenario = "rum"), prob = c(0.05, 0.25, 0.5, 0.75, 0.95)),
      advSumQ(as.data.frame(advSum(markov, long = FALSE, years = ac(2016:maxyr)), scenario = "markov"), prob = c(0.05, 0.25, 0.5, 0.75, 0.95)),
      advSumQ(as.data.frame(advSum(gravity_trad, long = FALSE, years = ac(2016:maxyr)), scenario = "gravity_trad"), prob = c(0.05, 0.25, 0.5, 0.75, 0.95))
      )



bio_all <- rbind(
      as.data.frame(bioSum(base, long = FALSE, years = ac(2016:maxyr), scenario = "base")),
      as.data.frame(bioSum(gravity, long = FALSE, years = ac(2016:maxyr), scenario = "gravity")),
      as.data.frame(bioSum(rum, long = FALSE, years = ac(2016:maxyr), scenario = "rum")),
      as.data.frame(bioSum(markov, long = FALSE, years = ac(2016:maxyr), scenario = "markov")),
      as.data.frame(bioSum(gravity_trad, long = FALSE, years = ac(2016:maxyr), scenario = "gravity_trad"))
)

## Add on ref pts

load(file.path("..", "model_inputs", "advice_ctrl.RData"))

Blim     <- sapply(advice.ctrl, function(x) x$ref.pts["Blim",1])
Btrigger <- sapply(advice.ctrl, function(x) x$ref.pts["Btrigger",1])

Fmsy     <- sapply(advice.ctrl, function(x) x$ref.pts["Fmsy",1])

Blim     <- as.data.frame(Blim)
Btrigger <- as.data.frame(Btrigger)
Fmsy     <- as.data.frame(Fmsy) 

bio_all$Blim     <- Blim$Blim[match(bio_all$stock, rownames(Blim))]
bio_all$Btrigger <- Btrigger$Btrigger[match(bio_all$stock, rownames(Btrigger))]
bio_all$Fmsy     <- Fmsy$Fmsy[match(bio_all$stock, rownames(Fmsy))]

bio_all$Blim.risk      <- ifelse(bio_all$ssb < bio_all$Blim, 1, 0)
bio_all$Btrigger.risk  <- ifelse(bio_all$ssb < bio_all$Btrigger, 1, 0)
bio_all$Fmsy.risk      <- ifelse(bio_all$f > bio_all$Fmsy, 1, 0) 

risk  <- bio_all %>% group_by(scenario, stock, year) %>% 
	summarise(Blim.risk = mean(Blim.risk),
		  Btrigger.risk = mean(Btrigger.risk),
		  Fmsy.risk = mean(Fmsy.risk))

save(bio, effort, effort_share, ie_otter_summary, advice, risk, file = file.path("..", "outputs","Results_summary.RData"))
