###################################################
## FLFleet object for effort model testing
##
###################################################

library(FLBEIA)
library(tidyverse)

## For quant sums of landings etc..
setGeneric('Sums', function(object, ...)
  standardGeneric('Sums')
)
setMethod('Sums', signature(object='FLQuants'),
          function(object, na.rm=FALSE, ...) {
            if(length(object) == 1)
              eturn(object[[1]])
            eval(parse(text=paste('object[[', paste(seq(length(object)),
                                                    collapse=']] + object[['), ']]', sep='')))
          }
)


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

load(file.path("..", "..", "data", "Processed_catch.RData"))

catch$species[catch$species == "ANF"] <- "MON"
catch$species[catch$species == "HKE"] <- "NHKE"
catch$species[catch$species == "LEZ"] <- "NMEG"

catch <- filter(catch, species != "NEP18")
catch <- filter(catch, species != "NEPOutsideFU")

## We also want the stock objects for the total landings etc..

stk.path <- file.path("..", "FCube", "stocks")

## 1. Load the stock objects
stocks<-FLStocks(lapply(list.files(stk.path),function(x){
                            load(file.path(stk.path,x))
                        res<-get("stock")
                        name(res)<-gsub('.RData',"",x)
                        res}))

## Fleet data
first.yr <- 2015 
last.yr  <- 2017


####################################
## First the Irish Otter trawlers ##
####################################

## Effort
effort <- filter(effort, year %in% 2014:2016) ## Note our years are different

## Need the proportion of effort per quarter

## quant dims are 1,3,1,4,1,1
Q             <- FLQuant(NA, dim = c(1,3,1,4,1,1), 
		dimnames = list(year = 2015:2017))
eff <- Q

effort_all <- effort %>% group_by(year, quarter) %>%
	      summarise(Effort = sum(Effort)) %>%
	      spread(quarter, -year) %>% as.data.frame()

effort_all[,2:5] <- effort_all[,2:5]/rowSums(effort_all[,2:5]) 

## kw days effort by quarter

fl1 <- "IE_Otter_10<24m"
fl2 <- "IE_Otter_24<40m"

eff[,,,1] <- effort_all[,2] * fleets[[fl1]]@effort + 
			       fleets [[fl2]]@effort
eff[,,,2] <- effort_all[,3] * fleets[[fl1]]@effort + 
			       fleets [[fl2]]@effort
eff[,,,3] <- effort_all[,4] * fleets[[fl1]]@effort + 
			       fleets [[fl2]]@effort
eff[,,,4] <- effort_all[,5] * fleets[[fl1]]@effort + 
			       fleets [[fl2]]@effort

eff <- window(eff, first.yr, last.yr)

fcost <- cap <- crew <- Q ## empty, no data - could use AER

cap <- eff * 1.2  ## For now, high limit 

fcost <- window(fcost, first.yr, last.yr)
cap   <- window(cap, first.yr, last.yr)
crew  <- window(crew, first.yr, last.yr)

## set correct units
units(eff) <- "000 kwdays"
units(fcost) <- "000 euros"
units(cap) <- "000 kwdays"
units(crew) <-  "NA"

## Now we generate the FLMetier and FLCatches

## Let's rename the metier a bit
max_no <- max(catch$group)
catch$group <- as.factor(catch$group)
levels(catch$group) <- LETTERS[seq_len(max_no)] 
effort$group <- as.factor(effort$group)
levels(effort$group) <- LETTERS[seq_len(max_no)] 

mets <- unique(effort$group)

catch$group <- as.character(catch$group)
effort$group <- as.character(effort$group)

#########################
######## FLMetier #######
#########################

metiers <- FLMetiersExt(lapply(mets, function(met) {

print(met)

effort_met <- effort %>% filter(year %in% 2014:2016, 
			      group == met) %>%
	     select(-Effort, - total) %>%
	     complete(group, year = 2014:2016, 
		      quarter = 1:4, 
		      fill = list(effshare = 0)) %>%
	     as.data.frame()

## effshare
effsh <- Q
effsh[,,,1] <- effort_met[effort_met$quarter == 1,"effshare"]
effsh[,,,2] <- effort_met[effort_met$quarter == 2,"effshare"]
effsh[,,,3] <- effort_met[effort_met$quarter == 3,"effshare"]
effsh[,,,4] <- effort_met[effort_met$quarter == 4,"effshare"]

effsh<- window(effsh, first.yr, last.yr)

## vcost
vcost <- Q  ## none at moment
vcost <- window(vcost, first.yr, last.yr)

units(effsh) <- "NA"
units(vcost) <- "000 euros"

###############
## FLCatches
###############

stks <- sort(unique(catch[catch$group == met & 
	       catch$year %in% 2015:2017,"species"]))

catchMet <- FLCatchesExt(lapply(stks, function(S) {

print(S)

## Fix mismatch between names
if(S == "COD") { s <- "COD-CS" }
if(S == "HAD") { s <- "HAD-CS" }
if(S == "WHG") { s <- "WHG-CS" }
if(S == "NHKE") { s <- "N-HKE" }
if(S == "MON") { s <- "MON-CS" }
if(S == "NMEG") { s <- "N-MEG" }
if(S == "NEP16") {s <- "NEP16"}
if(S == "NEP17") {s <- "NEP17"}
if(S == "NEP19") {s <- "NEP19"}
if(S == "NEP2021") {s <- "NEP2021"}
if(S == "NEP22") {s <- "NEP22"}
if(S == "NEPOutsideFU") { s <- "NEP7OTH" }

## Empty quants
Q_age   <- biols[[S]]@n; Q_age[] <- NA 
Q_bio   <- ssb(biols[[S]]); Q_bio[] <- NA

## Metiers in the FLFleet object
mt1 <- fleets[[fl1]]@metiers@names
mt2 <- fleets[[fl2]]@metiers@names


##############
## landings ##
##############

catch_met <- catch %>% filter(year %in% 2014:2016, 
			      group == met, species == S) %>%
	     select(-landings, - total) %>%
	     complete(group, year = 2014:2016, 
		      quarter = 1:4, species,
		      fill = list(catchshare = 0)) %>%
	     as.data.frame()


land <- Q_bio
## Proportion of fleets landings of the total fleets landings
fl1_l <-Sums(FLQuants(lapply(mt1, function(x) {
		if(s %in% catchNames(fleets[[fl1]]@metiers[[x]])) {
		fleets[[fl1]]@metiers[[x]]@catches[[s]]@landings} else
		FLQuant(0, dim = c(1,3,1,1,1,1))
		})))
fl2_l <-Sums(FLQuants(lapply(mt2, function(x) {
		if(s %in% catchNames(fleets[[fl2]]@metiers[[x]])) {
		fleets[[fl2]]@metiers[[x]]@catches[[s]]@landings} else
		FLQuant(0, dim = c(1,3,1,1,1,1))
		})))
fl_l <- fl1_l + fl2_l ## the fleet level landings

land[,ac(2015:2017),,1] <- fl_l * (catch_met[catch_met$quarter == 1 ,"catchshare"])
land[,ac(2015:2017),,2] <- fl_l * (catch_met[catch_met$quarter == 2 ,"catchshare"])
land[,ac(2015:2017),,3] <- fl_l * (catch_met[catch_met$quarter == 3,"catchshare"])
land[,ac(2015:2017),,4] <- fl_l * (catch_met[catch_met$quarter == 4 ,"catchshare"])

land <- window(land, first.yr, last.yr)

## landings.n
land_age <- Q_age

## Need to do these by FLQuant

dim_q <- c(dim(stocks[[s]])[1],3,1,1,1,1)

## This is the stock landing.n * (fleet landings / total landings) * fleet
## landings in quarter

land_age[,ac(2015:2017),,1] <- stocks[[s]]@landings.n[,ac(2015:2017)] *
			       FLQuant(rep(as.vector(fl_l / stocks[[s]]@landings[,ac(2015:2017)]), each = dim_q[1]),
				       dim = dim_q) *
			   FLQuant(rep(catch_met[catch_met$quarter == 1 ,"catchshare"], each = dim_q[1]), dim = dim_q)

land_age[,ac(2015:2017),,2] <- stocks[[s]]@landings.n[,ac(2015:2017)] *
			       FLQuant(rep(as.vector(fl_l / stocks[[s]]@landings[,ac(2015:2017)]), each = dim_q[1]),
				       dim = dim_q) *
			   FLQuant(rep(catch_met[catch_met$quarter == 2,"catchshare"], each = dim_q[1]), dim = dim_q)

land_age[,ac(2015:2017),,3] <- stocks[[s]]@landings.n[,ac(2015:2017)] *
			       FLQuant(rep(as.vector(fl_l / stocks[[s]]@landings[,ac(2015:2017)]), each = dim_q[1]),
				       dim = dim_q) *
			   FLQuant(rep(catch_met[catch_met$quarter == 3 ,"catchshare"], each = dim_q[1]), dim = dim_q)

land_age[,ac(2015:2017),,4] <- stocks[[s]]@landings.n[,ac(2015:2017)] *
			       FLQuant(rep(as.vector(fl_l / stocks[[s]]@landings[,ac(2015:2017)]), each = dim_q[1]),
				       dim = dim_q) *
			   FLQuant(rep(catch_met[catch_met$quarter == 4 ,"catchshare"], each = dim_q[1]), dim = dim_q)

## redistribute the first age and season to the others
land_age[1,ac(2015:2017),,2:4] <- land_age[1,ac(2015:2017),,2:4] + (1/3 * as.vector((land_age[1,ac(2015:2017),,1])))
land_age[1,ac(2015:2017),,1]  <- 0

land_age <- window(land_age, first.yr, last.yr)

## landings.wt - from the biols
land_wt <- Q_age
land_wt <- biols[[S]]@wt[,ac(2015:2017)]
land_wt <- window(land_wt,  , 2017) 

land_wt <- window(land_wt, first.yr, last.yr)

## discards

disc <- Q_bio
## Proportion of fleets landings of the total fleets landings
fl1_d <-Sums(FLQuants(lapply(mt1, function(x) {
		if(s %in% catchNames(fleets[[fl1]]@metiers[[x]])) {
		fleets[[fl1]]@metiers[[x]]@catches[[s]]@discards} else
		FLQuant(0, dim = c(1,3,1,1,1,1))
		})))
fl2_d <-Sums(FLQuants(lapply(mt2, function(x) {
		if(s %in% catchNames(fleets[[fl2]]@metiers[[x]])) {
		fleets[[fl2]]@metiers[[x]]@catches[[s]]@discards} else
		FLQuant(0, dim = c(1,3,1,1,1,1))
		})))
fl_d <- fl1_d + fl2_d ## the fleet level discards 

disc[,ac(2015:2017),,1] <- fl_d * (catch_met[catch_met$quarter == 1 ,"catchshare"])
disc[,ac(2015:2017),,2] <- fl_d * (catch_met[catch_met$quarter == 2 ,"catchshare"])
disc[,ac(2015:2017),,3] <- fl_d * (catch_met[catch_met$quarter == 3 ,"catchshare"])
disc[,ac(2015:2017),,4] <- fl_d * (catch_met[catch_met$quarter == 4 ,"catchshare"])

disc <- window(disc, first.yr, last.yr)

## discards.n
disc_age <- Q_age

## Need to do these by FLQuant

## This is the stock discards.n * (fleet discards / total discards) * fleet
## landings in quarter

disc_age[,ac(2015:2017),,1] <- stocks[[s]]@discards.n[,ac(2015:2017)] *
			       FLQuant(rep(as.vector(fl_d / stocks[[s]]@discards[,ac(2015:2017)]), each = dim_q[1]),
				       dim = dim_q) *
			   FLQuant(rep(catch_met[catch_met$quarter == 1 ,"catchshare"], each = dim_q[1]), dim = dim_q)

disc_age[,ac(2015:2017),,2] <- stocks[[s]]@discards.n[,ac(2015:2017)] *
			       FLQuant(rep(as.vector(fl_d / stocks[[s]]@discards[,ac(2015:2017)]), each = dim_q[1]),
				       dim = dim_q) *
			   FLQuant(rep(catch_met[catch_met$quarter == 2, "catchshare"], each = dim_q[1]), dim = dim_q)

disc_age[,ac(2015:2017),,3] <- stocks[[s]]@discards.n[,ac(2015:2017)] *
			       FLQuant(rep(as.vector(fl_d / stocks[[s]]@discards[,ac(2015:2017)]), each = dim_q[1]),
				       dim = dim_q) *
			   FLQuant(rep(catch_met[catch_met$quarter == 3 ,"catchshare"], each = dim_q[1]), dim = dim_q)

disc_age[,ac(2015:2017),,4] <- stocks[[s]]@discards.n[,ac(2015:2017)] *
			       FLQuant(rep(as.vector(fl_d / stocks[[s]]@discards[,ac(2015:2017)]), each = dim_q[1]),
				       dim = dim_q) *
			   FLQuant(rep(catch_met[catch_met$quarter == 4 ,"catchshare"], each = dim_q[1]), dim = dim_q)

disc_age[is.na(disc_age)] <- 0

## redistribute the first age and season to the others
disc_age[1,ac(2015:2017),,2:4] <- disc_age[1,ac(2015:2017),,2:4] + (1/3 * as.vector(disc_age[1,ac(2015:2017),,1]))
disc_age[1,ac(2015:2017),,1]  <- 0


disc_age <- window(disc_age, first.yr, last.yr)

## discards.wt - from biols
disc_wt <- Q_age
disc_wt <- biols[[S]]@wt[,ac(2015:2017)]

disc_wt <- window(disc_wt, first.yr, last.yr)

## price

## just take from metier 1
pr <- Q_age

tot <- FLQuants(lapply(mt1, function(x) {
		if(s %in% catchNames(fleets[[fl1]]@metiers[[x]])) {
		fleets[[fl1]]@metiers[[x]]@catches[[s]]@price} else
		FLQuant(0, dim = c(1,dim_q[2],1,1,1,1))
		}))

for(i in 1:length(tot)) {
tot[[i]][is.na(tot[[i]])] <- 5  ## remove any NAs, replace with mean val
}

tot = Sums(tot) / length(tot)
pr <- FLQuant(rep(as.vector(tot), each = dim_q[1]), dim = dim(land_age),
	      dimnames = list(age = dimnames(biols[[S]])$age, 
			      year = first.yr:last.yr))

pr <- window(pr, first.yr, last.yr)

## alpha

al <- Q_age
al[] <- 1

al <- window(al, first.yr, last.yr)
## beta

be <- Q_age
be[] <- 1

be <- window(be, first.yr, last.yr)

## landings.sel
land.sel <- land_age / (land_age + disc_age)

land.sel <- window(land.sel, first.yr, last.yr)

## discards.sel
disc.sel <- 1 - land.sel 

disc.sel <- window(disc.sel, first.yr, last.yr)

## q -calculate

eff_a <- Q_age

for(i in 1:4) {
eff_a[,ac(2015:2017),,i,] <- rep(as.vector(eff[,,,i]), each = dim_q[1])
}

eff_a <- window(eff_a, first.yr, last.yr)

q <- ((land_age * land_wt) + 
     (disc_age * disc_wt))/
      (biomass[[S]][,ac(2015:2017)]^al * 
      eff_a^be)


## Units

units(land) <- "tonnes"
units(disc) <- "tonnes"
units(land_age) <- '1000'
units(disc_age) <- '1000'
units(land_wt) <- "kg"
units(disc_wt) <- "kg"
units(pr) <- "000 euros"

ca <- FLCatchExt(name = S, landings = land, landings.n = land_age, 
		 landings.wt = land_wt, discards = disc, discards.n = disc_age,
		 discards.wt = disc_wt, landings.sel = land.sel, discards.sel = disc.sel,
		 price = pr, catch.q = q, alpha = al, beta = be)
		 
return(ca)

}))

names(catchMet) <- stks 

m <- FLMetierExt(name = as.character(met), 
		 catches = catchMet, 
		 effshare = effsh, vcost = vcost)
return(m)


	}))

names(metiers) <- mets

IE_Otter <- FLFleetExt(metiers = metiers, name = "IE_Otter",
		       effort = eff, capacity = cap, fcost = fcost,
		       crewshare = crew)

IE_Otter@range["minyear"] <- 2015
IE_Otter@range["maxyear"] <- 2017

unique(catch$species)
catchNames(IE_Otter)

#################################
### Now we make the others fleets
###################################

## quant dims are 1,3,1,4,1,1
Q             <- FLQuant(NA, dim = c(1,3,1,4,1,1), 
		dimnames = list(year = 2015:2017))
eff <- Q
eff[] <- 1e5
eff <- window(eff, first.yr, last.yr)

fcost <- cap <- crew <- Q ## empty, no data - could use AER

cap <- eff * 1.2

fcost <- window(fcost, first.yr, last.yr)
cap   <- window(cap, first.yr, last.yr)
crew  <- window(crew, first.yr, last.yr)

## set correct units
units(eff) <- "000 kwdays"
units(fcost) <- "000 euros"
units(cap) <- "000 kwdays"
units(crew) <-  "NA"

## Metier

effsh <- Q
effsh<- window(effsh, first.yr, last.yr)
effsh[] <- 1

## vcost
vcost <- Q  ## none at moment
vcost <- window(vcost, first.yr, last.yr)

units(effsh) <- "NA"
units(vcost) <- "000 euros"


##########################
##
## Now for the residual catch
## from fleets
##
##########################
## We can loop this...

for(S in catchNames(IE_Otter)) {

print(S)

## Fix mismatch between names
if(S == "COD") { s <- "COD-CS" }
if(S == "HAD") { s <- "HAD-CS" }
if(S == "WHG") { s <- "WHG-CS" }
if(S == "NHKE") { s <- "N-HKE" }
if(S == "MON") { s <- "MON-CS" }
if(S == "NMEG") { s <- "N-MEG" }
if(S == "NEP16") {s <- "NEP16"}
if(S == "NEP17") {s <- "NEP17"}
if(S == "NEP19") {s <- "NEP19"}
if(S == "NEP2021") {s <- "NEP2021"}
if(S == "NEP22") {s <- "NEP22"}

## Empty quants
Q_age   <- biols[[S]]@n; Q_age[] <- NA 
Q_bio   <- ssb(biols[[S]]); Q_bio[] <- NA

## landings ##
land <- Q_bio

## residual landings weight
flt     <- apply(landWStock.f(IE_Otter, S),c(2), sum)
res	<- stocks[[s]]@landings[,ac(first.yr:last.yr)] - flt

land[,ac(2015:2017),,] <- res / 4
land <- window(land, first.yr, last.yr)

## landings.n
land_age <- Q_age

flt	<- apply(landStock.f(IE_Otter, S), c(1,2), sum)
res	<- stocks[[s]]@landings.n[,ac(first.yr:last.yr)] - flt

## For first age, only split among seasons 2 - 4
land_age[,ac(2015:2017),,] <- res / 4
land_age[1,ac(2015:2017),,2:4] <- land_age[1,ac(2015:2017),,2:4] + as.vector(1/3 * (land_age[1,ac(2015:2017),,1]))
land_age[1,ac(2015:2017),,1]  <- 0

land_age <- window(land_age, first.yr, last.yr)

## landings.wt - from the biols
land_wt <- Q_age
land_wt <- biols[[S]]@wt[,ac(2015:2017)]
land_wt <- window(land_wt,  , 2017) 

land_wt <- window(land_wt, first.yr, last.yr)

## discards

disc <- Q_bio

flt	<- apply(discWStock.f(IE_Otter, S),c(2), sum)
res	<- stocks[[s]]@discards[,ac(first.yr:last.yr)] - flt

disc[,ac(2015:2017),,] <- res / 4 
disc <- window(disc, first.yr, last.yr)

## discards.n
disc_age <- Q_age

flt	<- apply(discStock.f(IE_Otter, S), c(1,2), sum)
res	<- stocks[[s]]@discards.n[,ac(first.yr:last.yr)] - flt

disc_age[,ac(2015:2017),,] <- res / 4
disc_age[1,ac(2015:2017),,2:4] <- disc_age[1,ac(2015:2017),,2:4] + (1/3 * as.vector(disc_age[1,ac(2015:2017),,1]))
disc_age[1,ac(2015:2017),,1]  <- 0

disc_age[is.na(disc_age)] <- 0
disc_age <- window(disc_age, first.yr, last.yr)

## discards.wt - from biols
disc_wt <- Q_age
disc_wt <- biols[[S]]@wt[,ac(2015:2017)]

disc_wt <- window(disc_wt, first.yr, last.yr)

## price

## just take from fleet 1
pr <- Q_age
pr[] <- 100

pr <- window(pr, first.yr, last.yr)

## alpha

al <- Q_age
al[] <- 1

al <- window(al, first.yr, last.yr)
## beta

be <- Q_age
be[] <- 1

be <- window(be, first.yr, last.yr)

## landings.sel
land.sel <- land_age / (land_age + disc_age)
land.sel <- window(land.sel, first.yr, last.yr)

## discards.sel
disc.sel <- 1 - land.sel 
disc.sel <- window(disc.sel, first.yr, last.yr)

## q -calculate
eff_a <- Q_age
eff_a[] <- 1000

eff_a <- window(eff_a, first.yr, last.yr)

q <- ((land_age * land_wt) + 
     (disc_age * disc_wt))/
      (biomass[[S]][,ac(2015:2017)]^al * 
      eff_a^be)


## Units
units(land) <- "tonnes"
units(disc) <- "tonnes"
units(land_age) <- '1000'
units(disc_age) <- '1000'
units(land_wt) <- "kg"
units(disc_wt) <- "kg"
units(pr) <- "000 euros"

ca <- FLCatchExt(name = S, landings = land, landings.n = land_age, 
		 landings.wt = land_wt, discards = disc, discards.n = disc_age,
		 discards.wt = disc_wt, landings.sel = land.sel, discards.sel = disc.sel,
		 price = pr, catch.q = q, alpha = al, beta = be)

## Metier
m <- FLMetiersExt(FLMetierExt(name = "Z", 
		 catches = ca, 
		 effshare = effsh, vcost = vcost))
names(m) <- "Z"
## Fleet

assign(paste0(S, "_fleet"),
       FLFleetExt(metiers = m, name = paste0(S, "_fleet"),
		       effort = eff, capacity = cap, fcost = fcost,
		       crewshare = crew)
       )

}



## set ranges

MON_fleet@range["minyear"]  <- 2015
MON_fleet@range["maxyear"]  <- 2017
COD_fleet@range["minyear"]  <- 2015
COD_fleet@range["maxyear"]  <- 2017
HAD_fleet@range["minyear"]  <- 2015
HAD_fleet@range["maxyear"]  <- 2017
WHG_fleet@range["minyear"]  <- 2015
WHG_fleet@range["maxyear"]  <- 2017
NMEG_fleet@range["minyear"]  <- 2015
NMEG_fleet@range["maxyear"]  <- 2017
NHKE_fleet@range["maxyear"]  <- 2017
NHKE_fleet@range["minyear"]  <- 2015
NEP16_fleet@range["maxyear"]  <- 2017
NEP16_fleet@range["minyear"]  <- 2015
NEP17_fleet@range["maxyear"]  <- 2017
NEP17_fleet@range["minyear"]  <- 2015
NEP19_fleet@range["maxyear"]  <- 2017
NEP19_fleet@range["minyear"]  <- 2015
NEP2021_fleet@range["maxyear"]  <- 2017
NEP2021_fleet@range["minyear"]  <- 2015
NEP22_fleet@range["maxyear"]  <- 2017
NEP22_fleet@range["minyear"]  <- 2015




fleets <- FLFleetsExt(IE_Otter, MON_fleet, COD_fleet, HAD_fleet, NHKE_fleet,
		      NMEG_fleet, NEP16_fleet, NEP17_fleet, NEP19_fleet, NEP22_fleet,
		      WHG_fleet, NEP2021_fleet)

names(fleets) <- c("IE_Otter", "MON_fleet", "COD_fleet", "HAD_fleet", "NHKE_fleet",
		      "NMEG_fleet", "NEP16_fleet", "NEP17_fleet", "NEP19_fleet", "NEP22_fleet",
		      "WHG_fleet", "NEP2021_fleet")


fleets <- fleets[sort(names(fleets))]

## range in fleets
lapply(fleets, range)

## range in metiers

lapply(fleets, function(fl) {
	       lapply(fl@metiers, range) 
		      })

lapply(fleets, function(fl) lapply(fl@metiers, function(m) lapply(m@catches, range)))



save(fleets, file = file.path("..", "fleets", "fleets.RData"))
