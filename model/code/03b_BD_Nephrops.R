###############################################################
##
## Code to generate FLBDsim objects for the Nephrops stocks
## allows some simulated dynamics in abundance and catch rates
## using Pella-Tomlinson BD model
## k = capacity, r = growth rate, p = asymmetry, set to 1
#############################################################

library(FLBEIA)

## Load Biols
load(file.path("..", "biols", "biols.RData"))

## Load fleet objects for catches
load(file.path("..", "fleets", "fleets.RData"))

## Function to get total landings in the fleet object
get_fleet_catch <- function(fleets, stock, type) {
  
  res5<- lapply(fleets, function(f) {
    res3 <- lapply(f@metiers, function(m) {
      res<- lapply(m@catches@names, function(c) {
        if(c == stock) {
          if(type == "landings") {
            return(m@catches[[c]]@landings[,ac(first.yr:last.yr)])
          }
          if(type == "discards") {
            return(m@catches[[c]]@discards[,ac(first.yr:last.yr)])
                      }
          
        } else
          return(FLQuant(0,dim=c(1,length(first.yr:last.yr),1,4,1,1), dimnames = list(year = first.yr:last.yr)))
        
            })
      res2<- Reduce("+", res)
      return(res2)
      
    })
    res4<- Reduce("+", res3)
    return(res4)
    
  }) 
  res6<- Reduce("+", res5)
  return(res6)
         
  }


first.yr <- 2004
last.yr  <- 2017

## Fleet object landings and discards - so we get the right catch levels
flt_land <- lapply(catchNames(fleets), function(x) { get_fleet_catch(fleets, x, "landings") } )
names(flt_land) <- catchNames(fleets)

flt_disc <- lapply(catchNames(fleets), function(x) { get_fleet_catch(fleets, x, "discards") } )
names(flt_disc) <- catchNames(fleets)

## Expand to year range 1971 - 2017
flt_land <- lapply(flt_land, expand, year = 1971:2017)
flt_disc <- lapply(flt_disc, expand, year = 1971:2017)


### Example biomass dynamic object
data(multi)
multiBD

## Now to make for each Nephrops stock
##
# NEP16
BD_nep16 <- multiBD[[1]]

BD_nep16@name <- "NEP16"
BD_nep16 <- window(BD_nep16, start = 1971, end = 2017)
BD_nep16 <- trim(BD_nep16, year  = 1971:2017)

BD_nep16@biomass <- window(biols[["NEP16"]]@n * biols[["NEP16"]]@wt, start = 1971, end = 2017)
BD_nep16@catch   <- flt_land[["NEP16"]] + flt_disc[["NEP16"]]

# Now to adjust the uncertainty and parameters (K, r and a) and also alpha
BD_nep16@uncertainty[] <- 1 

## params is an array, so we need to set up our own
params <- array(data = NA, dim = c(3, dim(BD_nep16@biomass)[c(2,4)],1), dimnames = 
		list(param = c("K", "p", "r"),
		     year = 1971:2017,
		     season = 1:4,
		     iter = 1))

BD_nep16@params <- params
BD_nep16@params[1,1:47,1:4,1] <- max(BD_nep16@biomass, na.rm = T) * 1.2  ## k  - carrying capacity
BD_nep16@params[2,1:47,1:4,1] <- 1  ## p  - assymetry
BD_nep16@params[3,1:47,1:4,1] <- 0.1  ## r  - growth rate, porcupine so slower than rest

al <- array(data = NA, dim = c(47,4,1))
BD_nep16@alpha <- al
BD_nep16@alpha[] <- 1.2 ## how much above K can stock go

## gB is the biomass growth, set at NA
BD_nep16@gB[] <- NA

# NEP17
BD_nep17 <- multiBD[[1]]

BD_nep17@name <- "NEP17"
BD_nep17 <- window(BD_nep17, start = 1971, end = 2017)

BD_nep17@biomass <- window(biols[["NEP17"]]@n * biols[["NEP17"]]@wt, start = 1971, end = 2017)
BD_nep17@catch   <- flt_land[["NEP17"]] + flt_disc[["NEP17"]]

# Now to adjust the uncertainty and parameters (K, r and a) and also alpha
BD_nep17@uncertainty[] <- 1 

BD_nep17@params <- params
BD_nep17@params[1,1:47,1:4,1] <- max(BD_nep17@biomass, na.rm = T) * 1.2  ## k  - carrying capacity
BD_nep17@params[2,1:47,1:4,1] <- 1  ## p  - assymetry
BD_nep17@params[3,1:47,1:4,1] <- 0.2  ## r  - growth rate, porcupine so slower than rest
BD_nep17@alpha <- al
BD_nep17@alpha[] <- 1.2 ## how much above K can stock go
BD_nep17@gB[] <- NA

# NEP19
BD_nep19 <- multiBD[[1]]

BD_nep19@name <- "NEP19"
BD_nep19 <- window(BD_nep19, start = 1971, end = 2017)

BD_nep19@biomass <- window(biols[["NEP19"]]@n * biols[["NEP19"]]@wt, start = 1971, end = 2017)
BD_nep19@catch   <- flt_land[["NEP19"]] + flt_disc[["NEP19"]]

# Now to adjust the uncertainty and parameters (K, r and a) and also alpha
BD_nep19@uncertainty[] <- 1 

BD_nep19@params <- params
BD_nep19@params[1,1:47,1:4,1] <- max(BD_nep19@biomass, na.rm = T) * 1.2  ## k  - carrying capacity
BD_nep19@params[2,1:47,1:4,1] <- 1  ## p  - assymetry
BD_nep19@params[3,1:47,1:4,1] <- 0.2  ## r  - growth rate, porcupine so slower than rest

BD_nep19@alpha <- al
BD_nep19@alpha[] <- 1.2 ## how much above K can stock go
BD_nep19@gB[] <- NA


# NEP2021
BD_nep2021 <- multiBD[[1]]

BD_nep2021@name <- "NEP2021"
BD_nep2021 <- window(BD_nep2021, start = 1971, end = 2017)

BD_nep2021@biomass <- window(biols[["NEP2021"]]@n * biols[["NEP2021"]]@wt, start = 1971, end = 2017)
BD_nep2021@catch   <- flt_land[["NEP2021"]] + flt_disc[["NEP2021"]]

# Now to adjust the uncertainty and parameters (K, r and a) and also alpha
BD_nep2021@uncertainty[] <- 1 

BD_nep2021@params <- params
BD_nep2021@params[1,1:47,1:4,1] <- max(BD_nep2021@biomass, na.rm = T) * 1.2  ## k  - carrying capacity
BD_nep2021@params[2,1:47,1:4,1] <- 1  ## p  - assymetry
BD_nep2021@params[3,1:47,1:4,1] <- 0.3  ## r  - growth rate, porcupine so slower than rest

BD_nep2021@alpha <- al
BD_nep2021@alpha[] <- 1.2 ## how much above K can stock go
BD_nep2021@gB[] <- NA


# NEP22
BD_nep22 <- multiBD[[1]]

BD_nep22@name <- "NEP22"
BD_nep22 <- window(BD_nep22, start = 1971, end = 2017)

BD_nep22@biomass <- window(biols[["NEP22"]]@n * biols[["NEP22"]]@wt, start = 1971, end = 2017)
BD_nep22@catch   <- flt_land[["NEP22"]] + flt_disc[["NEP22"]]

# Now to adjust the uncertainty and parameters (K, r and a) and also alpha
BD_nep22@uncertainty[] <- 1

BD_nep22@params <- params
BD_nep22@params[1,1:36,1:4,1] <- max(BD_nep22@biomass, na.rm = T) * 1.2  ## k  - carrying capacity
BD_nep22@params[2,1:36,1:4,1] <- 1  ## p  - assymetry
BD_nep22@params[3,1:36,1:4,1] <- 0.2  ## r  - growth rate, porcupine so slower than rest

BD_nep22@alpha <- al
BD_nep22@alpha[] <- 1.2 ## how much above K can stock go
BD_nep22@gB[] <- NA


## Save the BDs as a list
BDs <- list(NEP16 = BD_nep16,
	    NEP17 = BD_nep17,
	    NEP19 = BD_nep19,
	    NEP2021 = BD_nep2021,
	    NEP22 = BD_nep22)


save(BDs, file = file.path("..", "biols", "BDs.RData"))




