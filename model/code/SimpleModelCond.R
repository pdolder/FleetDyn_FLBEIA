################################################
## A basic Celtic Sea model
## To check everything works
##
################################################

library(FLBEIA)

## fleets
load(file.path("..", "fleets", "fleets.RData"))

## biols
load(file.path("..", "biols", "biols.RData"))

## SR
load(file.path("..", "biols","SR_fits.RData"))

ls()


######################
## main control
#####################

n.proj.yrs <- 5
ni <- 1
ns <- 4

data.yrs <- c(range(biols)["minyear"][],
	    range(biols)["maxyear"][])

proj.yrs<-seq(data.yrs[2]+1,data.yrs[2]+n.proj.yrs) # years for projections
first.yr<-data.yrs[1] ; 
last.yr<-proj.yrs[length(proj.yrs)]; 
proj.yr<-proj.yrs[1]  
hist.yrs<-first.yr:(proj.yr-1)  

main.ctrl<-list()
main.ctrl$sim.years<-c(initial=proj.yrs[1],final=proj.yrs[length(proj.yrs)])
save(main.ctrl,file=file.path("..","model_inputs",'main.ctrl.RData'))



######################
## Fleet conditioning
######################

fls   <- names(fleets)
n.fls <-length(fleets) #number of the fleets

stks <- names(biols)
n.stks<-sum(sapply(sapply(fleets, catchNames), length)) # number of the fleet/stocks
n.flts.stks      <- sapply(sapply(fleets, catchNames), length) # number of stocks caught by each fleet.
flts.stksnames   <- NULL; for(f in 1:length(fleets))  flts.stksnames <- c(flts.stksnames, catchNames(fleets[[f]])) 

# Years over which to average the fleet inputs
n.avg.yrs<-3     ;  fl.proj.avg.yrs<- ac((data.yrs[2]-(n.avg.yrs-1)):data.yrs[2])

#### FLEET MODELS
## Fixed effort, SMFB etc...
## SMFB, min equivilent to FCube min
effort.models    <- rep("SMFB",n.fls) ;          names(effort.models)<-fls
## using SMFB set the effort limitation by fleet, i.e. vector with n.fl values with min, max etc..
effort.restr <- rep("min",n.fls) ;                     names(effort.restr) <-fls 
## using SMFB, set a rest]riction on 'catch' or 'landings' by fleet
restriction  <- rep("catch",n.fls) ;                     names(restriction) <-fls 

### CATCH MODELS
## Can change for each fleet/stock
c.mod<-stack(lapply(fleets,catchNames))
c.mod$catch.mod<-sapply(c.mod$values,function(x) {
  if(x %in% c("COD","HAD","WHG","MON","NHKE","NHKE")) return("CobbDouglasAge") else
  return("CobbDouglasBio")
})
catch.models     <- c.mod$catch.mod ; names(catch.models)<-paste(c.mod$ind,c.mod$values,sep=".")

### CAPTIAL MODELS
## Options are:
capital.models   <- rep("fixedCapital",n.fls)           ; names(capital.models)<-fls

### PRICE MODELS
## Options are:
price.models     <- rep("fixedPrice",n.stks)            ; names(price.models)<-paste(c.mod$ind,c.mod$values,sep=".")

## No covariates
covars<-NULL


###############################################
## Expands the fleet object to the projection
## years
###############################################

# Expand the fleet.....

print("############  Expanding the FLFleet object to projection years ############")

lapply(fleets, function(fl) 
lapply(fl@metiers, function(met)
       unlist(dims(met)[c("minyear", "maxyear")])))

lapply(fleets, function(fl) 
lapply(fl@metiers, function(met) 
lapply(met@catches, function(c)
t(as.matrix(unlist(dims(c)[c("minyear", "maxyear")]))))))


## Fudge to overcome conflict with validFLMetierExt check

setMethod("window", signature(x="FLFleetExt"),
 function(x, start=dims(x)$minyear, end=dims(x)$maxyear, extend=TRUE, frequency=1) {

    # window fleet
    x <- qapply(x, window, start, end, extend, frequency)

   # window catches
    catches <- list()
    
    metiers <- x@metiers

    for(i in seq(length(x@metiers))){

      metiers[[i]]@catches <- FLCatchesExt(lapply(x@metiers[[i]]@catches, window, start, end, extend, frequency))


    }

    # window metiers
        metiers <- FLMetiersExt(lapply(metiers, window, start, end))

       
    x@metiers <- metiers

		x@range["minyear"] <- start
		x@range["maxyear"] <- end

		return(x)
	}
)	

fleets<-lapply(fleets,window,data.yrs[1],proj.yrs[length(proj.yrs)]) ## now works

nms.fls <- names(fleets)
l.fls   <- length(nms.fls)

for(i in 1:l.fls){
  print(nms.fls[i])
  
  nms.metiers <- names(fleets[[i]]@metiers)
  l.metiers   <- length(nms.metiers)
  
  fleets[[i]]@effort[, ac(proj.yrs)]    <- yearMeans(fleets[[i]]@effort[, fl.proj.avg.yrs])
  fleets[[i]]@fcost[, ac(proj.yrs)]     <- yearMeans(fleets[[i]]@fcost[, fl.proj.avg.yrs])
  fleets[[i]]@capacity[, ac(proj.yrs)]  <- yearMeans(fleets[[i]]@capacity[, fl.proj.avg.yrs])
  fleets[[i]]@crewshare[, ac(proj.yrs)] <- yearMeans(fleets[[i]]@crewshare[, fl.proj.avg.yrs])
  
  for( j in 1:l.metiers){
    fleets[[i]]@metiers[[j]]@effshare[, ac(proj.yrs)] <- yearMeans(fleets[[i]][[j]]@effshare[, fl.proj.avg.yrs])
    fleets[[i]]@metiers[[j]]@vcost[, ac(proj.yrs)]    <- yearMeans(fleets[[i]][[j]]@vcost[, fl.proj.avg.yrs])
    
    nms.stks <- names(fleets[[nms.fls[i]]]@metiers[[nms.metiers[j]]]@catches)
    l.stks <- length(nms.stks)
    
    for( k in 1:l.stks){
      fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(proj.yrs)]  <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[,fl.proj.avg.yrs])
      fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(proj.yrs)]  <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[,fl.proj.avg.yrs])
      fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[, ac(proj.yrs)] <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[,fl.proj.avg.yrs])
      #fleets[[i]]@metiers[[j]]@catches[[k]]@discards.sel[, proj.yrs] <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@discards.sel[,fl.proj.avg.yrs])
      
      # set any NAs in the proj year to 0 (in case of no catch)
      fleets[[1]]@metiers[[1]]@catches[[1]]@discards.sel[, ac(proj.yrs)][is.na(fleets[[1]]@metiers[[1]]@catches[[1]]@discards.sel[, ac(proj.yrs)])]<-0
      # discards selectivity as the inverse of the landings sel
      fleets[[i]]@metiers[[j]]@catches[[k]]@discards.sel[, ac(proj.yrs)] <- 1-fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[, ac(proj.yrs)]
      fleets[[i]]@metiers[[j]]@catches[[k]]@alpha[, ac(proj.yrs)]        <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@alpha[,fl.proj.avg.yrs])
      fleets[[i]]@metiers[[j]]@catches[[k]]@beta[, ac(proj.yrs)]         <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@beta[,fl.proj.avg.yrs])
      fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[,ac(proj.yrs) ]     <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[,fl.proj.avg.yrs])
      fleets[[i]]@metiers[[j]]@catches[[k]]@price[, ac(proj.yrs)]  <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@price[,fl.proj.avg.yrs])
      
    }
  }
}

save(fleets,file=file.path("..", "model_inputs",'FLFleetsExt_expanded.RData'))


###########################
##### Fleet controls ######
###########################

eff.res <- "min"

flq   <- FLQuant(dimnames = list(quant = 'all', year = data.yrs[1]:proj.yrs[length(proj.yrs)], season = 1:ns), iter = ni)

fleets.ctrl      <- create.fleets.ctrl(fls = fls,n.fls.stks=n.flts.stks,fls.stksnames=flts.stksnames,
                                         effort.models= effort.models,catch.models=catch.models,
                                         capital.models=capital.models, price.models=price.models,flq=flq,
                                       "effort.restr.IE_otter" = eff.res,
				       "effort.restr.COD_fleet" = eff.res,
				       "effort.restr.HAD_fleet" = eff.res,
				       "effort.restr.WHG_fleet" = eff.res,
				       "effort.restr.NHKE_fleet" = eff.res,
				       "effort.restr.MON_fleet" = eff.res,
				       "effort.restr.NMEG_fleet" = eff.res,
				       "effort.restr.NEP16_fleet" = eff.res,
				       "effort.restr.NEP17_fleet" = eff.res,
				       "effort.restr.NEP19_fleet" = eff.res,
				       "effort.restr.NEP2021_fleet" = eff.res,
				       "effort.restr.NEP22_fleet" = eff.res
				       )

#### Expand and save objects

## This function is not in package, for some reason....

propagateFLF <- function(object, iter, fill.iter){
  
  nmet <- length(object@metiers)
  
  catches <- lapply(object@metiers, function(x) 
    lapply(x@catches, propagate, iter = iter, fill.iter = fill.iter))
  
  metiers <- vector('list', nmet)
  
  for(mt in 1:nmet){
    
    metiers[[mt]] <- FLMetierExt(name = object[[mt]]@name, desc = object[[mt]]@desc, range = object[[mt]]@range, gear = object[[mt]]@gear,
                                 effshare = propagate(object@metiers[[mt]]@effshare, iter = iter, fill.iter = fill.iter),
                                 vcost = propagate(object@metiers[[mt]]@vcost, iter = iter, fill.iter = fill.iter),
                                 catches = FLCatchesExt(catches[[mt]]))  
  }
  
  names(metiers) <- names(object@metiers)
  
  fleet <- FLFleetExt(name = object@name, desc = object@desc, range = object@range,
                      effort = propagate(object@effort, iter = iter, fill.iter = fill.iter), 
                      fcost = propagate(object@fcost, iter = iter, fill.iter = fill.iter),
                      capacity = propagate(object@capacity, iter = iter, fill.iter = fill.iter),
                      crewshare = propagate(object@crewshare, iter = iter, fill.iter = fill.iter),
                      metiers = FLMetiersExt(metiers))
  
  return(fleet)
}


fleets <- FLFleetsExt(lapply(fleets, propagateFLF, iter = ni, fill.iter= T))
save(fleets,file=file.path("..", "model_inputs", 'FLFleetsExt_expanded.RData'))


###########################################################################
## Conditioning the stocks ##
###########################################################################
## Expand the biols to simulation years
biols<-FLBiols(lapply(biols,window,data.yrs[1],proj.yrs[length(proj.yrs)])) 

# Now fill the slots in projection years for FLBiols
biols<-FLBiols(lapply(names(biols),function(x) {
  s<-biols[[x]]
  s@m[,ac(proj.yrs)]<-yearMeans(s@m[,ac((data.yrs[2]-2):data.yrs[2])])
  s@wt[,ac(proj.yrs)]<-yearMeans(s@wt[,ac((data.yrs[2]-2):data.yrs[2])])
  mat(s)[,ac(proj.yrs)]<-yearMeans(s@mat$mat[,ac((data.yrs[2]-2):data.yrs[2])])
  fec(s)[,ac(proj.yrs)]<-yearMeans(s@fec$fec[,ac((data.yrs[2]-2):data.yrs[2])])
  s@spwn <- s@n
  s@spwn[] <- 0
  return(s)
}))


## Expand biols to iters
biols <- FLBiols(lapply(biols, propagate, iter = ni, fill.iter = T))
############################ ASPG stocks ##############################
# n.b. this is all for the moment

# Load stock recruit fits
load(file.path("..", "biols", 'SR_fits.RData'))

################
## COD
###############

COD.sr<-sr.fits[["COD.sr"]]

COD_sr.model<-'segreg'

COD_params.n<-length(params(COD.sr))
ny<-last.yr-first.yr+1

COD_params.array<-array(dim=c(COD_params.n, ny, ns=ns, ni=ni), dimnames=list(param=c("a","b"), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

COD_params.array[1,,2,]<-params(COD.sr)[1]
COD_params.array[2,,2,]<-params(COD.sr)[2]
COD_params.name     <- c('a','b') 

## multiplicative uncertainty
unit.flq<-array(1,dim=c(1, ny, ns=ns, ni=ni), dimnames=list(age=as.character(1), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

COD_uncertainty.flq <- FLQuant(exp(rnorm(length(first.yr:last.yr)*ni,mean=0,
                                            sd(residuals(COD.sr)))),dimnames=list(age='1',year=first.yr:last.yr,iter=1:ni))     


## proportion of spawning occurring in season x
COD_proportion.flq  <-  FLQuant(1,dimnames=list(year=first.yr:last.yr))

## need to set the following?
COD_prop.avg.yrs    <- ac(2015:2017)

## age at rec, season
age.rec<-1
COD_timelag.matrix  <- matrix(c(age.rec,ns),2,ns, dimnames = list(c('year', 'season'),season = 1:ns)) 

## these will be defined elsewhere

COD_age.min<-COD_range.min<-as.numeric(dimnames(n(biols[["COD"]]))$age[1])
COD_age.max<-COD_range.max<-COD_range.plusgroup<-as.numeric(dimnames(n(biols[["COD"]]))$age[length(dimnames(n(biols[["COD"]]))$age)])
COD_unit<-2 ## in which season does the stock recruit
COD_range.minyear<-first.yr

## expand to entire time period
COD_rec.flq            <- propagate(window(rec(COD.sr), first.yr,proj.yr-1), iter = ni, fill = T)
COD_ssb.flq            <- propagate(window(ssb(COD.sr), first.yr,proj.yr-1), iter = ni, fill = T)
##COD_ssb.flq[,ac(2016)] <- ssb(biols[["COD_NS"]])[,ac(2016)]

################
## HAD
###############


HAD.sr<-sr.fits[["HAD.sr"]]

HAD_sr.model<-'segreg'

HAD_params.n<-length(params(HAD.sr))
ny<-last.yr-first.yr+1

HAD_params.array<-array(dim=c(HAD_params.n, ny, ns=ns, ni=ni), dimnames=list(param=c("a","b"), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

HAD_params.array[1,,2,]<-params(HAD.sr)[1]
HAD_params.array[2,,2,]<-params(HAD.sr)[2]
HAD_params.name     <- c('a','b') 

## multiplicative uncertainty
unit.flq<-array(1,dim=c(1, ny, ns=ns, ni=ni), dimnames=list(age=as.character(1), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

HAD_uncertainty.flq <- FLQuant(exp(rnorm(length(first.yr:last.yr)*ni,mean=0,
                                            sd(residuals(HAD.sr)))),dimnames=list(age='1',year=first.yr:last.yr,iter=1:ni))     


## proportion of spawning occurring in season x
HAD_proportion.flq  <-  FLQuant(1,dimnames=list(year=first.yr:last.yr))

## need to set the following?
HAD_prop.avg.yrs    <- ac(2015:2017)

## age at rec, season
age.rec<-1
HAD_timelag.matrix  <- matrix(c(age.rec,ns),2,ns, dimnames = list(c('year', 'season'),season = 1:ns)) 

## these will be defined elsewhere

HAD_age.min<-HAD_range.min<-as.numeric(dimnames(n(biols[["HAD"]]))$age[1])
HAD_age.max<-HAD_range.max<-HAD_range.plusgroup<-as.numeric(dimnames(n(biols[["HAD"]]))$age[length(dimnames(n(biols[["HAD"]]))$age)])
HAD_unit<-2 ## in which season does the stock recruit
HAD_range.minyear<-first.yr

## expand to entire time period
HAD_rec.flq            <- propagate(window(rec(HAD.sr), first.yr,proj.yr-1), iter = ni, fill = T)
HAD_ssb.flq            <- propagate(window(ssb(HAD.sr), first.yr,proj.yr-1), iter = ni, fill = T)
##HAD_ssb.flq[,ac(2016)] <- ssb(biols[["HAD_NS"]])[,ac(2016)]








## 
stks.data <- list(COD =ls(pattern="^COD"),
                  HAD =ls(pattern="^HAD"),
                  WHG =ls(pattern="^WHG"),
                  MON =ls(pattern="^MON"),
                  NHKE =ls(pattern="^NHKE"),
                  NMEG =ls(pattern="^NMEG")
                                    ) 

SRs      <- create.SRs.data(yrs = c(first.yr = first.yr, proj.yr = proj.yr, last.yr = last.yr), ns = 1, ni = ni, stks.data = stks.data)

## For fixedPopulation stocks fill the biols @n with a value
## and create a stock-recruit for fixed Pop stocks with constant high recruitment
nep.stks <- grep("NEP", stks, value = T)

for (i in nep.stks) {
  biols[[i]]@n[,ac(proj.yrs)]<-biols[[i]]@n[,ac(data.yrs[2])]  # fill the population n with last
   ## if its a non UWTV survey Nephrop stock, fill with an arbritary large number
  if(is.na(biols[[i]]@n[,ac(data.yrs[2])])) {
    biols[[i]]@n[,ac(proj.yrs)]<- 1e6
  }
  SRs[[i]]<-SRs[["COD_NS"]]
  SRs[[i]]@rec[]<-biols[[i]]@n[,ac(data.yrs[length(data.yrs)])]
  SRs[[i]]@ssb[]<-biols[[i]]@n[,ac(data.yrs[length(data.yrs)])]
  SRs[[i]]@model<-'geomean'
}


save(SRs,file=file.path(in.path,'SRs.RData'))






