################################################
## A basic Celtic Sea model
## To simulate the effect of the  
## location choice models
##
################################################

library(FLBEIA)

## fleets
load(file.path("..", "fleets", "fleets.RData"))

## biols
load(file.path("..", "biols", "biols.RData"))

## SR
load(file.path("..", "biols","SR_fits2.RData"))

ls()


######################
## main control
#####################

n.proj.yrs <- 15 
ni <- 500
ns <- 4

data.yrs <- c(range(biols)[["minyear"]],
	    range(biols)[["maxyear"]])

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
## using SMFB, set a rest]riction on 'catch' or 'landings' by fleet
restriction  <- rep("catch",n.fls) ;                     names(restriction) <-fls 

### CATCH MODELS
## Can change for each fleet/stock
c.mod<-stack(lapply(fleets,catchNames))
c.mod$catch.mod<-sapply(c.mod$values,function(x) {
  if(x %in% c("COD","HAD","MON","NHKE","NMEG", "WHG")) return("CobbDouglasAge") else
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

## Takes a long time, so load if already generated
#if('FLFleetsExt_expanded.RData' %in% list.files(file.path("..", "model_inputs"))) { 
#  load(file.path("..", "model_inputs", 'FLFleetsExt_expanded.RData'))
#}

#if(!'FLFleetsExt_expanded.RData' %in% list.files(file.path("..", "model_inputs"))) {


print("############  Expanding the FLFleet object to projection years ############")

fleets<-lapply(fleets,window,data.yrs[1],proj.yrs[length(proj.yrs)]) 

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
      fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[, ac(proj.yrs)][is.na(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[, ac(proj.yrs)])]<-0
      # discards selectivity as the inverse of the landings sel
      fleets[[i]]@metiers[[j]]@catches[[k]]@discards.sel[, ac(proj.yrs)] <- 1-fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[, ac(proj.yrs)]
      fleets[[i]]@metiers[[j]]@catches[[k]]@alpha[, ac(proj.yrs)]        <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@alpha[,fl.proj.avg.yrs])
      fleets[[i]]@metiers[[j]]@catches[[k]]@beta[, ac(proj.yrs)]         <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@beta[,fl.proj.avg.yrs])
      fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[,ac(proj.yrs) ]     <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[,fl.proj.avg.yrs])
      fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[, ac(proj.yrs)][is.na(fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[, ac(proj.yrs)])]<-0
      fleets[[i]]@metiers[[j]]@catches[[k]]@price[, ac(proj.yrs)]  <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@price[,fl.proj.avg.yrs])
      
    }
  }
}

## Expand for the iterations
fleets <- FLFleetsExt(lapply(fleets, FLBEIA:::propagateFLF, iter = ni, fill.iter = TRUE))

save(fleets,file=file.path("..", "model_inputs",'FLFleetsExt_expanded.RData'))

#}

###########################
##### Fleet controls ######
###########################

eff.res <- "prev"

flq   <- FLQuant(dimnames = list(quant = 'all', year = data.yrs[1]:proj.yrs[length(proj.yrs)], season = 1:ns), iter = ni)

fleets.ctrl      <- create.fleets.ctrl(fls = fls,n.fls.stks=n.flts.stks,fls.stksnames=flts.stksnames,
                                         effort.models= effort.models,catch.models=catch.models,
                                         capital.models=capital.models, price.models=price.models,flq=flq,
                                       "effort.restr.IE_Otter" = eff.res,
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

save(fleets.ctrl, file = file.path("..", "model_inputs", "fleets_ctrl.RData"))


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
load(file.path("..", "biols", 'SR_fits2.RData'))

################
## COD
###############

COD.sr<-sr.fits[["COD.sr"]]
COD_sr.model<-'segreg'

COD_params.n<-length(params(COD.sr))
ny<-last.yr-first.yr+1

COD_params.array<-array(dim=c(COD_params.n, ny, ns=ns, ni=ni), dimnames=list(param=c("a","b"), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

COD_params.array[1,,,]<-params(COD.sr)[1]
COD_params.array[2,,,]<-params(COD.sr)[2]
COD_params.name     <- c('a','b') 

## multiplicative uncertainty
unit.flq<-array(1,dim=c(1, ny, ns=ns, ni=ni), dimnames=list(age=as.character(1), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

COD_uncertainty.flq <- FLQuant(NA,dimnames=list(age='1',year=first.yr:last.yr, season = 1:ns, iter=1:ni))     
COD_uncertainty.flq[,,,,] <- exp(rnorm(length(first.yr:last.yr)*ni,mean=0, sd(residuals(COD.sr))))


## proportion of spawning occurring in season x
COD_proportion.flq  <-  FLQuant(0,dimnames=list(year=first.yr:last.yr, season = 1:ns))
COD_proportion.flq[,,,2] <- 1

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
#COD_ssb.flq[,ac(2017)] <- ssb(biols[["COD"]])[,ac(2017)]

################
## HAD
###############

HAD.sr<-sr.fits[["HAD.sr"]]
HAD_sr.model<-'segreg'

HAD_params.n<-length(params(HAD.sr))
ny<-last.yr-first.yr+1

HAD_params.array<-array(dim=c(HAD_params.n, ny, ns=ns, ni=ni), dimnames=list(param=c("a","b"), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

HAD_params.array[1,,,]<-params(HAD.sr)[1]
HAD_params.array[2,,,]<-params(HAD.sr)[2]
HAD_params.name     <- c('a','b') 

## multiplicative uncertainty
unit.flq<-array(1,dim=c(1, ny, ns=ns, ni=ni), dimnames=list(age=as.character(1), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

HAD_uncertainty.flq <- FLQuant(exp(rnorm(length(first.yr:last.yr)*ni,mean=0,
                                            sd(residuals(HAD.sr)))),dimnames=list(age='1',year=first.yr:last.yr, season = 1:ns, iter=1:ni))     


## proportion of spawning occurring in season x
HAD_proportion.flq  <-  FLQuant(0,dimnames=list(year=first.yr:last.yr, season = 1:ns))
HAD_proportion.flq[,,,2]  <- 1

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

#########
## WHG ##
#########

WHG.sr<-sr.fits[["WHG.sr"]]
WHG_sr.model<-'segreg'

WHG_params.n<-length(params(WHG.sr))
ny<-last.yr-first.yr+1

WHG_params.array<-array(dim=c(WHG_params.n, ny, ns=ns, ni=ni), dimnames=list(param=c("a","b"), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

WHG_params.array[1,,,]<-params(WHG.sr)[1]
WHG_params.array[2,,,]<-params(WHG.sr)[2]
WHG_params.name     <- c('a','b') 

## multiplicative uncertainty
unit.flq<-array(1,dim=c(1, ny, ns=ns, ni=ni), dimnames=list(age=as.character(0), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

WHG_uncertainty.flq <- FLQuant(exp(rnorm(length(first.yr:last.yr)*ni,mean=0,
                                            sd(residuals(WHG.sr)))),dimnames=list(age='0',year=first.yr:last.yr,season = 1:ns,iter=1:ni))     

## proportion of spawning occurring in season x
WHG_proportion.flq  <-  FLQuant(0,dimnames=list(year=first.yr:last.yr, season = 1:ns))
WHG_proportion.flq[,,,2]  <- 1


## need to set the following?
WHG_prop.avg.yrs    <- ac(2015:2017)

## age at rec, season
age.rec<-1
WHG_timelag.matrix  <- matrix(c(age.rec,ns),2,ns, dimnames = list(c('year', 'season'),season = 1:ns)) 

## these will be defined elsewhere

WHG_age.min<-WHG_range.min<-as.numeric(dimnames(n(biols[["WHG"]]))$age[1])
WHG_age.max<-WHG_range.max<-WHG_range.plusgroup<-as.numeric(dimnames(n(biols[["WHG"]]))$age[length(dimnames(n(biols[["WHG"]]))$age)])
WHG_unit<-2 ## in which season does the stock recruit
WHG_range.minyear<-first.yr

## expand to entire time period
WHG_rec.flq            <- propagate(window(rec(WHG.sr), first.yr,proj.yr-1), iter = ni, fill = T)
WHG_ssb.flq            <- propagate(window(ssb(WHG.sr), first.yr,proj.yr-1), iter = ni, fill = T)
##WHG_ssb.flq[,ac(2016)] <- ssb(biols[["WHG_NS"]])[,ac(2016)]

#########
## MON ##
#########

MON.sr<-sr.fits[["MON.sr"]]
MON_sr.model<-'segreg'

MON_params.n<-length(params(MON.sr))
ny<-last.yr-first.yr+1

MON_params.array<-array(dim=c(MON_params.n, ny, ns=ns, ni=ni), dimnames=list(param=c("a","b"), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

MON_params.array[1,,,]<-params(MON.sr)[1]
MON_params.array[2,,,]<-params(MON.sr)[2]
MON_params.name     <- c('a','b') 

## multiplicative uncertainty
unit.flq<-array(1,dim=c(1, ny, ns=ns, ni=ni), dimnames=list(age=as.character(0), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

MON_uncertainty.flq <- FLQuant(exp(rnorm(length(first.yr:last.yr)*ni,mean=0,
                                            sd(residuals(MON.sr)))),dimnames=list(age='0',year=first.yr:last.yr, season = 1:ns,iter=1:ni))     

## proportion of spawning occurring in season x
MON_proportion.flq  <-  FLQuant(0,dimnames=list(year=first.yr:last.yr, season = 1:ns))
MON_proportion.flq[,,,2] <- 1 

## need to set the following?
MON_prop.avg.yrs    <- ac(2015:2017)

## age at rec, season
age.rec<-1
MON_timelag.matrix  <- matrix(c(age.rec,ns),2,ns, dimnames = list(c('year', 'season'),season = 1:ns)) 

## these will be defined elsewhere

MON_age.min<-MON_range.min<-as.numeric(dimnames(n(biols[["MON"]]))$age[1])
MON_age.max<-MON_range.max<-MON_range.plusgroup<-as.numeric(dimnames(n(biols[["MON"]]))$age[length(dimnames(n(biols[["MON"]]))$age)])
MON_unit<-2 ## in which season does the stock recruit
MON_range.minyear<-first.yr

## expand to entire time period
MON_rec.flq            <- propagate(window(rec(MON.sr), first.yr,proj.yr-1), iter = ni, fill = T)
MON_ssb.flq            <- propagate(window(ssb(MON.sr), first.yr,proj.yr-1), iter = ni, fill = T)
##MON_ssb.flq[,ac(2016)] <- ssb(biols[["MON_NS"]])[,ac(2016)]

##########
## NHKE ##
##########

NHKE.sr<-sr.fits[["NHKE.sr"]]
NHKE_sr.model<-'segreg'

NHKE_params.n<-length(params(NHKE.sr))
ny<-last.yr-first.yr+1

NHKE_params.array<-array(dim=c(NHKE_params.n, ny, ns=ns, ni=ni), dimnames=list(param=c("a","b"), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

NHKE_params.array[1,,,]<-params(NHKE.sr)[1]
NHKE_params.array[2,,,]<-params(NHKE.sr)[2]
NHKE_params.name     <- c('a','b') 

## multiplicative uncertainty
unit.flq<-array(1,dim=c(1, ny, ns=ns, ni=ni), dimnames=list(age=as.character(0), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

NHKE_uncertainty.flq <- FLQuant(exp(rnorm(length(first.yr:last.yr)*ni,mean=0,
                                            sd(residuals(NHKE.sr)))),dimnames=list(age='0',year=first.yr:last.yr, season = 1:ns, iter=1:ni))     

## proportion of spawning occurring in season x
NHKE_proportion.flq  <-  FLQuant(0,dimnames=list(year=first.yr:last.yr, season = 1:ns))
NHKE_proportion.flq[,,,2] <- 1

## need to set the following?
NHKE_prop.avg.yrs    <- ac(2015:2017)

## age at rec, season
age.rec<-1
NHKE_timelag.matrix  <- matrix(c(age.rec,ns),2,ns, dimnames = list(c('year', 'season'),season = 1:ns)) 

## these will be defined elsewhere

NHKE_age.min<-NHKE_range.min<-as.numeric(dimnames(n(biols[["NHKE"]]))$age[1])
NHKE_age.max<-NHKE_range.max<-NHKE_range.plusgroup<-as.numeric(dimnames(n(biols[["NHKE"]]))$age[length(dimnames(n(biols[["NHKE"]]))$age)])
NHKE_unit<-2 ## in which season does the stock recruit
NHKE_range.minyear<-first.yr

## expand to entire time period
NHKE_rec.flq            <- propagate(window(rec(NHKE.sr), first.yr,proj.yr-1), iter = ni, fill = T)
NHKE_ssb.flq            <- propagate(window(ssb(NHKE.sr), first.yr,proj.yr-1), iter = ni, fill = T)
##NHKE_ssb.flq[,ac(2016)] <- ssb(biols[["NHKE_NS"]])[,ac(2016)]


##########
## NMEG ##
##########

NMEG.sr<-sr.fits[["NMEG.sr"]]
NMEG_sr.model<-'segreg'

NMEG_params.n<-length(params(NMEG.sr))
ny<-last.yr-first.yr+1

NMEG_params.array<-array(dim=c(NMEG_params.n, ny, ns=ns, ni=ni), dimnames=list(param=c("a","b"), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

NMEG_params.array[1,,,]<-params(NMEG.sr)[1]
NMEG_params.array[2,,,]<-params(NMEG.sr)[2]
NMEG_params.name     <- c('a','b') 

## multiplicative uncertainty
unit.flq<-array(1,dim=c(1, ny, ns=ns, ni=ni), dimnames=list(age=as.character(1), year=as.character(first.yr:last.yr), season=as.character(1:ns), iter=as.character(1:ni)))

NMEG_uncertainty.flq <- FLQuant(exp(rnorm(length(first.yr:last.yr)*ni,mean=0,
                                            sd(residuals(NMEG.sr)))),dimnames=list(age='1',year=first.yr:last.yr, season = 1:ns, iter=1:ni))     

## proportion of spawning occurring in season x
NMEG_proportion.flq  <-  FLQuant(1,dimnames=list(year=first.yr:last.yr, season = 1:ns))
NMEG_proportion.flq[,,,2] <- 1  

## need to set the following?
NMEG_prop.avg.yrs    <- ac(2015:2017)

## age at rec, season
age.rec<-1
NMEG_timelag.matrix  <- matrix(c(age.rec,ns),2,ns, dimnames = list(c('year', 'season'),season = 1:ns)) 

## these will be defined elsewhere

NMEG_age.min<-NMEG_range.min<-as.numeric(dimnames(n(biols[["NMEG"]]))$age[1])
NMEG_age.max<-NMEG_range.max<-NMEG_range.plusgroup<-as.numeric(dimnames(n(biols[["NMEG"]]))$age[length(dimnames(n(biols[["NMEG"]]))$age)])
NMEG_unit<-2 ## in which season does the stock recruit
NMEG_range.minyear<-first.yr

## expand to entire time period
NMEG_rec.flq            <- propagate(window(rec(NMEG.sr), first.yr,proj.yr-1), iter = ni, fill = T)
NMEG_ssb.flq            <- propagate(window(ssb(NMEG.sr), first.yr,proj.yr-1), iter = ni, fill = T)
##NMEG_ssb.flq[,ac(2016)] <- ssb(biols[["NMEG_NS"]])[,ac(2016)]

###########

stks.data <- list(COD =ls(pattern="^COD"),
                  HAD =ls(pattern="^HAD"),
                  WHG =ls(pattern="^WHG"),
                  MON =ls(pattern="^MON"),
                  NHKE =ls(pattern="^NHKE"),
                  NMEG =ls(pattern="^NMEG")
                                    ) 

SRs      <- create.SRs.data(yrs = c(first.yr = first.yr, proj.yr = proj.yr, last.yr = last.yr), ns = ns, ni = ni, stks.data = stks.data)

## For fixedPopulation stocks fill the biols @n with a value
## and create a stock-recruit for fixed Pop stocks with constant high recruitment
#nep.stks <- grep("NEP", stks, value = T)

#for (i in nep.stks) {
#  biols[[i]]@n[,ac(proj.yrs)]<-biols[[i]]@n[,ac(data.yrs[2])]  # fill the population n with last
#   ## if its a non UWTV survey Nephrop stock, fill with an arbritary large number
#  if(any(is.na(biols[[i]]@n[,ac(data.yrs[2])]))) {
#    biols[[i]]@n[,ac(proj.yrs)]<- 1e6
#  }
#  SRs[[i]]<-SRs[["COD"]]
#  SRs[[i]]@rec[]<-biols[[i]]@n[,ac(data.yrs[length(data.yrs)])]
#  SRs[[i]]@ssb[]<-biols[[i]]@n[,ac(data.yrs[length(data.yrs)])]
#  SRs[[i]]@model<-'geomean'
#}

## Missing SSBs

SRs[["COD"]]@ssb[,ac(2017)]  <- ssb(biols[["COD"]])[,ac(2017)]
SRs[["NMEG"]]@ssb[,ac(2017)] <- ssb(biols[["NMEG"]])[,ac(2017)]


SRs <- SRs[sort(names(SRs))]
save(SRs,file=file.path("..", "model_inputs",'SRs.RData'))

########################### BD STOCKS #################################
#######################################################################

load(file.path("..", "biols", "BDs.RData"))

## Expand the object and fill the parameters

BDs <- lapply(BDs, function(x) {
   x@biomass     <-  expand(x@biomass, year = first.yr:last.yr)
   x@gB          <-  expand(x@gB, year = first.yr:last.yr)
   x@catch       <-  expand(x@catch, year = first.yr:last.yr)
   x@uncertainty <-  expand(x@uncertainty, year = first.yr:last.yr)
   x@uncertainty[,ac(proj.yr:last.yr),,] <- 1.2 

   params <- array(data = NA, dim = c(3, dim(x@biomass)[c(2,4)],1), dimnames = 
		   list(param = c("K", "p", "r"),
				  year = first.yr:last.yr,
		   season = 1:4,
		   iter = 1))
     params[1,1:length(first.yr:last.yr),1:4,1] <- x@params[1,1,1,1] 
     params[2,1:length(first.yr:last.yr),1:4,1] <- x@params[2,1,1,1] 
     params[3,1:length(first.yr:last.yr),1:4,1] <- x@params[3,1,1,1] 
     x@params <- params

     al  <- array(data = NA, dim = c(length(first.yr:last.yr), 4, 1))
     al[] <- x@alpha[1,1,1]
     x@alpha <- al

   return(x)
			}) 

BDs <- lapply(BDs, propagate, iter = ni, fill.iter = TRUE)

save(BDs, file = file.path("..", "model_inputs", "BDs.RData"))

#######################################################################
########################## BIOLS CTRL #################################
#######################################################################

# N.B. MUST be in alphabetical order
biols <- biols[sort(names(biols))]
stks<-names(biols)

growth.model     <- c(COD='ASPG',HAD='ASPG',
		      MON='ASPG',
                      NEP16 = 'BDPG',
                      NEP17 = 'BDPG',
                      NEP19 = 'BDPG',
                      NEP2021 = 'BDPG',
                      NEP22 = 'BDPG',
		      NHKE = 'ASPG', NMEG = 'ASPG',
                      WHG_NS="ASPG")
growth.model <- growth.model[sort(names(growth.model))]

biols.ctrl       <- create.biols.ctrl(stksnames=stks,growth.model=growth.model)

########################################################################
########################################################################

save(biols,file=file.path('..', 'model_inputs','biols_expanded.RData'))
save(biols.ctrl,file=file.path('..', 'model_inputs', 'biols_ctrl.RData'))


####################################################################################
############################## ASSESSMENT CONTROL ##################################
####################################################################################

assess.models<-rep('NoAssessment',length(stks))           ; names(assess.models)<-stks
assess.ctrl<-create.assess.ctrl(stksnames=stks,assess.models=assess.models)

assess.ctrl <- assess.ctrl[sort(names(assess.ctrl))]

save(assess.ctrl,file=file.path("..", "model_inputs",'assess_ctrl.RData'))

#####################################################################################
############################## OBSERVATION CONTROL ##################################
#####################################################################################

stkObs.models<-rep('perfectObs',length(stks)) ; names(stkObs.models)<-stks
obs.ctrl  <- create.obs.ctrl(stksnames = stks,  stkObs.models = stkObs.models)

obs.ctrl <- obs.ctrl[sort(names(obs.ctrl))]
save(obs.ctrl,file=file.path("..", "model_inputs", 'obs_ctrl.RData'))

#####################################################################################
################################### INDICES #########################################
#####################################################################################
# Create FLIndex for BD and ASPG models

indices<-NULL

save(indices,file=file.path("..", "model_inputs" ,'indices.RData'))
#####################################################################################
#####################################################################################
#####################################################################################

####################################################################################
############################## CONTROLS FOR QUOTA SHARES ###########################
####################################################################################

# Years on which the share is based [can be an average or single year]
COD_advice.avg.yrs   <-  HAD_advice.avg.yrs   <-  MON_advice.avg.yrs   <-  
NHKE_advice.avg.yrs   <-  NMEG_advice.avg.yrs   <-  NEP16_advice.avg.yrs   <-  
NEP17_advice.avg.yrs   <-  NEP19_advice.avg.yrs   <-  NEP2021_advice.avg.yrs   <-  
NEP22_advice.avg.yrs   <-  WHG_advice.avg.yrs   <-  
c((data.yrs[2]-2):(data.yrs[2]))


for(s in stks) {
	assign(paste0(s,"_advice.TAC.flq"),
#	FLQuant(NA,dimnames=list(year=data.yrs[1]:proj.yrs[length(proj.yrs)], season = 1:ns), iter = ni)) 
	FLQuant(NA,dimnames=list(year=data.yrs[1]:proj.yrs[length(proj.yrs)]), iter = ni)) 

}

COD_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]     <- apply(quantSums(landWStock(fleets,"COD"))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
HAD_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]     <- apply(quantSums(landWStock(fleets,'HAD'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
MON_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]     <- apply(quantSums(landWStock(fleets,'MON'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
NHKE_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]    <- apply(quantSums(landWStock(fleets,'NHKE'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
NMEG_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]    <- apply(quantSums(landWStock(fleets,'NMEG'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
NEP16_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]   <- apply(quantSums(landWStock(fleets,'NEP16'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
NEP17_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]   <- apply(quantSums(landWStock(fleets,'NEP17'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
NEP19_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]   <- apply(quantSums(landWStock(fleets,'NEP19'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
NEP2021_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])] <- apply(quantSums(landWStock(fleets,'NEP2021'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
NEP22_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]   <- apply(quantSums(landWStock(fleets,'NEP22'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)
WHG_advice.TAC.flq[,ac(data.yrs[1]:data.yrs[2])]     <- apply(quantSums(landWStock(fleets,'WHG'))[,ac(data.yrs[1]:data.yrs[2])],2,sum)

stks.data <- list(COD =ls(pattern="^COD"),
                  HAD =ls(pattern="^HAD"),
		  MON = ls(pattern="^MON"),
		  NHKE = ls(pattern="^NHKE"),
		  NMEG = ls(pattern="^NMEG"),
		  NEP16 = ls(pattern="^NEP16"),
		  NEP17 = ls(pattern="^NEP17"),
	   	  NEP19 = ls(pattern="^NEP19"),
		  NEP2021 = ls(pattern="^NEP2021"),
		  NEP22 = ls(pattern="^NEP22"),
		  WHG = ls(pattern="^WHG")
		  )  

advice   <- create.advice.data(yrs = c(first.yr = first.yr, proj.yr = proj.yr, last.yr = last.yr), ns = ns, ni = ni, stks.data = stks.data,
                               fleets = fleets)

for(st in stks) {
	advice$quota.share[[st]][is.na(advice$quota.share[[st]])] <- 0
	advice$quota.share[[st]][advice$quota.share[[st]]> 1 ] <- 1 ## fix as Irish landings higher than stock
	advice$quota.share[[st]][advice$quota.share[[st]]< 0 ] <- 0 ## fix as Irish landings higher than stock
}


### NEED TO CHANGE THE TACS FOR 2018 !!!!
## Use same as WG assumption
### Else assumes the average of last 3 years TACs!!

## Nephrops don't really matter as fixed pop

advice$TAC["COD","2018"][] <- 2354 
advice$TAC["HAD","2018"][] <- 8225 
advice$TAC["MON","2018"][] <- 28414  
advice$TAC["NHKE","2018"][] <- 94812
advice$TAC["NMEG","2018"][] <- 14397
advice$TAC["NEP16","2018"][] <- 2734 
advice$TAC["NEP17","2018"][] <- 551
advice$TAC["NEP19","2018"][] <- 173
advice$TAC["NEP2021","2018"][] <- 8673
advice$TAC["NEP22","2018"][] <- 4322
advice$TAC["WHG","2018"][] <- 10332



## Take the int year TAC from single stock advice!!
save(advice,file=file.path("..", "model_inputs",'advice.RData'))

####################################################################################
############################## REF POINTS AND HARVEST CONTROL RULES ################
####################################################################################

HCR.models       <- c(COD = "IcesHCR", HAD = "IcesHCR", MON = 'IcesHCR',
		      NHKE = "IcesHCR", NMEG = "IcesHCR", 
		      NEP16 = "IcesHCR", NEP17 = "IcesHCR",
		      NEP19 = "IcesHCR", NEP2021 = "IcesHCR",
		      NEP22 = "IcesHCR",
                      WHG = "IcesHCR"
                      ) 
HCR.models <- HCR.models[sort(names(HCR.models))]

ref.pts.cod        <- matrix(c(0.35, 7300, 10300), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))
ref.pts.had        <- matrix(c(0.4, 6700, 10000), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))
ref.pts.mon        <- matrix(c(0.28, 16032, 22278), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))
ref.pts.nhke       <- matrix(c(0.28, 32000, 45000), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))
ref.pts.nmeg       <- matrix(c(0.191, 37100, 41800), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))
ref.pts.whg        <- matrix(c(0.52, 25000, 35000), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))

msy_trig           <- 0.7 * BDs[["NEP16"]]@params[1] ## approximated at 70% K 
ref.pts.nep16      <- matrix(c(0.062, 0.4 * msy_trig, msy_trig), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))

msy_trig           <- mean(biols[["NEP17"]]@wt, na.rm = T) * 540 ## from advice sheet
ref.pts.nep17      <- matrix(c(0.085, 0.4 * msy_trig,msy_trig), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))

msy_trig           <- mean(biols[["NEP19"]]@wt, na.rm = T) * 430 ## from advice sheet
ref.pts.nep19      <- matrix(c(0.093,0.4 * msy_trig ,msy_trig), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))

msy_trig           <- 0.7 * BDs[["NEP2021"]]@params[1] ## approximated at 70% K 
ref.pts.nep2021    <- matrix(c(0.06, 0.4 * msy_trig, msy_trig), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))

msy_trig           <- mean(biols[["NEP22"]]@wt, na.rm = T) * 990 ## from advice sheet
ref.pts.nep22      <- matrix(c(0.128, 0.4 * msy_trig, msy_trig), 3,ni, dimnames = list(c('Fmsy', 'Blim', 'Btrigger'), 1:ni))


advice.ctrl      <- create.advice.ctrl(stksnames = stks, HCR.models =  HCR.models, 
                                       ref.pts.COD = ref.pts.cod, ref.pts.HAD = ref.pts.had,
				       ref.pts.MON = ref.pts.mon, ref.pts.NHKE = ref.pts.nhke,
				       ref.pts.NMEG = ref.pts.nmeg,ref.pts.WHG = ref.pts.whg,
				       ref.pts.NEP16 = ref.pts.nep16, ref.pts.NEP17 = ref.pts.nep17,
				       ref.pts.NEP19 = ref.pts.nep19, ref.pts.NEP2021 = ref.pts.nep2021,
				       ref.pts.NEP22 = ref.pts.nep22,
				       first.yr = first.yr, last.yr = last.yr)

# default option : if you want to change do it after you can access it 
# used only in the management procedure evry time step in the simulation for the intermediate year 

# for the short term forcast (intermediate year) == WG 
advice.ctrl[['COD']][['sr']]            <- list()
advice.ctrl[['COD']][['sr']][['model']] <- 'geomean'
advice.ctrl[['COD']][['sr']][['years']] <- c(y.rm = 2, num.years = 10)

advice.ctrl[['HAD']][['sr']]            <- list()
advice.ctrl[['HAD']][['sr']][['model']] <- 'geomean'
advice.ctrl[['HAD']][['sr']][['years']] <- c(y.rm = 2, num.years = 10)

advice.ctrl[['MON']][['sr']]            <- list()
advice.ctrl[['MON']][['sr']][['model']] <- 'geomean'
advice.ctrl[['MON']][['sr']][['years']] <- c(y.rm = 2, num.years = 10)

advice.ctrl[['NHKE']][['sr']]            <- list()
advice.ctrl[['NHKE']][['sr']][['model']] <- 'geomean'
advice.ctrl[['NHKE']][['sr']][['years']] <- c(y.rm = 2, num.years = 10)

advice.ctrl[['NMEG']][['sr']]            <- list()
advice.ctrl[['NMEG']][['sr']][['model']] <- 'geomean'
advice.ctrl[['NMEG']][['sr']][['years']] <- c(y.rm = 2, num.years = 10)

advice.ctrl[['WHG']][['sr']]            <- list()
advice.ctrl[['WHG']][['sr']][['model']] <- 'geomean'
advice.ctrl[['WHG']][['sr']][['years']] <- c(y.rm = 2, num.years = 10)

advice.ctrl[["NEP16"]][["growth.years"]] <- c(y.rm =1, num.years = 5)
advice.ctrl[["NEP17"]][["growth.years"]] <- c(y.rm =1, num.years = 5)
advice.ctrl[["NEP19"]][["growth.years"]] <- c(y.rm =1, num.years = 5)
advice.ctrl[["NEP2021"]][["growth.years"]] <- c(y.rm =1, num.years = 5)
advice.ctrl[["NEP22"]][["growth.years"]] <- c(y.rm =1, num.years = 5)


# advice based on catch, not landings
advice.ctrl$COD$AdvCatch[]      <- TRUE
advice.ctrl$HAD$AdvCatch[]      <- TRUE
advice.ctrl$MON$AdvCatch[]      <- TRUE
advice.ctrl$NHKE$AdvCatch[]     <- TRUE
advice.ctrl$NMEG$AdvCatch[]     <- TRUE
advice.ctrl$NEP16$AdvCatch[]    <- TRUE
advice.ctrl$NEP17$AdvCatch[]    <- TRUE
advice.ctrl$NEP19$AdvCatch[]    <- TRUE
advice.ctrl$NEP2021$AdvCatch[]  <- TRUE
advice.ctrl$NEP22$AdvCatch[]    <- TRUE
advice.ctrl$WHG$AdvCatch[]      <- TRUE

advice.ctrl <- advice.ctrl[sort(names(advice.ctrl))]
save(advice.ctrl,file=file.path("..", "model_inputs", 'advice_ctrl.RData'))

fleets2 <- calculate.q.sel.flrObjs(biols, fleets, NULL, fleets.ctrl, 2015:2017, proj.yrs) 

## Condition uncertainty in catch.q - IE_Otter only
## Add some lognormal error around the mean value
## But what's the basis of this?  Do we just want to sample from the past 3
## years or add random noise (variability)??

## Likely the relationships between species are mixed, so sample from the most
## recent years?

## Do we also want to include combinations of the years?

yr.ref <- sample(2015:2017, ni, replace = TRUE)

i  <- "IE_Otter"
  nms.metiers <- names(fleets2[[i]]@metiers)
  
  for( j in nms.metiers){
    nms.stks <- names(fleets2[[i]]@metiers[[j]]@catches)
    
    for( k in nms.stks){

for(it in 1:ni) {
fleets2[[i]]@metiers[[j]]@catches[[k]]@catch.q[,ac(proj.yrs),,,,it]  <- fleets2[[i]]@metiers[[j]]@catches[[k]]@catch.q[,ac(yr.ref[it]),,,,1]
	   }

      fleets2[[i]]@metiers[[j]]@catches[[k]]@catch.q[, ac(proj.yrs)][is.na(fleets2[[i]]@metiers[[j]]@catches[[k]]@catch.q[, ac(proj.yrs)])]<-0
      
    }
  }

fleets2[["IE_Otter"]][[1]][["COD"]]@catch.q[,ac(proj.yrs),,]
fleets2[["IE_Otter"]][[1]][["COD"]]@catch.q[,"2015",,1] 


## use the inbuilt function calcs - not sure why different
fleets <- fleets2

save(fleets,file=file.path("..", "model_inputs", 'FLFleetsExt_expanded.RData'))

