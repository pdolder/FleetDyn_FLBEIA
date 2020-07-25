##############################################
##############################################
## Simple stock-recruit fits for the age-based
## stocks
##############################################

library(FLCore)
library(FLBEIA)

load(file.path("..", "biols", "biols.RData"))

#########
## COD ##
#########
# create an FLSR object fitting ricker, segreg and bevholt
COD.SR <- as(biols[["COD"]], "FLSR")
model(COD.SR) <- "segreg"
COD.SR <- trim(COD.SR, season = 2)

## update as as.FLSR no longer works on biol...!
COD.SR <- FLSR(model = "segreg")
COD.SR@rec <- biols[["COD"]]@n[1,]
COD.SR@ssb <- ssb(biols[["COD"]])
COD.SR <- expand(COD.SR, season = 1:4)
COD.SR <- trim(COD.SR, season = 2)
COD.SR<-fmle(COD.SR)
plot(COD.SR)

###########
## HAD   ##
###########

HAD.SR <- as(biols[["HAD"]], "FLSR")
model(HAD.SR) <- "segreg"


## update as as.FLSR no longer works on biol...!
HAD.SR <- FLSR(model = "segreg")
HAD.SR@rec <- biols[["HAD"]]@n[1,]
HAD.SR@ssb <- ssb(biols[["HAD"]])
HAD.SR     <- expand(HAD.SR, season = 1:4)
HAD.SR <- trim(HAD.SR, season = 2)

HAD.SR<-fmle(HAD.SR)
plot(HAD.SR)

###########
## WHG   ##
###########

WHG.SR <- as(biols[["WHG"]], "FLSR")
model(WHG.SR) <- "segreg"

WHG.SR <- FLSR(model = "segreg")
WHG.SR@rec <- biols[["WHG"]]@n[1,]
WHG.SR@ssb <- ssb(biols[["WHG"]])
WHG.SR     <- expand(WHG.SR, season = 1:4)
WHG.SR <- trim(WHG.SR, season = 2)

WHG.SR<-fmle(WHG.SR)
plot(WHG.SR)

###########
## MON   ##
###########

MON.SR <- as(biols[["MON"]], "FLSR")
model(MON.SR) <- "segreg"

## update as as.FLSR no longer works on biol...!
MON.SR <- FLSR(model = "segreg")
MON.SR@rec <- biols[["MON"]]@n[1,]
MON.SR@ssb <- ssb(biols[["MON"]])
MON.SR     <- expand(MON.SR, season = 1:4)
MON.SR <- trim(MON.SR, season = 2)

MON.SR<-fmle(MON.SR)
plot(MON.SR)

###########
## NHKE  ##
###########

NHKE.SR <- as(biols[["NHKE"]], "FLSR")
model(NHKE.SR) <- "segreg"
NHKE.SR <- trim(NHKE.SR, season = 2)

## update as as.FLSR no longer works on biol...!
NHKE.SR <- FLSR(model = "segreg")
NHKE.SR@rec <- biols[["NHKE"]]@n[1,]
NHKE.SR@ssb <- ssb(biols[["NHKE"]])
NHKE.SR     <- expand(NHKE.SR, season = 1:4)
NHKE.SR <- trim(NHKE.SR, season = 2)

NHKE.SR<-fmle(NHKE.SR)
plot(NHKE.SR)

###########
## NMEG  ##
###########

NMEG.SR <- as(biols[["NMEG"]], "FLSR")
model(NMEG.SR) <- "segreg"
NMEG.SR <- trim(NMEG.SR, season = 2)

## update as as.FLSR no longer works on biol...!
NMEG.SR <- FLSR(model = "segreg")
NMEG.SR@rec <- biols[["NMEG"]]@n[1,]
NMEG.SR@ssb <- ssb(biols[["NMEG"]])
NMEG.SR     <- expand(NMEG.SR, season = 1:4)
NMEG.SR <- trim(NMEG.SR, season = 2)

NMEG.SR<-fmle(NMEG.SR)
plot(NMEG.SR)

sr.fits <- list("COD.sr" = COD.SR,
		"HAD.sr" = HAD.SR,
		"WHG.sr" = WHG.SR,
		"MON.sr" = MON.SR,
		"NMEG.sr" = NMEG.SR,
		"NHKE.sr" = NHKE.SR)

sr.fits <- sr.fits[sort(names(sr.fits))]


save(sr.fits, file = file.path("..", "biols", "SR_fits2.RData"))
