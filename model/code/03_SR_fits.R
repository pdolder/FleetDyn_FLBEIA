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

COD.SR<-fmle(COD.SR)
plot(COD.SR)

###########
## HAD   ##
###########

HAD.SR <- as(biols[["HAD"]], "FLSR")
model(HAD.SR) <- "segreg"
HAD.SR <- trim(HAD.SR, season = 2)
HAD.SR<-fmle(HAD.SR)
plot(HAD.SR)

###########
## WHG   ##
###########

WHG.SR <- as(biols[["WHG"]], "FLSR")
model(WHG.SR) <- "segreg"
WHG.SR <- trim(WHG.SR, season = 2)

WHG.SR<-fmle(WHG.SR)
plot(WHG.SR)

###########
## MON   ##
###########

MON.SR <- as(biols[["MON"]], "FLSR")
model(MON.SR) <- "segreg"
MON.SR <- trim(MON.SR, season = 2)

MON.SR<-fmle(MON.SR)
plot(MON.SR)

###########
## NHKE  ##
###########

NHKE.SR <- as(biols[["NHKE"]], "FLSR")
model(NHKE.SR) <- "segreg"
NHKE.SR <- trim(NHKE.SR, season = 2)

NHKE.SR<-fmle(NHKE.SR)
plot(NHKE.SR)

###########
## NMEG  ##
###########

NMEG.SR <- as(biols[["NMEG"]], "FLSR")
model(NMEG.SR) <- "segreg"
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


save(sr.fits, file = file.path("..", "biols", "SR_fits.RData"))
