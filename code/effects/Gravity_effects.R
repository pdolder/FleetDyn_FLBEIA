
library(FLBEIA)
library(tidyverse)

## Load in all files
lapply(list.files(file.path("..", "..","model","model_inputs"), full.names = TRUE), load, .GlobalEnv)



## Get the data
flnm <- "IE_Otter"

fl    <- fleets[[flnm]]

if(is.null(fleets.ctrl[[flnm]][['stocks.restr']]) |  length(fleets.ctrl[[flnm]][['stocks.restr']]) == 0) {
      fleets.ctrl[[flnm]][['stocks.restr']] <- catchNames(fleets[[flnm]])
    } 
sts   <- intersect(fleets.ctrl[[flnm]][['stocks.restr']], catchNames(fl))

stnms <- names(biols)
    mtnms <- names(fl@metiers)
    nmt   <- length(mtnms)
    nst   <- length(biols)
    ns    <- dim(biols[[1]]@n)[4] 
    dimnms <- dimnames(biols[[1]]@n) 
    nit <- dim(biols[[1]]@n)[6]
   
    year <- 47
    season <- 1
    yr <- year
    if(is.character(year)) yr <- which(dimnms[[2]] %in% year)
    if(length(yr) == 0) stop('The year is outside object time range')  
    
    # 'season' dimension.
    ss <- season
    if(is.character(season)) ss <- which(dimnms[[4]] %in% season)
    if(length(ss) == 0) stop('The season is outside object season range')  
    
    # Check fleets.ctrl elements.
    restriction <- ifelse(length(fleets.ctrl[[flnm]]$restriction) == 1, fleets.ctrl[[flnm]]$restriction, fleets.ctrl[[flnm]]$restriction[year])
    if(!(restriction %in% c('catch', 'landings')))
        stop("fleets.ctrl[[f]]$restriction must be equal to 'catch' or 'landings'")
    
    
    # Advice season for each stock
    adv.ss <- setNames( rep(NA,nst), stnms)
 for (st in stnms) adv.ss[st] <- ifelse(is.null(advice.ctrl[[st]][["adv.season"]]), ns, advice.ctrl[[st]][["adv.season"]]) # [nst]


 fleets.ctrl[[flnm]]$LandObl <- FALSE

 list2env(FLBEIA:::FLObjs2S3_fleetSTD(biols = biols, fleets = fleets, advice = advice, covars = NULL, 
                                biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl, BDs=BDs, 
                                flnm = flnm, yr = yr, ss = ss, iters = 1:nit, adv.ss), environment())
  
## For some reason Nephrops are not filling...

 for(i in c("NEP16", "NEP17", "NEP19", "NEP2021", "NEP22")) {
N[[i]][] <- biols[[i]]@n[,ac(2017),1,1,1]
 }


cr_mult <- lapply(catchNames(fleets[[flnm]]), function(s) {

 spp_res <- sapply(seq(0,5,0.01), function(mult) {
    N0 <- N
  
    V.m  <- Reduce('+', lapply(names(q.m), function(x) 

     if(x == s) {
     mult * apply(q.m[[x]]*(sweep(wl.m[[x]], 2:4, N0[[x]], "*")^beta.m[[x]])*ret.m[[x]]*pr.m[[x]],c(1,4),sum) } else {
     apply(q.m[[x]]*(sweep(wl.m[[x]], 2:4, N0[[x]], "*")^beta.m[[x]])*ret.m[[x]]*pr.m[[x]],c(1,4),sum)
     }
      ))
    TotV <- apply(V.m,2,sum)
    res <- sweep(V.m, 2, TotV, "/")
    return(res)
})

 return(cbind(stock = s, mult= seq(0,5,0.01),as.data.frame(t(spp_res))))

})


cr_effect <- bind_rows(cr_mult)
colnames(cr_effect)[3:9] <- LETTERS[1:7]

cr_eff <- reshape2::melt(cr_effect, id = c("stock", "mult"))

colnames(cr_eff)[3] <- "area"

## Rename the stocks
cr_eff$stock[cr_eff$stock == "NMEG"] <- "LEZ"
cr_eff$stock[cr_eff$stock == "MON"]  <- "ANF"
cr_eff$stock[cr_eff$stock == "NHKE"] <- "HKE"

theme_set(theme_bw())
ggplot(cr_eff, aes(x = mult, y = value)) +
	geom_line(aes(colour = area)) +
	facet_wrap(~stock) +
	ggtitle("Catch rate multiplier on choice probabilities") +
	ylab("Choice probability / share") + xlab("Catch Rate multiplier")# + 
#	scale_x_log10()
ggsave("Gravity_Metier_Catch_Rate_Multiplier.png")



