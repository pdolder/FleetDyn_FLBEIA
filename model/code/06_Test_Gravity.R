#########################################################################
## Example of expected catch rates per metier
## vs the observed
#########################################################################

library(FLBEIA)
load(file.path("..", "outputs", "BaseModel.RData"))

#########################################################################

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

## VPUEs by metier and stock

fl <- SC1$fleets[["IE_Otter"]]


res <- lapply(fl@metiers, function(m)  {
stnm <- names(m@catches)

values <- lapply(stnm, function(st) {
	              m@catches[[st]]@price[is.na(m@catches[[st]]@price)] <- 0
	     res <-   apply(m@catches[[st]]@landings.n * m@catches[[st]]@landings.wt * m@catches[[st]]@price,c(2,4),sum)
})

## sum across list of quants
## probably a better way..
l.val <- length(values)
res2 <- values[[1]]

for(i in 2:l.val) {
res2 <- res2 + values[[i]]
}

return(res2)
})


## res is catch value by metier
## Now we want the VPUE

names(res) <- fl@metiers@names

vpue <- lapply(fl@metiers, function(met) {
	n. <- met@name
	vpue <- res[[n.]] / (fl@effort * met@effshare)
	return(vpue)
})

l.vpue <- length(vpue)
vpue.sum <- vpue[[1]]
for(i in 2:l.vpue) {
vpue.sum <- vpue.sum + vpue[[i]]
}

eff.share <- lapply(vpue, function(x) x / vpue.sum)


eff.share[[1]][,,,4]
fl@metiers[[1]]@effshare[,,,4]



lapply(met@catches, function(x) x@price[,ac(2018)])

