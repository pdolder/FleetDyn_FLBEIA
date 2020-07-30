
library(FLBEIA)
library(nnet)
library(tidyverse)

## Load in all files
load(file.path("..","..","model", "model_inputs", "FLFleetsExt_expanded.RData"))

## Test Markov fit
load(file = file.path("..", "..","tests", "Markov_model.RData"))

model <- Markov_fit

## catch rates per stock and metier

mean_CR <- lapply(fleets[["IE_Otter"]]@metiers@names, function(m) {

stks<-   catchNames(fleets[["IE_Otter"]]@metiers[[m]])
res3 <- sapply(stks, function(s) { 
	   res<- fleets[["IE_Otter"]]@metiers[[m]]@catches[[s]]@landings/
		       (fleets[["IE_Otter"]]@effort *
       		       fleets[["IE_Otter"]]@metiers[[m]]@effshare)
	   res2 <- mean(res[,ac(2015:2017)])
	   return(res2)
})

return(data.frame(area = m, stock = names(res3), cr = res3))
	       
})

mean_CR <- bind_rows(mean_CR)

mean_CR <- reshape2::dcast(mean_CR, area ~ stock, drop = FALSE, fill = 0, value.var = "cr")

newdata = data.frame(state.tminus1 = LETTERS[1:7],
		     season = factor(2),
		     COD = 0, 
		     HAD = 0,
		     MON = 0,
		     NEP16 = 0,
		     NEP17 = 0,
		     NEP19 = 0,
		     NEP2021 = 0,
		     NEP22 = 0, 
		     NHKE = 0,
		     NMEG = 0,
		     WHG = 0)

newdata[,3:ncol(newdata)] <- mean_CR[,2:ncol(mean_CR)]


cr_mult <- lapply(colnames(newdata)[3:13], function(s) {  
res <- sapply(seq(0,20,0.1), function(x) {

		      newdata2 <- newdata
		      newdata2[,s] <- newdata2[,s] * x

FLBEIA:::predict_Markov(model, newdata2, 
			fleet = fleets[["IE_Otter"]], 
			season = 1, year = 45, close = NA, iter = 1)
})

return(cbind(data.frame(stock = s, mult = seq(0, 20, 0.1)), as.data.frame(t(res))))

})


cr_effect <- bind_rows(cr_mult)

cr_eff <- reshape2::melt(cr_effect, id = c("stock", "mult"))

colnames(cr_eff)[3] <- "area"

theme_set(theme_bw())
ggplot(cr_eff, aes(x = mult, y = value)) +
	geom_line(aes(colour = area)) +
	facet_wrap(~stock) +
	ggtitle("Catch rate multiplier on choice probabilities") +
	ylab("Choice probability / share") + xlab("Catch Rate multiplier")# + 
#	scale_x_log10()
ggsave("Markov_Metier_Catch_Rate_Multiplier.png")


## What about the seasonal effect??


seas <- lapply(1:4, function(s) {  

		      newdata2 <- newdata
		      newdata2[,"season"] <- factor(s)

res <- FLBEIA:::predict_Markov(model, newdata2, 
			fleet = fleets[["IE_Otter"]], 
			season = 1, year = 45, close = NA, iter = 1)

return(res)

})


seas <- do.call(rbind, seas)


seas <- cbind(season = 1:4, seas)

seas <- reshape2::melt(as.data.frame(seas), id = c("season"))

colnames(seas) <- c("season", "metier", "proportion")

ggplot(seas, aes(x = season, y = proportion)) + 
	geom_line(aes(colour = metier))
  ggsave("Markov_metier_seasonal_effect.png")
