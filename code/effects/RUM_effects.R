#################################################
##
## Plot the marginal effects of the RUM model 
##
#################################################

library(FLBEIA)
library(mlogit)
library(tidyverse)

load(file.path("..", "..","tests", "RUM_model.RData"))
load("RUM_data.RData")

load(file.path("..", "..", "model", "fleets", "fleets.RData"))

model <- RUM_model_fit

n_met <- 7
##########################
# calculating the probs
########################

# The dataframe to predict over
predict.df <- FLBEIA:::make_RUM_predict_df(model, fleets[["IE_Otter"]], 1)

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

## Update the data with the mean CR

for(s in c("COD", "MON", "NEP19", "NEP22", "NHKE", "WHG")) {
predict.df[,s] <- mean_CR[,s]
}


cr_mult <- lapply(colnames(predict.df)[4:9], function(s) {  
res <- sapply(seq(0,5,0.01), function(x) {

		      predict.df2 <- predict.df 
		      predict.df2[,s] <- predict.df2[,s] * x

FLBEIA:::predict_RUM(model, predict.df2, 
			season = 1, close = NA)
})

return(cbind(data.frame(stock = s, mult = seq(0, 5, 0.01)), as.data.frame(t(res))))

})


cr_effect <- bind_rows(cr_mult)
colnames(cr_effect)[3:9] <- LETTERS[1:7]

cr_eff <- reshape2::melt(cr_effect, id = c("stock", "mult"))

colnames(cr_eff)[3] <- "area"

exp(coef(RUM_model_fit))

theme_set(theme_bw())
ggplot(cr_eff, aes(x = mult, y = value)) +
	geom_line(aes(colour = area)) +
	facet_wrap(~stock) +
	ggtitle("Catch rate multiplier on choice probabilities") +
	ylab("Choice probability / share") + xlab("Catch Rate multiplier") #+ 
#	scale_x_log10()
ggsave("RUM_Metier_Catch_Rate_Multiplier.png")


## What about the seasonal effect??

seas <- lapply(1:4, function(s) {  
		       
res <- FLBEIA:::predict_RUM(model, predict.df, 
			season = s, close = NA)

return(res)

})


seas <- do.call(rbind, seas)

colnames(seas) <- LETTERS[1:7]

seas <- cbind(season = 1:4, seas)

seas <- reshape2::melt(as.data.frame(seas), id = c("season"))

colnames(seas) <- c("season", "metier", "proportion")

ggplot(seas, aes(x = season, y = proportion)) + 
	geom_line(aes(colour = metier))
  ggsave("RUM_metier_seasonal_effect.png")
