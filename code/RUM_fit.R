###############################################################
## A simplified RUM fit to the IE_Otter data
###############################################################

library(FLBEIA)
library(tidyverse)

load(file.path("..", "model", "fleets", "fleets.RData"))

fl <- fleets[["IE_Otter"]]


## effort
eff <- as.data.frame(fl@effort)

# effort by metier
eff_met <- lapply(fl@metiers, function(m) as.data.frame(m@effshare))
eff_met <- do.call(rbind, eff_met)
eff_met$metier <- substr(rownames(eff_met),1,1)

## total effort by metier
eff_met$tot <- eff$data[match(paste(eff_met$year, eff_met$season),
			      paste(eff$year, eff$season))]
eff_met$effort <- eff_met$data * eff_met$tot

choices <- eff_met

## simplest model...

library(mlogit)

## For each month and year for each choice, 
## we need the alternatives that weren't chosen
## We only have proportions data, not individual trips/fishing activities...
## let's use the number of choices in 1000, so we effectively simulate
## 1000 trip choices from the probabilities each quarter and year

test <- filter(choices, year == 2015, season == 1)

metiers <- sample(test$metier, round(mean(test$tot),0), 
		  prob = test$data, replace = T)
table(metiers)

## Now we need these metiers to make up the choice, and an alternative of each
## of the other values

choice_set <- data.frame(index = seq_len(length(metiers)),
			 metier = metiers, choice = "yes")

## now we stitch the choices that weren't made

mets <- unique(metiers)

choice_set2 <- lapply(choice_set$index, function(x) {

choice_set_alt <- data.frame(index = x,  
			     metier = mets[mets != choice_set[choice_set$index == x,"metier"]],
			     choice = "no")
return(rbind(choice_set[choice_set$index == x,],
	     choice_set_alt))

})

choice_set2 <- do.call(rbind, choice_set2)

choice_set2$choice <- factor(choice_set2$choice)

LD <- mlogit.data(choice_set2, choice = "choice", shape = "long",
		  chid.var = "index", alt.var = "metier", drop.index = FALSE)

m1 <- mlogit(choice ~ 1, data = LD, print.level = 5)
summary(m1)

predictions <- predict(m1, newdata = LD, type = "probs")


f####################################
## Now over all years and seasons
#####################################

res.df <- lapply(unique(choices$year), function(y) {

	season_res <- lapply(unique(choices$season), function(s) {
			      
df <- filter(choices, year == y, season == s)

metiers <- sample(df$metier, round(mean(df$tot),0), 
		  prob = df$data, replace = T)

## Now we need these metiers to make up the choice, and an alternative of each
## of the other values
choice_set <- data.frame(index = seq_len(length(metiers)),
			 year = y, season = s,
			 metier = metiers, choice = "yes")

## now we stitch the choices that weren't made
mets <- unique(metiers)

choice_set2 <- lapply(choice_set$index, function(x) {

choice_set_alt <- data.frame(index = x, year = y, season = s, 
			     metier = mets[mets != choice_set[choice_set$index == x,"metier"]],
			     choice = "no")
return(rbind(choice_set[choice_set$index == x,],
	     choice_set_alt))

})

choice_set2 <- do.call(rbind, choice_set2)
choice_set2$choice <- factor(choice_set2$choice)

			     })

	res <- do.call(rbind, season_res)

	return(res)

		  })


