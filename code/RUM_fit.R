###############################################################
## A simplified RUM fit to the IE_Otter data
###############################################################

library(FLBEIA)
library(tidyverse)
library(mlogit)

load(file.path("..", "model", "fleets", "fleets.RData"))

fl <- fleets[["IE_Otter"]]


#################
### processing 
#################

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


###############################################################
## simplest model...
##################################################################

## For each month and year for each choice, 
## we need the alternatives that weren't chosen
## We only have proportions data, not individual trips/fishing activities...
## let's use the number of choices in 1000, so we effectively simulate
## 1000 trip choices from the probabilities each quarter and year
## over all years and seasons
#####################################

choices$season <- as.numeric(as.character(choices$season))


res.df <- lapply(unique(choices$year), function(y) {
			 print(y)

	season_res <- lapply(unique(choices$season), function(s) {
				     print(s)
			      
df <- filter(choices, year == y, season == s)

#metiers <- sample(df$metier, round(mean(df$tot),0), 
#		  prob = df$data, replace = T)

metiers <- sample(df$metier, 1000, 
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
return(choice_set2)

			     })

	res <- do.call(rbind, season_res)

	return(res)

		  })


res <- do.call(rbind, res.df)

## unique index
#res$season <- as.factor(res$season)
res$index <- paste(res$index, res$year, res$season, sep = "_")

LD <- mlogit.data(res, choice = "choice", shape = "long",
		  chid.var = "index", alt.var = "metier", drop.index = TRUE)


#m0 <- mlogit(choice ~ 1, data = LD, print.level = 5)
#summary(m0)

m1 <- mlogit(choice ~ 1 | season , data = LD, print.level = 5)
summary(m1)
m1

summary(m1)
apply(fitted(m1, outcome = FALSE),2,mean)

fit <- fitted(m1, type = "probabilities")
fit <- as.data.frame(fit)

## add season variable
fit$season <- sapply(strsplit(rownames(fit), "_"), "[[", 3)
fit$season <- as.factor(fit$season)

fit_l <- fit %>% gather(metier, prob, -season)

fit_mean <- fit_l %>% group_by(season, metier) %>%
	summarise(prob.mean = mean(prob),sd = sd(prob)) %>% 
	as.data.frame()


choice_mean <- choices %>% group_by(season, metier) %>%
	summarise(prob = mean(data)) %>% as.data.frame()

choice_mean$season <- as.factor(choice_mean$season)

ggplot(fit_mean, aes(x = paste(metier,season), y = prob.mean, fill = season)) +
	geom_bar(stat="identity") + geom_errorbar(aes(ymin = prob.mean - (1.96 * sd), 
						      ymax = prob.mean + (1.96 * sd))) +
geom_point(data = choice_mean, aes(x = paste(metier, season), y = prob), colour = "red")

## These aren't very different by season!!

