#n##############################################################
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

## Now catches

res <- lapply(fl@metiers, function(m) {
			      
			res <-   lapply(m@catches, function(ca) {
			return(cbind(stock = ca@name, as.data.frame(ca@landings)))
})
			res2 <- do.call(rbind, res)
			res2 <- cbind(metier = m@name, res2)

			      })

catch_met <- do.call(rbind, res)

## make choice set by combining data
catch_met_wide <- catch_met %>% select(-age, -unit, -area, -iter) %>% 
	reshape2::dcast(metier + year + season ~ stock, value.var = "data")

catch_met_wide[is.na(catch_met_wide)]  <- 0

choices <- merge(eff_met, catch_met_wide)

## covert catch to catch rates
choices[,11:ncol(choices)]  <- (choices[,11:ncol(choices)] / choices[,"effort"] ) * 1000 ## multiply to make more meaningful unit
choices[is.na(choices)] <- 0

head(choices)

table(choices$year, choices$metier, choices$season)

## Exclude F as causing fitting problems....
## choices <- filter(choices, metier != "F") 

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

metiers <- sample(df$metier, 1000, 
		  prob = df$data, replace = T)

## Now we need these metiers to make up the choice, and an alternative of each
## of the other values
choice_set <- data.frame(index = seq_len(length(metiers)),
			 year = y, season = s,
			 metier = metiers, choice = "yes")
choice_set <- left_join(choice_set, df[,c(1:3, 11:ncol(df))],
			by = c("year", "season", "metier"))

## now we stitch the choices that weren't made
mets <- unique(metiers)

choice_set2 <- lapply(choice_set$index, function(x) {

choice_set_alt <- data.frame(index = x, year = y, season = s, 
			     metier = mets[mets != choice_set[choice_set$index == x,"metier"]],
			     choice = "no")
choice_set_alt <- left_join(choice_set_alt, df[,c(1:3, 11:ncol(df))],
			    by = c("year", "season", "metier"))

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


table(res$year, res$metier)
table(res$year, res$metier, res$season)


LD <- mlogit.data(res, choice = "choice", shape = "long",
		  chid.var = "index", alt.var = "metier", drop.index = TRUE)


#m0 <- mlogit(choice ~ 1, data = LD, print.level = 5)
#summary(m0)

## This is a multinomial model
m1 <- mlogit(choice ~ 1 | season , data = LD, print.level = 2)
summary(m1)
m1

## The mean probabilities
apply(fitted(m1, outcome = FALSE),2,mean)


## Let's combine Nephrops to make simpler

LD$NEP_all <- LD$NEP16 + LD$NEP17 + LD$NEP19 + LD$NEP2021 + LD$NEP22

## This is mixed, conditional | multinomial
m2 <- mlogit(choice ~ COD + HAD + MON + NEP_all + NHKE + NMEG + WHG | season, data = LD, 
	     print.level = 2)
summary(m2)

##############################################
### How you make predictions with different ##
### covariate values
### from https://cran.r-project.org/web/packages/mlogit/vignettes/c3.rum.html 
##############################################

## How does COD catch rates affect choice...
LD.cod <- LD
LD.cod$COD <- LD.cod$COD * 1.2  ## increase cod catch rate by 20 %

## The original fitted probabilities 
p0 <- fitted(m2, type = "probabilities")

## the new predictions with higher catch rates
p <- predict(m2, newdata = LD.cod, type = "probabilities")

cbind(p0[1,1], p[1,1])  ## example difference in metier A

##
## Example of plotting the means by season
## Not sure this is right, really need to get the marginal effect of season
## TO DO !!
## 


## I am confused here which is the right approach...here are the outcome
## probabilities

fit <- fitted(m2, type = "outcome")
fit <- as.data.frame(fit)

LD_check <- LD
LD_check$metier <- sapply(strsplit(rownames(LD_check), ".", fixed = T), "[[", 2)

LD_check <- filter(LD_check, choice == TRUE)
LD_check$pred <- fit$fit

LD_check <- LD_check %>% group_by(season, metier) %>%
	summarise(prob = mean(pred)) %>% as.data.frame()

choice_mean <- choices %>% group_by(season, metier) %>%
	summarise(prob = mean(data)) %>% as.data.frame()

choice_mean$season <- as.factor(choice_mean$season)

## Better on seasonal variability...but doesn't sum to one ??
LD_check2 <- LD_check %>% spread(metier, prob)
rowSums(LD_check2[,2:ncol(LD_check2)])

ggplot(LD_check, aes(x = paste(metier,season), y = prob , fill = factor(season))) +
	geom_bar(stat="identity")  +
geom_point(data = choice_mean, aes(x = paste(metier, season), y = prob), colour = "red")

fit <- fitted(m2, type = "probabilities")
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

## But maybe this can be done with a model matrix approach ??

## Over to you Cóilín....

Beta <- as.matrix(coef(m2))
mat1 <- model.matrix(m2)

bm   <- mat1 %*% Beta

fit2 <- fitted(m2) 


###############################
### Marginal effects
###############################

## By default computed at the sample mean
effects(m2, covariate = "season")

## Can interpret at there being this % increase or decrease in prob
## of choice given 100% increase in season...i.e. as %
100 * effects(m2, covariate = "season")

## So...stands to reason that the mean of the prob of season 1 area A *
## effects(m2, covariates) for A * 2 == season 2 area A...

fit_mean[fit_mean$metier == "A" & fit_mean$season %in% 1:2,]

fit_mean[fit_mean$metier == "A" & fit_mean$season %in% 1, "prob.mean"] +
(fit_mean[fit_mean$metier == "A" & fit_mean$season %in% 1, "prob.mean"] *
	(effects(m2, covariate = "season", type = "aa")[["A"]]))

fit_mean[fit_mean$metier == "A" & fit_mean$season %in% 2, "prob.mean"] 

# note options are a for absolute change, and r for relative change.
# these can be any combined so ar is absolute variate in probability for a relative
# change in the covariate. aa is default
100 * effects(m2, covariate = "season", type = "aa")
100 * effects(m2, covariate = "season", type = "ar")

## and for alternative specific covariates....

## This is metier by metier matrix, so for example the change in cod catch rate in metier a
## on probability of choosing metier a - l etc...
effects(m2, covariate = "COD", type = "aa")

## This is mixed, conditional | multinomial  - and we want choice to interact
## with season...

effects(m3, covariate = "season")

###################################################
###################################################
## Someone else developed some functions to help
#source('https://github.com/gregmacfarlane/MLogitTools/blob/master/predictfunction.R')

source('https://raw.githubusercontent.com/gregmacfarlane/MLogitTools/master/predictfunction.R')

utils <- utilities.mlogit(m2, list("B", "C","D","E","F","G","H","J","K","L")) 

mf <- cbind(alt = LETTERS[1:12], LD[1,-c(1,3,7:11)])
mf <- mf[,c(1:5,9,6:8)]

compute.Utility(mf = mf, utilities = utils, type = "P")

## Doesn't seem to work...
