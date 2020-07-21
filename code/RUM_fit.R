################################################################
## A simplified RUM fit to the IE_Otter data
###############################################################

library(FLBEIA)
library(tidyverse)
library(mlogit)

set.seed(123)

if(length(grep("coilin", getwd())) > 0){
    load(file.path("~", "Dropbox", "FLeetDyn_FLBEIA", "model", "fleets", "fleets.RData"))
}else{
    load(file.path("..", "model", "fleets", "fleets.RData"))
}


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
choices[,11:ncol(choices)]  <- (choices[,11:ncol(choices)] / choices[,"effort"] )# * 1000 ## multiply to make more meaningful unit
choices[is.na(choices)] <- 0

head(choices)

table(choices$year, choices$metier, choices$season)

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

## Only Nephrops data in 2015 - 2017, we need to fill the earlier years
mean_nep <- res %>% filter(year %in% 2015:2017) %>% group_by(season, metier) %>%
	summarise(NEP16 = mean(NEP16), NEP17 = mean(NEP17), NEP19 = mean(NEP19), 
		  NEP2021 = mean(NEP2021), NEP22 = mean(NEP22))

res1 <- res %>% filter(year < 2015)
res2 <- res %>% filter(year > 2014)

res1$NEP16   <- mean_nep$NEP16[match(paste0(res1$season, res1$metier),
				   paste0(mean_nep$season, mean_nep$metier))]
res1$NEP17   <- mean_nep$NEP17[match(paste0(res1$season, res1$metier),
				   paste0(mean_nep$season, mean_nep$metier))]
res1$NEP19   <- mean_nep$NEP19[match(paste0(res1$season, res1$metier),
				   paste0(mean_nep$season, mean_nep$metier))]
res1$NEP2021 <- mean_nep$NEP2021[match(paste0(res1$season, res1$metier),
				   paste0(mean_nep$season, mean_nep$metier))]
res1$NEP22   <- mean_nep$NEP22[match(paste0(res1$season, res1$metier),
				   paste0(mean_nep$season, mean_nep$metier))]

res <- rbind(res1, res2)

## Let's combine Nephrops to make simpler when fitting a model with species
res$NEP_all <- apply(res[,c("NEP16", "NEP17", "NEP19", "NEP2021", "NEP22")],1, mean)

####
## Some contrast in the catch data...
####

## For each of the catch records, take a value around the mean
for(i in 6:17) {
res[,i] <- sapply(res[,i], function(x) { rnorm(1, mean = x, sd = 0.2 * x) }) 
}

## Lagged effort share
res$effshare <- NA

res$effshare <- choices$data[match(paste(res$year-1, res$season, res$metier),
				   paste(choices$year, choices$season, choices$metier))]

res$season <- as.factor(res$season)

## Noise around the past effort share
res[,"effshare"] <- sapply(res[,"effshare"], function(x) { rnorm(1, mean = x, sd = 0.2 * x) }) 


## unique index
#res$season <- as.factor(res$season)
res$index <- paste(res$index, res$year, res$season, sep = "_")

table(res$year, res$metier)
table(res$year, res$metier, res$season)

LD <- mlogit.data(res, choice = "choice", shape = "long",
		  chid.var = "index", alt.var = "metier", drop.index = TRUE)

save(res, LD, file = file.path("effects", "RUM_data.RData"))

#m0 <- mlogit(choice ~ 1, data = LD, print.level = 5)
#summary(m0)

## This is a multinomial model
m1 <- mlogit(choice ~ 1 | season , data = LD, print.level = 2)
summary(m1)
m1

## The mean probabilities
apply(fitted(m1, outcome = FALSE),2,mean)

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


#########################################################################
## Cóilín's code = by hand predictions
#########################################################################

## probabilities from mlogit
p_hat <- fitted(m1, outcome = FALSE)

## coefficient vector
beta <- as.matrix(coef(m1))

## model matrix
X <- model.matrix(m1)

## linear predictor long
eta_long <- X %*% beta

##eta_long <-apply(X, 1, function(x) x %*% beta)

## linear predictor wide
eta_wide <- matrix(eta_long, ncol = 12, byrow = TRUE)
names(eta_wide) <- toupper(letters[1:12])

## convert to a probability
own_p_hat <- exp(eta_wide) / rowSums(exp(eta_wide))

## check
options(scipen = 4)
range(p_hat - own_p_hat)

## note some large numbers exponentiated to guard against computational infinities perhaps use Brobdingnag

#############################################################################
### Full method for predictions
#############################################################################

## 1. Generate own data frame similar to res with the data

res2<- data.frame(season = rep(1:4, each = 12), metier = rep(LETTERS[1:12],4), choice = "yes")

## Use mFormula to define model form
LD2 <- mlogit.data(res2, choice = "choice", shape = "long",
		   alt.var = "metier")

mod.mat <- model.matrix(m1$formula, data = LD2)
beta <- as.matrix(coef(m1))

## linear predictor long
eta_long <- mod.mat %*% beta

## linear predictor wide
eta_wide <- matrix(eta_long, ncol = 12, byrow = TRUE)
names(eta_wide) <- toupper(letters[1:12])

## convert to a probability
own_p_hat <- exp(eta_wide) / rowSums(exp(eta_wide))

own_p_hat

## So this would be the probabilities for each season
colnames(own_p_hat) <- LETTERS[1:12]
rownames(own_p_hat) <- 1:4

own_p_hat <- as.data.frame(own_p_hat) %>% gather(metier, prob)
own_p_hat$season = rep(1:4, times = 12)

ggplot(own_p_hat, aes(x = paste(metier, season), y = prob)) +
	geom_bar(stat = "identity", aes(fill = factor(season))) +
	ggtitle("Seasonal probabilities from m1 model")
ggsave("season_probs_m1.png")


#######################################################
### Now let's try with m2 -
########################################################

## How does HAD catch rate affect probabilities
cr <- seq(0, max(LD$HAD), 0.01)

res3<- data.frame(season = 1, metier = rep(LETTERS[1:12], times = length(cr)), choice = "yes", 
		   "COD" = 1, "HAD" = rep(cr, each = 12), "MON" = 1, "NEP_all" = 1, "NHKE" = 1, "NMEG" = 1,
		   "WHG" = 1)

res3$index <- paste(res3$metier, res3$HAD, sep ="_")
## Use mFormula to define model form
LD3 <- mlogit.data(res3, choice = "choice", shape = "long",
		   alt.var = "metier", chid.var = "index")

mod.mat <- model.matrix(m2$formula, data = LD3)
beta <- as.matrix(coef(m2))

## linear predictor long
eta_long <- mod.mat %*% beta

## linear predictor wide
eta_wide <- matrix(eta_long, ncol = 12, byrow = TRUE)
names(eta_wide) <- toupper(letters[1:12])

## convert to a probability
own_p_hat <- exp(eta_wide) / rowSums(exp(eta_wide))

head(own_p_hat)

## So this would be the probabilities for each season
colnames(own_p_hat) <- LETTERS[1:12]
rownames(own_p_hat) <- cr 

own_p_hat <- as.data.frame(own_p_hat) %>% gather(metier, prob)
own_p_hat$cr = rep(cr, times = 12)

ggplot(own_p_hat, aes(x = cr, y = prob)) +
	geom_line() +
	facet_wrap(~metier, scale = "free_y") +
	ggtitle("HAD catch rate affecting prob.png")
ggsave("HAD_probs_m2.png")


## Well, it doesn't - as the catch rate in all areas goes up, the probabilities
## are affected equally which neutralises when we divide by rowSums....
## So let's use some of the "real" catch rates and increase them by a %% 

## How does catch rate affect probabilities

## Let's loop this over all species


res_stock <- lapply(c("COD", "HAD", "MON", "NEP_all", "NHKE", "NMEG", "WHG"), function(x) {

print(x)

cr_spp <- res %>% group_by(metier) %>% summarise(cr  = mean(get(x)) ) %>% as.data.frame()

## Increase in %s

## 0% to 100% higher CPUE 
cr <- lapply(seq(1,2,0.1), function(y) {
       cr <- cr_spp$cr  * y
       return(cr)
})

cr <- do.call(rbind, cr) ## matrix

if(x != "COD") {COD = 1}
if(x != "HAD") {HAD = 1}
if(x != "MON") {MON = 1}
if(x != "NEP_all") {NEP_all = 1}
if(x != "NHKE") {NHKE = 1}
if(x != "NMEG") {NMEG = 1}
if(x != "WHG") {WHG = 1}

assign(x, as.vector(t(cr)))

res3<- data.frame(season = 1, metier = rep(LETTERS[1:12], times = nrow(cr)), choice = "yes", 
		   "COD" = COD, "HAD" = HAD, "MON" = MON, "NEP_all" = NEP_all, "NHKE" = NHKE, "NMEG" = NMEG,
		   "WHG" = WHG)

res3$index <- paste(res3$metier, get(x), sep ="_")
## Use mFormula to define model form
LD3 <- mlogit.data(res3, choice = "choice", shape = "long",
		   alt.var = "metier", chid.var = "index")

mod.mat <- model.matrix(m2$formula, data = LD3)
beta <- as.matrix(coef(m2))

## linear predictor long
eta_long <- mod.mat %*% beta

## linear predictor wide
eta_wide <- matrix(eta_long, ncol = 12, byrow = TRUE)
names(eta_wide) <- toupper(letters[1:12])

## convert to a probability
own_p_hat <- exp(eta_wide) / rowSums(exp(eta_wide))

head(own_p_hat)

## So this would be the probabilities for each season
colnames(own_p_hat) <- LETTERS[1:12]
rownames(own_p_hat) <- seq(1, 2, 0.1)

own_p_hat <- as.data.frame(own_p_hat) %>% gather(metier, prob)
own_p_hat$percIncrease = seq(1,2,0.1) 

own_p_hat$stock <- x

return(own_p_hat)

})


res_all <- do.call(rbind, res_stock)

ggplot(res_all, aes(x = percIncrease, y = prob)) +
	geom_line(stat = "identity", aes(colour = stock)) +
	facet_wrap(~metier) + theme_bw() +
	ggtitle("Catch rate multiplier effect on choice probabilities \n 
		in season 1 from m2 model") +
		xlab("Catch Rate multiplier") +
		ylab("Choice probability / share")
ggsave("Catch_Rate_Multi.png")


##############################################################################
##############################################################################
## What we want to do in FLBEIA -
## treat as if implemented internally
##############################################################################
##############################################################################

## 1. Make a dataframe over which to predict
## However, it depends on the coefficients of the model
## Lets assume the options are:
####### 1. Season
####### 2. Catch or catch rate
####### 3. Variable costs
####### 4. Previous effort (effshare, y-1,s) - this is a common predictor.
## Notes: this could be extended to include parameters from the covars object
## Maybe we also want to include option for the predictors to be on the log
## scale ??
## 2. Update the predictions - this comes from the internal FLBEIA functions
## 3. Make the predictions - this should return a vector of effort shares
## 4. Update and pass back the fleets object


make_RUM_predict_df <- function(model = NULL, fleet = NULL, season = s) {

## Pass mlogit model object
## Pass fleet object

mod.coefs <- names(coef(model)) ## Model coefficients

## 1. season - note, just return the season for which we're predicting
seas <- if(any(grepl("season", mod.coefs))) { season } else { NA }

## 2. catch or catch rates
C <- if(any(sapply(catchNames(fl), grepl, mod.coefs))) {

## Return the catchnames that are in the coefficients
catchNames(fl)[unlist(sapply(catchNames(fl), function(n) { any(grepl(n, mod.coefs))}))]

} else { NA }

## 3. vcost 
v    <- if(any(grepl("vcost", mod.coefs))) { -1 } else { NA }

## 4. effshare 
e    <- if(any(grepl("effshare", mod.coefs))) { -1 } else { NA }

## Construct the dataframe
predict.df <- expand.grid(metier = fl@metiers@names, 
			  choice = "yes", 
			  season = as.numeric(seas), 
			  vcost = v, 
			  effshare = e,
			  stringsAsFactors = FALSE)
## Remove any columns with NAs, indicating variable not used
predict.df <- predict.df[,which(sapply(predict.df, function(x) all(!is.na(x))))]

## Combine with the catch rate columns
if(!all(is.na(C))) {	
C.df <- as.data.frame(matrix(-1, ncol = length(C), nrow = nrow(predict.df)))
colnames(C.df) <- C

predict.df <- cbind(predict.df, C.df)
}

predict.df$index <- seq_len(nrow(predict.df)) 
## Use mFormula to define model form
LD.predict <- mlogit.data(predict.df, choice = "choice", shape = "long",
		   alt.var = "metier", chid.var = "index")

return(LD.predict)
}


predict.df <- make_RUM_predict_df(model = m2, fleet = fl, s = 1)


########################
## Update parameters
########################


update_RUM_params <- function(model = NULL, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) {

## Update the values in the predict.df

## 2. catch / catch rates - on same scale.
## Note, these should be updated based on the biomass increases, so we do a
## similar calculation as for the gravity model
## Here have to be careful as not all metiers may catch all stocks...
	
if(any(sapply(catchNames(fl), grepl, names(coef(model))))) {

N0 <- lapply(names(N), function(x) array(N[[x]], dim = dim(N[[x]])[c(1,3,6)]))
      names(N0) <- names(N)

## This should be the catch rate per stock per metier ??
CR.m   <- lapply(names(q.m), function(x) 
	cbind(stock = x,
	as.data.frame(
	apply(q.m[[x]]*(sweep(wl.m[[x]], 2:4, N0[[x]], "*")^beta.m[[x]])*ret.m[[x]]*pr.m[[x]],c(1,4),sum)
	)
	)
	)

CR <- do.call(rbind, CR.m)

for(st in unique(CR$stock)) {
	predict.df[,st] <- CR[CR$stock == st,2] 
}
predict.df[is.na(predict.df)] <- 0

}

# 3. vcost
if("vcost" %in% colnames(predict.df)) {
v <- do.call(rbind, lapply(fl@metiers, function(x) cbind(metier = x@name,as.data.frame(x@vcost[,yr,,s]))))
predict.df$vcost <- v$data
}

# 4. effort share - past effort share, y-1
if("effshare" %in% colnames(predict.df)) {
 e <- do.call(rbind, lapply(fl@metiers, function(x) cbind(metier = x@name,as.data.frame(x@effshare[,yr-1,,s]))))
predict.df$effshare <- e$data
}

return(predict.df)

}


##########################
##
## For testing the function!!
load(file.path("RUMtestData.RData"))

## NOTE: This is old and missing nephrops catch

##########################

updated.df <- update_RUM_params(model = m2, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 

########################
## Make predictions
########################

predict_RUM <- function(model = mod, updated.df = updated.df) {

## Extract the model matrix and parameter coefficients
mod.mat <- model.matrix(model$formula, data = updated.df)
beta <- as.matrix(coef(model))

## Check the model matrix and coefficients are ordered correctly
if(any(!colnames(mod.mat) == rownames(beta))) {
stop("Model matrix and coefficients are not the same")
}

## linear predictor long
eta_long <- mod.mat %*% beta

## linear predictor wide
eta_wide <- matrix(eta_long, ncol = length(unique(updated.df$metier)), byrow = TRUE)
names(eta_wide) <- updated.df$metier 

## convert to a probability
p_hat <- exp(eta_wide) / rowSums(exp(eta_wide))
colnames(p_hat) <- updated.df$metier 
p_hat <- as.data.frame(t(p_hat))

return(p_hat)

}

#####################################
## Fit a model with all the species
#####################################

m3 <- mlogit(choice ~ COD + HAD + MON + NEP16 + NEP17 + NEP19 + NEP2021 + NEP22 + NHKE + NMEG + WHG | season, data = LD, 
	     print.level = 2)
summary(m3)

# Try with only recent years
LD_reduced <- mlogit.data(filter(res, year %in% 2015:2017), choice = "choice", shape = "long",
		  chid.var = "index", alt.var = "metier", drop.index = TRUE)


m3_red <- mlogit(choice ~ COD + HAD + MON + NEP16 + NEP17 + NEP19 + NEP2021 + NEP22 + NHKE + NMEG + WHG | season, data = LD_reduced, 
	     print.level = 2)
summary(m3_red)

## Worse...

## Drop the non-sig species: COD, NEP22

m3_drop <- mlogit(choice ~ HAD + MON + NEP16 + NEP17 + NEP19 + NEP2021 + NHKE + NMEG + WHG | season, data = LD, 
	     print.level = 2)
summary(m3_drop)




## step 1 
predict.df <- make_RUM_predict_df(model = m3, fleet = fl, s = 1)

## step 2 
updated.df <- update_RUM_params(model = m3, predict.df = predict.df, fleet = fl, covars = covars, season = 1,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 

## step 3 
predicted.share <- predict_RUM(model = m3, updated.df = updated.df)

real_share <- filter(eff_met, year == 2017, season == 1)$data

data.frame("metier" = rownames(predicted.share),
	   "pred" = predicted.share[,1], 
	   "real" = real_share)

#########################
## Including lagged effshare
########################

m4 <- mlogit(choice ~ COD + HAD + MON + NEP16 + NEP17 + NEP19 + NEP2021 + NEP22 + NHKE + NMEG + WHG | effshare + season, data = LD, 
	     print.level = 2, iterlim = 1e4)
summary(m4)

## Very strong influence of past share!!!

## Drop the non-sig species: COD 

m4_drop <- mlogit(choice ~ HAD + MON + NEP16 + NEP17 + NEP19 + NEP2021 + NEP22 + NHKE + NMEG + WHG | season + effshare, data = LD, 
	     print.level = 2, iterlim = 1e4)
summary(m4_drop)

AIC(m1, m2, m3, m4)



## Test
yr <- 3
s <- 1

## step 1 
predict.df <- make_RUM_predict_df(model = m4, fleet = fl, s = 1)

## step 2 
updated.df <- update_RUM_params(model = m4, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 

## step 3 
predicted.share_past <- predict_RUM(model = m4, updated.df = updated.df)

real_share <- filter(eff_met, year == 2017, season == 1)$data

data.frame("metier" = rownames(predicted.share),
	   "pred" = predicted.share[,1], 
	   "pred_past" = predicted.share_past[,1], 
	   "real" = real_share)


###################################
## Save model as input to FLBEIA  #
###################################

## By default computed at the sample mean
effects(m4, covariate = "season")


lims <- lapply(colnames(res)[6:16], function(x) range(effects(m4, covariate = x)))
lims <- range(unlist(lims))

par(mfrow = c(4,3)) 
for(i in colnames(res)[6:16]) { 
image(effects(m4, covariate = i), main = i, zlim = lims)
}

effects(m4, covariate = "NEP16")
effects(m4, covariate = "NEP2021")


############################
## Catch rate effect m3
############################

res_stock <- lapply(colnames(res)[6:16], function(x) {

print(x)

cr_spp <- res %>% group_by(metier) %>% summarise(cr  = mean(get(x)) ) %>% as.data.frame()

## Increase in %s

## 0% to 100% higher CPUE 
cr <- lapply(seq(1,16,0.1), function(y) {
       cr <- cr_spp$cr  * y
       return(cr)
})

cr <- do.call(rbind, cr) ## matrix

if(x != "COD") {COD = 1}
if(x != "HAD") {HAD = 1}
if(x != "MON") {MON = 1}
if(x != "NEP16") {NEP16 = 1}
if(x != "NEP17") {NEP17 = 1}
if(x != "NEP19") {NEP19 = 1}
if(x != "NEP2021") {NEP2021 = 1}
if(x != "NEP22") {NEP22 = 1}
if(x != "NHKE") {NHKE = 1}
if(x != "NMEG") {NMEG = 1}
if(x != "WHG") {WHG = 1}

assign(x, as.vector(t(cr)))

res3<- data.frame(season = 1, metier = rep(LETTERS[1:12], times = nrow(cr)), choice = "yes", 
		   "COD" = COD, "HAD" = HAD, "MON" = MON, 
		   "NEP16" = NEP16, "NEP17" = NEP17, "NEP19" = NEP19, "NEP2021" = NEP2021, "NEP22" = NEP22, 
		   "NHKE" = NHKE, "NMEG" = NMEG,
		   "WHG" = WHG)

#res3$index <- paste(res3$metier, get(x), sep ="_")
res3$index <- seq_len(nrow(res3))
## Use mFormula to define model form
LD3 <- mlogit.data(res3, choice = "choice", shape = "long",
		   alt.var = "metier", chid.var = "index")

mod.mat <- model.matrix(m3$formula, data = LD3)
beta <- as.matrix(coef(m3))

## linear predictor long
eta_long <- mod.mat %*% beta

## linear predictor wide
eta_wide <- matrix(eta_long, ncol = 12, byrow = TRUE)
names(eta_wide) <- toupper(letters[1:12])

## convert to a probability
own_p_hat <- exp(eta_wide) / rowSums(exp(eta_wide))

head(own_p_hat)

## So this would be the probabilities for each season
colnames(own_p_hat) <- LETTERS[1:12]
rownames(own_p_hat) <- seq(1, 16, 0.1)

own_p_hat <- as.data.frame(own_p_hat) %>% gather(metier, prob)
own_p_hat$percIncrease = seq(1,16,0.1) 

own_p_hat$stock <- x

return(own_p_hat)

})


res_all <- do.call(rbind, res_stock)

ggplot(res_all, aes(x = percIncrease, y = prob)) +
	geom_line(stat = "identity", aes(colour = stock)) +
	facet_wrap(~metier) + theme_bw() +
	ggtitle("Catch rate multiplier effect on choice probabilities \n 
		in season 1 from m3 model") +
		xlab("Catch Rate multiplier") +
		ylab("Choice probability / share")
ggsave("Catch_Rate_Multi_m3.png")

#########################



## Model without Nephrops dynamics

m5 <- mlogit(choice ~ COD + HAD + MON + NHKE + NMEG + WHG | season, data = LD, 
	     print.level = 2)
summary(m5)

### step 1
s <- 2
predict.df <- make_RUM_predict_df(model = m5, fleet = fl, s = s)
## step 2 
updated.df <- update_RUM_params(model = m5, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 
## step 3 
predicted.share <- predict_RUM(model = m5, updated.df = updated.df)

print(predicted.share)


############################
## Catch rate effect m5
############################

res_stock <- lapply(colnames(res)[c(6:8,14:16)], function(x) {

print(x)

cr_spp <- res %>% group_by(metier) %>% summarise(cr  = mean(get(x)) ) %>% as.data.frame()

## Increase in %s

## 0% to 100% higher CPUE 
cr <- lapply(seq(1,16,0.1), function(y) {
       cr <- cr_spp$cr  * y
       return(cr)
})

cr <- do.call(rbind, cr) ## matrix

if(x != "COD") {COD = 1}
if(x != "HAD") {HAD = 1}
if(x != "MON") {MON = 1}
if(x != "NHKE") {NHKE = 1}
if(x != "NMEG") {NMEG = 1}
if(x != "WHG") {WHG = 1}

assign(x, as.vector(t(cr)))

res3<- data.frame(season = 1, metier = rep(LETTERS[1:12], times = nrow(cr)), choice = "yes", 
		   "COD" = COD, "HAD" = HAD, "MON" = MON, 
		   "NHKE" = NHKE, "NMEG" = NMEG,
		   "WHG" = WHG)

#res3$index <- paste(res3$metier, get(x), sep ="_")
res3$index <- seq_len(nrow(res3))
## Use mFormula to define model form
LD3 <- mlogit.data(res3, choice = "choice", shape = "long",
		   alt.var = "metier", chid.var = "index")

mod.mat <- model.matrix(m5$formula, data = LD3)
beta <- as.matrix(coef(m5))

## linear predictor long
eta_long <- mod.mat %*% beta

## linear predictor wide
eta_wide <- matrix(eta_long, ncol = 12, byrow = TRUE)
names(eta_wide) <- toupper(letters[1:12])

## convert to a probability
own_p_hat <- exp(eta_wide) / rowSums(exp(eta_wide))

head(own_p_hat)

## So this would be the probabilities for each season
colnames(own_p_hat) <- LETTERS[1:12]
rownames(own_p_hat) <- seq(1, 16, 0.1)

own_p_hat <- as.data.frame(own_p_hat) %>% gather(metier, prob)
own_p_hat$percIncrease = seq(1,16,0.1) 

own_p_hat$stock <- x

return(own_p_hat)

})


res_all <- do.call(rbind, res_stock)

ggplot(res_all, aes(x = percIncrease, y = prob)) +
	geom_line(stat = "identity", aes(colour = stock)) +
	facet_wrap(~metier) + theme_bw() +
	ggtitle("Catch rate multiplier effect on choice probabilities \n 
		in season 1 from m5 model") +
		xlab("Catch Rate multiplier") +
		ylab("Choice probability / share")
ggsave("Catch_Rate_Multi_m5.png")


m6 <- mlogit(choice ~ COD + HAD + MON + NHKE + NMEG + WHG + effshare | season, data = LD, 
	     print.level = 2, iterlim = 1e4)
summary(m6)

### step 1
s <- 1

predict.df <- make_RUM_predict_df(model = m6, fleet = fl, s = s)

## step 2 
updated.df <- update_RUM_params(model = m6, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 

## step 3 
predicted.share_past <- predict_RUM(model = m6, updated.df = updated.df)

# step 1 
predict.df <- make_RUM_predict_df(model = m4, fleet = fl, s = 1)

## step 2 
updated.df <- update_RUM_params(model = m4, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 

## step 3 
predicted.share_past <- predict_RUM(model = m4, updated.df = updated.df)


AIC(m1, m2, m3, m4, m5, m6)

#########################################################
##
# Let's check the fitted values against the observations
#########################################################

# Data for each combination of year, season and metier
# Note we lose 2004, as have no data on effort share the previous year
##
check_fit <- as.data.frame(fitted(m3, type = "probabilities"))

check_fit$year   <- sapply(strsplit(rownames(check_fit), "_", fixed = TRUE), "[", 2)
check_fit$season <- sapply(strsplit(rownames(check_fit), "_", fixed = TRUE), "[", 3)

filter(check_fit, year == 2015, season == 1) %>%
	summarise(A = mean(A), B = mean(B), C = mean(C), D = mean(D), E = mean(E))

choices2 <- choices[order(choices$year, choices$season, choices$metier),]
matrix(filter(choices, season == 1)$data, ncol = 5, byrow = TRUE)

par(mfrow=c(2,2))
for(i in 1:4) {
matplot(matrix(filter(choices, season == i)$data, ncol = 5, byrow = TRUE), type = "b")
}

check_fit %>% 	group_by(season) %>%
	summarise(A = mean(A), B = mean(B), C = mean(C), D = mean(D), E = mean(E)) %>%
	as.data.frame()

# Plot the fitted against obs

obs <- matrix(filter(choices, season == 1)$data, ncol = 5, byrow = TRUE)

fit <- check_fit %>% group_by(season) %>%
	summarise(A = mean(A), B = mean(B), C = mean(C), D = mean(D), E = mean(E)) %>%
	as.data.frame() 

fit <- as.matrix(fit[,2:6])
 
obs_df <- data.frame(year = rep(2004:2017, times = 5), season = 1, metier = rep(LETTERS[1:5], times = length(2004:2017)),obs = as.vector(obs))
obs_df$fit <- fit[1,]

plot(obs_df$fit ~ obs_df$obs)

## Plot the observations against the fitted values
## for all seasons

obs_df     <- data.frame(year = rep(2004:2017, each = 5 * 4), season = rep(1:4, each = 5), metier = LETTERS[1:5], obs = choices$data)
obs_df$fit <- as.vector(t(fit)) 

plot(obs_df$fit ~ obs_df$obs)

theme_set(theme_bw())

cols <- c("blue", "purple", "green", "red", "brown")

for(i in 1:5) {
assign(paste0("p", i), ggplot(obs_df, aes(x = obs, y = fit)) +
	geom_point(colour = "grey") + 
	geom_point(data = filter(obs_df, metier== LETTERS[i]), colour = cols[i]) +
ylab("fitted proportions")  + xlab("Observed proportions")
)
}

library(cowplot)

p6 <- plot_grid(p1,p2,p3,p4,p5, labels = LETTERS[1:5])
save_plot("Observed_and_fitted_RUM.png", p6, base_width = 10, base_height = 8)


## effects of different covariates
effects(m4, "effshare", type = "rr")
effects(m4, "NEP2021", type = "rr")
effects(m4, "NEP22", type = "rr")

effects(m3, "NEP22", type = "rr")
effects(m3, "MON", type = "rr")
effects(m3, "COD", type = "rr")


## With the individual variation

obs_df     <- choices2 %>% select(year, season, metier, data)
	
fit <- check_fit %>% group_by(year, season) %>%
summarise(A = mean(A), B = mean(B), C = mean(C), D = mean(D), E = mean(E)) %>%
as.data.frame() 

fit <- as.matrix(fit[,3:7])
 
obs_df$fit <- as.vector(t(fit)) 

plot(obs_df$fit ~ obs_df$data)

theme_set(theme_bw())

cols <- c("blue", "purple", "green", "red", "brown")

for(i in 1:5) {
assign(paste0("p", i), ggplot(obs_df, aes(x = data, y = fit)) +
	geom_point(colour = "grey") + 
	geom_point(data = filter(obs_df, metier== LETTERS[i]), colour = cols[i]) +
ylab("fitted proportions")  + xlab("Observed proportions")
)
}

library(cowplot)

p6 <- plot_grid(p1,p2,p3,p4,p5, labels = LETTERS[1:5])
save_plot("Observed_and_fitted_RUM.png", p6, base_width = 10, base_height = 8)


###################################
## Increment the species included
###################################

m5 <- mlogit(choice ~ COD | season, data = LD, 
	     print.level = 2)
summary(m5)

####################
## Model selection
###################




#####################
#
# Save output for FLBEIA
#
#####################
RUM_model_fit <- m5

save(RUM_model_fit, file = file.path("..", "tests", "RUM_model.RData"))
