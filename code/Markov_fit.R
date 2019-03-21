fn##############################################################
## A simplified Markov fit to the IE_Otter data
###############################################################

library(FLBEIA)
library(tidyverse)
library(nnet)
library(msm)

set.seed(111)

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

choices$season <- as.numeric(as.character(choices$season))


#### we need some way of simulating a chain of fishing events with these
#### proportions of observations....

## choose season 1
seas1 <- filter(choices, year == 2015, season == 1)

n_trans <- 1e3  ## number of observed transitions / number of vessels

# sample metier
metiers <- sample(seas1$metier, n_trans,
		  prob = seas1$data, replace = T)

trans   <- statetable.msm(metiers, subject = NULL)
p_trans <- trans/rowSums(trans)

image(p_trans)

metiers <- data.frame(state = as.factor(metiers))
metiers$state.tminus1[2:nrow(metiers)] <- metiers$state[-1]

plot(seq_len(nrow(metiers)), metiers$state, type = "l")
## This isn't very realistic - needs to be more autocorrelation
## but gives some data at least...


### Let's do this by season
n_trans <- 1e4

sim_data <- NULL

## loop over year and season with separate probabilities
for(y in 2015:2017) {
print(y)

for(i in 1:4) {
print(i)
	seas <- filter(choices, year == y, season == i)
metiers <- sample(seas$metier, n_trans,
		  prob = seas$data, replace = T)
sim_data <- rbind(sim_data,
		  data.frame(year = y, season = i, 
			     vessel = sample(seq_len(n_trans), 
					     n_trans, replace = F), 
			     metier = metiers))
}

}

## Add on the catch info - covariates which affected probs
sim_data <- left_join(sim_data, choices, by = c("year", "season", "metier")) %>%
	select(-quant, -unit, -area, -iter, -tot, -effort)

sim_data <- sim_data[order(sim_data$vessel, sim_data$year, sim_data$season),]

head(sim_data)

## Add state t-1, except for first obs for each "vessel"
colnames(sim_data)[4] <- "state"
sim_data$state.tminus1 <- c(NA, sim_data$state[1:(nrow(sim_data)-1)])
sim_data[seq(1,nrow(sim_data), 12),"state.tminus1"] <- NA

sim_data$state         <- as.factor(sim_data$state) 
sim_data$state.tminus1 <- as.factor(sim_data$state.tminus1) 
sim_data$season        <- as.factor(sim_data$season)

#############################
## Let's estimate with nnet
## multinom function
#############################

## Without season
m1 <- multinom(state ~ state.tminus1, data = sim_data)
pred.data <- data.frame(state.tminus1 = LETTERS[1:12])
predictions <- cbind(pred.data, predict(m1, pred.data, type = "probs"))
pred1 <- predictions %>% gather(state, prob,  -state.tminus1)

pred1_mat <- pred1 %>% spread(state, prob) 

par(mfrow=c(1,2))
image(p_trans)
image(as.matrix(pred1_mat[,2:ncol(pred1_mat)]))


## With season
m2 <- multinom(state ~ state.tminus1:season, data = sim_data)
pred.data <- data.frame(season = rep(1:4, each = 12), state.tminus1 = rep(LETTERS[1:12], times = 4))
pred.data$season <- as.factor(pred.data$season)
predictions <- cbind(pred.data, predict(m2, pred.data, type = "probs"))
pred2 <- predictions %>% gather(state, prob,  -season, -state.tminus1)
pred2 <- pred2[order(pred2$season),]

## Seasonal  transition probs
par(mfrow=c(2,2))
for(i in 1:4) {
seas <- filter(pred2, season == i)
seas <- seas %>% select(-season) %>% spread(state, prob)
image(as.matrix(seas[,2:ncol(seas)]))
}

## with season and species predictors...
m3 <- multinom(state ~ state.tminus1:season + COD + HAD + MON + 
	       NEP16 + NEP17 + NEP19 + NEP2021 + NEP22 + 
	       NHKE + NMEG + WHG, data = sim_data)

AIC(m1, m2, m3)

## m3 is way better ??
## the summary function with nnet is no good though, takes forever to return
## anything

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
## 3. Make the predictions - this should return a matrix of transition
## probabilities 
## 4. Now multiply the current effort shares (s-1) by the transition probs
## 5. Return a vector of effort share by metier


make_Markov_predict_df <- function(model = NULL, fleet = NULL, season = s) {

## Pass multinom model object
## Pass fleet object

mod.coefs <- model$coefnames ## Model coefficients

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
## Note, we need the state from which vessels are coming
predict.df <- expand.grid(state.tminus1 = fl@metiers@names,
			  season = as.numeric(seas), 
			  vcost = v, 
			  effshare = e,
			  stringsAsFactors = FALSE)
## Remove any columns with NAs, indicating variable not used
predict.df <- predict.df[,which(sapply(predict.df, function(x) all(!is.na(x))))]

## Correct attributes for prediction data
if(!is.na(seas)) { 
	if(attr(model$terms, "dataClasses")[["season"]] == "factor") {
	predict.df$season <- as.factor(predict.df$season)
	}
}

## Combine with the catch rate columns
if(!all(is.na(C))) {	
C.df <- as.data.frame(matrix(-1, ncol = length(C), nrow = nrow(predict.df)))
colnames(C.df) <- C

predict.df <- cbind(predict.df, C.df)
}

return(predict.df)
}


predict.df <- make_Markov_predict_df(model = m3, fleet = fl, s = 1)



########################
## Update parameters
########################


update_Markov_params <- function(model = NULL, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) {

## Update the values in the predict.df

## 2. catch / catch rates - on same scale.
## Note, these should be updated based on the biomass increases, so we do a
## similar calculation as for the gravity model
## Here have to be careful as not all metiers may catch all stocks...
	
if(any(sapply(catchNames(fl), grepl, model$coefnames))) {

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
	predict.df[,st] <- CR[CR$stock == st,2]  ## This will repeat, to ensure we get for each metier combinations
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
##########################

updated.df <- update_Markov_params(model = m3, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 


########################
## Make predictions
########################

predict_Markov <- function(model = mod, updated.df = updated.df, fleet = fl, season, year) {

# Transition probs
p_hat <- cbind(updated.df[c("state.tminus1")], predict(model, updated.df, type = "probs"))
p_hat_mat <- as.matrix(p_hat[,2:ncol(p_hat)])

# past effort

# New year
if(season == 1) {
last.season <- dims(fl)[["season"]]
cur.eff <- as.matrix(sapply(fl@metiers, function(x) x@effshare[,year-1, , last.season]))
}

# Same year
if(season > 1) {
cur.eff <- as.matrix(sapply(fl@metiers, function(x) x@effshare[, year, , season-1]))
}

new.share <- apply(p_hat_mat, 2, function(x) x %*% cur.eff)

if(sum(new.share) != 1) {stop("Error - effort share does not sum to 1")}

return(new.share)

}

## step 1 
predict.df <- make_Markov_predict_df(model = m3, fleet = fl, s = 1)

## step 2 
updated.df <- update_Markov_params(model = m3, predict.df = predict.df, fleet = fl, covars = covars, season = 1,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 

## step 3 
predicted.share <- predict_Markov(model = m3, updated.df = updated.df, fleet = fl, year = 3, season = 1)

real_share <- filter(eff_met, year == 2017, season == 1)$data

test <- data.frame("metier" = names(predicted.share),
	   "pred" = round(predicted.share,8), 
	   "real" = real_share)
print(test)
colSums(test[,2:3])


