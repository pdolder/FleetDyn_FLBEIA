################################################################
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

n_met <- length(fleets[["IE_Otter"]]@metiers)

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
for(y in 2004:2017) {
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

## Add some variation in the covariate data
## For each of the catch records, take a value around the mean
for(i in 6:16) {
sim_data[,i] <- sapply(sim_data[,i], function(x) { rnorm(1, mean = x, sd = 0.2 * x) }) 
}


## Add state t-1, except for first obs for each "vessel"
colnames(sim_data)[4] <- "state"
sim_data$state.tminus1 <- c(NA, sim_data$state[1:(nrow(sim_data)-1)])
sim_data[seq(1,nrow(sim_data), 8),"state.tminus1"] <- NA

sim_data$state         <- as.factor(sim_data$state) 
sim_data$state.tminus1 <- as.factor(sim_data$state.tminus1) 
sim_data$season        <- as.factor(sim_data$season)

colnames(sim_data)[5] <- "effshare"

## Summarise proportions in data
prop.table(table(sim_data$state.tminus1, sim_data$state, sim_data$season), margin = c(1,3))

## Investigate some of the covariates
## summarise the mean catch rates vs the proportions of effort

sim_props <- sim_data %>% group_by(year, season, state, state.tminus1) %>%
	summarise(N = n(), COD = mean(COD), HAD = mean(HAD), MON = mean(MON),
		  NEP16 = mean(NEP16), NEP17 = mean(NEP17), NEP19 = mean(NEP19),
		  NEP2021 = mean(NEP2021), NEP22 = mean(NEP22), NHKE = mean(NHKE), 
		  NMEG = mean(NMEG), WHG = mean(WHG)) %>% as.data.frame()

 ggplot(sim_props, aes(x = NEP2021, y = N)) + geom_point()

#############################
## Let's estimate with nnet
## multinom function
#############################

## Without season
m1 <- multinom(state ~ state.tminus1, data = sim_data)
pred.data <- data.frame(state.tminus1 = LETTERS[1:n_met])
predictions <- cbind(pred.data, predict(m1, pred.data, type = "probs"))
pred1 <- predictions %>% gather(state, prob,  -state.tminus1)

pred1_mat <- pred1 %>% spread(state, prob) 

par(mfrow=c(1,2))
image(p_trans)
image(as.matrix(pred1_mat[,2:ncol(pred1_mat)]))


## With season
m2 <- multinom(state ~ state.tminus1:season, data = sim_data)
pred.data <- data.frame(season = rep(1:4, each = n_met), state.tminus1 = rep(LETTERS[1:n_met], times = 4))
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
m3 <- multinom(state ~ state.tminus1 + season + COD + HAD + MON + 
	       NEP16 + NEP17 + NEP19 + NEP2021 + NEP22 + 
	       NHKE + NMEG + WHG, data = sim_data, maxit = 1e6)

m4 <- multinom(state ~ state.tminus1 + effshare +  season + COD + HAD + MON + 
	       NEP16 + NEP17 + NEP19 + NEP2021 + NEP22 + 
	       NHKE + NMEG + WHG, data = sim_data, maxit = 1e6)

m5 <- multinom(state ~ state.tminus1:season + COD + NEP2021, data = sim_data,
	       maxit = 1e6)

m7 <- multinom(state ~ state.tminus1:season + COD + HAD + MON + 
	       NEP16 + NEP17 + NEP19 + NEP2021 + NEP22 + 
	       NHKE + NMEG + WHG, data = sim_data, maxit = 1e6)

m8 <- multinom(state ~ state.tminus1:season + HAD + WHG, data = sim_data,
	       maxit = 1e6)
coef(m8)

AIC(m1, m2, m3, m4, m5)

## m3 is way better ??
## the summary function with nnet is no good though, takes forever to return
## anything

sim_data$lCOD     <- log(sim_data$COD+0.001)
sim_data$lHAD     <- log(sim_data$HAD+0.001)
sim_data$lMON     <- log(sim_data$MON+0.001)
sim_data$lNEP16   <- log(sim_data$NEP16+0.001)
sim_data$lNEP17 <- log(sim_data$NEP17+0.001)
sim_data$lNEP19   <- log(sim_data$NEP19+0.001)
sim_data$lNEP2021 <- log(sim_data$NEP2021+0.001)
sim_data$lNEP22   <- log(sim_data$NEP22+0.001)
sim_data$lNHKE    <- log(sim_data$NHKE+0.001)
sim_data$lNMEG    <- log(sim_data$NMEG+0.001)
sim_data$lWHG     <- log(sim_data$WHG+0.001)


#m4 <- multinom(state ~ state.tminus1*season + lCOD + lHAD + lMON + 
#	       lNEP16 + lNEP17 + lNEP19 + lNEP2021 + lNEP22 + 
#	       lNHKE + lNMEG + lWHG, data = sim_data, maxit = 1e3)
## same as m3


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
##load(file.path("RUMtestData.RData"))
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

if(round(sum(new.share),6) != 1) {stop("Error - effort share does not sum to 1")}

return(new.share)

}


########################################
########################################
## Full procedure 
########################################
########################################

## step 1

s <- 2
predict.df <- make_Markov_predict_df(model = m3, fleet = fl, s = s)

## step 2 
updated.df <- update_Markov_params(model = m3, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 

## step 3 
predicted.share <- predict_Markov(model = m3, updated.df = updated.df, fleet = fl, year = 3, season = s)

print(as.matrix(round(predicted.share,4)))

real_share <- filter(eff_met, year == 2017, season == 1)$data

test <- data.frame("metier" = names(predicted.share),
	   "pred" = round(predicted.share,8), 
	   "real" = real_share)
print(test)
#png("Markov_predictions_with_catch_rates.png")
matplot(test[,2:3], type = "l", xlab = "Metier", ylab = "Share")
legend(x  = 2, y = 0.4, col = 1:2, lty = 1:2, legend = c("pred", "obs"))
#dev.off()
colSums(test[,2:3])

## Metier K is very high compared to the observations
## Metier I is very low compared to the observations
test[test$metier == "K",]
test[test$metier == "I",]

table(sim_data$state, sim_data$season)
round(table(sim_data$state, sim_data$season) / colSums(table(sim_data$state, sim_data$season)),2)

## 43% predicted vs 8 % observed!!

################
###############
## Let's try with model 2 (season only)

predict.df <- make_Markov_predict_df(model = m2, fleet = fl, s = 1)
updated.df <- update_Markov_params(model = m2, predict.df = predict.df, fleet = fl, covars = covars, season = 1,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 
predicted.share <- predict_Markov(model = m2, updated.df = updated.df, fleet = fl, year = 3, season = 1)

real_share <- filter(eff_met, year == 2017, season == 1)$data

test <- data.frame("metier" = names(predicted.share),
	   "pred" = round(predicted.share,8), 
	   "real" = real_share)
print(test)
colSums(test[,2:3])

#png("Markov_predictions_seasonal_only.png")
matplot(test[,2:3], type = "l", xlab = "Metier", ylab = "Share")
legend(x  = 1.5, y = 0.18, col = 1:2, lty = 1:2, legend = c("pred", "obs"))
#dev.off()
## Nice,....so maybe the catch rates are throwing off the predictions ??
## Could be they need to be transformed to be more informative. Might want to
## include log transformation as an optional input


##############
## Let's explore some of the effects...

## Make sure the fit is best possible
m2 <- multinom(state ~ state.tminus1:season, data = sim_data, maxit = 1e3)

#Markov_fit <- m2
#save(Markov_fit, file = "Markov_fit.RData")

seasonal_preds <- lapply(1:4, function(s) {

predict.df <- make_Markov_predict_df(model = Markov_fit, fleet = fl, s = s)
updated.df <- update_Markov_params(model = Markov_fit, predict.df = predict.df, fleet = fl, covars = covars, season = s,
		       N, q.m, wl.m, beta.m, ret.m, pr.m) 
predicted.share <- predict_Markov(model = Markov_fit, updated.df = updated.df, fleet = fl, year = 3, season = s)
	   })

seasonal_preds <- do.call(rbind, seasonal_preds)
seasonal_preds <- as.data.frame(seasonal_preds)
seasonal_preds$season <- 1:4

seasonal_preds <- seasonal_preds %>% gather(metier, probs, -season)
seasonal_preds$season <- as.factor(seasonal_preds$season)

ggplot(seasonal_preds, aes(x = metier, y = probs, group = season)) +
	geom_point(aes(shape = season, colour = season), size = 3) +
	geom_line(aes(colour = season), size = 1.5) +
	ggtitle("Predicted seasonal shares of effort by metier") 
ggsave("Markov_seasonal_shares.png")



############################ 
####  Catch rate effect ####
############################

## Seem to be some issues here where out of bag predictions result in
## total effort shares > 1
## Need to think why...

stks <- c("COD", "HAD", "MON", 
		      "NEP16", "NEP17", "NEP19", "NEP2021", "NEP22",
		      "NHKE", "NMEG", "WHG")

res_stock <- lapply(stks, function(x) {

print(x)

predicted.share <- list()

cr_spp <- sim_data %>% group_by(state) %>% summarise(cr  = mean(get(x)) ) %>% as.data.frame()

## Increase in %s

## 0% to 100% higher CPUE 
cr <- lapply(seq(0.1,2,0.1), function(y) {
       cr <- cr_spp$cr  * y
       return(cr)
})

cr <- do.call(rbind, cr) ## matrix

for(i in 1:nrow(cr)) {
print(i)
cr_sub <- cr[i,]

if(x != "COD") {COD = 0.01}
if(x != "HAD") {HAD = 0.01}
if(x != "MON") {MON = 0.01}
if(x != "NEP16") {NEP16 = 0.01}
if(x != "NEP17") {NEP17 = 0.01}
if(x != "NEP19") {NEP19 = 0.01}
if(x != "NEP2021") {NEP2021 = 0.01}
if(x != "NEP22") {NEP22 = 0.01}
if(x != "NHKE") {NHKE = 0.01}
if(x != "NMEG") {NMEG = 0.01}
if(x != "WHG") {WHG = 0.01}

assign(x, cr_sub)

updated.df  <- data.frame(state.tminus1 = LETTERS[1:n_met], season = as.factor(1), 
		   "COD" = COD, "HAD" = HAD, "MON" = MON,
		   "NEP16" = NEP16,"NEP17" = NEP17,"NEP19" = NEP19,"NEP2021" = NEP2021,"NEP22" = NEP22,
		   "NHKE" = NHKE, "NMEG" = NMEG,
		   "WHG" = WHG)

predicted.share[[i]] <- predict_Markov(model = m3, updated.df = updated.df, fleet = fl, year = 3, season = 1)

}

return(predicted.share)


	   })


names(res_stock) <- stks

## crude plot

png(file.path("..", "plots", "Markov_Catch_Rate_Mult.png"), width = 1600, height = 1800)
par(mfrow=c(4,3), mar = c(1,1,1,1))
for(i in stks) {
matplot(do.call(rbind, res_stock[[i]]), type = "l", main = i, ylab = "Effshare", xlab = "Catch rate multiplier", lwd = 2, lty = 1, cex.main = 2) ## 
}
plot(x=seq(0,2,0.1), y = seq(0,1,0.05), type = "n", xlab = "", ylab = "", ann = F, xaxt = 'n', yaxt = 'n')
legend(0.1,0.8, lty = 1, legend = LETTERS[1:floor(n_met/2)], col = 1:floor(n_met/2)], cex = 3.7, bty = "n", lwd = 2)
legend(1.1,0.8, lty = 1, legend = LETTERS[ceiling(n_met/2):n_met], col = ceiling(n_met/2):n_met, cex = 3.7, bty = "n", lwd = 2)
dev.off()

####################
## Model selection
###################

# To fit to same data we need to drop first year where previous effort
# share is not available

library(MuMIn)
library(linear.tools)
library(doParallel)

sim_data2 <- sim_data[!is.na(sim_data$state.tminus1),]

## Define a global model
## all covariates on the lhs and rhs
covars <- c("state.tminus1", "season", "COD", "HAD", "WHG",  "NEP2021", "NEP22","NEP16", "NEP17", "NEP19", "NHKE", "MON", "NMEG")

## Use lm as dredge doesn't work with mlogit, eval = FALSE
glob.lm <- lm(paste("state ~", paste(covars, collapse = " + ")), data = sim_data2, na.action = na.fail)

all.models <- dredge(glob.lm, eval = FALSE)

rm(glob.lm) 

## Define BIC function

BIC <- function(k,n,ll) {
# k number params
# n number of data points
# L the maxLik
(k*log(n)) - (2 * ll)
}

AICc <- function(AICv, k, n) {
	AICv + ((2*k)*(k+1))/(n - k - 1)
}

####################################
## In parallel
####################################

mod <- multinom(all.models[[8192]], data = sim_data2)


k <- length(coefficients(mod))
n <- nrow(sim_data2)
ll <- -mod$value
BIC(k,n,ll)

mod$AIC

rm(mod)
gc()

registerDoParallel(10)

models_fits <- foreach(i = 2:length(all.models)) %dopar% {

print(i)
mod <- multinom(all.models[[i]], data = sim_data2, maxit = 1e6)
return(c(npar = length(coefficients(mod)),
	 AIC = mod$AIC,
	 AICc = AICc(AICv = mod$AIC, k = length(coefficients(mod)), n = nrow(sim_data2)),
	 BIC = BIC(k = length(coefficients(mod)), 
	 n = nrow(sim_data2), 
	 ll = -mod$value)))
}

stopImplicitCluster()


Markov_selection <- dplyr::bind_rows(models_fits)

Markov_selection$model <- unlist(lapply(all.models, function(x) paste_formula(formula(x))))[2:length(all.models)]

Markov_selection <- Markov_selection %>% select(model, npar, AIC, AICc, BIC)

save(Markov_selection, models_fits, file = "Markov_model_selection.RData")

## By AIC

Markov_selection[order(Markov_selection$AIC),]

Markov_selection[order(Markov_selection$BIC, Markov_selection$AIC),] %>% as.data.frame() %>%
       head(20)	

ranked_mods <- Markov_selection[order(Markov_selection$AIC, Markov_selection$BIC),] %>% as.data.frame() %>%
 select(model)

ranked_mods$effshare <- grepl("effshare", ranked_mods$model) ## all the models including effshare

head(ranked_mods[ranked_mods$effshare == FALSE,])

best.mod <- multinom(state ~ state.tminus1 + season + COD + HAD + MON + NEP16 + NEP17 + NEP19 + 
		     NEP2021 + NEP22 + NHKE + NMEG + WHG, data = sim_data2, maxit = 1e6)

coef(best.mod)

BIC(k = length(coefficients(best.mod)), 
	 n = nrow(sim_data2), 
	 ll = -best.mod$value)

best.mod2 <- multinom(state ~ state.tminus1:season + COD + HAD + MON + NEP16 + NEP17 + NEP19 + 
		     NEP2021 + NEP22 + NHKE + NMEG + WHG, data = sim_data2, maxit = 1e6)


coef(best.mod2)

BIC(k = length(coefficients(best.mod2)), 
	 n = nrow(sim_data2), 
	 ll = -best.mod2$value)

######################
## in serial..
#####################

#model_fits <- matrix(NA, ncol =3, nrow = length(all.models))
#colnames(model_fits) <- c("model", "AIC", "BIC")
#
#for(i in 2:length(all.models)) {
#print(i)
#mod <- mlogit(all.models[[i]], data = LD)
#
#model_fits[i,"model"] <- i
#model_fits[i,"AIC"] <- AIC(mod) 
#model_fits[i,"BIC"] <- BIC(k = length(mod$coefficients), 
#n = nrow(LD), 
#L = -mod$logLik[[1]])
#}




Markov_fit <- best.mod 
save(Markov_fit, file = file.path("..", "tests","Markov_model.RData"))


