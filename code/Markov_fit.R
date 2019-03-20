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

seas1 <- filter(choices, year == 2015, season == 1)

#### we need some way of simulating a chain of fishing events with these
#### proportions of observations....

set.seed(111)

n_trans <- 1e4

metiers <- sample(seas1$metier, n_trans,
		  prob = seas1$data, replace = T)

trans   <- statetable.msm(metiers, subject = NULL)
p_trans <- trans/rowSums(trans)

image(p_trans)

metiers <- data.frame(state = as.factor(metiers))
metiers$state.tminus1[2:nrow(metiers)] <- metiers$state[-1]

plot(seq_len(nrow(metiers)), metiers$state, type = "l")
## This isn't very realistic - needs to be more autocorrelation

### Let's do this by season

n_trans <- 1e5

sim_data <- NULL

for(i in 1:4) {
	seas <- filter(choices, year == 2015, season == i)
metiers <- sample(seas$metier, n_trans,
		  prob = seas$data, replace = T)
sim_data <- rbind(sim_data,
		  data.frame(season = i, 
			     vessel = sample(seq_len(n_trans), 
					     n_trans, replace = F), 
			     metier = metiers))
}

sim_data <- sim_data[order(sim_data$vessel),]


## Remove first obs from data for each "vessel"
colnames(sim_data)[3] <- "state"
sim_data$state.tminus1 <- c(NA, sim_data$state[1:(nrow(sim_data)-1)])
sim_data[seq(1,nrow(sim_data), 4),"state.tminus1"] <- NA

sim_data$state         <- as.factor(sim_data$state) 
sim_data$state.tminus1 <- as.factor(sim_data$state.tminus1) 

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
image(as.matrix(pred1_mat[,2:ncol(pred1_mat)]))


## With season
m2 <- multinom(state ~ state.tminus1:factor(season), data = sim_data)
pred.data <- data.frame(season = rep(1:4, each = 12), state.tminus1 = rep(LETTERS[1:12], times = 4))
pred.data$season <- as.factor(pred.data$season)
predictions <- cbind(pred.data, predict(m2, pred.data, type = "probs"))
pred2 <- predictions %>% gather(state, prob,  -season, -state.tminus1)
pred2 <- pred2[order(pred2$season),]

## Seasonal  transition probs
par(mfrow=c(2,2))
for(i in 1:4) {
seas1 <- filter(pred2, season == i)
seas1 <- seas1 %>% select(-season) %>% spread(state, prob)
image(as.matrix(seas1[,2:ncol(seas1)]))
}

