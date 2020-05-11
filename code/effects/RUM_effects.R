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

model <- RUM_model_fit

##########################
# calculating the probs
########################

res_stock <- lapply(colnames(res)[6:16], function(x) {

print(x)

cr_spp <- res %>% group_by(metier) %>% summarise(cr  = mean(get(x)) ) %>% as.data.frame()

## Increase in %s

ratios <- seq(0.1, 20, 0.1)

## 0% to 100% higher CPUE 
cr <- lapply(ratios, function(y) {
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

res3<- data.frame(season = rep(as.factor(1:4),each = nrow(cr)*12), metier = rep(LETTERS[1:12], times = nrow(cr)*4), choice = "yes", 
		   "COD" = COD, "HAD" = HAD, "MON" = MON, 
		   "NEP16" = NEP16, "NEP17" = NEP17, "NEP19" = NEP19, "NEP2021" = NEP2021, "NEP22" = NEP22, 
		   "NHKE" = NHKE, "NMEG" = NMEG,
		   "WHG" = WHG, effshare = 0)

res3$index <- seq_len(nrow(res3))
## Use mFormula to define model form
LD3 <- mlogit.data(res3, choice = "choice", shape = "long",
		   alt.var = "metier", chid.var = "index")


## For each season

seasons_out <- lapply(1:4, function(season) {

print(season)
mod.mat <- model.matrix(model$formula, data = LD3)
beta <- as.matrix(coef(model))


## linear predictor long
seas <- 1:max(as.numeric(as.character(model.frame(model)$season)),na.rm=T)
toRemove <- paste0("season", seas[!seas %in% season])

# remove from mod.mat
mod.mat <- mod.mat[,!colnames(mod.mat) %in% grep(paste(toRemove, collapse = "|"), colnames(mod.mat), value = T)]
# remove from beta
beta <- beta[!rownames(beta) %in% grep(paste(toRemove, collapse = "|"), rownames(beta), value = T),]

eta_long <- mod.mat %*% beta

## linear predictor wide
eta_wide <- matrix(eta_long, ncol = 12, byrow = TRUE)
names(eta_wide) <- toupper(letters[1:12])

## convert to a probability
own_p_hat <- exp(eta_wide) / rowSums(exp(eta_wide))

head(own_p_hat)

own_p_hat <- own_p_hat[1:length(ratios),]  ## is repeated 4 times

## So this would be the probabilities for each season
colnames(own_p_hat) <- LETTERS[1:12]
rownames(own_p_hat) <- ratios

own_p_hat <- as.data.frame(own_p_hat) %>% gather(metier, prob)
own_p_hat$percIncrease = ratios

own_p_hat$stock <- x
own_p_hat$season <- season

return(own_p_hat)

		   })

seasons_out2 <- bind_rows(seasons_out)

return(seasons_out2)


})

res_all <- bind_rows(res_stock)

## Note, does not interact with season
ggplot(filter(res_all,season==1), aes(x = percIncrease, y = prob, group = stock)) +
geom_line(aes(colour = stock, group = stock)) +
facet_wrap(~metier, scale = "free_y") + theme_bw() +
ggtitle(paste0("Catch rate multiplier effect on choice probabilities")) +
xlab("Catch Rate multiplier") +
ylab("Choice probability / share")
ggsave(paste0("RUM_Stock_Catch_Rate_Multiplier.png"))
#

#######################
# Seasonal effect
######################

res2 <- res %>% filter(choice == "yes") %>%
	       group_by(season, metier, choice) %>% 
	       summarise(COD=mean(COD),
			 HAD = mean(HAD),
			 MON = mean(MON),
			 NEP16 = mean(NEP16),
			 NEP17 = mean(NEP17),
			 NEP19 = mean(NEP19),
			 NEP2021 = mean(NEP2021),
			 NEP22 = mean(NEP22),
			 NHKE = mean(NHKE),
			 NMEG = mean(NMEG),
			 WHG = mean(WHG)) %>%
	       mutate(effshare = 0) %>%
	       as.data.frame()


res2$index <- seq_len(nrow(res2))
## Use mFormula to define model form
LD4 <- mlogit.data(res2, choice = "choice", shape = "long",
		   alt.var = "metier", chid.var = "index")

## For each season
mod.mat <- model.matrix(model$formula, data = LD4)
beta <- as.matrix(coef(model))

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
rownames(own_p_hat) <- paste0("season", 1:4) 

own_p_hat <- as.data.frame(own_p_hat) %>% gather(metier, prob)
own_p_hat$season <- rep(1:4, times = 12)


ggplot(own_p_hat, aes(x = season, y = prob)) +
	geom_line(aes(colour = metier)) + theme_bw() +
ggtitle(paste0("Seasonal effect on baseline choice probabilities")) +
xlab("Season") +
ylab("Seasonal effect")
ggsave(paste0("RUM_Seasonal_effect.png"))
#

