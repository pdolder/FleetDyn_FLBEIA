########################################################
##  Covert FCube Stock objects to seasonal biol objects
##  for FLBEIA
########################################################

library(FLBEIA)
library(TMB)

## Paths
stk.path <- file.path("..", "FCube", "stocks")

## 1. Load the stock objects
stocks<-FLStocks(lapply(list.files(stk.path),function(x){
                            load(file.path(stk.path,x))
                        res<-get("stock")
                        name(res)<-gsub('.RData',"",x)
                        res}))

## For some reason Nep22 has no stock weight, we'll use the catch weight
stocks[["NEP22"]]@stock.wt[] <- stocks[["NEP22"]]@catch.wt

## And let's ignore NEPOTH as its small 
stocks <- stocks[!names(stocks) == "NEP7OTH"]


# 2. Move to FLBiols
biols <- FLBiols(lapply(stocks, function(x) {
    as(x, "FLBiol")
}))

## Want to ensure the fbar ranges carry across. For some reason not automatic
for(s in names(biols)) {
biols[[s]]@range <- stocks[[s]]@range
}


# 3. expand the biols for a seasonal component 
biols <- FLBiols(lapply(biols, function(x) expand(x, season = 1:4)))

## Need to adjust the numbers, weights etc.. for seasonal growth

## First try weights
test <- biols[[1]]

## within a given year, weight at age
wts <- as.data.frame(test@wt[,1,,1])
plot(wts$data ~ wts$age, type = "b")

## We fit some lm's with polynomials 
fit <- lm(wts$data ~ wts$age)
fit2 <- lm(wts$data ~ poly(wts$age,2,raw = T))
fit3 <- lm(wts$data ~ poly(wts$age,3,raw = T))
fit4 <- lm(wts$data ~ poly(wts$age,4,raw = T))

plot(wts$data ~ wts$age)
lines(1:7, predict(fit, data.frame(age = 1:7)))
lines(1:7, predict(fit2, data.frame(age = 1:7)), col = "blue")
lines(1:7, predict(fit3, data.frame(age = 1:7)), col = "red")
lines(1:7, predict(fit4, data.frame(age = 1:7)), col = "green")

## can do better
wts <- as.data.frame(test@wt[,,,1]) ## all years

## Fit a von bertalanffy growth model
## In TMB
## this is the method in the package fishmethods
compile("vonbert.cpp")
dyn.load(dynlib("vonbert"))

dat <- list(wgt = wts$data, age = wts$age, age_q = seq(min(wts$age), max(wts$age)+0.75, 0.25),
	    yr = as.numeric(as.factor(wts$year))-1)
pars <- list(logK = exp(0.3), t0 = -1, Sinf = 200, logSigma = 0, 
	     year_ef = rep(0, length(unique(dat$yr))), sigma_yr = 0)

obj <- MakeADFun(dat, pars, DLL = "vonbert")#, random = "year_ef")
obj$fn()
obj$gr()

opt <- nlminb(obj$par, obj$fn, obj$gr)
sdreport(obj)
report <- obj$report()

pdf("test_vbgf.pdf", width = 6, height = 6)

ggplot(data.frame(year = factor(dat$yr), age = dat$age, obs = dat$wgt, pred = report$fitted), 
       aes(x = age, y = pred, group = year)) + 
	geom_point(aes(colour = year)) + 
	geom_line(aes(colour = year)) +
	geom_point(aes(y = obs), shape = "x")  

plot(report$residuals, type = "p")
abline(h = 0)

qqnorm(report$residuals)
abline(0,1)

dev.off()

## For MON-CS and N-MEG the model doesn't fit....
## Also, for Nephrops it makes no sense

## Let's start with Monkfish
test <- as.data.frame(biols[["MON-CS"]]@wt[,1,,1])
wt <- test$data
age <- test$age

plot(wt ~ age, type = "b")

fit <- lm(wt~ poly(age,3,raw = T))

plot(wt ~ age, type = "b")
lines(seq(0, 7.75, 0.25), 
predict(fit, newdata = data.frame(age = seq(0,7.75, 0.25))), col = "blue")

## That will do...

## And N-MEG
test <- as.data.frame(biols[["N-MEG"]]@wt[,1,,1])
wt <- test$data
age <- test$age

plot(wt ~ age, type = "b")

fit <- lm(wt~ poly(age,3,raw = T))

plot(wt ~ age, type = "b")
lines(seq(1, 10.75, 0.25), 
predict(fit, newdata = data.frame(age = seq(1,10.75, 0.25))), col = "blue")

## That will do...

## This leave Nephrops
### We can just assume its the same in each season  

neps <- grep("NEP", names(biols), value = T) ## names of nephrops stocks



###############################################################

## OK, let's do this for each year updates to the biols

## try for cod, had, whg, n-hake, n-meg and mon
## we need to do something else for nep

stks <- c("COD-CS", "HAD-CS", "WHG-CS", "N-HKE", "MON-CS", "N-MEG")

pars_out <- c("logK", "t0", "Sinf", "logSigma")

## Let's also record the parameter estimates
param_ests <- matrix(NA, nrow = length(stks), ncol = length(pars_out),
       dimnames = list(stks, pars_out))

pdf("VBGF_fits.pdf", height = 12, width = 4)

for(n. in stks) {
	       print(n.)
	       x <- biols[[n.]]
	
	for(y in range(x)[["minyear"]]:range(x)[["maxyear"]]) {
	
		print(y)
		df <- as.data.frame(x@wt[,ac(y),,1])  
		wt <- df$data 
		age <- df$age

		## fit the vb growth model and predict the increments
		
		dat <- list(wgt = wt, age = age, age_q = seq(min(age), max(age)+0.75, 0.25))
		pars <- list(logK = 0.3, t0 = -1, Sinf = max(wt)*50, logSigma = 0)
		obj <- MakeADFun(dat, pars, DLL = "vonbert")
		opt <- nlminb(obj$par, obj$fn, obj$gr)
		report <- obj$report()

		param_ests[n.,] <- opt$par
		
		preds <-data.frame(age = rep(age, each = 4), 
		season = rep(1:4, times = length(age)),
	data =  report$predictions)
		
		par(mfrow=c(3,1))
		
		plot(age, wt, type = "p", main = paste( n., y, sep = " "))
		points(age, report$fitted, col = "blue")
		lines(seq(min(age), max(age)+0.75, 0.25), report$predictions)

		plot(report$residuals, type = "p")
		abline(h=0)
		
		qqnorm(report$residuals)
		abline(0,1)


## Fill the biol for the year
for(i in 1:4) { 
	biols[[n.]]@wt[,ac(y),,i][] <- preds$data[preds$season == i]
	biols[[n.]]@wt[biols[[n.]]@wt<0] <- 0.001
}

}

}

dev.off()

param_ests

## Note residual patterns, especially noticeable for N-hake

## Recruitment -- season 2 only

biols <- FLBiols(lapply(biols, function(x) { 
				x@rec$rec[,,,c(1,3,4)] <- 0 
				return(x)
}))

## Natural mortality
## equal per month
biols <- FLBiols(lapply(biols, function(x) { 
				x@m <- x@m/4
				return(x)
}))


## Numbers at age

## Here we can apply 1/4 of the mortality rate (M+F) each timestep
## We need to loop over the matrix of Ns at age

## Really this should be proportionate to the landings by quarter...
## else maybe the catchability conditioning will be wrong ??

test <- biols[["COD-CS"]]@n

ages  <- seq_len(dim(test)[1])
years <- seq_len(dim(test)[2])

for(a in ages) {
	for(y in years) {
		for(s in 2:4) {
test[a,y,,s] <- test[a,y,,s-1] * 
	exp(-(stocks[["COD-CS"]]@harvest[a,y,,1]/4 + 
	      biols[["COD-CS"]]@m[a,y,,1]))
}}}

## OK, now over all stocks

biols <- FLBiols(lapply(biols, function(x) {
		
				n. <- name(x)
				print(n.)
				
				ages  <- seq_len(dim(x@n)[1])
				years <- seq_len(dim(x@n)[2])

				for(y in years) {
					
				# recruitment at season 2, so move the ns over
					
				if(!grepl("NEP", n.)) {
				x@n[1,y,,2] <- x@n[1,y,,1]
				# first age is always 0 at season 0
				x@n[1,y,,1] <- 0
				}
				# now loop over rest
					for(a in ages) {
						for(s in 2:4) {
				if(!(s == 2 & a == 1)) {  ## don't want to overwrite these ns
				if(!grepl("NEP", n.)) {
				x@n[a,y,,s] <- x@n[a,y,,s-1] *
				exp(-(stocks[[n.]]@harvest[a,y,,1]/4 + 
				x@m[a,y,,1]))
				}

				if(grepl("NEP", n.)) {
				x@n[a,y,,s] <- x@n[a,y,,s-1] *
				(1-stocks[[n.]]@harvest[a,y,,1]/4)
				}
						}
				}
				}
				}
		
				return(x)
}))



## Plot

x <- as.data.frame(ssb(biols[[1]]))
x$year_season <- paste(x$year, x$season)

ggplot(x, aes(x = year_season, y = data)) +
geom_point(aes(colour = season)) +
geom_line(aes(group = season, colour = season))


## rename

names(biols)<-gsub("CS","",names(biols))
names(biols)<-gsub("-","",names(biols))
names(biols)


for(s in names(biols)) {
biols[[s]]@name <- s
}


save(biols,file=file.path("..", "biols",'biols.RData'))
