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

## Add cohort onto the data
## should be a cohort effect, not year effect
wts2 <- as.data.frame(FLCohort(test@wt)[,47,,1]) ## all years
wts2 <- wts2[!is.na(wts2$data),]  ## remove NAs

ggplot(wts2, aes(x = age, y = data, group = cohort)) +
	geom_point(aes(colour = cohort)) + geom_line(aes(colour = cohort)) +
	facet_wrap(~cohort)


## Fit a von bertalanffy growth model
## In TMB
## this is the method in the package fishmethods
compile("vonbert.cpp")
dyn.load(dynlib("vonbert"))
dat <- list(wgt = wts2$data, age = wts2$age, 
	    age_q = seq(min(wts$age), max(wts$age)+0.75, 0.25))   ## for predictions

pars <- list(logK = exp(0.3), t0 = -1, Sinf = 200, logSigma = 0) 

obj <- MakeADFun(dat, pars, DLL = "vonbert")
obj$fn()
obj$gr()

opt <- nlminb(obj$par, obj$fn, obj$gr)
sdreport(obj)
report <- obj$report()

obs   <- data.frame(age = dat$age, obs = dat$wgt)
preds <- data.frame(age = dat$age_q,
		    pred = report$predictions)

theme_set(theme_bw())
pdf("test_vbgf.pdf", width = 6, height = 6)

ggplot(preds, aes(x = age, y = pred)) + 
	geom_line(size = 1.5, col = "blue") +
	geom_point(data = obs, aes(y = obs), shape = "o", size = 4)  

plot(report$residuals, type = "p")
abline(h = 0)

qqnorm(report$residuals)
abline(0,1)

dev.off()

### For Nephrops we can just assume its the same in each season  
neps <- grep("NEP", names(biols), value = T) ## names of nephrops stocks

###############################################################

## OK, let's do this for each year updates to the biols

## try for cod, had, whg, n-hake, n-meg and mon
## we need to do something else for nep

stks <- c("COD-CS", "HAD-CS", "WHG-CS", "N-HKE", "MON-CS", "N-MEG")

pdf("VBGF_fits.pdf", height = 12, width = 4)

for(n. in stks) {
	       print(n.)
	       x <- biols[[n.]]
	       x <- FLCohort(x@wt)	       

	for(y in dimnames(x)$cohort) {
	
		print(y)
		df <- as.data.frame(x[,ac(y),,1])  
		wt <- df$data 
		age <- df$age

		age <- age[!is.na(wt)]
		wt  <- wt[!is.na(wt)]

		## fit the vb growth model and predict the increments
		
		dat <- list(wgt = wt, age = age, age_q = seq(min(age), max(age)+0.75, 0.25))
		pars <- list(logK = 0.3, t0 = -1, Sinf = max(wt)*50, logSigma = 0)
		obj <- MakeADFun(dat, pars, DLL = "vonbert")
		opt <- nlminb(obj$par, obj$fn, obj$gr)
		report <- obj$report()

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


## Fill the biol for the cohort 
## Need to loop through each
for(i in 1:nrow(preds)) {

	y1 <- as.numeric(y)+ preds$age[i]  ## year to replace
	i1 <- preds$season[i]              ## season to replace
	a1 <- preds$age[i]                 ## age to replace
		
	biols[[n.]]@wt[ac(a1),ac(y1),,ac(i1)][] <- preds$data[i]
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
