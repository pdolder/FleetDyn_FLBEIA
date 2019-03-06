########################################################
##  Covert FCube Stock objects to seasonal biol objects
##  for FLBEIA
########################################################

library(FLBEIA)
library(fishmethods)

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

# 3. expand the biols for a seasonal component 
biols <- FLBiols(lapply(biols, function(x) expand(x, season = 1:4)))

## Need to adjust the numbers, weights etc.. for seasonal growth

## First try weights
test <- biols[[1]]

## within a given year, weight at age
wts <- as.data.frame(test@wt[,1,,1])
plot(wts$data ~ wts$age, type = "b")

## We fit a VBGF
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

## Note Sinf, K and t0 are starting values for optimiser
fit <- growth(intype = 2, unit = 2, size = wts$data, age = wts$age,
       Sinf = 200, K = 0.3, t0 = -1)

## take the vbgf
plot(wts$data ~ wts$age)
lines(seq(1,7.75,0.25),predict(fit$vout, data.frame(age = seq(1,7.75, 0.25))), type = "b", col = "blue")

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

## try for cod, had, whg, n-hake
## we need to do something else for nep, n-meg and mon

stks <- c("COD-CS", "HAD-CS", "WHG-CS", "N-HKE", "MON-CS", "N-MEG")

for(n. in stks) {
	       print(n.)
	       x <- biols[[n.]]
	
	for(y in range(x)[["minyear"]]:range(x)[["maxyear"]]) {
	
		print(y)
		df <- as.data.frame(x@wt[,ac(y),,1])  
		wt <- df$data
		age <- df$age

		if(n. %in% c("COD-CS", "HAD-CS", "WHG-CS", "N-HKE")) {

		## fit the growth model and predict the increments
		fit <- growth(intype = 2, unit = 2, size = wt, age = age,
	        Sinf = 200, K = 0.3, t0 = 0, graph = TRUE)

		preds <-data.frame(age = rep(age, each = 4), 
		season = rep(1:4, times = length(age)),
	data = predict(fit$vout, data.frame(age = seq(min(age),max(age)+0.75, 0.25))))
		
		}
		
		if(n. %in% c("MON-CS", "N-MEG")) {
	
		fit <- lm(wt~ poly(age,3,raw = T))
		
		preds <-data.frame(age = rep(age, each = 4), 
		season = rep(1:4, times = length(age)),
	data = predict(fit, data.frame(age = seq(min(age),max(age)+0.75, 0.25))))
		
		}

## Fill the biol for the year
for(i in 1:4) { 
	biols[[n.]]@wt[,ac(y),,i][] <- preds$data[preds$season == i]
	biols[[n.]]@wt[biols[[n.]]@wt<0] <- 0.001
}

}

}
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

				for(a in ages) {
					for(y in years) {
						for(s in 2:4) {
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
