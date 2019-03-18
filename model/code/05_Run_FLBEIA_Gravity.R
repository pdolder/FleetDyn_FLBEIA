###################################################
## Testing basic model
###################################################

library(FLBEIA)
library(ggplot2)


## Load in all files
lapply(list.files(file.path("..", "model_inputs"), full.names = TRUE), load, .GlobalEnv)


fleets.ctrl[['IE_Otter']][['effort.model']]   <- 'SMFB_ES'
fleets.ctrl[['IE_Otter']][['effshare.model']] <- 'gravity'


SC2 <- FLBEIA(biols = biols, 
	      SRs = SRs, 
	      BDs = NULL, 
	      fleets=fleets, 
	      covars =NULL, 
              indices = NULL, 
	      advice = advice, 
	      main.ctrl = main.ctrl, 
              biols.ctrl = biols.ctrl, 
	      fleets.ctrl = fleets.ctrl, 
              covars.ctrl =covars.ctrl, 
	      obs.ctrl = obs.ctrl, 
              assess.ctrl = assess.ctrl, 
	      advice.ctrl = advice.ctrl) 

save(SC2, file = file.path("..", "outputs", "GravityModel.RData"))

names(SC2)

###############
## Check the effect
###############


load(file.path("..", "outputs", "GravityModel.RData"))
load(file.path("..", "outputs", "BaseModel.RData"))

###############################################
### Check effort share calc working correctly
#################################################

fl <- SC2$fleets[["IE_Otter"]]

## predicted effort shares from gravity model

met_vals <- lapply(fl@metiers, function(met) {
	      
	  res <- lapply(met@catches, function(s) {
		n. <- s@name
		if(grepl("NEP", n.)) { SC2$biols[[n.]]@m[] <- 0  }
	        st.val <- as.data.frame(apply(
					      (s@catch.q[,49,,1] * 
					      (s@landings.wt[,49,,1] *  
						(SC2$biols[[n.]]@n[,49,,1]* 
					      exp(-SC2$biols[[n.]]@m[,49,,1]/2))^s@beta[,49,,1]) * 
					      s@landings.sel[,49,,1] * 
					      s@price[,49,,1]), c(2), sum))
		

		return(st.val)
})
	  res2 <- do.call(rbind,res)
	  val <- sum(res2$data)
	  return(val)
	      
	      })

met_vals <- do.call(rbind, met_vals)

cbind(met_vals/colSums(met_vals),
as.matrix(unlist(lapply(fl@metiers, function(x) x@effshare[,49,,1])), ncol = 1))

## Success!


################################
### Compare the 'base' model and
### Gravity model
################################

## Effort in each scenario
df <- rbind(cbind(scenario = "base", as.data.frame(SC1$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "gravity", as.data.frame(SC2$fleets[["IE_Otter"]]@effort))
)
df <- df[df$year > 2014,]


ggplot(df, aes(x = year, y = data)) + geom_line(aes(colour = scenario)) +
	facet_wrap(~season, scale = "free") + ggtitle("Effort by quarter")
## in multi-stock example, total effort quite different by season 

## Compare effort predictions with fixed effort dist and gravity model
SC1$fleets[["IE_Otter"]][[3]]@effshare[,ac(2018:2022)]
SC2$fleets[["IE_Otter"]][[3]]@effshare[,ac(2018:2022)]


library(ggplotFL)
plot(FLStocks("base" = SC1[["stocks"]][["HAD"]], "gravity" = SC2[["stocks"]][["HAD"]]))
fbar(SC1[["stocks"]][["COD"]])
fbar(SC2[["stocks"]][["COD"]])
## F higher under the gravity model

s1_adv_l    <- advSum(SC1, long = FALSE, years = ac(2016:2022))             
s2_adv_l    <- advSum(SC2, long = FALSE, years = ac(2016:2022))             

adv <- rbind(cbind(sc = "base", s1_adv_l),
	     cbind(sc = "gravity", s2_adv_l))

ggplot(adv, aes(x = paste(year, sc), y = quotaUpt, fill = sc)) +
	geom_bar(stat= "identity") +
	facet_wrap(~stock) + ggtitle("quota uptake") +
	theme(axis.text.x = element_text(angle = -90))


SC1[["stocks"]][["COD"]]@landings 

plot(SC1[["stocks"]][["HAD"]])
fbar(SC1[["stocks"]][["HAD"]])
SC1[["stocks"]][["HAD"]]@landings 

plotFLBiols(SC1$biols)
plotFLFleets(SC1$fleets)

s1_bio_l    <- bioSum(SC1, long = FALSE, years = ac(2016:2022))            
s2_bio_l    <- bioSum(SC2, long = FALSE, years = ac(2016:2022))            

s_bio_l <- rbind(cbind(sc = "base", s1_bio_l),
		 cbind(sc = "gravity", s2_bio_l))  

ggplot(s_bio_l, aes(x = year, y = f)) + 
	geom_line(aes(colour = sc)) + facet_wrap(~stock) 

ggplot(s_bio_l, aes(x = year, y = landings)) + 
	geom_line(aes(colour = sc)) + facet_wrap(~stock) 


