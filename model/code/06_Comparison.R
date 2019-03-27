
# Comparison of effort shares
###############

library(FLBEIA)
library(tidyverse)
library(ggplotFL)

load(file.path("..", "outputs", "Base_Model.RData"))
load(file.path("..", "outputs", "Gravity_Model.RData"))
load(file.path("..", "outputs", "RUM_Model.RData"))

base    <- SC1
gravity <- SC2
rum     <- SC3

rm(SC1, SC2, SC3)

maxyr <- range(base[["stocks"]][[1]])[["maxyear"]]

################################
### Compare the 'base' model and
### Gravity model
################################

## Total effort in each scenario
df <- rbind(cbind(scenario = "base", as.data.frame(base$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "gravity", as.data.frame(gravity$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "rum", as.data.frame(rum$fleets[["IE_Otter"]]@effort))

)
df <- df[df$year > 2014,]


ggplot(df, aes(x = year, y = data)) + geom_line(aes(colour = scenario), size = 1.5) +
	facet_wrap(~season, scale = "free") + ggtitle("Effort by quarter")
## in multi-stock example, total effort quite different by season 

## Effort shares

df2 <- rbind(cbind(scenario = "base", do.call(rbind, lapply(base$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))),
	     cbind(scenario = "gravity", do.call(rbind, lapply(gravity$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))),
	     cbind(scenario = "rum", do.call(rbind, lapply(rum$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare)))))
	     )
df2 <- filter(df2, year > 2014)

ggplot(df2, aes(x = paste(year, season), y = data, group = scenario)) +
#	geom_point(aes(colour = scenario), size = 1.5) +
	geom_line(aes(colour = scenario), size = 1.5) + 
#	geom_bar(stat = "identity", aes(fill = season)) +
	facet_wrap(~metier) + 	theme_bw() +
	theme(axis.text.x = element_text(angle = -90),
	      legend.position = "top") 
ggsave(file.path("..", "plots", "Effort_shares.png"), height = 7, width = 12)

S <- "COD"
plot(FLStocks(base = base[["stocks"]][[S]][,ac(2015:maxyr)], 
	      gravity = gravity[["stocks"]][[S]][,ac(2015:maxyr)],
	      rum = rum[["stocks"]][[S]][,ac(2015:maxyr)])) + 
	    theme(legend.position = "top") + ggtitle(paste("Comparison", S))

bio <- rbind(
      cbind(sc = "base", bioSum(base, long = FALSE, years = ac(2016:maxyr+2))),
cbind(sc = "gravity", bioSum(gravity, long = FALSE, years = ac(2016:maxyr+2))),
cbind(sc = "rum", bioSum(rum, long = FALSE, years = ac(2016:maxyr+2)))
)

ggplot(bio, aes(x = year, y = f)) +
	geom_line(aes(colour = sc), size = 1.5) + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + ggtitle("Fishing mortality") +
expand_limits(y = 0)
ggsave(file.path("..", "plots","F_difference.png"), height = 7, width = 12)

ggplot(bio, aes(x = year, y = landings)) +
	geom_line(aes(colour = sc), size = 1.5) + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + expand_limits(y = 0) + 
	ggtitle("Catch differences")
ggsave(file.path("..", "plots","C_difference.png"), height = 7, width = 12)

ggplot(bio, aes(x = year, y = ssb)) +
	geom_line(aes(colour = sc), size = 1.5) + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + expand_limits(y = 0) + 
	ggtitle("SSB differences")
ggsave(file.path("..", "plots","SSB_difference.png"), height = 7, width = 12)


advice <- rbind(
      cbind(sc = "base", advSum(base, long = FALSE, years = ac(2016:maxyr+2))),
      cbind(sc = "gravity", advSum(gravity, long = FALSE, years = ac(2016:maxyr+2))),
      cbind(sc = "rum", advSum(rum, long = FALSE, years = ac(2016:maxyr+2)))
      )

ggplot(advice, aes(x = paste(year, sc), y = quotaUpt, fill = sc)) +
	geom_bar(stat= "identity") +
	facet_wrap(~stock) + ggtitle("quota uptake") +
	theme(axis.text.x = element_text(angle = -90)) +
	ggtitle("Quota Uptake")
ggsave(file.path("..", "plots","Uptake_differences.png"), height = 7, width = 12)

