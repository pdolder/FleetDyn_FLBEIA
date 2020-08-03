
# Comparison of effort shares
###############

library(FLBEIA)
library(tidyverse)
library(ggplotFL)

load(file.path("..", "outputs", "Base_Model.RData"))
load(file.path("..", "outputs", "Gravity_Model.RData"))
load(file.path("..", "outputs", "Gravity_Model_Trad.RData"))
load(file.path("..", "outputs", "RUM_Model.RData"))
load(file.path("..", "outputs", "Markov_Model.RData"))

base          <- SC1
gravity       <- SC2
rum           <- SC3
markov        <- SC4
gravity_trad  <- SC5 
rm(SC1, SC2, SC3, SC4, SC5)

maxyr <- range(base[["stocks"]][[1]])[["maxyear"]]

################################
### Compare the 'base' model and
### Gravity model
################################

## Total effort in each scenario
df <- rbind(cbind(scenario = "base", as.data.frame(base$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "gravity", as.data.frame(gravity$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "rum", as.data.frame(rum$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "markov", as.data.frame(markov$fleets[["IE_Otter"]]@effort)),
      cbind(scenario = "gravity_trad", as.data.frame(gravity_trad$fleets[["IE_Otter"]]@effort))
)

df <- df %>% group_by(scenario, year, season) %>% summarise(val = mean(data, na.rm = T), 
							    lo = quantile(data, 0.05,na.rm = T),
							    high = quantile(data, 0.95, na.rm = T))

df <- df[df$year > 2014,]

theme_set(theme_minimal())

ggplot(df, aes(x = year, y = val)) + geom_line(aes(colour = scenario), size = 1.5) +
	geom_ribbon(aes(ymin = lo, ymax = high, fill = scenario), alpha = 0.1) + 
	facet_wrap(~season, scale = "free") + ggtitle("Effort by quarter") 
ggsave(file.path("..", "plots", "Effort_by_quarter.png"))


## Effort shares

df2 <- rbind(cbind(scenario = "base", do.call(rbind, lapply(base$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))),
	     cbind(scenario = "gravity", do.call(rbind, lapply(gravity$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))),
	     cbind(scenario = "rum", do.call(rbind, lapply(rum$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))),
	     cbind(scenario = "markov", do.call(rbind, lapply(markov$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare))))), 
	     cbind(scenario = "gravity_trad", do.call(rbind, lapply(gravity_trad$fleets[["IE_Otter"]]@metiers, function(x) cbind(metier = x@name, as.data.frame(x@effshare)))))
	     )

df2  <- df2 %>% group_by(scenario, metier, year, season) %>%
	summarise(val = mean(data, na.rm = T), lo = quantile(data, 0.05, na.rm = T),
		  high = quantile(data, 0.95, na.rm = T))


df2 <- filter(df2, year > 2014)

ggplot(df2, aes(x = paste(year, season), y = val, group = scenario)) +
	geom_line(aes(colour = scenario), size = 1.5) +
       geom_ribbon(aes(ymin = lo, ymax = high, fill = scenario), alpha = 0.5) + 	
	facet_grid(scenario~metier) + 	theme_bw() +
	theme(axis.text.x = element_text(angle = -90),
	      legend.position = "top") 
ggsave(file.path("..", "plots", "Effort_shares.png"), height = 7, width = 12)

## effort share annualised

ggplot(filter(df3, scenario %in% c("base","gravity","gravity_trad","rum", "markov")), aes(x = year, season, y = data, group = scenario)) +
  #	geom_point(aes(colour = scenario), size = 1.5) +
  geom_line(aes(colour = scenario), size = 1.5) + 
  #	geom_bar(stat = "identity", aes(fill = season)) +
  facet_wrap(~metier) + 	theme_bw() +
  theme(axis.text.x = element_text(angle = -90),
        legend.position = "top") 
ggsave("effort_shares_annual.png")



for(s in unique(df2$scenario)) {
ggplot(filter(df2, scenario == s), aes(x = year, y = val, group = season)) +
	geom_line(aes(colour = season), size = 1.5) + 
	facet_wrap(~metier) + 	theme_bw() +
	theme(axis.text.x = element_text(angle = -90),
	      legend.position = "top") 
ggsave(file.path("..", "plots", paste0("Effort_shares_", s,".png")), height = 7, width = 12)
}



S <- "COD"
plot(FLStocks(base = base[["stocks"]][[S]][,ac(2015:maxyr)], 
	      gravity = gravity[["stocks"]][[S]][,ac(2015:maxyr)],
	      rum = rum[["stocks"]][[S]][,ac(2015:maxyr)],
     	      markov = markov[["stocks"]][[S]][,ac(2015:maxyr)],
	      gravity_trad = gravity_trad[["stocks"]][[S]][,ac(2015:maxyr)])
     ) + theme(legend.position = "top") + ggtitle(paste("Comparison", S))
ggsave(file.path("..", "plots", "Cod_by_scenario.png"))

S <- "WHG"
plot(FLStocks(base = base[["stocks"]][[S]][,ac(2015:maxyr)], 
	      gravity = gravity[["stocks"]][[S]][,ac(2015:maxyr)],
	      rum = rum[["stocks"]][[S]][,ac(2015:maxyr)],
     	      markov = markov[["stocks"]][[S]][,ac(2015:maxyr)],
	      gravity_trad = gravity_trad[["stocks"]][[S]][,ac(2015:maxyr)])
     ) + theme(legend.position = "top") + ggtitle(paste("Comparison", S))
ggsave(file.path("..", "plots", "Whg_by_scenario.png"))

bio <- rbind(
      cbind(sc = "base", bioSumQ(as.data.frame(bioSum(base, long = FALSE, years = ac(2016:maxyr+2))))),
cbind(sc = "gravity", bioSumQ(as.data.frame(bioSum(gravity, long = FALSE, years = ac(2016:maxyr+2))))),
cbind(sc = "rum", bioSumQ(as.data.frame(bioSum(rum, long = FALSE, years = ac(2016:maxyr+2))))),
cbind(sc = "markov", bioSumQ(as.data.frame(bioSum(markov, long = FALSE, years = ac(2016:maxyr+2))))),
cbind(sc = "gravity_trad", bioSumQ(as.data.frame(bioSum(gravity_trad, long = FALSE, years = ac(2016:maxyr+2)))))
)

ggplot(bio, aes(x = year, y = f_q50, group = sc)) +
	geom_line(aes(colour = sc), size = 1.5) +
       geom_ribbon(aes(ymin = f_q05, ymax = f_q95, fill = sc), alpha = 0.2) + 	
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + ggtitle("Fishing mortality") +
expand_limits(y = 0)
ggsave(file.path("..", "plots","F_difference.png"), height = 7, width = 12)

ggplot(filter(bio, sc %in% c("base", "gravity", "gravity_trad", "rum", "markov")), 
       aes(x = year, y = f_q50, group = sc)) +
  geom_line(aes(colour = sc), size = 1.5) + 
  geom_ribbon(aes(ymin = f_q05, ymax = f_q95, fill = sc), alpha = 0.2) + 
  facet_wrap(~stock, scale = "free_y") + 
  theme_bw() + ggtitle("Fishing mortality") +
  expand_limits(y = 0)
ggsave(file.path("..", "plots","F_difference_lim.png"), height = 7, width = 12)


ggplot(bio, aes(x = year, y = catch_q50, group = sc)) +
	geom_line(aes(colour = sc), size = 1.5) + 
	geom_ribbon(aes(ymin = catch_q05, ymax = catch_q95, fill = sc), alpha = 0.2) + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + expand_limits(y = 0) + 
	ggtitle("Catch differences")
ggsave(file.path("..", "plots","C_difference.png"), height = 7, width = 12)

ggplot(bio, aes(x = year, y = ssb_q50, group = sc)) +
	geom_line(aes(colour = sc), size = 1.5) + 
	geom_ribbon(aes(ymin = ssb_q05, ymax = ssb_q95, fill = sc), alpha = 0.2) + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + expand_limits(y = 0) + 
	ggtitle("SSB differences")
ggsave(file.path("..", "plots","SSB_difference.png"), height = 7, width = 12)

ggplot(bio, aes(x = year, y = biomass_q50, group = sc)) +
	geom_line(aes(colour = sc), size = 1.5) + 
	geom_ribbon(aes(ymin = biomass_q05, ymax = biomass_q95, fill = sc), alpha = 0.2) + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + expand_limits(y = 0) + 
	ggtitle("biomass differences")
ggsave(file.path("..", "plots","biomass_difference.png"), height = 7, width = 12)

ggplot(bio, aes(x = year, y = rec_q50, group = sc)) +
	geom_line(aes(colour = sc), size = 1.5) + 
	geom_ribbon(aes(ymin = rec_q05, ymax = rec_q95, fill = sc), alpha = 0.2) + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + expand_limits(y = 0) + 
	ggtitle("Recruitment differences")
ggsave(file.path("..", "plots","recruitment_difference.png"), height = 7, width = 12)

## Catch by IE_Otter fleet....

scenarios <- c("base","gravity", "gravity_trad", "rum", "markov")

ie_otter <- lapply(scenarios, function(s) {
 sc    <- lapply(get(s)[["fleets"]][["IE_Otter"]]@metiers, function(x) {
       catch <- lapply(x@catches, function(x2) cbind(stock = x2@name,as.data.frame(x2@landings + x2@discards)))
       catch_all <- cbind(metier = x@name, bind_rows(catch))
       return(catch_all)
       })
 return(cbind(scenario = s, bind_rows(sc)))
       })

ie_otter <- bind_rows(ie_otter)

ie_otter_summary <- ie_otter %>% group_by(scenario, stock, year, iter) %>% 
	summarise(data = sum(data, na.rm = T)) %>%
	group_by(scenario, stock, year) %>%
	summarise(q05 = quantile(data, 0.05, na.rm = T),
		  q50 = quantile(data, 0.5, na.rm = T),
		  q95 = quantile(data, 0.95, na.rm = T))

ggplot(filter(ie_otter_summary, year > 2010), aes(x = year, y = q50)) +
       geom_line(aes(colour = scenario)) + facet_wrap(~stock) +
       geom_ribbon(aes(ymin = q05, ymax = q95, fill = scenario), alpha = 0.3) + 
theme_bw()       
ggsave(file.path("..", "plots", "IE_Otter_catches.png"))



advice <- rbind(
      cbind(sc = "base", as.data.frame(advSum(base, long = FALSE, years = ac(2016:maxyr+2)))),
      cbind(sc = "gravity", as.data.frame(advSum(gravity, long = FALSE, years = ac(2016:maxyr+2)))),
      cbind(sc = "rum", as.data.frame(advSum(rum, long = FALSE, years = ac(2016:maxyr+2)))),
      cbind(sc = "markov", as.data.frame(advSum(markov, long = FALSE, years = ac(2016:maxyr+2)))),
      cbind(sc = "gravity_trad", as.data.frame(advSum(gravity_trad, long = FALSE, years = ac(2016:maxyr+2))))
      )

ggplot(advice, aes(x = paste(year, sc), y = quotaUpt, fill = sc, group = sc)) +
	geom_bar(stat= "identity") +
	facet_wrap(~stock) + ggtitle("quota uptake") +
	theme(axis.text.x = element_text(angle = -90)) +
	ggtitle("Quota Uptake")
ggsave(file.path("..", "plots","Uptake_differences.png"), height = 7, width = 12)

