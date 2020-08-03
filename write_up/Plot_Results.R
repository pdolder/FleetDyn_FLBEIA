#####################################
##
## Plot the summary results for each
## of the location choice models
## 
#####################################

library(tidyverse)

theme_set(theme_minimal())

###
load(file.path("..", "model", "outputs", "Results_summary.RData"))


## Effort share per time period

ggplot(filter(effort_share,year > 2016), aes(x = paste(year, season), y = q50, group = scenario)) +
	geom_line(aes(colour = scenario), size = 1) +
       geom_ribbon(aes(ymin = q05, ymax = q95, fill = scenario), alpha = 0.2) + 	
	facet_grid(scenario~metier) + 	theme_bw() +
	theme(axis.text.x = element_text(angle = -90),
	      legend.position = "top") + 
scale_x_discrete(breaks = paste(2016:2032, 1), labels = c(2016:2032)) + 
xlab("year") + ylab("Proportion Effort")
ggsave(file.path("figures", "Effort_shares.png"), height = 7, width = 12)

## Mean effort shares to show contrast among models

effort_means <- effort_share %>% group_by(scenario, metier, year) %>%
	summarise(q05 = mean(q05), q50 = mean(q50),
		  q95 = mean(q95))


ggplot(filter(effort_means,year > 2016), aes(x = year, y = q50, group = scenario)) +
	geom_line(aes(colour = scenario), size = 1) +
	geom_ribbon(aes(ymin = q05, ymax = q95, fill = scenario), alpha = 0.2) +
	facet_wrap(~metier) + 	theme_bw() +
	theme(axis.text.x = element_text(angle = -90),
	      legend.position = "top")  + xlab("year") + 
ylab("Proportion Effort")
ggsave(file.path("figures", "Effort_shares_annual.png"), height = 7, width = 7)

## Fishing mortalities
ggplot(bio, aes(x = year, y = f_q50, group = sc)) +
	geom_line(aes(colour = sc), size = 1) +
       geom_ribbon(aes(ymin = f_q05, ymax = f_q95, fill = sc), alpha = 0.2) + 	
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + ggtitle("Fishing mortality") +
expand_limits(y = 0) + ylab("Fishing mortality") + 
xlab("year") + theme(axis.text.x = element_text(angle = -90))
ggsave(file.path("figures","F_difference.png"), height = 7, width = 12)

## SSB
ggplot(filter(bio,!stock %in% c("NEP16", "NEP17", "NEP19", "NEP2021", "NEP22")), aes(x = year, y = ssb_q50, group = sc)) +
	geom_line(aes(colour = sc), size = 1) + 
	geom_ribbon(aes(ymin = ssb_q05, ymax = ssb_q95, fill = sc), alpha = 0.2) + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + expand_limits(y = 0) + 
	ggtitle("SSB differences") + xlab("year") + ylab("Spawning Stock Biomass") + 
	theme(axis.text.x = element_text(angle = -90))
ggsave(file.path("figures","SSB_difference.png"), height = 7, width = 12)

# IE Otter catches

ggplot(filter(ie_otter_summary, year > 2003), aes(x = year, y = q50)) + 
	geom_line(aes(colour = scenario)) +
	geom_ribbon(aes(fill = scenario, ymin = q05, ymax = q95), alpha = 0.2) + 
	facet_wrap(~stock) +
theme_bw() + xlab("year") + ylab("Catch (tonnes)") + 
theme(axis.text.x = element_text(angle = -90))
ggsave(file.path("figures","IE_Otter_catches.png"), height = 7, width = 12)


## Risks

ggplot(filter(risk, stock == "COD"), aes(x = year, y = Blim.risk)) + 
	geom_line(aes(colour = scenario)) + 
	facet_wrap(~stock)

ggplot(filter(risk, stock == "COD"), aes(x = year, y = Btrigger.risk)) + 
	geom_line(aes(colour = scenario)) + 
	facet_wrap(~stock)

ggplot(filter(risk, stock %in% c("COD", "HAD", "WHG")), aes(x = year, y = Fmsy.risk)) + 
	geom_line(aes(colour = scenario)) + 
	facet_wrap(~stock)
ggsave(file.path("figures", "Fmsy_risk.png"), height = 7, width = 12)


risk2 <- reshape2::melt(risk, id = c("scenario", "stock", "year"), value.name = "data")

ggplot(filter(risk2,!stock %in% c("NEP16", "NEP17", "NEP19", "NEP2021", "NEP22")), aes(x = year, y = variable)) + 
	geom_tile(aes(fill = data)) + 
	facet_grid(scenario ~ stock) + 
	scale_fill_gradient("stock risk",low = "green", high = "red") +
	theme(axis.text.x = element_text(angle = -90)) + 
	ylab("Risk type")
	
ggsave(file.path("figures", "stock_risks.png"), height = 7, width = 12)
