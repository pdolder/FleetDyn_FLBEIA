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
	geom_ribbon(aes(ymin = q05, ymax = q95, fill = scenario), alpha = 0.3) +
	geom_line(aes(colour = scenario), size = 0.5) +
       geom_vline(aes(xintercept = "2018 1"), colour = "grey") +
       geom_vline(aes(xintercept = "2021 1"), linetype = "dashed", colour = "grey") + 
	facet_grid(scenario~metier) + 	theme_bw() +
	theme(axis.text.x = element_text(angle = -90),
	      legend.position = "none", panel.grid.major = element_blank(),
	      panel.grid.minor = element_blank()) + 
scale_x_discrete(breaks = paste(seq(2016,2032,2), 1), labels = seq(2016,2032,2)) + 
xlab("year") + ylab("Proportion Effort")
ggsave(file.path("figures", "Effort_shares.png"), height = 7, width = 12)

## Mean effort shares to show contrast among models

effort_means <- effort_share %>% group_by(scenario, metier, year) %>%
	summarise(q05 = mean(q05), q50 = mean(q50),
		  q95 = mean(q95))


ggplot(filter(effort_means,year > 2016), aes(x = year, y = q50, group = scenario)) +
	geom_ribbon(aes(ymin = q05, ymax = q95, fill = scenario), alpha = 0.2) +
	geom_line(aes(colour = scenario), size = 1) +
	geom_vline(aes(xintercept = 2018), colour = "grey") +
       geom_vline(aes(xintercept = 2021), linetype = "dashed", colour = "grey") + 
	facet_wrap(~metier) + 	theme_bw() +
	theme(axis.text.x = element_text(angle = -90),, panel.grid.major = element_blank(),
	      panel.grid.minor = element_blank(),
	      legend.position = "top")  + xlab("year") + 
ylab("Proportion Effort")
ggsave(file.path("figures", "Effort_shares_annual.png"), height = 7, width = 7)

## Difference in effort following closures
eff_diff1 <- filter(effort_means, year == 2020)
eff_diff2 <- filter(effort_means, year == 2021)

eff_diff1$Cq05 <- eff_diff2$q05[match(paste(eff_diff1$scenario, eff_diff1$metier),
					 paste(eff_diff2$scenario, eff_diff2$metier))]

eff_diff1$Cq50 <- eff_diff2$q50[match(paste(eff_diff1$scenario, eff_diff1$metier),
					 paste(eff_diff2$scenario, eff_diff2$metier))]

eff_diff1$Cq95 <- eff_diff2$q95[match(paste(eff_diff1$scenario, eff_diff1$metier),
					 paste(eff_diff2$scenario, eff_diff2$metier))]

eff_diff <- eff_diff1; rm(eff_diff1, eff_diff2)

eff_diff$Cmid <- (eff_diff$Cq50 - eff_diff$q50) / eff_diff$q50 * 100
eff_diff$Clo  <- (eff_diff$Cq05 - eff_diff$q05) / eff_diff$q05 * 100
eff_diff$Cup  <- (eff_diff$Cq95 - eff_diff$q95) / eff_diff$q95 * 100

ggplot(eff_diff, aes(x = metier, y = Cmid)) +
	geom_bar(stat = "identity", aes(fill = metier)) + 
	facet_wrap(~ scenario) + ylab("Median % change in effort") + 
	xlab("métier") + theme_bw() +
	theme(axis.text.x = element_text(angle = -90),, panel.grid.major = element_blank(),
	      panel.grid.minor = element_blank(),
	      legend.position = "none")  
	ggsave(file.path("figures", "Change_effort.png"), height = 7, width = 7)


## Difference in effort before the closures
eff_diff1 <- filter(effort_means, year == 2018)
eff_diff2 <- filter(effort_means, year == 2020)

eff_diff1$Cq05 <- eff_diff2$q05[match(paste(eff_diff1$scenario, eff_diff1$metier),
					 paste(eff_diff2$scenario, eff_diff2$metier))]

eff_diff1$Cq50 <- eff_diff2$q50[match(paste(eff_diff1$scenario, eff_diff1$metier),
					 paste(eff_diff2$scenario, eff_diff2$metier))]

eff_diff1$Cq95 <- eff_diff2$q95[match(paste(eff_diff1$scenario, eff_diff1$metier),
					 paste(eff_diff2$scenario, eff_diff2$metier))]

eff_diff <- eff_diff1; rm(eff_diff1, eff_diff2)

eff_diff$Cmid <- (eff_diff$Cq50 - eff_diff$q50) / eff_diff$q50 * 100
eff_diff$Clo  <- (eff_diff$Cq05 - eff_diff$q05) / eff_diff$q05 * 100
eff_diff$Cup  <- (eff_diff$Cq95 - eff_diff$q95) / eff_diff$q95 * 100

ggplot(eff_diff, aes(x = metier, y = Cmid)) +
	geom_bar(stat = "identity", aes(fill = metier)) + 
	facet_wrap(~ scenario) + ylab("Median % change in effort") + 
	xlab("métier") + theme_bw() +
	theme(axis.text.x = element_text(angle = -90),, panel.grid.major = element_blank(),
	      panel.grid.minor = element_blank(),
	      legend.position = "none")  
	ggsave(file.path("figures", "Change_effort_before.png"), height = 7, width = 7)






## Add reference points
load(file.path("..", "model", "model_inputs", "advice_ctrl.RData"))

Blim     <- sapply(advice.ctrl, function(x) x$ref.pts["Blim",1])
Btrigger <- sapply(advice.ctrl, function(x) x$ref.pts["Btrigger",1])

Fmsy     <- sapply(advice.ctrl, function(x) x$ref.pts["Fmsy",1])

Blim     <- as.data.frame(Blim)
Btrigger <- as.data.frame(Btrigger)
Fmsy     <- as.data.frame(Fmsy) 

bio$Blim     <- Blim$Blim[match(bio$stock, rownames(Blim))]
bio$Btrigger <- Btrigger$Btrigger[match(bio$stock, rownames(Btrigger))]
bio$Fmsy     <- Fmsy$Fmsy[match(bio$stock, rownames(Fmsy))]

## Rename MON, NHKE and NMEG for consistency

levels(bio$stock)[levels(bio$stock) == "MON"] <-  "ANF"
levels(bio$stock)[levels(bio$stock) == "NHKE"] <- "HKE"
levels(bio$stock)[levels(bio$stock) == "NMEG"] <- "LEZ"

colnames(bio)[1:2] <- c("scenario", "sc")

## Fishing mortalities
ggplot(bio, aes(x = year, y = f_q50, group = scenario)) +
       geom_ribbon(aes(ymin = f_q05, ymax = f_q95, fill = scenario), alpha = 0.2) + 
geom_line(aes(colour = scenario), size = 1) +
       geom_hline(aes(yintercept = Fmsy), linetype = "dashed", colour = "red") +   
       geom_vline(aes(xintercept = 2018), colour = "grey") +
       geom_vline(aes(xintercept = 2021), linetype = "dashed", colour = "grey") + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw()  +
expand_limits(y = 0) + ylab("Fishing mortality") + 
xlab("year") + theme(axis.text.x = element_text(angle = -90),
		     panel.grid.major = element_blank(),
	      panel.grid.minor = element_blank(), legend.position = "top")
ggsave(file.path("figures","F_difference.png"), height = 7, width = 12)

## SSB
ggplot(filter(bio,!stock %in% c("NEP16", "NEP17", "NEP19", "NEP2021", "NEP22")), aes(x = year, y = ssb_q50, group = scenario)) +
	geom_ribbon(aes(ymin = ssb_q05, ymax = ssb_q95, fill = scenario), alpha = 0.2) +
	geom_line(aes(colour = scenario), size = 1) + 
	geom_hline(aes(yintercept = Blim), linetype = "dotdash", colour = "blue") + 
	geom_hline(aes(yintercept = Btrigger), linetype = "dashed", colour = "blue") +  
	geom_vline(aes(xintercept = 2018), colour = "grey") +
       geom_vline(aes(xintercept = 2021), linetype = "dashed", colour = "grey") + 
	facet_wrap(~stock, scale = "free_y") + 
	theme_bw() + expand_limits(y = 0) + xlab("year") + ylab("Spawning Stock Biomass (tonnes)") + 
	theme(axis.text.x = element_text(angle = -90),
		     panel.grid.major = element_blank(),
	      panel.grid.minor = element_blank(), legend.position = "top")
ggsave(file.path("figures","SSB_difference.png"), height = 7, width = 12)

# IE Otter catches

ggplot(filter(ie_otter_summary, year > 2003), aes(x = year, y = q50)) + 
	geom_ribbon(aes(fill = scenario, ymin = q05, ymax = q95), alpha = 0.2) + 
	geom_line(aes(colour = scenario)) +
	geom_vline(aes(xintercept = 2018), colour = "grey") +
        geom_vline(aes(xintercept = 2021), linetype = "dashed", colour = "grey") + 
	facet_wrap(~stock) +
theme_bw() + xlab("year") + ylab("Catch (tonnes)") + 
theme(axis.text.x = element_text(angle = -90),
		     panel.grid.major = element_blank(),
	      panel.grid.minor = element_blank(), legend.position = "top")
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
	facet_wrap(~stock) + 
ggsave(file.path("figures", "Fmsy_risk.png"), height = 7, width = 12)


risk2 <- reshape2::melt(risk, id = c("scenario", "stock", "year"), value.name = "data")


## Order the plots so Fmsy.risk is first
risk2$variable <- factor(risk2$variable, c("Fmsy.risk", "Blim.risk", "Btrigger.risk")) 
levels(risk2$variable)[levels(risk2$variable) == "Btrigger.risk"] <- "Bmsytrigger.risk"
levels(risk2$stock)[levels(risk2$stock) == "NMEG"] <- "LEZ"


ggplot(filter(risk2,!stock %in% c("NEP16", "NEP17", "NEP19", "NEP2021", "NEP22", "MON", "NHKE","WHG")), 
       aes(x = year, y = scenario)) + 
	geom_tile(aes(fill = data)) + 	
	geom_vline(aes(xintercept = 2018), colour = "grey") +
        geom_vline(aes(xintercept = 2021), linetype = "dashed", colour = "grey") + 
	facet_grid(variable ~ stock) + 
	scale_fill_gradient("stock risk",low = "#2dc937", high = "#cc3232") +
	theme(axis.text.x = element_text(angle = -90), legend.position = "top") + 
	ylab("Risk type")
	
ggsave(file.path("figures", "stock_risks.png"), height = 7, width = 12)
