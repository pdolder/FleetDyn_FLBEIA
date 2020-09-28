###############################################################
##
## Clustering of Irish DTS data
## to define Metier for FLBEIA model
###############################################################

## Library loading
library(tidyverse)
library(fpc)
library(PBSmapping)
library(vmstools)
library(mapplots)
library(RColorBrewer)
library(cowplot)

## Read in stat rect landings data
dat <- read.csv(file.path("..", "data", "landings_data.csv"))
eff <- read.csv(file.path("..", "data", "effort_(hours_fished)_data.csv"))

dat.bk <- dat
eff.bk <- eff

### Format data
## Landings
dat <- dat %>% group_by(ICES.rectangle, quarter, species) %>% 
	filter(year %in% 2014:2016, vessel.length %in% c("O15M","O10T15M"),
	      species %in% c("COD", "HAD", "WHG", "HKE",
					 "ANF", "LEZ", "NEP")) %>% 
	summarise(landings = mean(landings, na.rm = T)) %>%
	ungroup %>% as.data.frame()

## quick look
head(dat[order(-dat$landings),])

## Effort
eff <- eff %>% group_by(ICES.rectangle, quarter) %>% filter(year %in% 2014:2016, 
				vessel.length %in% c("O15M","O10T15M")) %>%
	summarise(Effort = mean(Effective.Effort, na.rm = T)) %>% ungroup() %>%
	as.data.frame()
head(eff)

## Add lat lons to effort
ICESr<-ICESrectangle2LonLat(eff$ICES.rectangle,midpoint=T)
eff$lat<-ICESr$SI_LATI
eff$lon<-ICESr$SI_LONG
rm(ICESr)


## Plot details
we<-c(-16); ea<-c(-5); so<-c(48); no<-c(55) # North Sea
data(coast)

###########################################################
## Let's keep the proportions annual for now...
## And we'll do the clustering based just on the spp
## being modelled
###########################################################

## The clustering
head(eff)

comp <- dat %>% group_by(ICES.rectangle, species) %>% 
	summarise(landings = sum(landings)) %>% ungroup() %>%
	as.data.frame()

q <- dat %>% group_by(ICES.rectangle, species) %>% 
	summarise(landings = sum(landings)) %>% ungroup() %>%
	spread(species, landings) %>% as.data.frame()
q[is.na(q)] <- 0
rownames(q) <- q$ICES.rectangle
q <- q[,2:ncol(q)]

## scale the data
clust<-scale(q)

# Ward Hierarchical Clustering
d <- dist(clust, method = "euclidean") # distance matrix
fit<-hclust(d,method="ward.D2")
plot(fit)# display dendogram

n_clust <- 6

pam_fit <- pamk(clust, n_clust)

# draw dendogram with red borders around the n clusters 
rect.hclust(fit, k=n_clust, border="red")

#groups <- pam_fit$pamobject$clustering 
groups <- cutree(fit, k=n_clust) # cut tree into n clusters

## plot the groups
df  <-data.frame(rect=names(groups),area=as.data.frame(groups))

# add lats and lons to data
ICESr<-ICESrectangle2LonLat(df$rect,midpoint=T)
ICESr2<-ICESrectangle2LonLat(comp$ICES.rectangle,midpoint=T)
df$lat<-ICESr$SI_LATI
df$lon<-ICESr$SI_LONG
comp$lat<-ICESr2$SI_LATI
comp$lon<-ICESr2$SI_LONG

rm(ICESr, ICESr2)

## make into xyz data
p<-make.xyz(df$lon,df$lat,df$groups,as.factor(df$groups))
col<- brewer.pal(n_clust, "Set3")

p2<-make.xyz(comp$lon,comp$lat,comp$landings,as.factor(comp$species))
#col2<-rainbow(length(unique(comp$species)))
col2<-brewer.pal(12, "Set3")

eff_q <- eff %>% group_by(ICES.rectangle, lat, lon) %>%
	summarise(Effort = sum(Effort)) %>% as.data.frame()

png(file.path("..", "plots", "CatchComp_scaled.png"))
par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
draw.rect()
draw.barplot2D(p2$x, p2$y,p2$z, width=1, height=0.5, col=col, scale = T)
draw.shape(coast, col = "lightgreen")
box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Raw catch composition, scaled", sep = " "),cex=1,font=2)
legend('bottomleft', colnames(p2$z), fill=col, bty='n', 
       ncol=2,cex=1)
dev.off()

png(file.path("..", "plots", "CatchComp_unscaled.png"))
par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
draw.rect()
draw.barplot2D(p2$x, p2$y,p2$z, width=1, height=0.5, col=col, scale = F)
draw.shape(coast, col = "lightgreen")
box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Raw catch composition, unscaled", sep = " "),cex=1,font=2)
legend('bottomleft', colnames(p2$z), fill=col, bty='n', 
       ncol=2,cex=1)
dev.off()

png(file.path("..", "plots", "Cluster_Effort.png"))
par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
draw.rect()
draw.barplot2D(p$x, p$y,p$z, width=1, height=0.5, col=col)
draw.bubble(x = eff_q$lon, y = eff_q$lat, z = eff_q$Effort, maxradius = 0.2)
draw.shape(coast, col = "lightgreen")
box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Clustering of landings for Irish TR fleet > 10m \n
      with effort (hrs) proportionate to bubble size", sep = " "),cex=1,font=2)
legend('bottomleft', colnames(p$z), fill=col, bty='n', 
       ncol=2,cex=1)
dev.off()

## Draw areas with labels

png(file.path("..", "plots", "Metier_locations.png"))
par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
#draw.rect()
draw.barplot2D(p$x, p$y,p$z, width=1, height=0.5, col=col, 
	       lwd.frame = 0, col.frame = "grey10")
draw.shape(coast, col = "lightgreen")
#box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Defined Metier/Areas", sep = " "),cex=1,font=2)
legend('bottomleft', LETTERS[1:n_clust], fill=col, bty='n', 
       ncol=2,cex=1)
dev.off()

## We want to redefine some of the clusters
## As they are separate Nephrops FUs
nep <- read.delim(file.path("..", "data", "rect_FU_MA_UPDATED_2017.txt"))

df[df$group == 2 & df$rect %in% nep$Rectangle[nep$FU == 16],"groups"]  <- 2
df[df$rect %in% c("32D9","32E0"),"groups"]  <- 4
df[df$rect %in% c("32E1"),"groups"]  <- 5
df[df$rect %in% c("30E2"),"groups"]  <- 5
df[df$rect %in% c("33D7"),"groups"]  <- 2

df[df$group == 2 & df$lat > 53,"groups"]  <- 1
df[df$group == 2 & df$lat < 50,"groups"]  <- 1

## Seperate Nephrops FU17
df[df$rect %in% nep$Rectangle[nep$FU == 17],"groups"]  <- 7

##############################
## Associate clusters with 
## catch and effort data
##############################

## First relabel the nephrops catches
nep <- read.delim(file.path("..", "data", "rect_FU_MA_UPDATED_2017.txt"))

dat.nep  <- filter(dat.bk, species == "NEP")
dat.dem  <- filter(dat.bk, species != "NEP")

dat.nep$FU      <- nep$FU[match(dat.nep$ICES.rectangle, nep$Rectangle)]
dat.nep$FU      <- gsub(" ", "", dat.nep$FU)
dat.nep$FU      <- gsub("-", "", dat.nep$FU)
dat.nep$species <- paste("NEP", dat.nep$FU, sep = "") 
dat.nep <- dat.nep %>% select(-FU)

dat.comb <- rbind(dat.nep, dat.dem) 

nepfus <- grep("NEP", unique(dat.comb$species), value = T)

catch <- dat.comb %>% filter(vessel.length %in% c("O15M", "O10T15M"),
			     species %in% c("ANF", "COD", "HAD", "WHG",
					    "HKE","LEZ", nepfus))  %>%
	group_by(ICES.rectangle, year, quarter, species) %>%
	summarise(landings = sum(landings)) %>% ungroup() %>%
	as.data.frame()

catch$group <- df$groups[match(catch$ICES.rectangle,
			       df$rect)]

catch <- catch %>% group_by(group, year, quarter, species) %>% 
	summarise(landings = sum(landings)) %>% as.data.frame()

## Look at catch comp by group in last year

ggplot(filter(catch, year ==2016), aes(x = factor(group), y = landings)) +
	geom_bar(aes(fill = species),stat = "identity") +
	facet_wrap(~quarter, ncol = 1)# + scale_fill_manual(values = col)

## And the effort

effort <- eff.bk %>% filter(vessel.length %in% c("O10M15M", "O15M")) %>%
	group_by(ICES.rectangle, year, quarter) %>% 
	summarise(Effort = sum(Effective.Effort)) %>% as.data.frame()

effort$group <- df$groups[match(effort$ICES.rectangle,
			     df$rect)]

effort <- effort[!is.na(effort$group),]  ## some effort where no catches

effort <- effort %>% group_by(group, year, quarter) %>%
	summarise(Effort = sum(Effort)) %>% as.data.frame()

ggplot(filter(effort, year == 2016), aes(x = factor(group), y = Effort)) +
	geom_bar(aes(fill = factor(quarter)), stat = "identity") +
	scale_fill_manual(values = col)


##################################
## Now turn these into proportions
##################################

## sum across groups
eff_all <- effort %>% group_by(year, quarter) %>%
	summarise(Effort = sum(Effort)) %>% as.data.frame() 

effort$total <- eff_all$Effort[match(paste(effort$year,effort$quarter),
				     paste(eff_all$year,eff_all$quarter))]

effort$effshare <- effort$Effort / effort$total

## And the catches

catch_all <- catch %>% group_by(year, species) %>%
	summarise(landings = sum(landings)) %>% as.data.frame()

catch$total <- catch_all$landings[match(paste(catch$year, catch$species),
					paste(catch_all$year, catch_all$species))]

catch$catchshare <- catch$landings / catch$total

catch <- catch[!is.na(catch$group),]


## Look at the catch compositions

catch %>% group_by(group, species) %>% summarise(landings = mean(landings)) %>%
	reshape2::dcast(group ~ species, value.var = "landings")


save(catch, effort, file = file.path("..", "data", "Processed_catch.RData"))


##########################
## Figure
##########################

n_clust <- length(unique(df$group)) 

p<-make.xyz(df$lon,df$lat,df$groups,as.factor(df$groups))
col<- brewer.pal(n_clust, "Set3")

size <- 0.5

png(file.path("..", "plots", "Final_Metier_locations.png"), height = 1500, width = 1200, res = 400)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(3,3,1,1))
basemap(c(we, ea), c(so, no), bg = "grey95", cex.axis = size, mgp = c(1,0.3,0), cex.lab = size)
#draw.rect()
draw.barplot2D(p$x, p$y,p$z, width=1, height=0.5, col=col, 
	       lwd.frame = 0, col.frame = "grey90")
draw.bubble(x = eff_q$lon, y = eff_q$lat, z = eff_q$Effort, maxradius = 0.2)
draw.shape(coast, col = "white")
#axis(2,at=seq(so,no,2), col="gray80", cex.axis= 0.1) 
#axis(1,at=seq(we,ea,2), col="gray80", cex.axis = 0.2)
#mtext(side=3,las=1,adj=0, line=-2,text=paste("  Defined Métier", sep = " "),cex=size,font=2)
legend('bottomleft', LETTERS[1:n_clust], fill=col, bty='n', 
       ncol=3,cex= 0.7, title = "Métier")
text(-18,56.5, label = "A", xpd = NA)
dev.off()

## Catch comp

catch_avg <- catch %>% group_by(group, species) %>% summarise(landings = sum(landings))

grp_sum <- catch_avg %>% group_by(group) %>% summarise(landings = sum(landings))

catch_avg$grpL  <- grp_sum$landings[match(catch_avg$group, grp_sum$group)]
catch_avg$share <- catch_avg$landings / catch_avg$grpL

catch_avg <- reshape2::dcast(catch_avg, species ~ group, value.var = "share")

spp <- catch_avg$species

catch_avg <- as.matrix(catch_avg[,2:8])
colnames(catch_avg) <- LETTERS[1:7]
rownames(catch_avg) <- spp 
catch_avg[is.na(catch_avg)] <- 0

library(RColorBrewer)
cols <- c(brewer.pal(13, "Set3"),"white")


png(file.path("..", "plots", "Final_Metier_catchcomp.png"), 
    width = 2600, height = 1600, res = 400)

par(mar=c(2,3,7,2))
barplot(catch_avg, col = cols, mgp = c(1,0.3,0), ylab = "Proportion of landings by weight",
	xlab = "Métier") 
par(xpd = TRUE)
legend(0.1,1.4, legend = spp, fill = cols, cex = 0.7, ncol = 5, pt.cex = 2)
text(-0.5, 1.5, labels= "B", xpd = NA)
dev.off()


