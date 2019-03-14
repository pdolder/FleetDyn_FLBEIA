###############################################################
##
## Clustering of Irish DTS data
##
###############################################################

## Library loading
library(tidyverse)
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
	filter(year %in% 2014:2016) %>% 
	summarise(landings = mean(landings, na.rm = T)) %>%
	ungroup %>% as.data.frame()

## quick look
head(dat[order(-dat$landings),])

## Effort
eff <- eff %>% group_by(ICES.rectangle, quarter) %>% filter(year %in% 2014:2016) %>%
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

## Loop through quarters

pdf(file=file.path("..", "plots",'TR_landings_and_effort.pdf'))


for (i in 0:4) {

if(i == 0) {
	
comp <- dat %>% group_by(ICES.rectangle, species) %>%
	summarise(landings = sum(landings)) %>% ungroup() %>%
	as.data.frame()

## Find the top 12 species
comp_spp <- comp %>% group_by(species) %>% 
	summarise(landings = sum(landings)) %>%
	ungroup() %>% as.data.frame()
comp_spp <- comp_spp[order(-comp_spp$landings),]

comp_spp <- comp_spp$species[1:12]
comp$species[!comp$species %in% comp_spp] <- "OTH"

comp <- comp %>% group_by(ICES.rectangle, species) %>%
	summarise(landings = sum(landings)) %>% ungroup() %>% as.data.frame()

q <- dat %>% group_by(ICES.rectangle, species) %>% select(-quarter) %>%
	summarise(landings = sum(landings)) %>% ungroup() %>%
	spread(species, landings) %>% as.data.frame()
q[is.na(q)] <- 0
rownames(q) <- q$ICES.rectangle
q <- q[,2:ncol(q)]

## scale the data
clust<-scale(q)

# Ward Hierarchical Clustering
d <- dist(clust, method = "euclidean") # distance matrix
fit<-hclust(d,method="ward")
plot(fit)# display dendogram

n_clust <- 11 

groups <- cutree(fit, k=n_clust) # cut tree into n clusters
# draw dendogram with red borders around the n clusters 
rect.hclust(fit, k=n_clust, border="red")

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

par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
draw.rect()
draw.barplot2D(p2$x, p2$y,p2$z, width=1, height=0.5, col=col, scale = T)
draw.shape(coast, col = "lightgreen")
box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Q", i, "Raw catch composition, scaled", sep = " "),cex=1,font=2)
legend('bottomleft', colnames(p2$z), fill=col, bty='n', 
       ncol=6,cex=1)

par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
draw.rect()
draw.barplot2D(p2$x, p2$y,p2$z, width=1, height=0.5, col=col, scale = F)
draw.shape(coast, col = "lightgreen")
box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Q", i, "Raw catch composition, unscaled", sep = " "),cex=1,font=2)
legend('bottomleft', colnames(p2$z), fill=col, bty='n', 
       ncol=6,cex=1)

par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
draw.rect()
draw.barplot2D(p$x, p$y,p$z, width=1, height=0.5, col=col)
draw.bubble(x = eff_q$lon, y = eff_q$lat, z = eff_q$Effort, maxradius = 0.2)
draw.shape(coast, col = "lightgreen")
box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Q", i, "Clustering of landings for Irish TR fleet \n
      with effort in bubbles", sep = " "),cex=1,font=2)
legend('bottomleft', colnames(p$z), fill=col, bty='n', 
       ncol=6,cex=1)

	}

if(i > 0) {

comp <- dat %>% filter(quarter == i)

## Find the top 12 species
comp_spp <- comp %>% group_by(species) %>% 
	summarise(landings = sum(landings)) %>%
	ungroup() %>% as.data.frame()
comp_spp <- comp_spp[order(-comp_spp$landings),]

comp_spp <- comp_spp$species[1:12]
comp$species[!comp$species %in% comp_spp] <- "OTH"

comp <- comp %>% group_by(ICES.rectangle, quarter, species) %>%
	summarise(landings = sum(landings)) %>% ungroup() %>% as.data.frame()

q <- dat %>% filter(quarter == i) %>% select(-quarter) %>%
	spread(species, landings)
q[is.na(q)] <- 0
rownames(q) <- q$ICES.rectangle
q <- q[,2:ncol(q)]

## scale the data
clust<-scale(q)

# Ward Hierarchical Clustering
d <- dist(clust, method = "euclidean") # distance matrix
fit<-hclust(d,method="ward")
plot(fit)# display dendogram

n_clust <- 11 

groups <- cutree(fit, k=n_clust) # cut tree into n clusters
# draw dendogram with red borders around the n clusters 
rect.hclust(fit, k=n_clust, border="red")

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

eff_q <- filter(eff, quarter == i) 

par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
draw.rect()
draw.barplot2D(p2$x, p2$y,p2$z, width=1, height=0.5, col=col, scale = T)
draw.shape(coast, col = "lightgreen")
box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Q", i, "Raw catch composition, scaled", sep = " "),cex=1,font=2)
legend('bottomleft', colnames(p2$z), fill=col, bty='n', 
       ncol=6,cex=1)

par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
draw.rect()
draw.barplot2D(p2$x, p2$y,p2$z, width=1, height=0.5, col=col, scale = F)
draw.shape(coast, col = "lightgreen")
box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Q", i, "Raw catch composition, unscaled", sep = " "),cex=1,font=2)
legend('bottomleft', colnames(p2$z), fill=col, bty='n', 
       ncol=6,cex=1)




par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(1,1,1,1))
basemap(c(we, ea), c(so, no))
draw.rect()
draw.barplot2D(p$x, p$y,p$z, width=1, height=0.5, col=col)
draw.bubble(x = eff_q$lon, y = eff_q$lat, z = eff_q$Effort, maxradius = 0.2)
draw.shape(coast, col = "lightgreen")
box(col="gray80")
axis(2,at=seq(so,no,2), col="gray80")
axis(1,at=seq(we,ea,2), col="gray80")
mtext(side=3,las=1,adj=0,line=-3,text=paste("Q", i, "Clustering of landings for Irish TR fleet \n
      with effort in bubbles", sep = " "),cex=1,font=2)
legend('bottomleft', colnames(p$z), fill=col, bty='n', 
       ncol=6,cex=1)
}
}

dev.off()


