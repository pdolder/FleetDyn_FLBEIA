###################################################
## Testing basic model
###################################################

library(FLBEIA)


## Load in all files
lapply(list.files(file.path("..", "model_inputs"), full.names = TRUE), load, .GlobalEnv)

SC1 <- FLBEIA(biols = biols, 
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

save(SC1, file = file.path("..", "outputs", "BaseModel.RData"))

names(SC1)


library(ggplotFL)
plot(SC1[["stocks"]][["COD"]])
fbar(SC1[["stocks"]][["COD"]])
SC1[["stocks"]][["COD"]]@landings 

plot(SC1[["stocks"]][["HAD"]])
fbar(SC1[["stocks"]][["HAD"]])
SC1[["stocks"]][["HAD"]]@landings 

plotFLBiols(SC1$biols)
plotFLFleets(SC1$fleets)



s1_bio    <- bioSum(SC1)           # Data frame (DF) of biological indicators.
s0_adv    <- advSum(SC1)           # DF of management advice (TAC). 
s0_flt    <- fltSum(SC1)           # DF of economic indicators at fleet level.
s0_fltStk <- fltStkSum(SC1)        # DF of indicators at fleet and stock level.
s0_mt     <- mtSum(s0)            # DF of indicators at fleet.
s0_mtStk  <- mtStkSum(s0)         # DF of indicators at fleet and metier level.
s0_vessel <- vesselSum(s0)        # DF of indicators at vessel level.
s0_vesselStk <- vesselStkSum(s0)  # DF of indicators at vessel and stock level.
s0_npv  <- npv(s0, y0 = '2014')   # DF of net present value per fleet over the selected range of years.
s0_risk <- riskSum(s0, Bpa = c(stk1= 135000), Blim = c(stk1= 96000), Prflim = c(fl1 = 0))


# Exploring data frames
head(s0_bio); unique(s0_bio$indicator)
head(s0_adv); unique(s0_adv$indicator)
head(s0_flt); unique(s0_flt$indicator)
head(s0_fltStk); unique(s0_fltStk$indicator)
head(s0_mt); unique(s0_mt$indicator)
head(s0_mtStk); unique(s0_mtStk$indicator)
head(s0_vessel); unique(s0_vessel$indicator)
head(s0_vesselStk); unique(s0_vesselStk$indicator)
head(s0_risk); unique(s0_risk$indicator)


s0_bio_l    <- bioSum(SC1, long = FALSE, years = ac(2016:2022))            
head(s0_bio_l)

ggplot(s0_bio_l, aes(x = year, y = f)) + 
	geom_line() + facet_wrap(~stock)

s0_adv_l    <- advSum(SC1, long = FALSE, years = ac(2016:2020))             
s0_flt_l    <- fltSum(SC1, long = FALSE, years = ac(2016:2020))
s0_fltStk_l <- fltStkSum(s0, long = FALSE, years = ac(2016:2020))          
s0_mt_l     <- mtSum(s0, long = FALSE, years = ac(2016:2020))             
s0_mtStk_l  <- mtStkSum(s0, long = FALSE, years = ac(2016:2020))           
s0_vessel_l <- vesselSum(s0, long = FALSE, years = ac(2016:2020))
s0_vesselStk_l <- vesselStkSum(s0, long = FALSE, years = ac(2016:2020))       

# Exploring data frames
head(s0_bio_l, 2)
head(s0_adv_l, 2)
head(s0_flt_l, 2)
head(s0_fltStk_l, 2)
head(s0_mt_l, 2)
head(s0_mtStk_l, 2)
head(s0_vessel_l, 2)
head(s0_vesselStk_l, 2)

plot(s0$biols[[1]])
plot(s0$stocks[[1]])

plotFLBiols(s0$biols, pdfnm="s0")
plotFLBiols(s0$biols, pdfnm="s0")
plotFLFleets(s0$fleets, pdfnm="s0")
plotEco(s0, pdfnm="s0")
plotfltStkSum(s0, pdfnm="s0")
plotFLFleets(s0$fleets, pdfnm="s0")
plotEco(s0, pdfnm="s0")
plotfltStkSum(s0, pdfnm="s0")

aux <- subset(s0_bio, indicator=="catch" )

p <- ggplot(data=aux, aes(x=year, y=value, color=stock))+ geom_line()+
geom_vline(xintercept = 2016, linetype = "longdash")+ theme_bw()+
theme(text=element_text(size=10), title=element_text(size=10,face="bold"),
strip.text=element_text(size=10))+
ylab("Catch (t)")

print(p)

