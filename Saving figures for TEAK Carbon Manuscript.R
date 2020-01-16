#Set working directory
setwd("C:/Users/mjgoodwin/Desktop/TEAK Carbon Manuscript Code")
library(ggplot2)
library(ggsci)
library(data.table)

#Figure 1: Carbon Carrying Capacity
ccc <- read.csv("carboncarryingcapacity.csv")

ccc$year <- as.factor(ccc$year)

hline_dat <- ccc[which(ccc$year == "2002"),]

plot <- ggplot(data = ccc, aes(x=year, y=carbon)) + geom_bar(fill="darkseagreen4", color="black", stat = "identity")+
  geom_hline(data=hline_dat, aes(yintercept=treatment_line), linetype="dashed", color="darkblue", size=1)+
  geom_hline(data=hline_dat, aes(yintercept=drought_line), linetype="dashed", color="darkred", size=1)+
  ylab(bquote('Live Carbon (Mg '*~ha^-1*')'))+
  scale_x_discrete(labels=c('2002' ="1999", '2004'="2002", '2011'="2011", '2017'="2017", '2018'="2018"))+
  facet_wrap(~Treatment, labeller = as_labeller(treatments))


p <- plot+theme_bw(base_size=15)+theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45, hjust=1))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())


ggsave("CarbonCarryingCapacity.png", plot=p, width= 8, height= 6, dpi = 500)

#Carbon Carrying Capacity (a second visualization)
ccc <- read.csv("changecarboncarryingcapacity.csv")

ccc <- ccc[which(ccc$periodorder != "3"),]

ccc$order <- as.factor(ccc$order)
ccc$periodorder <- as.factor(ccc$periodorder)

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

period <- c('1' = "Treatment", '2'="Drought")

plot <- ggplot(data = ccc, aes(x=order, y=Treatment.Change, fill=periodorder)) + geom_bar(color="black", stat = "identity", position="dodge")+
   ylab(bquote('Reduction in Carbon Carrying Capacity (Mg '*~ha^-1*')'))+
   scale_x_discrete(labels=addline_format(c("Control", "Burn Only","Understory Thin","Burn Understory Thin", "Overstory Thin", "Burn Overstory Thin")))+
   xlab(" ")+
   scale_fill_manual(labels = c("Treatment", "Drought"), values=c("#4d4d4d", "#d6604d"))+
   geom_hline(aes(yintercept=0), linetype="solid", color="black", size=0.5)+
   facet_wrap(~periodorder, labeller = as_labeller(period))
 

p <- plot + theme_bw(base_size=13)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.spacing.x = unit(0.25, 'cm'))+ theme(legend.title = element_blank())+
  theme(legend.position = "none") ##Remove legend position if you want legend to show up
  
ggsave("ChangeCarbonCarryingCapacity(panels).png", plot=p, width= 8, height= 6, dpi = 500)

#Figure 2a - Logistic Regression of Survival Probability
ddd <- read.csv("ddd.csv")

ddd$Size <- NA
ddd[which(ddd$DBH_11 < 75 | ddd$DBH_11 > 25), "Size"] <- "Medium"
ddd[which(ddd$DBH_11 > 75), "Size"] <- "Large"
ddd[which(ddd$DBH_11 < 25), "Size"] <- "Small"

ddd$Area <- as.numeric(ddd$Area)
ddd$Treatment <- as.factor(ddd$Treatment)

ddd$SDD <- NA
ddd[which(ddd$DDD == "1"), "SDD"] <- 0
ddd[which(ddd$DDD == "0"), "SDD"] <- 1
ddd$sizeclass <- as.factor(ddd$sizeclass)

treatments <- c('bc' = "Burn/Understory Thin", 'bn' = "Burn Only", 'bs' = 'Burn/Overstory Thin', 'uc'= "Understory Thin", 'un'="Control", 'us'="Overstory Thin")

ddd$Treatment_ordered <- factor(ddd$Treatment, levels=c('un', 'uc','us', 'bn', 'bc', 'bs'))

p <-ggplot(ddd, aes(x=Area, y=SDD, colour=Size, group=Size)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments),scales="free_x")+
  ylab("Probability of Surviving Drought") +
  xlab("Growing Space (meters)")+
  scale_color_uchicago(labels = c("DBH > 75 cm", "25 cm  < DBH < 75 cm", "DBH < 25 cm"))


size_log <- p + theme_bw(base_size=13) + theme(legend.title=element_blank()) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.5, 'cm'))

ggsave("SurvivalRegression.png", plot=size_log, width= 8, height= 6, dpi = 500)


#Figure 2b - Large Tree Carbon Stability
ave_all <- read.csv("carbonstabilityfig.csv")

ave_all$order <- as.factor(ave_all$order)

ave_all$Stability_Treatment <- as.numeric(ave_all$Stability_Treatment)
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

stab <- ggplot(ave_all, aes(x=order, y=Stability_Treatment)) + geom_point(size = 3, shape=24, colour="black", fill="gray30") + scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(labels=addline_format(c("Control", "Burn Only","Understory Thin","Burn Understory Thin", "Overstory Thin", "Burn Overstory Thin")))+
  ylab("Large Tree Carbon Stability") 


stab <- stab + theme_bw(base_size=15)+
  theme(axis.title.x=element_blank())+ theme(legend.position = "none")


ggsave("CarbonStability.jpeg", plot=stab, width= 8, height= 6, dpi = 500)


#Proportion of Total Ecosystem Carbon
bm <- read.csv("treatmentaverages.csv")
bm$year <- as.factor(bm$year)

bm <- bm[which(bm$year != 2002),]

bm$Fuel_Class <- NA
bm[which(bm$fuel_type == "CWD"|bm$fuel_type == "FWD" |bm$fuel_type == "Litter"|bm$fuel_type == "Shrub"), 'Fuel_Class'] <- "Surface Fuel"

bm[which(bm$fuel_type == "Live"), 'Fuel_Class'] <- "Live Tree"
bm[which(bm$fuel_type == "Dead"), 'Fuel_Class'] <- "Snag"

prop <- aggregate(proportion ~ Treatment + year + Fuel_Class, bm, sum)

prop$Fuel_Class <- as.character(prop$Fuel_Class)
prop$proportion <- prop$proportion * 100

treatments <- c('bc' = "Burn/Understory Thin", 'bn' = "Burn Only", 'bs' = 'Burn/Overstory Thin', 'uc'= "Understory Thin", 'un'="Control", 'us'="Overstory Thin")

prop$Treatment_ordered <- factor(prop$Treatment, levels=c('un', 'uc','us', 'bn', 'bc', 'bs'))

plot <- ggplot(prop, aes(x=year)) +
  geom_bar(data=prop[prop$Fuel_Class=="Live Tree",], aes(y=proportion, fill=Fuel_Class), colour="black", stat="identity", width=0.7) +
  geom_bar(data=prop[prop$Fuel_Class=="Snag"|prop$Fuel_Class == "Surface Fuel",], aes(y=-proportion, fill=Fuel_Class), colour="black", position = position_stack(reverse = TRUE), stat="identity", width=0.7) +
  geom_hline(yintercept=0, colour="black", lwd=0.5) +
  scale_y_continuous(breaks=seq(-75,75,25), labels=c(75,50,25,0,25,50,75)) +
  labs(y="Proportion of Carbon Stocks (%)", x="Year", fill="") +
  coord_flip(ylim=c(-100,100)) +
  scale_x_discrete("Year", labels=c("2004" = "2002", "2011" = "2011", "2017"= "2017")) +
  scale_fill_manual(labels = c("Live Tree", "Dead Tree", "Surface Fuel"), values=c( "#01665e", "#bf812d", "#dfc27d"))+

  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments))+
  theme(strip.background = element_blank(),strip.text.x = element_blank())

carb_pyramid <- plot + theme_bw(base_size=13) + theme(panel.grid.major = element_blank(),
                                          panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom")

ggsave("CarbonPyramid.png", plot=carb_pyramid, width= 8, height= 6, dpi = 500)


#Figure 4- Change in Surface Fuels
temp <- read.csv("fuels.csv")

temp$treatment_order <- as.factor(temp$treatment_order)

temp$year <- as.factor(temp$year)

temp <- temp[which(temp$year != 2002),]

sd <- data.table(temp)[,list(carbon = sum(carbon), std = sum(std), order = "e"), by = c("treatment_order", "year")]

Treatment_Title <- c('5' = "Burn/Understory Thin", '6'="Burn/Overstory Thin", '4'="Burn Only", '2' = "Understory Thin", '1'="Control", '3'="Overstory Thin")

plot <- ggplot (data = temp, aes(x=year, y=carbon, fill=order)) + geom_bar(colour="black", position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data=sd, aes(ymax= carbon + std, ymin = carbon),position="dodge", colour="black", width=.2)+
  scale_fill_manual(labels = c("1000 hr", "100 hr", "10 hr", "1 hr", "Litter"), values=c("#67a9cf", "#8c510a", "#7fbf7b", "#1b7837","#fee08b"), name= "Fuel Type")+
  scale_x_discrete(labels=c('2004'="2002", '2011'="2011", '2017'="2017", '2018'="2018"))+
  ylab(bquote('Carbon (Mg '*~ha^-1*')')) + 
  facet_wrap(~treatment_order, labeller = as_labeller(Treatment_Title))
  

plot_fuel <- plot + theme_bw(base_size=13) + theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45, hjust=1)) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())    

ggsave("surfacefuel.jpeg", plot=plot_fuel, width= 8, height= 6, dpi = 500)


#Total Change in Live and Dead/CWD Pools by Treatment (use this to supplement a conceptual figure?)
bm <- read.csv("livedead.csv")
bm$year <- as.factor(bm$year)

treatments <- c('bc' = "Burn/Understory Thin", 'bn' = "Burn Only", 'bs' = 'Burn/Overstory Thin', 'uc'= "Understory Thin", 'un'="Control", 'us'="Overstory Thin")

bm$Treatment_ordered <- factor(bm$Treatment, levels=c('un', 'uc','us', 'bn', 'bc', 'bs'))

plot <- ggplot(data = bm, aes(x=year, y=carbon, fill=order)) + geom_bar(colour="black", position = position_stack(reverse = TRUE), stat = "identity")+
  ylab(bquote('Carbon (Mg '*~ha^-1*')')) +
  geom_hline(aes(yintercept=0), linetype="solid", color="black", size=0.5)+
  scale_x_discrete("Year", labels=c("2004" = "2002", "2011" = "2011", "2017"= "2017")) +
  scale_fill_manual(labels = c("Live Tree", "Dead Tree", "Surface Fuel"), values=c( "#01665e", "#bf812d", "#dfc27d"))+
  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments))


plot_carb<- plot + theme_bw(base_size=15) + theme(legend.title=element_blank()) + theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45, hjust=1)) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
             theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.3, 'cm'))

ggsave("Live and Dead Carbon.jpeg", plot=plot_carb, width= 8, height= 6, dpi = 500)


#Snag Consumption during the Second Entry Burn
snag <- read.csv("burnsnags.csv")

snag$treatment <- substring(snag$Plot, 1, 2)

snag$consumed <- 0

snag[which(snag$Species_2018 == ""), 'consumed'] <- 1

snag <- snag[which(snag$Species_2017 == "abco" | snag$Species_2017 == "cade" | snag$Species_2017 == "pila" | snag$Species_2017 == "pije"),]

snag$DC_2017 <- as.factor(snag$DC_2017)

treatments <- c('bc' = "Burn/Understory Thin", 'bn' = "Burn Only", 'bs' = 'Burn/Overstory Thin')

p <- ggplot(snag, aes(x=DBH_17, y=consumed, colour=DC_2017, group=DC_2017)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  ylab("Probability of Snag Consumption") +
  xlab("DBH")+
  facet_grid(~treatment, labeller=as_labeller(treatments))+
  scale_color_uchicago(name = "Decay Class")

snag_cons <- p + theme_bw(base_size=13) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.5, 'cm'))+
  theme(legend.box.background = element_rect(color="black", size=0.7))

snag_cons


ggsave("Snag Consumption.png", plot=snag_cons, width= 8, height= 6, dpi = 500)


#Conceptual Figure
temp <- read.csv("conceptualfig.csv")

func1 <- sapply(temp$Carbon, FUN = y ~ s(log(x)))

p <- ggplot(data = temp, aes(x=Time, y=Carbon)) +
  scale_y_continuous(limits=c(0,1200), breaks = seq(0, 1000, by = 100))+
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(log(x)), color="red", size=1.5)+
  ylab("Live Carbon")+
  annotate("rect", xmin=0, xmax=20, ymin=0, ymax=250, fill="darkgreen", alpha=0.2, color=NA)+
  annotate("rect", xmin=20, xmax=40, ymin=0, ymax=250, fill="salmon", alpha=0.2, color=NA)+
  annotate("rect", xmin=40, xmax=60, ymin=0, ymax=250, fill="darkblue", alpha=0.2, color=NA)+
  geom_hline(aes(yintercept=1000),linetype="dashed", color="black", size=1.5)+
  geom_vline(aes(xintercept=20), linetype="dashed", color="black")+
  geom_vline(aes(xintercept=40), linetype="dashed", color="black")+
  geom_ribbon(data=subset(temp, 40 <= Time & Time <=60), aes(ymin= 500, ymax=1000),fill="tan4", alpha="0.5")



conc <- p + theme_bw(base_size=15) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())

conc

ggsave("Concpetual Figure.png", plot=conc, width= 8, height= 6, dpi = 500)



