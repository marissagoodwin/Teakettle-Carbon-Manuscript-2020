#Set working directory
setwd("C:/Users/maris/OneDrive/Desktop/TEAK Carbon Manuscript Code/Final")
library(ggplot2)
library(ggsci)
library(data.table)
library(ggpubr)

#Figure 1 - Survival Regression Faceted by Treatment
##Trees with a a growing space greater than 200 meters removed
ddd <- read.csv("ddd_200.csv") #Subsetted dataframe with trees that have a growing space of 200 meters squared or less

ddd$Size <- NA
ddd[which(ddd$DBH_11 < 75 | ddd$DBH_11 > 25), "Size"] <- "Medium"
ddd[which(ddd$DBH_11 > 75), "Size"] <- "Large"
ddd[which(ddd$DBH_11 < 25), "Size"] <- "Small"

ddd$Area <- as.numeric(ddd$Growing.Space)
ddd$Treatment <- as.factor(ddd$Treatment)

#Change "DDD" (Died during Drought) to "SDD" (Survived during Drought)
ddd$SDD <- NA
ddd[which(ddd$DDD == "1"), "SDD"] <- 0
ddd[which(ddd$DDD == "0"), "SDD"] <- 1

treatments <- c('bc' = "Burn/Understory Thin", 'bn' = "Burn Only", 'bs' = 'Burn/Overstory Thin', 'uc'= "Understory Thin", 'un'="Control", 'us'="Overstory Thin")

ddd$Treatment_ordered <- factor(ddd$Treatment, levels=c('un', 'uc','us', 'bn', 'bc', 'bs'))

p <-ggplot(ddd, aes(x=Growing.Space, y=SDD, colour=Size, group=Size)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE, level=0.95, aes(fill=Size), alpha=0.2)+ 
  ylab("Probability of Surviving Drought") +
  xlab(expression(paste("Growing Space (", m^2, ")")))+
  scale_color_manual(labels = c("DBH > 75 cm", "25 cm < DBH < 75 cm", "DBH < 25"), values=c( "goldenrod", "#1F968BFF", "#440154FF"))+
  scale_fill_manual(labels = c("DBH > 75 cm", "25 cm < DBH < 75 cm", "DBH < 25"), values=c( "goldenrod", "#1F968BFF", "#440154FF"))+
  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments))


size_log <- p + theme_bw() + theme(axis.title.x = element_text(size = 18),axis.text.x = element_text(size = 17),
  axis.title.y = element_text(size = 18), axis.text.y = element_text(size=17))+ 
  theme(strip.text.x = element_text(size = 17))+
  theme(legend.text=element_text(size=17))+
  theme(legend.title=element_blank()) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.5, 'cm'))

ggsave("Figure 1_temp.pdf", plot=size_log, width= 10, height= 7, dpi = 500)


#Figure 2 - Large Tree Carbon Stability
ave_all <- read.csv("carbonstabilityfig.csv")

ave_all$order <- as.factor(ave_all$order)
ave_all$rep <- as.factor(ave_all$rep)

ave_all$Stability_Treatment <- as.numeric(ave_all$Stability_Treatment)
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

stab <- ggplot(ave_all, aes(order, Stability_Treatment)) + geom_point(colour= "black", fill=alpha("darkgrey", 0.6), size = 5, shape=23, stroke=1.5) +
  scale_x_discrete(labels=addline_format(c("Control","Burn Only", "Understory Thin","Burn Understory Thin", "Overstory Thin", "Burn Overstory Thin")))+
  ylab(expression(paste("Large Tree Carbon Stability ( ", mu, " /" , sigma, ")")))+
  ylim(0, 10)


stab <- stab + theme_bw() + theme(axis.title.x = element_text(size = 20),axis.text.x = element_text(size = 19),
  axis.title.y = element_text(size = 20), axis.text.y = element_text(size=19))+ 
  theme(axis.title.x=element_blank())+ theme(legend.position = "none")+theme(
    panel.grid.major = element_blank(), panel.grid.minor=element_blank(), panel.grid.major.x = element_line( size=.1, color="darkgrey" ))

ggsave("Figure 2_temp.pdf", plot=stab, width= 10, height= 9, dpi = 500)


#Figure 3 - Proportion of Total Ecosystem Carbon
bm <- read.csv("treatmentaverages.csv")
bm$year <- as.factor(bm$year)

bm <- bm[which(bm$year != 2002),]

#Create a Fuel Class Column
bm$Fuel_Class <- NA
#Combine CWD, FWD, and Litter into Surface Fuel
bm[which(bm$fuel_type == "CWD"|bm$fuel_type == "FWD" |bm$fuel_type == "Litter"|bm$fuel_type == "Shrub"), 'Fuel_Class'] <- "Surface Fuel"

bm[which(bm$fuel_type == "Live"), 'Fuel_Class'] <- "Live Tree"
bm[which(bm$fuel_type == "Dead"), 'Fuel_Class'] <- "Snag"

#Aggregate proportion of C for all three carbon pools
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
  scale_x_discrete("Year", labels=c("2004" = "2002", "2011" = "2011", "2017"= "2017")) +
  scale_fill_manual(labels = c("Live Tree", "Dead Tree", "Surface Fuel"), values=c( "#01665e", "#bf812d", "#dfc27d"))+
  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments))+
  theme(strip.background = element_blank(),strip.text.x = element_blank())

carb_pyramid <- plot + theme_bw() + theme(axis.title.x = element_text(size = 18),axis.text.x = element_text(size = 17),
  axis.title.y = element_text(size = 18), axis.text.y = element_text(size=17))+ 
  theme(strip.text.x = element_text(size = 17))+
  theme(legend.text=element_text(size=17))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom")

##Hashmarks were added to post-drought bar in Adobe Illustrator##

ggsave("Figure 3_nohash.pdf", plot=carb_pyramid, width= 12, height= 9, dpi = 500)

#Figure 4 - the following code is to create a two part figure showing surface fuel loads
#and to compare the carbon emissions from the first and second entry burns

##First, create a figure comparing emissions from the First and Second Burn
emit <- read.csv("emissions_comparison.csv")

emit$Burn <- as.factor(emit$Burn)
emit$treatment_ordered = factor(emit$ï..Treatment, levels=c('BN','BC','BS'))

#sd and column Std in the dataframe are STANDARD ERROR not STANDARD DEVIATION!
sd <- data.table(emit)[,list(Emissions = sum(Emissions), std = sum(Std)), by = c("treatment_ordered", "Burn")]

treatments <- c('BC' = "Burn/Understory Thin", 'BN' = "Burn Only", 'BS' = 'Burn/Overstory Thin')

plot <- ggplot (data = emit, aes(x=Burn, y=Emissions, fill=Burn)) + geom_point(colour="black", shape=23, size=4)+
  scale_fill_manual(values=c("darkred", "darkred"))+
  geom_errorbar(data=sd, aes(ymax= Emissions + std, ymin = Emissions-std),position="dodge", colour="black", width=.2)+
  xlab("")+
  scale_y_continuous(position="left", limits=c(0,70), breaks=c(0, 10, 20, 30, 40, 50, 60))+
  ylab(bquote('Carbon Emissions (Mg '*~ha^-1*')'))+
  facet_wrap(vars(treatment_ordered), nrow=3, labeller=as_labeller(treatments))


p <- plot + theme_bw() + theme(axis.title.x = element_text(size = 18),axis.text.x = element_text(size = 17),
                               axis.title.y = element_text(size = 18), axis.text.y = element_text(size=17))+ 
  theme(strip.text.x = element_text(size = 17))+
  theme(legend.text=element_text(size=17))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 4, b = 0, l = 0)))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.spacing.x = unit(0.2, 'cm'))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "white"),legend.title = element_text(color = "white"),legend.key = element_rect(fill="white", colour="white"))


#Then, recreate the original Figure 4 which shows the change in surface fuels
temp <- read.csv("fuels.csv")

temp$treatment_order <- as.factor(temp$treatment_order)

temp$year <- as.factor(temp$year)

temp <- temp[which(temp$year != 2002),]

##STD is actually Standard Error (STD/sqrt(3)) not Standard Deviation
sd <- data.table(temp)[,list(carbon = sum(carbon), std = sum(std), order = "e"), by = c("treatment_order", "year")]

Treatment_Title <- c('5' = "Overstory Thin", '6'="Burn/Overstory Thin", '2'="Burn Only", '3' = "Understory Thin", '1'="Control", '5'="Overstory Thin", '4'="Burn/Understory Thin")

plot <- ggplot (data = temp, aes(x=year, y=carbon, fill=order)) + geom_bar(colour="black", position = position_stack(reverse = TRUE), stat = "identity")+
  scale_y_continuous(position="left", limits=c(0,70), breaks=c(0, 10, 20, 30, 40, 50, 60))+
  geom_errorbar(data=sd, aes(ymax= carbon + std, ymin = carbon),position="dodge", colour="black", width=.2)+
  scale_fill_brewer(labels = c("1000 hr", "100 hr", "10 hr", "1 hr", "Litter"), palette="GnBu", direction=-1, name= "Fuel Type")+
  scale_x_discrete(labels=c('2004'="2002", '2011'="2011", '2017'="2017", '2018'="2018"))+
  ylab(bquote('Carbon (Mg '*~ha^-1*')')) + 
  facet_wrap(vars(treatment_order), nrow=3, labeller = as_labeller(Treatment_Title))


plot_fuel <- plot + theme_bw() + theme(axis.title.x = element_text(size = 18),axis.text.x = element_text(size = 17),
                                       axis.title.y = element_text(size = 18), axis.text.y = element_text(size=17))+ 
  theme(strip.text.x = element_text(size = 17))+
  theme(legend.text=element_text(size=17), legend.title=element_text(size=17))+  
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+
  theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45, hjust=1)) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  guides(fill = guide_legend(reverse = TRUE))+theme(legend.position = "bottom", legend.direction="horizontal", legend.spacing.x= unit(0.2, 'cm'))

#Then save the two plots as a combined plot annotated with letters
arrange <- ggarrange(plot_fuel, p, labels=c("(a)", "(b)"), font.label = list(size = 17, color = "black", face = "bold", family = NULL), hjust=c(-0.5,-0.5), vjust=c(2.15,2.15), ncol=2, widths=c(2.75,1.5))
ggsave("Figure 4(withpoints)_temp.pdf", arrange, width=11, height=10, dpi=500)
#Legend in Figure 4b was removed in Illustrator

#Supplemental Figure 2 - Total Carbon Stocks by Year and Treatment
bm <- read.csv("treatmentaverages_absolute.csv")
bm$year <- as.factor(bm$year)

treatments <- c('bc' = "Burn/Understory Thin", 'bn' = "Burn Only", 'bs' = 'Burn/Overstory Thin', 'uc'= "Understory Thin", 'un'="Control", 'us'="Overstory Thin")

bm$Treatment_ordered <- factor(bm$Treatment, levels=c('un', 'uc','us', 'bn', 'bc', 'bs'))

plot <- ggplot(data = bm, aes(x=year, y=carbon, fill=order)) + geom_bar(colour="black", position = position_stack(reverse = TRUE), stat = "identity")+
  ylab(bquote('Carbon (Mg '*~ha^-1*')')) +
  geom_hline(aes(yintercept=0), linetype="solid", color="black", size=0.5)+
  scale_y_continuous(limits = c(-200, 300), breaks =c(-100, 0, 100, 200), labels=c(100, 0, 100, 200))+
  scale_x_discrete("Year", labels=c("2004" = "2002", "2011" = "2011", "2017"= "2017")) +
  scale_fill_manual(labels = c("Live Tree", "Dead Tree", "Surface Fuel"), values=c( "#01665e", "#bf812d", "#dfc27d"))+
  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments))


plot_carb <- plot + theme_bw() + theme(axis.title.x = element_text(size = 16),axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16), axis.text.y = element_text(size=16))+ 
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.text=element_text(size=14))+ 
  theme(legend.title=element_blank()) + theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45, hjust=1)) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.3, 'cm'))

ggsave("Supplemental Figure 2.tiff", plot=plot_carb, width= 10, height= 8, dpi = 500)

#Supplemental Figure 3 - Snag Fall during Drought
snag <- read.csv("snags.csv")
snag <- snag[which(snag$Died == "Drought"),]

snag$order <- NA
snag[snag$Treatment=="bc", 'order'] <- 5
snag[snag$Treatment=="bs", 'order'] <- 6
snag[snag$Treatment=="bn", 'order'] <- 4
snag[snag$Treatment=="un", 'order'] <- 1
snag[snag$Treatment=="uc", 'order'] <- 2
snag[snag$Treatment=="us", 'order'] <- 3

snag$order <- as.factor(snag$order)

#Plotting the Probability of Snagfall by Treatment
p <- ggplot(snag, aes(x=DBH_11, y=fell, color=order, group=order)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  ylab("Probability of Snag Fall") +
  xlab("DBH")+
  scale_color_uchicago(labels = c("Control", "Understory Thin", "Overstory Thin", "Burn/No Thin", "Burn/Understory Thin", "Burn/Overstory Thin"))

snag_fall <- p + theme_bw() + theme(axis.title.x = element_text(size = 15),axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 15), axis.text.y = element_text(size=14))+ 
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))+ 
  theme(legend.position = c(0.78,0.89)) + theme(legend.title = element_blank(), legend.background=element_blank()) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())

ggsave("Supplemental Figure 3.tiff", plot=snag_fall, width= 6, height= 8, dpi = 500)


#Supplemental Figure 4 - Snag Consumption during the Second Entry Burn
snag <- read.csv("burnsnags_secondburn.csv")

snag$treatment <- substring(snag$Plot, 1, 2)

snag$treatment_ordered = factor(snag$treatment, levels=c('bn','bc','bs'))

#Select Snags
snag <- snag[which(snag$DC_2017!= ""),]
#Create a binary column for consumption
snag$consumed <- 0

#If not species was listed in 2018 (ie. 2018 data is blank) add a 1 to indicate the snag was consumed
snag[which(snag$Species_2018 == ""), 'consumed'] <- 1

#Only use the primary tree species 
snag <- snag[which(snag$Species_2017 == "abco" | snag$Species_2017 == "cade" | snag$Species_2017 == "pila" | snag$Species_2017 == "pije"),]

snag$DC_2017 <- as.factor(snag$DC_2017)

treatments <- c('bc' = "Burn/Understory Thin", 'bn' = "Burn Only", 'bs' = 'Burn/Overstory Thin')

p <- ggplot(snag, aes(x=DBH_17, y=consumed, colour=DC_2017, group=DC_2017)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  ylab("Probability of Snag Consumption") +
  xlab("DBH")+
  facet_grid(~treatment_ordered, labeller=as_labeller(treatments))+
  scale_color_uchicago(name = "Decay Class")

snag_cons <- p + theme_bw() + theme(axis.title.x = element_text(size = 15),axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 15), axis.text.y = element_text(size=14))+ 
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.5, 'cm'))

ggsave("Supplemental Figure 4.tiff", plot=snag_cons, width= 8, height= 8, dpi = 500)
