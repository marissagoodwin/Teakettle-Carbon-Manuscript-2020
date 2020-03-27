#First created and worked on December 19, 2019

#Set Working Directory
setwd("C:/Users/maris/Desktop/TEAK Carbon Manuscript Code (Feb)")


#This dataframe includes all the trees that were Alive in 2011 and either dead standing or dead on the ground in 2017
snag <- read.csv("snags.csv")

temp <- aggregate(fell ~ Plot + Died, snag, sum)

temp2 <- aggregate(ddd ~ Plot + Died, snag, sum)

temp$ddd <- temp2$ddd

temp$propfell <- temp$fell/temp$ddd

temp #This dataframe has the number and proportion of trees that fell by Plot

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

snag_fall <- p + theme_bw(base_size=13) + theme(legend.position = c(0.88,0.87)) + theme(legend.title = element_blank(), legend.background=element_blank()) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())
snag_fall

ggsave("Snagfall.png", plot=snag_fall, width= 8, height= 6, dpi = 500)

#Statistical Analysis of Probability of Snagfall
snag.fall <- glm(fell ~ Treatment, data=snag, family=binomial)
summary(snag.fall)
comps <- glht(snag.fall, linfct = mcp(Treatment = "Tukey"))
cld(comps, alpha=0.05, Letters=letters)

#Lookng at Snag Consumption during the Two Burns
snag <- read.csv("burnsnags.csv")

snag$treatment <- substring(snag$Plot, 1, 2)

snag$Decay.Class <- as.factor(snag$Decay.Class)

treatments <- c('bc' = "Burn/Understory Thin", 'bn' = "Burn Only", 'bs' = 'Burn/Overstory Thin', 'First Burn' = 'First Burn', 'Second Burn' = 'Second Burn')

p <- ggplot(snag, aes(x=DBH_Pre, y=consumed, colour=Decay.Class, group=Decay.Class)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  ylab("Probability of Snag Consumption") +
  xlab("DBH")+
  facet_grid(Burn~treatment, labeller=as_labeller(treatments))+
  scale_color_uchicago(name = "Decay Class")

snag_cons <- p + theme_bw(base_size=13) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.5, 'cm'))+
  theme(legend.box.background = element_rect(color="black", size=1))

snag_cons


ggsave("Snag Consumption for Both Burns.png", plot=snag_cons, width= 8, height= 6, dpi = 500)

#Here I calculate the proportion of Carbon that remained standing versus fell

#First, calculate Carbon for each tree that died
#create new columns for biomass calculations
snag['speciesgroup'] <- 'NA'
snag['b0'] <- 'NA'
snag['b1'] <- 'NA'
snag['biomass'] <- 'NA'
snag['type'] <- 'NA'
snag['a0'] <- 'NA'
snag['a1'] <- 'NA'
snag['coarseroots'] <- 'NA'
snag['totalbiomass'] <- 'NA'

#Creating a new column with each Species Group 
Fir <- snag[which((snag$Species == 'abco')|(snag$Species == 'abmaa')|(snag$Species == 'unkn'|(snag$Species == 'absp'))),]
snag[which((snag$Species == 'abco')|(snag$Species == 'abma')|(snag$Species == 'unkn'|(snag$Species == 'absp'))), 'speciesgroup'] <- 'Fir'


Pine <- snag[which((snag$Species == 'pije')|(snag$Species == 'pila')|(snag$Species == 'pisp')),]
snag[which((snag$Species == 'pije')|(snag$Species == 'pila')|(snag$Species == 'pisp')), 'speciesgroup'] <- 'Pine'

Cedar <- snag[which(snag$Species == 'cade'),]
snag[which(snag$Species == 'cade'), 'speciesgroup'] <- 'Cedar'

Hardwood <- snag[which((snag$Species == 'quke')|(snag$Species == 'quch')|(snag$Species == 'quwi')),]
snag[which((snag$Species == 'quke')|(snag$Species == 'quch')|(snag$Species == 'quwi')),'speciesgroup'] <- 'Hardwood'

Shrub <- snag[which((snag$Species == 'prem')|(snag$Species == 'sasp')),]
snag[which((snag$Species == 'prem')|(snag$Species == 'sasp')), 'speciesgroup'] <- 'Shrub'

#Assigning Jenkins Parameters to each Species Group
snag[which(snag$speciesgroup == 'Fir'), 'b0'] <- -2.5384
snag[which(snag$speciesgroup == 'Pine'), 'b0'] <- -2.5356
snag[which(snag$speciesgroup == 'Cedar'), 'b0'] <- -2.0336
snag[which(snag$speciesgroup == 'Hardwood'), 'b0'] <- -2.0127
snag[which(snag$speciesgroup == 'Shrub'), 'b0'] <- -2.2094

snag[which(snag$speciesgroup == 'Fir'), 'b1'] <- 2.4814
snag[which(snag$speciesgroup == 'Pine'), 'b1'] <- 2.4349
snag[which(snag$speciesgroup == 'Cedar'), 'b1'] <- 2.2592
snag[which(snag$speciesgroup == 'Hardwood'), 'b1'] <- 2.4342
snag[which(snag$speciesgroup == 'Shrub'), 'b1'] <- 2.3867

#Specify whether tree is a Hardwood or Softwood
snag[which(snag$speciesgroup == 'Fir'), 'type'] <- 'softwood'
snag[which(snag$speciesgroup == 'Pine'), 'type'] <- 'softwood'
snag[which(snag$speciesgroup == 'Cedar'), 'type'] <- 'softwood'
snag[which(snag$speciesgroup == 'Hardwood'), 'type'] <- 'hardwood'
snag[which(snag$speciesgroup == 'Shrub'), 'type'] <- 'hardwood'

#Specify Coarse Root coefficients
snag[which(snag$type == 'softwood'), 'a0'] <- -1.5619
snag[which(snag$type == 'softwood'), 'a1'] <- 0.6614
snag[which(snag$type == 'hardwood'), 'a0'] <- -1.6911
snag[which(snag$type == 'hardwood'), 'a1'] <- 0.816

#Calculate Biomass
#First Change Beta Columns to numeric
snag$b0 <- as.numeric(snag$b0)
snag$b1 <- as.numeric(snag$b1)
snag$a0 <- as.numeric(snag$a0)
snag$a1 <- as.numeric(snag$a1)


#Then use Jenkins allometric equation to convert to biomass                  
snag$biomass = (exp(snag$b0 + (snag$b1*log(snag$DBH_Pre)))*0.001)

#Calculate Root Carbon and add to Biomass
snag$coarseroots = (exp(snag$a0+(snag$a1/snag$DBH_Pre)))*snag$biomass

#Create new columns with Decay Class Component Coefficients(try making this into a matrix/dataframe)
snag['foliage_a0'] <- NA
snag['foliage_a1'] <- NA
snag['stembark_a0'] <- NA
snag['stembark_a1'] <- NA
snag['stemwood_a0'] <- NA
snag['stemwood_a1'] <- NA
snag['branches'] <- NA
snag['foliage_comp'] <- NA
snag['stembark_comp'] <- NA
snag['stemwood_comp'] <- NA

#Then add Decay Class Component Coefficients to the columns
snag[which(snag$type == 'softwood'), 'foliage_a0'] <- -2.9584
snag[which(snag$type == 'softwood'), 'foliage_a1'] <- 4.4766
snag[which(snag$type == 'hardwood'), 'foliage_a0'] <- -4.0813
snag[which(snag$type == 'hardwood'), 'foliage_a1'] <- 5.8816

snag[which(snag$type == 'softwood'), 'stembark_a0'] <- -2.098
snag[which(snag$type == 'softwood'), 'stembark_a1'] <- -1.1432
snag[which(snag$type == 'hardwood'), 'stembark_a0'] <- -2.0129
snag[which(snag$type == 'hardwood'), 'stembark_a1'] <- -1.6805

snag[which(snag$type == 'softwood'), 'stemwood_a0'] <- -0.3737
snag[which(snag$type == 'softwood'), 'stemwood_a1'] <- -1.8055
snag[which(snag$type == 'hardwood'), 'stemwood_a0'] <- -0.3065
snag[which(snag$type == 'hardwood'), 'stemwood_a1'] <- -5.424

#Calculate Carbon Components
snag$foliage_comp <- (exp(snag$foliage_a0+(snag$foliage_a1/snag$DBH_Pre)))*snag$biomass
snag$stembark_comp <- (exp(snag$stembark_a0+(snag$stembark_a1/snag$DBH_Pre)))*snag$biomass
snag$stemwood_comp <- (exp(snag$stemwood_a0+(snag$stemwood_a1/snag$DBH_Pre)))*snag$biomass
snag$branches <- snag$biomass-snag$foliage_comp-snag$stembark_comp-snag$stemwood_comp

#Calculate total biomass depending on whether the tree is live or dead (this part isn't working yet)
snag$Decay.Class[is.na(snag$Decay.Class)] <- 0

DC0 <- which(snag$Decay.Class == 0)

snag$totalbiomass[DC0] <- (snag$biomass[DC0]+snag$coarseroots[DC0])

DC1 <- which(snag$Decay.Class == 1)

snag$totalbiomass[DC1] <- (snag$biomass[DC1]+snag$coarseroots[DC1])

DC2 <- which(snag$Decay.Class == 2) 

snag$totalbiomass[DC2] <- (snag$biomass[DC2]-snag$foliage_comp[DC2]+snag$coarseroots[DC2])

DC3 <- which(snag$Decay.Class == 3)

snag$totalbiomass[DC3] <- (snag$biomass[DC3]-snag$foliage_comp[DC3]-snag$branches[DC3]+snag$coarseroots[DC3])

DC4 <- which(snag$Decay.Class == 4)

snag$totalbiomass[DC4] <- (snag$biomass[DC4]-snag$foliage_comp[DC4]-snag$stembark_comp[DC4]-snag$branches[DC4]+snag$coarseroots[DC4])

DC5 <- which(snag$Decay.Class == 5)

snag$totalbiomass[DC5] <- (snag$biomass[DC5]-snag$foliage_comp[DC5]-snag$stembark_comp[DC5]-snag$branches[DC5]+snag$coarseroots[DC5])

snag$totalbiomass <- as.numeric(snag$totalbiomass)

#Biomass in Mg per Hectare

snag$biomass_mg_ha <- snag$totalbiomass/12

#Carbon
snag$Carbon <- snag$totalbiomass/2

#Carbon in Mg per Hectare
snag$carbon_mg_ha <- snag$Carbon/4

#Sum the Carbon for Each Treatment Unit for the Standing Trees and Fallen Trees
snagcarb <- aggregate(carbon_mg_ha ~ Plot + consumed + Burn + treatment, snag, sum)

#Saved the dataframe and calculated an average in Excel
write.table(snagcarb, "C:/Users/mjgoodwin/Desktop/TEAK Carbon Manuscript Code/snagcarbonconsumption.csv", sep=",", na="", row.names=FALSE)
