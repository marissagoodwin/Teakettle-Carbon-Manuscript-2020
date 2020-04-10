#Set working directory
setwd("C:/Users/maris/Desktop/TEAK Carbon Manuscript Code (Feb)")
library(ggplot2)
library(ggsci)
library(data.table)


#Figure 1 - Logistic Regression of Survival Probability
ddd <- read.csv("ddd_800.csv")

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

p <-ggplot(ddd, aes(x=Growing.Space, y=SDD, colour=Size, group=Size)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  ylab("Probability of Surviving Drought") +
  xlab(expression(paste("Growing Space (", m^2, ")")))+
  scale_color_uchicago(labels = c("DBH > 75 cm", "25 cm  < DBH < 75 cm", "DBH < 25 cm"))


size_log <- p + theme_bw(base_size=13) + theme(legend.title=element_blank()) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.5, 'cm'))

ggsave("SurvivalRegression(no facet).png", plot=size_log, width= 8, height= 6, dpi = 500)

#Histogram
p <- ggplot(ddd, aes(Growing.Space, fill=Size)) + 
  ylab("Number of Trees")+
  xlab(expression(paste("Growing Space (", m^2, ")")))+
  geom_histogram(alpha=0.5, breaks=seq(0, 600, by = 20), position="identity")+
  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments))


hist_area <- p + theme_bw(base_size=13) + theme(legend.title=element_blank()) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())

ggsave("growingspacedistribution(updatedwithsizeclass).png", plot=hist_area, width= 8, height= 6, dpi = 500)

#Reformatting the DDD dataframe

ddd$Growing.Space <- NA

ddd[ddd$Area <= 50, 'Growing.Space'] <- "0-50"
ddd[ddd$Area > 50 & ddd$Area <= 100, 'Growing.Space'] <- "50-100"
ddd[ddd$Area > 100 & ddd$Area <= 150, 'Growing.Space'] <- "100-150"
ddd[ddd$Area > 150 & ddd$Area <= 200, 'Growing.Space'] <- "150-200"
ddd[ddd$Area > 200 & ddd$Area <= 250, 'Growing.Space'] <- "200-250"
ddd[ddd$Area > 250 & ddd$Area <= 300, 'Growing.Space'] <- "250-300"
ddd[ddd$Area > 300 & ddd$Area <= 350, 'Growing.Space'] <- "300-350"
ddd[ddd$Area > 350 & ddd$Area <= 400, 'Growing.Space'] <- "350-400"
ddd[ddd$Area > 400 & ddd$Area <= 450, 'Growing.Space'] <- "400-450"
ddd[ddd$Area > 450 & ddd$Area <= 500, 'Growing.Space'] <- "450-500"
ddd[ddd$Area > 500 & ddd$Area <= 550, 'Growing.Space'] <- "500-550"
ddd[ddd$Area > 550 & ddd$Area <= 600, 'Growing.Space'] <- "550-600"
ddd[ddd$Area > 600 & ddd$Area <= 650, 'Growing.Space'] <- "600-650"

aggregate(count ~ Treatment, ddd, sum)

ddd$Total <- NA
ddd[ddd$Treatment == 'bc', 'Total'] <- 1061
ddd[ddd$Treatment == 'bn', 'Total'] <- 2619
ddd[ddd$Treatment == 'bs', 'Total'] <- 478
ddd[ddd$Treatment == 'uc', 'Total'] <- 1957
ddd[ddd$Treatment == 'un', 'Total'] <- 2269
ddd[ddd$Treatment == 'us', 'Total'] <- 1014

ddd$Prop <- ddd$count/ddd$Total

#Plotting Proportion of Trees rather than count of trees (doesn't look good)
ddd$Growing.Space <- as.factor(ddd$Growing.Space)

p <- ggplot(ddd, aes(x=Growing.Space, y=Prop)) + 
  ylab("Proportion of Trees")+
  xlab(expression(paste("Growing Space (", m^2, ")")))+
  geom_bar( stat="identity", fill="lavenderblush4")+
  scale_x_discrete(labels=c("50","100","150","200","250","300","350", "400", "450", "500", "550", "600", "650", "700"))+
  geom_hline(yintercept = 0)+
  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments))


bar_area <- p + theme_bw(base_size=13) + theme(legend.title=element_blank()) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("growingspacedistribution(bargraph).png", plot=bar_area, width= 8, height= 6, dpi = 500)

#Line graph with frequency
size=c('Large'= "DBH > 75 cm", 'Medium'="25 cm < DBH < 75 cm", 'Small' = "DBH < 25 cm")

p <- ggplot(ddd, aes(Growing.Space, colour = Treatment)) +
  ylab("Number of Trees")+
  xlab(expression(paste("Growing Space (", m^2, ")")))+
  geom_freqpoly(binwidth = 50)+
  scale_color_jco()+
  facet_grid(rows=vars(Size), labeller=as_labeller(size), scales="free_y")
line_area <- p + theme_bw(base_size=13) + theme(legend.title=element_blank()) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave("growingspacedistribution(linegraph_facet_updated).png", plot=line_area, width= 8, height= 6, dpi = 500)

#Creating a density distribution using geom_density
p <- ggplot(ddd, aes(Growing.Space, color=Size)) + geom_density()+
  ylab("Density")+
  xlab(expression(paste("Growing Space (", m^2, ")")))+
  scale_color_uchicago(labels = c("DBH > 75 cm", "25 cm  < DBH < 75 cm", "DBH < 25 cm"))+
  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments))

p <- p + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))


dens_area <- p + theme_bw(base_size=13) + theme(legend.title=element_blank()) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.5, 'cm'))

ggsave("densitydistribution.png", plot=dens_area, width= 8, height= 6, dpi = 500)

#Saving the Survival Regression and Density Distribution as a two part figure

fig1 <- plot_grid(size_log, dens_area, labels = "AUTO", ncol=2)


ggsave("Figure1.pdf", plot=fig1, width= 12, height= 6, dpi = 300)
