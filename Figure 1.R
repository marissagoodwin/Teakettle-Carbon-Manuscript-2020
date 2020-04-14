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
  scale_color_manual(labels = c("DBH > 75 cm", "25 cm < DBH < 75 cm", "DBH < 25"), values=c( "goldenrod", "#1F968BFF", "#440154FF"))+
  labs(tag="A")

size_log <- p + theme_bw(base_size=13) + theme(legend.title=element_blank()) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.5, 'cm'))

ggsave("SurvivalRegression(no facet).png", plot=size_log, width= 8, height= 6, dpi = 500)

#Creating a density distribution using geom_density
p <- ggplot(ddd, aes(Growing.Space, color=Size)) + geom_density()+
  ylab("Density")+
  xlab(expression(paste("Growing Space (", m^2, ")")))+
  scale_color_manual(labels = c("DBH > 75 cm", "25 cm < DBH < 75 cm", "DBH < 25"), values=c( "goldenrod", "#1F968BFF", "#440154FF"))+
  facet_wrap(~Treatment_ordered, labeller = as_labeller(treatments))+
  labs(tag="B")

p <- p + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))


dens_area <- p + theme_bw(base_size=13) + theme(legend.title=element_blank()) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_blank())+
  theme(legend.position="bottom", legend.direction="horizontal", legend.spacing.x= unit(0.5, 'cm'))

ggsave("densitydistribution.png", plot=dens_area, width= 8, height= 6, dpi = 500)

#Saving the Survival Regression and Density Distribution as a two part figure
library(cowplot)

fig1 <- plot_grid(size_log, dens_area, labels = "AUTO", ncol=2)


ggsave("Figure1.jpeg", plot=fig1, width= 12, height= 6, dpi = 600)

library(gridExtra)
library(ggpubr)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(size_log)


p3 <- grid.arrange(arrangeGrob(size_log + theme(legend.position="none"),
                               dens_area + theme(legend.position="none"),
                               nrow=1),

                   mylegend, nrow=2,heights=c(10, 2))


ggsave("Figure1.jpeg", plot=p3, width= 12, height= 6, dpi = 500)


