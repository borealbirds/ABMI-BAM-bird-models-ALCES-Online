library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
my.theme <- theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1))

alcesproj<-read.csv("May28_2020indicators.csv", header=TRUE)
str(alcesproj)

attw<-alcesproj[alcesproj$Species=="ATTW",]
str(attw)
levels(attw$Scenario)
attw$Scenario = factor(attw$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(attw$Scenario)

bbwa<-alcesproj[alcesproj$Species=="BBWA",]
str(bbwa)
levels(bbwa$Scenario)
bbwa$Scenario = factor(bbwa$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(bbwa$Scenario)

bbwo<-alcesproj[alcesproj$Species=="BBWO",]
str(bbwo)
levels(bbwo$Scenario)
bbwo$Scenario = factor(bbwo$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(bbwo$Scenario)

btnw<-alcesproj[alcesproj$Species=="BTNW",]
str(btnw)
levels(btnw$Scenario)
btnw$Scenario = factor(btnw$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(btnw$Scenario)

blpw<-alcesproj[alcesproj$Species=="BLPW",]
str(blpw)
levels(blpw$Scenario)
blpw$Scenario = factor(blpw$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(blpw$Scenario)

boch<-alcesproj[alcesproj$Species=="BOCH",]
str(boch)
levels(boch$Scenario)
boch$Scenario = factor(boch$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(boch$Scenario)

brcr<-alcesproj[alcesproj$Species=="BRCR",]
str(brcr)
levels(brcr$Scenario)
brcr$Scenario = factor(brcr$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(brcr$Scenario)

cawa<-alcesproj[alcesproj$Species=="CAWA",]
str(cawa)
levels(cawa$Scenario)
cawa$Scenario = factor(cawa$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(cawa$Scenario)

cmwa<-alcesproj[alcesproj$Species=="CMWA",]
str(cmwa)
levels(cmwa$Scenario)
cmwa$Scenario = factor(cmwa$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(cmwa$Scenario)

evgr<-alcesproj[alcesproj$Species=="EVGR",]
str(evgr)
levels(evgr$Scenario)
evgr$Scenario = factor(evgr$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(evgr$Scenario)

nofl<-alcesproj[alcesproj$Species=="NOFL",]
str(nofl)
levels(nofl$Scenario)
nofl$Scenario = factor(nofl$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(nofl$Scenario)

osfl<-alcesproj[alcesproj$Species=="OSFL",]
str(osfl)
levels(osfl$Scenario)
osfl$Scenario = factor(osfl$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(osfl$Scenario)

oven<-alcesproj[alcesproj$Species=="OVEN",]
str(oven)
levels(oven$Scenario)
oven$Scenario = factor(oven$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(oven$Scenario)

pawa<-alcesproj[alcesproj$Species=="PAWA",]
str(pawa)
levels(pawa$Scenario)
pawa$Scenario = factor(pawa$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(pawa$Scenario)

piwo<-alcesproj[alcesproj$Species=="PIWO",]
str(piwo)
levels(piwo$Scenario)
piwo$Scenario = factor(piwo$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(piwo$Scenario)

rubl<-alcesproj[alcesproj$Species=="RUBL",]
str(rubl)
levels(rubl$Scenario)
rubl$Scenario = factor(rubl$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(rubl$Scenario)

weta<-alcesproj[alcesproj$Species=="WETA",]
str(weta)
levels(weta$Scenario)
weta$Scenario = factor(weta$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(weta$Scenario)

wewp<-alcesproj[alcesproj$Species=="WEWP",]
str(wewp)
levels(wewp$Scenario)
wewp$Scenario = factor(wewp$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(wewp$Scenario)

wwcr<-alcesproj[alcesproj$Species=="WWCR",]
str(wwcr)
levels(wwcr$Scenario)
wwcr$Scenario = factor(wwcr$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(wwcr$Scenario)

ybsa<-alcesproj[alcesproj$Species=="YBSA",]
str(ybsa)
levels(ybsa$Scenario)
ybsa$Scenario = factor(ybsa$Scenario, levels=c("Al-Pac BAU","Al-Pac BAU with seismic reclamation","No Energy","No Fire","Increased Fire"), ordered=TRUE)
levels(ybsa$Scenario)

# Plot - Projected Population
library(dplyr)
library(viridis)
gg.attw<-attw %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab(paste0("American Three-toed ",'\n',"Woodpecker")) +
  my.theme + ylim(0,max(attw$Population.FMA))+
  xlab("Year")

gg.bbwa<-bbwa %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Bay-breasted Warbler") +
  my.theme + ylim(0,max(bbwa$Population.FMA))+
  xlab("Year")

gg.btnw<-btnw %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab(paste0("Black-throated Green",'\n',"Warbler")) +
  my.theme + ylim(0,max(btnw$Population.FMA))+
  xlab("Year")

gg.blpw<-blpw %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Blackpoll Warbler") +
  my.theme + ylim(0,max(blpw$Population.FMA))+
  xlab("Year")

gg.bbwo<-bbwo %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab(paste0("Black-backed",'\n',"Woodpecker")) +
  my.theme + ylim(0,max(bbwo$Population.FMA))+
  xlab("Year")

gg.boch<-boch %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Boreal Chickadee") +
  my.theme + ylim(0,max(boch$Population.FMA))+
  xlab("Year")

gg.brcr<-brcr %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Brown Creeper") +
  my.theme + ylim(0,max(brcr$Population.FMA))+
  xlab("Year")

gg.cawa<-cawa %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Canada Warbler") +
  my.theme + ylim(0,max(cawa$Population.FMA))+
  xlab("Year")

gg.cmwa<-cmwa %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Cape May Warbler") +
  my.theme + ylim(0,max(cmwa$Population.FMA))+
  xlab("Year")

gg.evgr<-evgr %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Evening Grosbeak") +
  my.theme + ylim(0,max(evgr$Population.FMA))+
  xlab("Year")

gg.nofl<-nofl %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Northern Flicker") +
  my.theme + ylim(0,max(nofl$Population.FMA))+
  xlab("Year")

gg.osfl<-osfl %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Olive-sided Flycatcher") +
  my.theme + ylim(0,max(osfl$Population.FMA))+
  xlab("Year")

gg.oven<-oven %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Ovenbird") +
  my.theme + ylim(0,max(oven$Population.FMA))+
  xlab("Year")
  
gg.pawa<-pawa %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Palm Warbler") +
  my.theme + ylim(0,max(pawa$Population.FMA))+
  xlab("Year")

gg.piwo<-piwo %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Pileated Woodpecker") +
  my.theme + ylim(0,max(piwo$Population.FMA))+
  xlab("Year")

gg.rubl<-rubl %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Rusty Blackbird") +
  my.theme + ylim(0,max(rubl$Population.FMA))+
  xlab("Year")

gg.weta<-weta %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Western Tanager") +
  my.theme + ylim(0,max(weta$Population.FMA))+
  xlab("Year")

gg.wewp<-wewp %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Western Wood-pewee") +
  my.theme + ylim(0,max(wewp$Population.FMA))+
  xlab("Year")

gg.wwcr<-wwcr %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("White-winged Crossbill") +
  my.theme + ylim(0,max(wwcr$Population.FMA))+
  xlab("Year")

gg.ybsa<-ybsa %>%
  ggplot( aes(x=TimePeriod, y=Population.FMA, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Yellow-bellied Sapsucker") +
  my.theme + ylim(0,max(ybsa$Population.FMA))+
  xlab("Year")


tiff('ALCESOnline_FMAPopulationChange.tiff', units="in", width=15, height=15, res=300)
p3 <- grid.arrange(arrangeGrob(gg.attw + theme(legend.position="none"),
                               gg.bbwa + theme(legend.position="none"),
                               gg.bbwo + theme(legend.position="none"),
                               gg.btnw + theme(legend.position="none"),
                               gg.blpw + theme(legend.position="none"),
                               gg.boch + theme(legend.position="none"),
                               gg.brcr + theme(legend.position="none"),
                               gg.cawa + theme(legend.position="none"),
                               gg.cmwa + theme(legend.position="none"),
                               gg.evgr + theme(legend.position="none"),
                               gg.nofl + theme(legend.position="none"),
                               gg.osfl + theme(legend.position="none"),
                               gg.oven + theme(legend.position="none"),
                               gg.pawa + theme(legend.position="none"),
                               gg.piwo + theme(legend.position="none"),
                               gg.rubl + theme(legend.position="none"),
                               gg.weta + theme(legend.position="none"),
                               gg.wewp + theme(legend.position="none"),
                               gg.wwcr + theme(legend.position="none"),
                               gg.ybsa + theme(legend.position="none"),
                               nrow=5, ncol=4))
dev.off()
#

# Plot - Projected Population
library(dplyr)
library(viridis)
gg.attw<-attw %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab(paste0("American Three-toed ",'\n',"Woodpecker")) +
  my.theme + ylim(0,max(attw$MeanDens))+
  xlab("Year")

gg.bbwa<-bbwa %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Bay-breasted Warbler") +
  my.theme + ylim(0,max(bbwa$MeanDens))+
  xlab("Year")

gg.btnw<-btnw %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab(paste0("Black-throated Green",'\n',"Warbler")) +
  my.theme + ylim(0,max(btnw$MeanDens))+
  xlab("Year")

gg.blpw<-blpw %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Blackpoll Warbler") +
  my.theme + ylim(0,max(blpw$MeanDens))+
  xlab("Year")

gg.bbwo<-bbwo %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab(paste0("Black-backed",'\n',"Woodpecker")) +
  my.theme + ylim(0,max(bbwo$MeanDens))+
  xlab("Year")

gg.boch<-boch %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Boreal Chickadee") +
  my.theme + ylim(0,max(boch$MeanDens))+
  xlab("Year")

gg.brcr<-brcr %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Brown Creeper") +
  my.theme + ylim(0,max(brcr$MeanDens))+
  xlab("Year")

gg.cawa<-cawa %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Canada Warbler") +
  my.theme + ylim(0,max(cawa$MeanDens))+
  xlab("Year")

gg.cmwa<-cmwa %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Cape May Warbler") +
  my.theme + ylim(0,max(cmwa$MeanDens))+
  xlab("Year")

gg.evgr<-evgr %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Evening Grosbeak") +
  my.theme + ylim(0,max(evgr$MeanDens))+
  xlab("Year")

gg.nofl<-nofl %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Northern Flicker") +
  my.theme + ylim(0,max(nofl$MeanDens))+
  xlab("Year")

gg.osfl<-osfl %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Olive-sided Flycatcher") +
  my.theme + ylim(0,max(osfl$MeanDens))+
  xlab("Year")

gg.oven<-oven %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Ovenbird") +
  my.theme + ylim(0,max(oven$MeanDens))+
  xlab("Year")

gg.pawa<-pawa %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Palm Warbler") +
  my.theme + ylim(0,max(pawa$MeanDens))+
  xlab("Year")

gg.piwo<-piwo %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Pileated Woodpecker") +
  my.theme + ylim(0,max(piwo$MeanDens))+
  xlab("Year")

gg.rubl<-rubl %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Rusty Blackbird") +
  my.theme + ylim(0,max(rubl$MeanDens))+
  xlab("Year")

gg.weta<-weta %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Western Tanager") +
  my.theme + ylim(0,max(weta$MeanDens))+
  xlab("Year")

gg.wewp<-wewp %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Western Wood-pewee") +
  my.theme + ylim(0,max(wewp$MeanDens))+
  xlab("Year")

gg.wwcr<-wwcr %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("White-winged Crossbill") +
  my.theme + ylim(0,max(wwcr$MeanDens))+
  xlab("Year")

gg.ybsa<-ybsa %>%
  ggplot( aes(x=TimePeriod, y=MeanDens, group=Scenario, color=Scenario)) +
  geom_line(size=1) +
  scale_color_manual(values=c("violet","gold","lightblue","darkblue","red")) +
  ylab("Yellow-bellied Sapsucker") +
  my.theme + ylim(0,max(ybsa$MeanDens))+
  xlab("Year")

tiff('ALCESOnline_DensityChange.tiff', units="in", width=15, height=15, res=300)
p3 <- grid.arrange(arrangeGrob(gg.attw + theme(legend.position="none"),
                               gg.bbwa + theme(legend.position="none"),
                               gg.bbwo + theme(legend.position="none"),
                               gg.btnw + theme(legend.position="none"),
                               gg.blpw + theme(legend.position="none"),
                               gg.boch + theme(legend.position="none"),
                               gg.brcr + theme(legend.position="none"),
                               gg.cawa + theme(legend.position="none"),
                               gg.cmwa + theme(legend.position="none"),
                               gg.evgr + theme(legend.position="none"),
                               gg.nofl + theme(legend.position="none"),
                               gg.osfl + theme(legend.position="none"),
                               gg.oven + theme(legend.position="none"),
                               gg.pawa + theme(legend.position="none"),
                               gg.piwo + theme(legend.position="none"),
                               gg.rubl + theme(legend.position="none"),
                               gg.weta + theme(legend.position="none"),
                               gg.wewp + theme(legend.position="none"),
                               gg.wwcr + theme(legend.position="none"),
                               gg.ybsa + theme(legend.position="none"),
                               nrow=5, ncol=4))
dev.off()


