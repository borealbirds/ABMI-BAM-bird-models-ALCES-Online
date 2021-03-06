library(dplyr)
library(tidyr)
library(raster)
library(maptools)
library(ncdf4)
library(rasterVis)
library(RColorBrewer)
library(zoo)
library(ggplot2)
library(gridExtra)
library(grid)
library(viridis)
my.theme <- theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1))

DIR<-getwd()

names<-c("Ovenbird",
         "WesternTanager") 

miat=c(-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4)
#this vector will be used to create a common set of values for the 
#colour legend in all four plots; otherwise, legends would be 
#independent and may give deceptive results when comparing scenarios

for (SPP in names){
  r0.bau=raster(paste0(DIR,"/prediction rasters/",SPP,"_BAU_Year0.tif"))
  r50.bau=raster(paste0(DIR,"/prediction rasters/",SPP,"_BAU_Year50.tif"))
  
  r0.0fire=raster(paste0(DIR,"/prediction rasters/",SPP,"_NoFire_Year0.tif"))
  r50.0fire=raster(paste0(DIR,"/prediction rasters/",SPP,"_NoFire_Year50.tif"))
  
  r0.0energy=raster(paste0(DIR,"/prediction rasters/",SPP,"_NoEnergy_Year0.tif"))
  r50.0energy=raster(paste0(DIR,"/prediction rasters/",SPP,"_NoEnergy_Year50.tif"))
  
  r0.2xfire=raster(paste0(DIR,"/prediction rasters/",SPP,"_IncreasedFire_Year0.tif"))
  r50.2xfire=raster(paste0(DIR,"/prediction rasters/",SPP,"_IncreasedFire_Year50.tif"))
  

  #Plot change over time for BAU Scenario
  #Year 0 (BAU Scenario)
  r0.bau_df <- as.data.frame(r0.bau, xy = TRUE)
  str(r0.bau_df)
  r0.bau_df$value<-r0.bau_df[,c(paste0(SPP,"_BAU_Year0"))]
  r0.bau_df$meandens = ifelse(is.na(r0.bau_df$value),"NA",
                              ifelse(r0.bau_df$value>4,4,r0.bau_df$value))
  
  CRS.new<-crs(r0.bau)
  r0.bau<-rasterFromXYZ(r0.bau_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)
  #get rid of box, axes, axis values
  levelplot(r0.bau, margin=F, par.settings = list(axis.line = list(col = "transparent"), 
                                                  strip.background = list(col = 'transparent'), 
                                                  strip.border = list(col = 'transparent')), scales = list(col = "transparent"))
  
  
  
  #change raster colours
  mapTheme<-rasterTheme(region = brewer.pal(8, "Greens"))
  
  levelplot(r0.bau, margin=F, 
            par.settings = list(axis.line = list(col = "transparent"), 
                                strip.background = list(col = 'transparent'), 
                                strip.border = list(col = 'transparent')), scales = list(col = "transparent"))
  
  levelplot(r0.bau, margin=F, par.settings=BuRdTheme())
  
  #Year 50 (BAU scenario)
  r50.bau_df <- as.data.frame(r50.bau, xy = TRUE)
  str(r50.bau_df)
  r50.bau_df$value<-r50.bau_df[,c(paste0(SPP,"_BAU_Year50"))]
  r50.bau_df$meandens = ifelse(is.na(r50.bau_df$value),"NA",
                               ifelse(r50.bau_df$value>4,4,r50.bau_df$value))
  
  
  CRS.new<-crs(r50.bau)
  r50.bau<-rasterFromXYZ(r50.bau_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)
  
  #Year 50 - Year 0 (BAU scenario)
  bau<-levelplot(r50.bau-r0.bau, margin=F,
                 par.settings = rasterTheme(region=magma(10), axis.line = list(col = "transparent"), 
                                            strip.background = list(col = 'transparent'), 
                                            strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
                 main=list(label='Current Fire: Year 50 - Year 0',cex=1),at=miat)
  
  #Plot change over time for Increased Fire Scenario
  #Year 0 (Increased Fire Scenario)
  r0.2xfire_df <- as.data.frame(r0.2xfire, xy = TRUE)
  str(r0.2xfire_df)
  r0.2xfire_df$value<-r0.2xfire_df[,c(paste0(SPP,"_IncreasedFire_Year0"))]
  r0.2xfire_df$meandens = ifelse(is.na(r0.2xfire_df$value),"NA",
                                 ifelse(r0.2xfire_df$value>4,4,r0.2xfire_df$value))
  
  
  CRS.new<-crs(r0.2xfire)
  r0.2xfire<-rasterFromXYZ(r0.2xfire_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)
  
  #Year 50 (Increased Fire Scenario)
  r50.2xfire_df <- as.data.frame(r50.2xfire, xy = TRUE)
  str(r50.2xfire_df)
  r50.2xfire_df$value<-r50.2xfire_df[,c(paste0(SPP,"_IncreasedFire_Year50"))]
  r50.2xfire_df$meandens = ifelse(is.na(r50.2xfire_df$value),"NA",
                                  ifelse(r50.2xfire_df$value>4,4,r50.2xfire_df$value))
  
  
  CRS.new<-crs(r50.2xfire)
  r50.2xfire<-rasterFromXYZ(r50.2xfire_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)
  
  #Year 50-0 (Increased Fire Scenario)
  inc.fire<-levelplot(r50.2xfire-r0.2xfire, margin=F,
                      par.settings = rasterTheme(region=magma(10), axis.line = list(col = "transparent"), 
                                                 strip.background = list(col = 'transparent'), 
                                                 strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
                      main=list(label='Increased Fire: Year 50 - Year 0',cex=1),at=miat)
  
  
  
  #Plot change over time for No Fire Scenario
  #Year 0 (No Fire Scenario)
  r0.0fire_df <- as.data.frame(r0.0fire, xy = TRUE)
  str(r0.0fire_df)
  r0.0fire_df$value<-r0.0fire_df[,c(paste0(SPP,"_NoFire_Year0"))]
  r0.0fire_df$meandens = ifelse(is.na(r0.0fire_df$value),"NA",
                                ifelse(r0.0fire_df$value>4,4,r0.0fire_df$value))
  
  
  CRS.new<-crs(r0.0fire)
  r0.0fire<-rasterFromXYZ(r0.0fire_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)
  
  #Year 50 (No Fire Scenario)
  r50.0fire_df <- as.data.frame(r50.0fire, xy = TRUE)
  str(r50.0fire_df)
  r50.0fire_df$value<-r50.0fire_df[,c(paste0(SPP,"_NoFire_Year50"))]
  r50.0fire_df$meandens = ifelse(is.na(r50.0fire_df$value),"NA",
                                 ifelse(r50.0fire_df$value>4,4,r50.0fire_df$value))
  
  
  CRS.new<-crs(r50.0fire)
  r50.0fire<-rasterFromXYZ(r50.0fire_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)
  
  #Year 50-0 (No Fire Scenario)
  no.fire<-levelplot(r50.0fire-r0.0fire, margin=F,
                     par.settings = rasterTheme(region=magma(10), axis.line = list(col = "transparent"), 
                                                strip.background = list(col = 'transparent'), 
                                                strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
                     main=list(label='No Fire: Year 50 - Year 0',cex=1),at=miat)
  
  
  
  
  #Plot change over time for No Energy Scenario
  #Year 0 (No Energy Scenario)
  r0.0energy_df <- as.data.frame(r0.0energy, xy = TRUE)
  str(r0.0energy_df)
  r0.0energy_df$value<-r0.0energy_df[,c(paste0(SPP,"_NoEnergy_Year0"))]
  r0.0energy_df$meandens = ifelse(is.na(r0.0energy_df$value),"NA",
                                  ifelse(r0.0energy_df$value>4,4,r0.0energy_df$value))
  
  
  CRS.new<-crs(r0.0energy)
  r0.0energy<-rasterFromXYZ(r0.0energy_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)
  
  #Year 50 (No Energy Scenario)
  r50.0energy_df <- as.data.frame(r50.0energy, xy = TRUE)
  str(r50.0energy_df)
  r50.0energy_df$value<-r50.0energy_df[,c(paste0(SPP,"_NoEnergy_Year50"))]
  r50.0energy_df$meandens = ifelse(is.na(r50.0energy_df$value),"NA",
                                   ifelse(r50.0energy_df$value>4,4,r50.0energy_df$value))
  
  
  CRS.new<-crs(r50.0energy)
  r50.0energy<-rasterFromXYZ(r50.0energy_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)
  
  #Year 50-0 (No Energy Scenario)
  no.energy<-levelplot(r50.0energy-r0.0energy, margin=F,
                       par.settings = rasterTheme(region=magma(10), axis.line = list(col = "transparent"), 
                                                  strip.background = list(col = 'transparent'), 
                                                  strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
                       main=list(label='No Energy: Year 50 - Year 0',cex=1),at=miat)
  
  
  
  
  lattice.options(
    layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
    layout.widths=list(left.padding=list(x=0), right.padding=list(x=0.5))
  )
  
  
  tg<-textGrob(paste0("Change in ",SPP," Density"),gp=gpar(fontsize=15,font=1))
  margin <- unit(0.5, "line")
  grided<-grid.arrange(bau, inc.fire, no.fire, no.energy, nrow=2, ncol=2)
  
  tiff(paste0("species maps/ALCESOnline_",SPP,"DensityMap.tiff"), units="in", width=15, height=15, res=300)
  
  grid.arrange(tg, grided,
               heights = unit.c(grobHeight(tg) + 1.2*margin, 
                                unit(1,"null")))
  dev.off()
  print(paste0("Map printed for ",SPP))
}

