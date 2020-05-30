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

#str_name<-'BRCR_BAU_Year0.tif' 
r0.brcr.bau=raster(paste0(DIR,"/prediction rasters/","BRCR_BAU_Year0.tif"))
r50.brcr.bau=raster(paste0(DIR,"/prediction rasters/","BRCR_BAU_Year50.tif"))

r0.brcr.0fire=raster(paste0(DIR,"/prediction rasters/","BRCR_NoFire_Year0.tif"))
r50.brcr.0fire=raster(paste0(DIR,"/prediction rasters/","BRCR_NoFire_Year50.tif"))

r0.brcr.2xfire=raster(paste0(DIR,"/prediction rasters/","BRCR_IncreasedFire_Year0.tif"))
r50.brcr.2xfire=raster(paste0(DIR,"/prediction rasters/","BRCR_IncreasedFire_Year50.tif"))

#Basic rasterVis plot
levelplot(r0.brcr.bau, margin=F)

#Year 0 (BAU scenario)
r0.brcr.bau_df <- as.data.frame(r0.brcr.bau, xy = TRUE)
str(r0.brcr.bau_df)
r0.brcr.bau_df$meandens = ifelse(r0.brcr.bau_df$BRCR_BAU_Year0<0.125,0,
                                     ifelse(r0.brcr.bau_df$BRCR_BAU_Year0<0.375,0.25,
                                            ifelse(r0.brcr.bau_df$BRCR_BAU_Year0<0.625,0.5,
                                                   ifelse(r0.brcr.bau_df$BRCR_BAU_Year0<0.75,0.625,
                                                          ifelse(r0.brcr.bau_df$BRCR_BAU_Year0<0.875,0.75,
                                                                 ifelse(r0.brcr.bau_df$BRCR_BAU_Year0<1,0.875,
                                                                        ifelse(r0.brcr.bau_df$BRCR_BAU_Year0<1.5,1,
                                                                               ifelse(r0.brcr.bau_df$BRCR_BAU_Year0<2.5,2,
                                                                                      ifelse(r0.brcr.bau_df$BRCR_BAU_Year0<3.5,3,4)))))))))

CRS.new<-crs(r0.brcr.bau)
r0.brcr.bau<-rasterFromXYZ(r0.brcr.bau_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)
#get rid of box, axes, axis values
levelplot(r0.brcr.bau, margin=F, par.settings = list(axis.line = list(col = "transparent"), 
                                               strip.background = list(col = 'transparent'), 
                                               strip.border = list(col = 'transparent')), scales = list(col = "transparent"))



#change raster colours
mapTheme<-rasterTheme(region = brewer.pal(8, "Greens"))

levelplot(r0.brcr.bau, margin=F, 
          par.settings = list(axis.line = list(col = "transparent"), 
                                             strip.background = list(col = 'transparent'), 
                                             strip.border = list(col = 'transparent')), scales = list(col = "transparent"))

levelplot(r0.brcr.bau, margin=F, par.settings=BuRdTheme())

#Year 50 (BAU scenario)
r50.brcr.bau_df <- as.data.frame(r50.brcr.bau, xy = TRUE)
str(r50.brcr.bau_df)
r50.brcr.bau_df$meandens = ifelse(r50.brcr.bau_df$BRCR_BAU_Year50<0.125,0,
                                 ifelse(r50.brcr.bau_df$BRCR_BAU_Year50<0.375,0.25,
                                        ifelse(r50.brcr.bau_df$BRCR_BAU_Year50<0.625,0.5,
                                               ifelse(r50.brcr.bau_df$BRCR_BAU_Year50<0.75,0.625,
                                                      ifelse(r50.brcr.bau_df$BRCR_BAU_Year50<0.875,0.75,
                                                             ifelse(r50.brcr.bau_df$BRCR_BAU_Year50<1,0.875,
                                                                    ifelse(r50.brcr.bau_df$BRCR_BAU_Year50<1.5,1,
                                                                           ifelse(r50.brcr.bau_df$BRCR_BAU_Year50<2.5,2,
                                                                                  ifelse(r50.brcr.bau_df$BRCR_BAU_Year50<3.5,3,4)))))))))

CRS.new<-crs(r50.brcr.bau)
r50.brcr.bau<-rasterFromXYZ(r50.brcr.bau_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)

#Year 50 - Year 0 (BAU scenario)
bau<-levelplot(r50.brcr.bau-r0.brcr.bau, margin=F,
               par.settings = rasterTheme(region=magma(10), axis.line = list(col = "transparent"), 
                                   strip.background = list(col = 'transparent'), 
                                   strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
               main=list(label='Current Fire: Year 50 - Year 0',cex=0.7))

#Plot change over time
#Year 0
r0.brcr.2xfire_df <- as.data.frame(r0.brcr.2xfire, xy = TRUE)
str(r0.brcr.2xfire_df)
r0.brcr.2xfire_df$meandens = ifelse(r0.brcr.2xfire_df$BRCR_IncreasedFire_Year0<0.125,0,
                                 ifelse(r0.brcr.2xfire_df$BRCR_IncreasedFire_Year0<0.375,0.25,
                                        ifelse(r0.brcr.2xfire_df$BRCR_IncreasedFire_Year0<0.625,0.5,
                                               ifelse(r0.brcr.2xfire_df$BRCR_IncreasedFire_Year0<0.75,0.625,
                                                      ifelse(r0.brcr.2xfire_df$BRCR_IncreasedFire_Year0<0.875,0.75,
                                                             ifelse(r0.brcr.2xfire_df$BRCR_IncreasedFire_Year0<1,0.875,
                                                                    ifelse(r0.brcr.2xfire_df$BRCR_IncreasedFire_Year0<1.5,1,
                                                                           ifelse(r0.brcr.2xfire_df$BRCR_IncreasedFire_Year0<2.5,2,
                                                                                  ifelse(r0.brcr.2xfire_df$BRCR_IncreasedFire_Year0<3.5,3,4)))))))))

CRS.new<-crs(r0.brcr.2xfire)
r0.brcr.2xfire<-rasterFromXYZ(r0.brcr.2xfire_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)

#Year 50
r50.brcr.2xfire_df <- as.data.frame(r50.brcr.2xfire, xy = TRUE)
str(r50.brcr.2xfire_df)
r50.brcr.2xfire_df$meandens = ifelse(r50.brcr.2xfire_df$BRCR_IncreasedFire_Year50<0.125,0,
                                 ifelse(r50.brcr.2xfire_df$BRCR_IncreasedFire_Year50<0.375,0.25,
                                        ifelse(r50.brcr.2xfire_df$BRCR_IncreasedFire_Year50<0.625,0.5,
                                               ifelse(r50.brcr.2xfire_df$BRCR_IncreasedFire_Year50<0.75,0.625,
                                                      ifelse(r50.brcr.2xfire_df$BRCR_IncreasedFire_Year50<0.875,0.75,
                                                             ifelse(r50.brcr.2xfire_df$BRCR_IncreasedFire_Year50<1,0.875,
                                                                    ifelse(r50.brcr.2xfire_df$BRCR_IncreasedFire_Year50<1.5,1,
                                                                           ifelse(r50.brcr.2xfire_df$BRCR_IncreasedFire_Year50<2.5,2,
                                                                                  ifelse(r50.brcr.2xfire_df$BRCR_IncreasedFire_Year50<3.5,3,4)))))))))

CRS.new<-crs(r50.brcr.2xfire)
r50.brcr.2xfire<-rasterFromXYZ(r50.brcr.2xfire_df[,c("x","y","meandens")],res=c(200,200),crs=CRS.new,digits=2)

#Year 50-0
inc.fire<-levelplot(r50.brcr.2xfire-r0.brcr.2xfire, margin=F,
                    par.settings = rasterTheme(region=magma(10), axis.line = list(col = "transparent"), 
                                               strip.background = list(col = 'transparent'), 
                                               strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
                    main=list(label='Increased Fire: Year 50 - Year 0',cex=0.7))



#Year 50 Increased Fire -Year 50 BAU
inc.fire.minus.bau<-levelplot(r50.brcr.2xfire-r50.brcr.bau, margin=F,
                    par.settings = rasterTheme(region=magma(10), axis.line = list(col = "transparent"), 
                                               strip.background = list(col = 'transparent'), 
                                               strip.border = list(col = 'transparent')), scales = list(col = "transparent"),
                    main=list(label='Year 50: Increased Fire - BAU',cex=0.7))



lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0.5))
)

# bau$par.settings$layout.heights[
#   c( 'bottom.padding',
#      'top.padding',
#      'key.sub.padding',
#      'axis.xlab.padding',
#      'key.axis.padding',
#      'main.key.padding') ] <- -1
# bau$aspect.fill <- TRUE
# 
# inc.fire$par.settings$layout.heights[
#   c( 'bottom.padding',
#      'top.padding',
#      'key.sub.padding',
#      'axis.xlab.padding',
#      'key.axis.padding',
#      'main.key.padding') ] <- -1
# inc.fire$aspect.fill <- TRUE

tg<-textGrob("Change in Brown Creeper Density",gp=gpar(fontsize=10,font=1))
margin <- unit(0.5, "line")
grided<-grid.arrange(bau, inc.fire, bau, ncol=2)

tiff('ALCESOnline_BrownCreeperDensityMap.tiff', units="in", width=15, height=15, res=300)

grid.arrange(tg, grided,
             heights = unit.c(grobHeight(tg) + 1.2*margin, 
                          unit(1,"null")))
dev.off()

