###########################################################
###TWAP figures
###########################################################

rm(list = ls())

## Downloading packages
library(rgdal)
library(raster)
library(sp)
library(maptools)
library(rgeos)
library(plotKML)
library(plyr)
library(rasterVis)
library(RColorBrewer)
library(fields)

source('~/ohiprep/src/R/common.R')

path_save <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ResultMaps/TWAP")

myFiles = file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013")


# land/ocean layers for plots ----
# rgn_ocn_cntry <- readOGR("/var/data/ohi/model/GL-NCEAS-OceanRegions_v2013a/data", layer="rgn_ocean_cntry_mol")
#rgn_ocn_cntry <- readOGR("/var/data/ohi/model/GL-NCEAS-OceanRegions_v2013a/data", layer="rgn_ocean_cntry_gcs")
rgn_ocn_cntry <- readOGR("/var/data/ohi/model/GL-NCEAS-OceanRegions_v2013a/data", layer="rgn_ocean_cntry_gcs_mer_neg150")
ocean  <-  rgn_ocn_cntry[is.na(rgn_ocn_cntry@data$ISO_3digit) & rgn_ocn_cntry@data$rgn_id==0,]
land  <-  rgn_ocn_cntry[!is.na(rgn_ocn_cntry@data$ISO_3digit) & rgn_ocn_cntry@data$rgn_id==0,]
rgnOHI <-  rgn_ocn_cntry[rgn_ocn_cntry@data$rgn_id>0,]

plot(ocean, col="blue")

# LME plots ----
data <- read.csv('ZonalExtractionData/oneYearNorm_2013_lme.csv') 


# LME regions for plots
# lme <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/RegionMaps/newLME_Mollweide",
#                 layer="newLME_Mollweide") 

# lme <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/TWAP LME summary/InputData/newLME_wgs",
#                 layer="newLME") 
lme  <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/TWAP LME summary/InputData/LME_wgs_mer_neg150", "LME_wgs_mer_neg150")

lme@data <- join(lme@data, data, by=c("LME_NAME", "LME_NUMBER"))

# warm <- readOGR(dsn=file.path(myFiles, "RegionMaps/WARM"), layer="WARM_mol")
#warm <- readOGR(dsn=file.path(myFiles, "RegionMaps/WARM"), layer="WARM")
warm <- readOGR(dsn=file.path(myFiles, "RegionMaps/WARM"), layer="WARM_gcs_mer_neg150")


data_warm <- read.csv('ZonalExtractionData/oneYearNorm_2013_warm.csv') %>%
  mutate(ProvCode= "WARM") %>%
  mutate(global_cumul_impact_2013_all_layers=round(global_cumul_impact_2013_all_layers, 3))
warm@data <- join(warm@data, data_warm)

col.brks=c(0, 2.95, 3.50, 3.86, 4.31, 10)
colors <- c('#92CDDC70', '#92D05070', '#FFFF0070', '#FFC00070', '#FF000070')

# function 
plotCHI <- function(fig=file.path(path_save, "CHI_LME_wgs_mer_neg150.png"), shapeFile=lme, var=lme@data$global_cumul_impact_2013_all_layers){
  png(file=fig, width=1200, height=800, res=150, pointsize=18, type='cairo')
 par(oma=c(0,0,0,0),
      mar=c(1.5,1,0,1))
  plot(ocean, col='gray90', border=NA)
  plot(rgnOHI, col='gray90', border=NA, add=TRUE)
  plot(land, col='gray80', border='grey75', add=TRUE)
  plot(lme, border='grey75', add=TRUE,
       col=colors[cut(var, col.brks, labels=1:5, right=FALSE)])
  plot(warm, border='grey75', add=TRUE,
       col=colors[cut(warm@data$global_cumul_impact_2013_all_layers, col.brks, labels=1:5, right=FALSE)])
  
  p=par('usr'); px=diff(p[1:2]); py=diff(p[3:4]) # c(x1, x2, y1, y2)
  
  # add label
  #text(x=p[1]+px*0.02, y=p[4]-py*0.1, labels="CHI category", pos=4) # +p.dx*0.1
  
  # plot legend
  yh=0.05; x1=p[1]+px*0.2; x2=p[2]-px*0.2; y1=p[3]; y2=p[3]+py*yh
  ix = seq(x1, x2, length.out=5)
  ixd = diff(ix)[1]/2
  par(xpd=TRUE) # turn off clipping to plot region
  image(x = ix,
        y = c(y1, y2),
        z = matrix(1:5), col=colors, add=T)
  rect(x1-ixd,y1,x2+ixd,y2, border='gray20')
  text(x = ix,
      y = y1, cex=1, pos=1, # adj=c(0.5,0), # , offset=0.1,
      labels=c(1:5)) 
 
 par(xpd=F) # turn back on clipping to plot region
  dev.off()
}
plotCHI()


data2 <- data %>% 
  mutate(cats=cut(data$global_cumul_impact_2013_all_layers, col.brks, labels=1:5, right=FALSE)) %>%
  select(region=LME_NAME, cats, score=global_cumul_impact_2013_all_layers)

data2 %>%
  arrange(score)
table(data2$cats)

data_warm$cats = cut(data_warm$global_cumul_impact_2013_all_layers, col.brks, labels=1:5, right=FALSE)




 # FAO plots ----
fao_data <- read.csv('ZonalExtractionData/oneYearNorm_2013_fao.csv')

# LME regions for plots
# fao <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/RegionMaps/FAO_regions",
#                layer="FAO_rgns_mol")

set_ll_warn(FALSE)
set_ll_TOL(0.1)

fao <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/RegionMaps/FAO_regions",
               layer="FAO_regions_gcs_mer_neg150")
fao@data <- join(fao@data, fao_data, by=c("rgn_id", "rgn_nam"))

col.brks=0:5
colors <- c('#1E90FF70', '#92D05070', '#FFFF0070', '#FFC00070', '#FF000070')
#1E90FF70 #cadetblue
#104E8B70
# function 
plotCHI_fao <- function(fig=file.path(path_save, "CHI_FAO_wgs_mer_neg150.png"), shapeFile=fao, var=fao@data$global_cumul_impact_2013_all_layers){
  png(file=fig, width=1200, height=800, res=150, pointsize=18, type='cairo')
  par(oma=c(0,0,0,0),
      mar=c(1.5,1,0,1))
  plot(ocean, col='gray90', border=NA)
  plot(rgnOHI, col='gray90', border=NA, add=TRUE)
  plot(land, col='gray80', border='grey75', add=TRUE)
  plot(fao, border='grey75', add=TRUE,
       col=colors[cut(var, col.brks, labels=1:5)])
  
  p=par('usr'); px=diff(p[1:2]); py=diff(p[3:4]) # c(x1, x2, y1, y2)
  
  # add label
  #text(x=p[1]+px*0.02, y=p[4]-py*0.1, labels="CHI category", pos=4) # +p.dx*0.1
  
  # plot legend
  yh=0.05; x1=p[1]+px*0.2; x2=p[2]-px*0.2; y1=p[3]; y2=p[3]+py*yh
  ix = seq(x1, x2, length.out=5)
  ixd = diff(ix)[1]/2
  par(xpd=TRUE) # turn off clipping to plot region
  image(x = ix,
        y = c(y1, y2),
        z = matrix(1:5), col=colors, add=T)
  rect(x1-ixd,y1,x2+ixd,y2, border='gray20')
  text(x = ix,
       y = y1, cex=1, pos=1, # adj=c(0.5,0), # , offset=0.1,
       labels=c(1:5)) 
  par(xpd=F) # turn back on clipping to plot region
  dev.off()
}

plotCHI_fao()
#testing

 

