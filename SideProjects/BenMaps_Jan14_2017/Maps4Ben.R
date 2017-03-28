library(rgdal)
library(raster)
library(sp)
library(RColorBrewer)
library(fields)
#library(ggplot2) #don't load this unless it is needed for a particular plot (messes things up)
#library(grid) #needed for myTheme function
#library(spatial.tools)
#library(classInt)

source('~/ohiprep/src/R/common.R')

#paths:
path <- file.path(dir_M, 'git-annex/Global/NCEAS-Pressures-Summaries_frazier2013')
path_trim <- file.path(dir_M, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/TrimmedPressureLayers")
path_save <- file.path(dir_M, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ResultMaps")

rasters = file.path(dir_M, 
                    'marine_threats/impact_layers_2013_redo')


rgn_ocn_cntry <- readOGR(file.path(dir_M, "model/GL-NCEAS-OceanRegions_v2013a/data"), layer="rgn_ocean_cntry_mol")
land <- rgn_ocn_cntry[!is.na(rgn_ocn_cntry@data$ISO_3digit) & rgn_ocn_cntry@data$rgn_id==0,]


legend.shrink <- 0.4
legend.width <- 0.6


raster_defaultLegend <- function(raster_data, saveLoc, title_legend=NA, title_plot=NA, cols){
  #   par(mar=c(2,2,2,2))
  #   par(oma=c(0,0,0,4))
  png(file.path("/home/shares/web/pressures/htdocs/data", saveLoc), res=1200, width=20, height=20, units="in")  
  #pdf(file.path("/home/shares/web/pressures/htdocs/data", saveLoc))  #, width=1200, height=1000, res=150, pointsize=14)
  #jpeg(file.path("/home/shares/web/pressures/htdocs/data", saveLoc), res=1200, width=20, height=20, units="in")  
  
  plot(raster_data, col=cols, axes=FALSE, box=FALSE, 
       axis.args=list(cex.axis=.8), 
       legend.args=list(text=title_legend, font=2, line=1, cex=1))
  title(main=title_plot, line=-5)
  plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)  
  dev.off()
}


pressures <- dir(file.path(path_trim, "Pressures2013"))
pressures <- gsub(".grd", "", pressures)
pressures <- gsub(".gri", "", pressures)
pressures <- unique(pressures)

for(pressure in pressures){ # pressure="artisanal_fishing_combo"
  
  diff_noLand <- raster(file.path(path_trim, sprintf("Pressures2013/%s", pressure)))
  
  raster_defaultLegend(raster_data=diff_noLand, 
                     saveLoc= sprintf("BenFigures_1_15_2017/%s.png", pressure), 
                     title_legend=pressure,
                     cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(250)))
}

#######################################################
### A cumulative impact map with hotspots empahsized
#######################################################

raster_breaks <- function(raster_data, saveLoc, title, title_legend=NULL, myBreaks, cols){
  #   par(mar=c(2,2,2,2))
  #   par(oma=c(0,0,0,4))
  png(file.path("/home/shares/web/pressures/htdocs/data", saveLoc), res=1200, width=20, height=20, units="in")  
  plot(raster_data, col=cols, axes=FALSE, box=FALSE, breaks=myBreaks, legend=FALSE)
  # add axis with fields package function:
  def_breaks <- seq(0, length(myBreaks), length.out=length(myBreaks))
  image.plot(raster_data, #zlim = c(min(myBreaks), max(myBreaks)), 
             legend.only = TRUE, 
             col = cols,
             legend.lab=title_legend,
             breaks=def_breaks,
             lab.breaks=round(myBreaks, 1),
             axis.args = list(cex.axis = 0.8))
  
  plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
  
  dev.off()
}


my_breaks <- c(0, 1.781894, 2.377814, 2.986494, 3.316144, 3.558642, 3.750878, 
               3.923132, 4.127960, 4.384074, 4.571275, 16)
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)+2))[2:12] #stripping extreme ends of the color spectrum

diff_noLand <- raster(file.path(path_trim, 
                                "Pressures2013/global_cumulative_impact_2013_all_layers"))

raster_breaks(raster_data=diff_noLand, 
              saveLoc="BenFigures_1_15_2017/CumulativeImpacts_hotspots.png", 
              myBreaks=my_breaks, cols=cols)

#########################################################
### need to zip figures so they will download
#########################################################

setwd("/home/shares/web/pressures/htdocs/data/BenFigures_1_15_2017")

figures <- dir()

zip(zipfile = "CumulativeImpacts", 
    files = figures)