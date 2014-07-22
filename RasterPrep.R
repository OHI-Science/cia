
#######################################################################
### NCEAS/OHI
### R script to replace the 0 values for land/border with NAs
### Melanie Frazier Dec 3, 2013
### Modified: May 2014
#######################################################################

rm(list = ls())

## Downloading packages
library(rgdal)
library(raster)
library(sp)
library(maptools)
library(rgeos)
library(plyr)
library(rasterVis)
library(RColorBrewer)


############################################################################
## prepare rasters:
## Creating raster files with land and boundaries converted to NA
## The first method using a mask is no longer used here because it is slow
############################################################################

### One odd thing is that the border and land regions are zero values rather than NA.  For plotting and analysis it is better to 
### convert these regions to NA (and saved as a new raster).  This was done using the land layer to clip out the data. 

# ### There are two methods for doing this (2nd one is better).  
# ### Method 1 (ARCHIVE): create a mask using the shape file:
# # need land layer for plot and clipping rasters (this is used to convert the land and boundary data to NA - rather than 0)
# rgn_ocn_cntry <- readOGR("N:\\model\\GL-NCEAS-OceanRegions_v2013a\\data", layer="rgn_ocean_cntry_mol")
# land <- rgn_ocn_cntry[!is.na(rgn_ocn_cntry@data$ISO_3digit) & rgn_ocn_cntry@data$rgn_id==0,]
# clipRegion  <-  rgn_ocn_cntry[!(!is.na(rgn_ocn_cntry@data$ISO_3digit) & rgn_ocn_cntry@data$rgn_id==0),]
# rasterize(clipRegion, plumes_fert_combo, mask=TRUE, 
#           file="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\plumes_fert_combo_clip",
#           progress="text")

### Method 2: Create a raster of the clip area and use raster math to remove
# create a raster of the land and outer areas that will be clipped.
#clipRegion@data$clip <- 1
#rasterize(clipRegion, global_cumulative_impact_2013_minus_2008, 
#          field="clip", filename="C:\\Users\\frazier\\Desktop\\pressures\\clip_raster", 
#          overwrite=TRUE, progress="text")

clip_raster <- raster("/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/clip_raster")
#plot(clip_raster)

## Function to clip:
clipLayer <- function(layer, file){
  #layer="artisanal_fishing_combo"
  #file=saveLocation
  s <- stack(get(layer), clip_raster)
  overlay(s, fun=function(x,y) x*y, 
          filename=paste(file, layer, sep=""),
          progress="text", overwrite=TRUE)
}

##----------------------------------------------------------------------------------------------------
## 2013 data ----
##--------------------------------------------------------------------------------------------------
# get dir_halpern2008
source('~/ohiprep/src/R/common.R')
setwd(file.path(dir_halpern2008, 
                '/mnt/storage/marine_threats/impact_layers_2013_redo',
                '/global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems'))

saveLocation <- "/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/TrimmedPressureLayers/Pressures2013/"

# load and check pressures layers
artisanal_fishing_combo <- raster("by_threat/artisanal_fishing_combo.tif")
clipLayer("artisanal_fishing_combo", saveLocation)

demersal_destructive_fishing_combo <- raster("by_threat/demersal_destructive_fishing_combo.tif")
clipLayer("demersal_destructive_fishing_combo", saveLocation)

demersal_nondest_high_bycatch_combo <- raster("by_threat/demersal_nondest_high_bycatch_combo.tif")
clipLayer("demersal_nondest_high_bycatch_combo", saveLocation)

demersal_nondest_low_bycatch_combo <- raster("by_threat/demersal_nondest_low_bycatch_combo.tif")
clipLayer("demersal_nondest_low_bycatch_combo", saveLocation)

inorganic_combo <- raster("by_threat/inorganic_combo.tif")
clipLayer("inorganic_combo", saveLocation)

invasives_combo <- raster("by_threat/invasives_combo.tif")
clipLayer("invasives_combo", saveLocation)

night_lights_combo <- raster("by_threat/night_lights_combo.tif")
clipLayer("night_lights_combo", saveLocation)

ocean_acidification_combo <- raster("by_threat/ocean_acidification_combo.tif")
clipLayer("ocean_acidification_combo", saveLocation)

ocean_pollution_combo <- raster("by_threat/ocean_pollution_combo.tif")
clipLayer("ocean_pollution_combo", saveLocation)

oil_rigs_combo <- raster("by_threat/oil_rigs_combo.tif")
clipLayer("oil_rigs_combo", saveLocation)

pelagic_high_bycatch_combo <- raster("by_threat/pelagic_high_bycatch_combo.tif")
clipLayer("pelagic_high_bycatch_combo", saveLocation)

pelagic_low_bycatch_combo <-  raster("by_threat/pelagic_low_bycatch_combo.tif")
clipLayer("pelagic_low_bycatch_combo", saveLocation)

plumes_fert_combo <- raster("by_threat/plumes_fert_combo.tif")
clipLayer("plumes_fert_combo", saveLocation)

plumes_pest_combo <- raster("by_threat/plumes_pest_combo.tif")
clipLayer("plumes_pest_combo", saveLocation)

population_combo <- raster("by_threat/population_combo.tif")
clipLayer("population_combo", saveLocation)

shipping_combo <- raster("by_threat/shipping_combo.tif")
clipLayer("shipping_combo", saveLocation)

slr_combo <- raster("by_threat/slr_combo.tif")
clipLayer("slr_combo", saveLocation)

sst_combo <- raster("by_threat/sst_combo.tif")
clipLayer("sst_combo", saveLocation)

uv_combo <- raster("by_threat/uv_combo.tif")
clipLayer("uv_combo", saveLocation)

global_cumulative_impact_2013_all_layers <- raster("all_layers/global_cumul_impact_2013_all_layers.tif")
clipLayer("global_cumulative_impact_2013_all_layers", saveLocation)

##----------------------------------------------------------------------------------------------------
## 2008 data ----
##--------------------------------------------------------------------------------------------------
# get dir_halpern2008
source('~/ohiprep/src/R/common.R')
setwd(file.path(dir_halpern2008, 
                '/mnt/storage/marine_threats/impact_layers_2013_redo',
                '/global_impact_model_2008/normalized_by_two_time_periods/averaged_by_num_ecosystems'))

saveLocation <- "/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/TrimmedPressureLayers/Pressures2008/"

# load and check pressures layers
global_cumulative_impact_2008_all_layers <- raster("all_layers_except_shipping_oceanpollution_invasives/cumulative/global_cumul_impact_2008_all_layers_except_shipping_oceanpollution_invasives.tif")
clipLayer("global_cumulative_impact_2008_all_layers", saveLocation)



##----------------------------------------------------------------------------------------------------
## 2013 minus 2008 data ----
##--------------------------------------------------------------------------------------------------
# get dir_halpern2008
source('~/ohiprep/src/R/common.R')
setwd(file.path(dir_halpern2008,
                '/mnt/storage/marine_threats/impact_layers_2013_redo',
                '/global_impact_difference_2008_2013/normalized_by_two_time_periods/averaged_by_num_ecosystems'))

saveLocation <- "/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/TrimmedPressureLayers/Pressures2013minus2008/"

### No data for: artisanal_fishing, inorganic_combo, and ocean_acidification_combo because they use the same data in 2008 and 2013
### No data for: invasives_combo, ocean_pollution_combo, shipping_combo, slr_combo because the techniques used to generate the data
###             were not comparable.

# load and check pressures layers
# artisanal_fishing_combo_dif <- raster("by_threat/artisanal_fishing_combo_2013_minus_2008.tif")
# clipLayer("artisanal_fishing_combo_dif", saveLocation)

demersal_destructive_fishing_combo_dif <- raster("by_threat/demersal_destructive_fishing_combo_2013_minus_2008.tif")
clipLayer("demersal_destructive_fishing_combo_dif", saveLocation)

demersal_nondest_high_bycatch_combo_dif <- raster("by_threat/demersal_nondest_high_bycatch_combo_2013_minus_2008.tif")
clipLayer("demersal_nondest_high_bycatch_combo_dif", saveLocation)

demersal_nondest_low_bycatch_combo_dif <- raster("by_threat/demersal_nondest_low_bycatch_combo_2013_minus_2008.tif")
clipLayer("demersal_nondest_low_bycatch_combo_dif", saveLocation)

# inorganic_combo_dif <- raster("by_threat/inorganic_combo_2013_minus_2008.tif")
# clipLayer("inorganic_combo_dif", saveLocation)

# invasives_combo_dif <- raster("by_threat/invasives_combo_2013_minus_2008.tif")
# clipLayer("invasives_combo_dif", saveLocation)

night_lights_combo_dif <- raster("by_threat/night_lights_combo_2013_minus_2008.tif")
clipLayer("night_lights_combo_dif", saveLocation)

# ocean_acidification_combo_dif <- raster("by_threat/ocean_acidification_combo_2013_minus_2008.tif")
# clipLayer("ocean_acidification_combo_dif", saveLocation)
# 
# ocean_pollution_combo_dif <- raster("by_threat/ocean_pollution_combo_2013_minus_2008.tif")
# clipLayer("ocean_pollution_combo_dif", saveLocation)

oil_rigs_combo_dif <- raster("by_threat/oil_rigs_combo_2013_minus_2008.tif")
clipLayer("oil_rigs_combo_dif", saveLocation)

pelagic_high_bycatch_combo_dif <- raster("by_threat/pelagic_high_bycatch_combo_2013_minus_2008.tif")
clipLayer("pelagic_high_bycatch_combo_dif", saveLocation)

pelagic_low_bycatch_combo_dif <-  raster("by_threat/pelagic_low_bycatch_combo_2013_minus_2008.tif")
clipLayer("pelagic_low_bycatch_combo_dif", saveLocation)

plumes_fert_combo_dif <- raster("by_threat/plumes_fert_combo_2013_minus_2008.tif")
clipLayer("plumes_fert_combo_dif", saveLocation)

plumes_pest_combo_dif <- raster("by_threat/plumes_pest_combo_2013_minus_2008.tif")
clipLayer("plumes_pest_combo_dif", saveLocation)

population_combo_dif <- raster("by_threat/population_combo_2013_minus_2008.tif")
clipLayer("population_combo_dif", saveLocation)

# shipping_combo_dif <- raster("by_threat/shipping_combo_2013_minus_2008.tif")
# clipLayer("shipping_combo_dif", saveLocation)

# slr_combo_dif <- raster("by_threat/slr_combo_2013_minus_2008.tif")
# clipLayer("slr_combo_dif", saveLocation)

sst_combo_dif <- raster("by_threat/sst_combo_2013_minus_2008.tif")
clipLayer("sst_combo_dif", saveLocation)

uv_combo_dif <- raster("by_threat/uv_combo_2013_minus_2008.tif")
clipLayer("uv_combo_dif", saveLocation)

global_cumulative_impact_dif <- raster("cumulative/global_cumul_impact_2013_minus_2008.tif")
clipLayer("global_cumulative_impact_dif", saveLocation)









## maybe relevant...maybe not:

### cumulative impact of specific combinations of layers ###

# Fishing pressures
setwd("C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters")

s <- stack(
  artisanal_fishing_combo <- raster("artisanal_fishing_combo_clip"),
  demersal_destructive_fishing_combo <- raster("demersal_destructive_fishing_combo_clip"),
  demersal_nondest_high_bycatch_combo <- raster("demersal_nondest_high_bycatch_combo_clip"),
  demersal_nondest_low_bycatch_combo <- raster("demersal_nondest_low_bycatch_combo_clip"),
  pelagic_high_bycatch_combo <- raster("pelagic_high_bycatch_combo_clip"),
  pelagic_low_bycatch_combo <-  raster("pelagic_low_bycatch_combo_clip"))

r5 <- overlay(s, fun=function(a,b,c,d,e,f) a+b+c+d+e+f, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\FishingPressures",
              progress="text", overwrite=TRUE)

# Land-based pollution pressures
s <- stack(
  inorganic_combo <- raster("inorganic_combo_clip"),
  plumes_fert_combo <- raster("plumes_fert_combo_clip"),
  plumes_pest_combo <- raster("plumes_pest_combo_clip")
)
r5 <- overlay(s, fun=function(a,b,c) a+b+c, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\LandBasedPollutionPressures",
              progress="text", overwrite=TRUE)

# Climate change pressures
s <- stack(
  ocean_acidification_combo <- raster("ocean_acidification_combo_clip"),
  slr_combo <- raster("slr_combo_clip"),
  sst_combo <- raster("sst_combo_clip"),
  uv_combo <- raster("uv_combo_clip"))
r5 <- overlay(s, fun=function(a,b,c,d) a+b+c+d, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\ClimateChangePressures",
              progress="text", overwrite=TRUE)

# Other pressures
s <- stack(
  invasives_combo <- raster("invasives_combo_clip"),
  night_lights_combo <- raster("night_lights_combo_clip"),
  ocean_pollution_combo <- raster("ocean_pollution_combo_clip"),
  oil_rigs_combo <- raster("oil_rigs_combo_clip"),
  population_combo <- raster("population_combo_clip"),
  shipping_combo <- raster("shipping_combo_clip")
)
r5 <- overlay(s, fun=function(a,b,c,d,e,f) a+b+c+d+e+f, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\OtherPressures",
              progress="text", overwrite=TRUE)


## Percent contribution of each pressure layer to total pressure
s <- stack(raster("artisanal_fishing_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\artisanal_fishing_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("demersal_destructive_fishing_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\demersal_destructive_fishing_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("demersal_nondest_high_bycatch_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\demersal_nondest_high_bycatch_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("demersal_nondest_low_bycatch_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\demersal_nondest_low_bycatch_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("pelagic_high_bycatch_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\pelagic_high_bycatch_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("pelagic_low_bycatch_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\pelagic_low_bycatch_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("pelagic_low_bycatch_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\pelagic_low_bycatch_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("inorganic_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\inorganic_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("plumes_fert_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\plumes_fert_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("plumes_pest_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\plumes_pest_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("ocean_acidification_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\ocean_acidification_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("slr_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\slr_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("sst_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\sst_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("uv_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\uv_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("population_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\population_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("shipping_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\shipping_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("invasives_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\invasives_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("night_lights_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\night_lights_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("ocean_pollution_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\ocean_pollution_combo_percent",
              progress="text", overwrite=TRUE)

s <- stack(raster("oil_rigs_combo_clip"),
           raster("global_cumulative_impact_2013_all_layers_clip"))
r5 <- overlay(s, fun=function(a,b) (a/b)*100, 
              filename="C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters\\oil_rigs_combo_percent",
              progress="text", overwrite=TRUE)

