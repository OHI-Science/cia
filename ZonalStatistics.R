#######################################################################
### NCEAS/OHI
### R script to obtain pressures for each lme/fao/meow/ region
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

source('~/ohiprep/src/R/common.R')
dir_out = '/home/frazier/cia/ZonalExtractionData'
rasters = file.path(dir_halpern2008, 
                    'mnt/storage/marine_threats/impact_layers_2013_redo')
myFiles = file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013")

if (!file.exists(dir_out)) dir.create(dir_out, showWarnings=F) #create the directory output file if it does not exist



# Originally performed using "extract" function...this takes
# forever, and is not practical for this.

### Another possible approach:
#1) Rasterize the polygons (to a single raster with the same dimensions as the input raster dataset). My polygons had a unique ID value that was burned into the raster. I did this with 'gdal_rasterize' 
#2) Use the 'crosstab' function in the 'raster' package to cross-tabulate the values from the ID raster with the values on the input raster dataset. This gives the full range of pixel values associated with each polygon ID. 
#3) From there, it is straightforward to apply whichever function to the pixel values for each ID.

################################################
## Preparing zone data ----
## (should only need to be done once)
################################################
#convert to raster
global_cumulative_impact_2013_minus_2008 <- raster(file.path(rasters, "global_impact_difference_2008_2013/normalized_by_two_time_periods/averaged_by_num_ecosystems/cumulative/global_cumul_impact_2013_minus_2008.tif"))
plot(global_cumulative_impact_2013_minus_2008)

## MEOW shp file: downloaded from http://www.marineregions.org/downloads.php
## on Jan 3, 2014 
#meow <- readOGR("C:\\Users\\frazier\\Desktop\\pressures\\MEOW", layer="meow_ecos_moll")
#plot(meow, add=TRUE)
#rasterize(meow, global_cumulative_impact_2013_minus_2008, field="ECO_CODE", filename="C:\\Users\\frazier\\Desktop\\pressures\\MEOW\\MEOW_raster", overwrite=TRUE)


## LME shp file: downloaded on Oct 17, 2013 
## from: http://www.lme.noaa.gov/index.php?option=com_content&view=article&id=177&Itemid=75
## modifed in script: C:\Users\Melanie\Desktop\NCEAS\Projects\LMEsummaryOct172013\Part1_LMEdataPrep.R
#lme <- readOGR(dsn="C:\\Users\\frazier\\Desktop\\pressures\\newLME_Mollweide", layer="newLME_Mollweide")
#plot(lme, add=TRUE)
#rasterize(lme, global_cumulative_impact_2013_minus_2008, 
#           field="LME_NUMBER", 
#           filename="C:\\Users\\frazier\\Desktop\\pressures\\newLME_Mollweide\\LME_raster", 
#           overwrite=TRUE,
#           progress="text")

## EEZ data:
#eez <- readOGR(dsn="N:\\model\\GL-NCEAS-OceanRegions_v2013a\\data", layer="eez_mol")
#plot(eez, add=TRUE)
#rasterize(eez, global_cumulative_impact_2013_minus_2008, 
#          field="eez_id", 
#          filename="C:\\Users\\frazier\\Desktop\\pressures\\EEZ\\EEZ_raster", 
#          overwrite=TRUE, 
#          progress="text")
# later associated with these data:
# N:\model\GL-NCEAS-OceanRegions_v2013a\data\eez_details.csv


## rgn areas removed and file transformed from gcs (wgs84) to mollweide
#hs <- readOGR(dsn="C:\\Users\\frazier\\Desktop\\pressures\\FAO_regions", layer="FAO_rgns_mol")
#plot(hs, add=TRUE)
#rasterize(hs, global_cumulative_impact_2013_minus_2008, 
#          field="rgn_id", 
#          filename="C:\\Users\\frazier\\Desktop\\pressures\\FAO_regions\\FAO_raster", 
#          overwrite=TRUE,
#          progress="text")

# file from Ben H. 8/20/2014 transformed from gcs (wgs84) to mollweide in ARC
warm <- readOGR(dsn=file.path(myFiles, "RegionMaps/WARM"), layer="WARM_mol")
plot(warm, add=TRUE)
rasterize(warm, global_cumulative_impact_2013_minus_2008, 
         field="ProvCode", 
         filename= file.path(myFiles, "RegionMaps/WARM/WARM_raster"), 
         overwrite=TRUE,
         progress="text")

plot(raster(file.path(myFiles, "RegionMaps/WARM/WARM_raster")))

############################################################################
## Altering extent (no longer necessary for current rasters)
############################################################################

# This was used to set the raster boundaries:
# meow_rast <- raster("C:\\Users\\frazier\\Desktop\\pressures\\MEOW\\MEOW_raster")

# plumes_fert_combo <- extend(plumes_fert_combo, meow_rast@extent)


############################################################################
## Functions to extract data for different zones
############################################################################
## MEOW
meow_extract <- function(stack=stack, fileOutput="test.csv"){
  meow_rast <- raster(file.path(myFiles, "RegionMaps/MEOW/MEOW_raster"))
  meow_poly <- readOGR(dsn=file.path(myFiles, "RegionMaps/MEOW"), layer="meow_ecos_moll")
  meow_poly_data <- meow_poly@data
  
  meow_data <- zonal(stack,  meow_rast, fun='mean', progress="text")
  meow_data_df <- data.frame(meow_data)
  setdiff(meow_data_df$zone, meow_poly_data$ECO_CODE)
  setdiff(meow_poly_data$ECO_CODE, meow_data_df$zone)
  
  meow_data2 <- merge(meow_poly_data, meow_data_df, by.x="ECO_CODE", by.y="zone")
  write.csv(meow_data2, file.path(dir_out, fileOutput), row.names=FALSE)  
}

## LME
lme_extract <- function(stack=stack, fileOutput="test.csv"){
  lme_rast <- raster(file.path(myFiles, 'RegionMaps/newLME_Mollweide/LME_raster'))
  lme_poly <- readOGR(dsn=file.path(myFiles, 'RegionMaps/newLME_Mollweide'), layer="newLME_Mollweide")
  lme_poly_data <- lme_poly@data
  
  lme_data <- zonal(stack,  lme_rast, fun='mean', progress="text")
  lme_data_df <- data.frame(lme_data)
  setdiff(lme_data_df$zone, lme_poly_data$LME_NUMBER)
  setdiff(lme_poly_data$LME_NUMBER, lme_data_df$zone)
  
  lme_data2 <- merge(lme_poly_data, lme_data_df, by.x="LME_NUMBER", by.y="zone")
  write.csv(lme_data2, file.path(dir_out, fileOutput), row.names=FALSE)  
}

## EEZ
eez_extract <- function(stack=stack, fileOutput=test3.csv){  
  eez_rast <- raster(file.path(myFiles, 'RegionMaps/EEZ/EEZ_raster'))
  eez_poly_data <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-OceanRegions_v2013a/data/eez_details.csv"))
  #eez_poly <- readOGR(dsn=file.path(dir_neptune_data, "model/GL-NCEAS-OceanRegions_v2013a/data", layer="eez_mol"))
  #eez_poly_data <- eez_poly@data
  
  eez_data <- zonal(stack,  eez_rast, fun='mean', progress="text")
  eez_data_df <- data.frame(eez_data)
  setdiff(eez_data_df$zone, eez_poly_data$eez_id)
  setdiff(eez_poly_data$eez_id, eez_data_df$zone)
  
  eez_data2 <- merge(eez_poly_data, eez_data_df, by.x="eez_id", by.y="zone")
  write.csv(eez_data2, file.path(dir_out, fileOutput), row.names=FALSE)  
}


## FAO
fao_extract <- function(stack=stack, fileOutput= "test.csv"){  
  fao_rast <- raster(file.path(myFiles, 'RegionMaps/FAO_regions/FAO_raster'))
  fao_poly_data <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv"))
  
  fao_data <- zonal(stack,  fao_rast, fun='mean', progress="text")
  fao_data_df <- data.frame(fao_data)
  setdiff(fao_data_df$zone, fao_poly_data$rgn_id)
  setdiff(fao_poly_data$rgn_id, fao_data_df$zone)
  
  fao_data2 <- merge(fao_poly_data, fao_data_df, by.x="rgn_id", by.y="zone")
  fao_id <- data.frame(rgn_id=fao_data2$rgn_id, fao_id=c(18, 21, 27, 31, 34, 41, 47, 48, 51, 57, 58, 61, 67, 71, 77, 81, 87, 88))
  fao_data3 <- merge(fao_id, fao_data2, by="rgn_id")
  
  write.csv(fao_data3, file.path(dir_out, fileOutput), row.names=FALSE)  
}


## WARM
warm_extract <- function(stack=stack, fileOutput="test.csv"){
  warm_rast <- raster(file.path(myFiles, "RegionMaps/WARM/WARM_raster"))
  warm_poly <- readOGR(dsn=file.path(myFiles, "RegionMaps/WARM"), layer="WARM_mol")
  warm_poly_data <- warm_poly@data
  
  warm_data <- zonal(stack,  warm_rast, fun='mean', progress="text")
  warm_data_df <- data.frame(warm_data)
  setdiff(warm_data_df$zone, warm_poly_data$ProvCode)
  setdiff(warm_poly_data$ProvCode, warm_data_df$zone)
  
  warm_data2 <- merge(warm_poly_data, warm_data_df, by.x="ProvCode", by.y="zone")
  write.csv(warm_data, file.path(dir_out, fileOutput), row.names=FALSE)  
}


########################################################
# 2013/normalized one time period/averaged by num ecosystems ---- 
# NOTE:  This is the most complete data for 2013, but can't be compared to 2008 data
########################################################
tifs = list.files(file.path(rasters, 'global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/by_threat'), pattern=glob2rx('*.tif'))

# #####################################
# ### trouble shooting for Ben----
# #####################################
# op <- raster(file.path(rasters, 'impact_layers/final_impact_layers/threats_2013_final/normalized_by_one_time_period/ocean_pollution.tif'))
# ship <- raster(file.path(rasters, 'impact_layers/final_impact_layers/threats_2013_final/normalized_by_one_time_period/shipping.tif'))
# 
# hs <- readOGR(dsn=file.path(myFiles, "RegionMaps/FAO_regions"), layer="FAO_rgns_mol")
# rasterize(hs, op, 
#           field="rgn_id", 
#           filename=file.path(myFiles, "RegionMaps/FAO_regions/FAO_raster_oceanPoll"), 
#           progress="text")
# 
# fao_rast_oceanPoll <- raster(file.path(myFiles, "RegionMaps/FAO_regions/FAO_raster_oceanPoll")) 
# fao_data_op <- zonal(op,  fao_rast_oceanPoll, fun='mean', progress="text")
# 
# 
# 
# 
# fao_rast <- raster(file.path(myFiles, 'RegionMaps/FAO_regions/FAO_raster'))
# fao_poly_data <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv"))
# 
# # resample(op, ship, method='ngb', 
# # filename=file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/op_resampled"), 
# # progress="text")
# 
# op2 <- raster(file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/op_resampled"))
# data <- stack(op2, ship)
# 
# fao_data <- zonal(data,  fao_rast, fun='mean', progress="text")
# fao_data_df <- data.frame(fao_data)
# setdiff(fao_data_df$zone, fao_poly_data$rgn_id)
# setdiff(fao_poly_data$rgn_id, fao_data_df$zone)
# 
# fao_data2 <- merge(fao_poly_data, fao_data_df, by.x="rgn_id", by.y="zone")
# fao_id <- data.frame(rgn_id=fao_data2$rgn_id, fao_id=c(18, 21, 27, 31, 34, 41, 47, 48, 51, 57, 58, 61, 67, 71, 77, 81, 87, 88))
# fao_data3 <- merge(fao_id, fao_data2, by="rgn_id")
# 
# ####################

stack_2013_one <- stack()
for(i in 1:length(tifs)){
  tmp <- raster(file.path(rasters, 'global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/by_threat', tifs[i]))
  stack_2013_one <- stack(stack_2013_one, tmp )
}

## Add in the cumulative score: 
stack_2013_one <- stack(stack_2013_one, raster(file.path(rasters, 
                                                         'global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/all_layers/global_cumul_impact_2013_all_layers.tif')))

## Summarize by region
meow_extract(stack=stack_2013_one, fileOutput="oneYearNorm_2013_meow.csv")
lme_extract(stack=stack_2013_one, fileOutput="oneYearNorm_2013_lme.csv")          
eez_extract(stack=stack_2013_one, fileOutput="oneYearNorm_2013_eez.csv")
fao_extract(stack=stack_2013_one, fileOutput="oneYearNorm_2013_fao.csv")
warm_extract(stack=stack_2013_one, fileOutput="oneYearNorm_2013_warm.csv")


 ########################################################
# 2013/normalized two time periods/averaged by num ecosystems ---- 
# NOTE:  This does not contain shipping, invasives, ocean pollution, slr
#         These are comparable with the 2008 data
########################################################
tifs = list.files(file.path(rasters, 'global_impact_model_2013/normalized_by_two_time_periods/averaged_by_num_ecosystems/by_threat'), pattern=glob2rx('*.tif'))

stack_2013_two <- stack()
for(i in 1:length(tifs)){
  tmp <- raster(file.path(rasters, 'global_impact_model_2013/normalized_by_two_time_periods/averaged_by_num_ecosystems/by_threat', tifs[i]))
  stack_2013_two <- stack(stack_2013_two, tmp )
}

## Add in the cumulative score:
stack_2013_two <- stack(stack_2013_two, raster(file.path(rasters, 
                                                         'global_impact_model_2013/normalized_by_two_time_periods/averaged_by_num_ecosystems/all_layers_except_shipping_oceanpollution_invasives_slr/cumulative/global_cumul_impact_2013_all_layers_except_shipping_oceanpollution_invasives_slr.tif')))

## Summarize by region
meow_extract(stack=stack_2013_two, fileOutput="twoYearNorm_2013_meow.csv")
lme_extract(stack=stack_2013_two, fileOutput="twoYearNorm_2013_lme.csv")          
eez_extract(stack=stack_2013_two, fileOutput="twoYearNorm_2013_eez.csv")
fao_extract(stack=stack_2013_two, fileOutput="twoYearNorm_2013_fao.csv")
warm_extract(stack=stack_2013_two, fileOutput="twoYearNorm_2013_warm.csv")


########################################################
# 2008/normalized two time periods/averaged by num ecosystems ---- 
# NOTE:  This is the most complete data for 2013, but can't be compared to 2008 data
########################################################
 tifs = list.files(file.path(rasters, 'global_impact_model_2008/normalized_by_two_time_periods/averaged_by_num_ecosystems/by_threat'), pattern=glob2rx('*.tif'))

stack_2008_two <- stack()
for(i in 1:length(tifs)){
  tmp <- raster(file.path(rasters, 'global_impact_model_2008/normalized_by_two_time_periods/averaged_by_num_ecosystems/by_threat', tifs[i]))
  stack_2008_two <- stack(stack_2008_two, tmp )
}

## Add in the cumulative score:
stack_2008_two <- stack(stack_2008_two, raster(file.path(rasters, 
                                                         'global_impact_model_2008/normalized_by_two_time_periods/averaged_by_num_ecosystems/all_layers_except_shipping_oceanpollution_invasives/cumulative/global_cumul_impact_2008_all_layers_except_shipping_oceanpollution_invasives.tif')))

## Summarize by region
meow_extract(stack=stack_2008_two, fileOutput="twoYearNorm_2008_meow.csv")
lme_extract(stack=stack_2008_two, fileOutput="twoYearNorm_2008_lme.csv")          
eez_extract(stack=stack_2008_two, fileOutput="twoYearNorm_2008_eez.csv")
fao_extract(stack=stack_2008_two, fileOutput="twoYearNorm_2008_fao.csv")
  

#########################################################
# Summarize historical 2008 data by EEZ (ben request:7/18/014 ) ----
########################################################

#historical 2008 pressures data (from: http://www.nceas.ucsb.edu/globalmarine/models)
old2008 <- raster(file.path(dir_neptune_data, "/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/Historical2008_fromWebsite/model/model"))

# #make raster for eez's based on historical 2008 data template: should only have to do once
# eez_poly <- readOGR(dsn="/var/data/ohi/model/GL-NCEAS-OceanRegions_v2013a/data", layer="eez_mol")
# rasterize(eez_poly, old2008, field="eez_id", 
#           filename=file.path(dir_neptune_data, "/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/Historical2008_fromWebsite/EEZ_historical2008"), 
#           overwrite=TRUE, progress="text")

# eez data
eezRaster <- raster(file.path(dir_neptune_data, "/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/Historical2008_fromWebsite/EEZ_historical2008"))
eez_poly_data <- read.csv(file.path(dir_neptune_data, "/model/GL-NCEAS-OceanRegions_v2013a/data/eez_details.csv"))

# extract zonal data
eez_data <- zonal(old2008,  eezRaster, fun='mean', progress="text")
eez_data_df <- data.frame(eez_data)
setdiff(eez_data_df$zone, eez_poly_data$eez_id)
setdiff(eez_poly_data$eez_id, eez_data_df$zone)

eez_data2 <- merge(eez_poly_data, eez_data_df, by.x="eez_id", by.y="zone")
#write.csv(eez_data2,"/home/frazier/impacts/ZonalExtractionData/historical2008_eez_cumImpact.csv", row.names=FALSE)  





#################################################
## ARCHIVE: Original method using extract: takes too long!
#################################################

# # takes about 2-4 hours for each layer!  Given time frame, ended up doing this in ArcGIS
# files <- c("artisanal_fishing_combo.tif",
#            "demersal_destructive_fishing_combo.tif",
#            "demersal_nondest_high_bycatch_combo.tif",
#            "demersal_nondest_low_bycatch_combo.tif",
#            "inorganic_combo.tif",
#            "invasives_combo.tif",
#            "night_lights_combo.tif",
#            "ocean_acidification_combo.tif",
#            "ocean_pollution_combo.tif",
#            "oil_rigs_combo.tif", 
#            "pelagic_high_bycatch_combo.tif",
#            "pelagic_low_bycatch_combo.tif",
#            "plumes_fert_combo.tif",
#            "plumes_pest_combo.tif",
#            "population_combo.tif",
#            "shipping_combo.tif",
#            "slr_combo.tif",
#            "sst_combo.tif",
#            "uv_combo.tif")
# 
# for(i in 1:length(pressure)){
#   pressure <- files[i]
#   ci <- raster(paste("by_threat\\", pressure, sep=""))
#   v <- extract(ci, LME, progress="text")
#   save(v, file=paste("data\\", gsub(".tif", "", pressure), ".Rdata", sep=""))
#   # mean for each polygonS
#   Summary <- ldply(v, function(x) data.frame(mean=round(mean(x), 3),
#                                              n=length(x)))
#   Summary <- data.frame(LME, Summary)
#   write.csv(Summary, file=paste("data\\", gsub(".tif", "", pressure), ".csv", sep=""))
# }
# 
