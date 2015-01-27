######################################################
## goal: try to figure out the differences between
## some of the data in the cumulative impacts folder
######################################################

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

## I'm confused by the threats_2013_final and normalized_by_one_time_period

final_sst <- raster(file.path(rasters, "impact_layers/final_impact_layers/threats_2013_final/normalized_by_one_time_period/sst.tif"))
interim_sst <- raster(file.path(rasters, 'impact_layers/final_impact_layers/threats_2013_interim/new_layers/sst/moll_nontrans_unclipped_1km_pos_transoneperiod_clipped/sst.tif'))
