#######################################
## converting files to wgs84
#######################################

rm(list = ls())

## Downloading packages
library(rgdal)
library(raster)
library(sp)
library(maptools)
library(rgeos)
library(plyr)

rasters = file.path('/var/cache/halpern-et-al', 
                    'mnt/storage/marine_threats/impact_layers_2013_redo')
myFiles = file.path("/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013")


tifs = list.files(file.path(myFiles, 'TrimmedPressureLayers/Pressures2013'), pattern=glob2rx('*.gri'))
tifs <- gsub(".gri", "", tifs)


newProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tif = 'global_cumulative_impact_2013_all_layers'
  tmp <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2013', tif))
  projectRaster(tmp, crs=newProj, method="ngb", 
                filename=file.path(myFiles, sprintf('TrimmedPressureLayers/Pressures2013_wgs/%s_wgs.tif', tif)), progress="text")
  tmp <- raster(file.path(myFiles, sprintf('TrimmedPressureLayers/Pressures2013_wgs/%s_wgs.tif', tif)))
  
tif = 'global_cumulative_impact_dif'
  tmp <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2013minus2008', tif))
  projectRaster(tmp, crs=newProj, method="ngb", 
              filename=file.path(myFiles, sprintf('TrimmedPressureLayers/Pressures2013minus2008_wgs/%s_wgs.tif', tif)), progress="text")
  tmp <- raster(file.path(myFiles, sprintf('TrimmedPressureLayers/Pressures2013minus2008_wgs/%s_wgs.tif', tif)))

old2008 <- raster(file.path(myFiles, "Historical2008_fromWebsite/model/model"))
projection(old2008) <- projection(tmp)    
projectRaster(old2008, crs=newProj, method="ngb", 
              filename=file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_CI_wgs.tif'), progress="text")
tmp <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_CI_wgs.tif'))

  
shipping <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/impacts/transformed/shipping/tif/shipping.tif"))
projection(shipping) <- projection(tmp)    
projectRaster(shipping, crs=newProj, method="ngb", 
              filename=file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_shipping_wgs.tif'), progress="text")
tmp <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_shipping_wgs.tif'))


sst <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/impacts/transformed/sst/tif/sst.tif"))
projection(sst) <- projection(tmp)    
projectRaster(sst, crs=newProj, method="ngb", 
              filename=file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_sst_wgs.tif'), progress="text")
tmp <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_sst_wgs.tif'))

nutrient <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/impacts/transformed/nutrient/tif/nutrient.tif"))
projection(nutrient) <- projection(tmp)    
projectRaster(nutrient, crs=newProj, method="ngb", 
              filename=file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_nutrient_wgs.tif'), progress="text")
tmp <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_nutrient_wgs.tif'))



dem_nd_hb <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/impacts/transformed/dem_nd_hb/tif/dem_nd_hb.tif"))
projection(dem_nd_hb) <- projection(tmp)    
projectRaster(dem_nd_hb, crs=newProj, method="ngb", 
              filename=file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_dem_nd_hb_wgs.tif'), progress="text")
tmp <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_dem_nd_hb_wgs.tif'))


dem_nd_lb <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/impacts/transformed/dem_nd_lb/tif/dem_nd_lb.tif"))
projection(dem_nd_lb) <- projection(tmp)    
projectRaster(dem_nd_lb, crs=newProj, method="ngb", 
              filename=file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_dem_nd_lb_wgs.tif'), progress="text")
tmp <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_dem_nd_lb_wgs.tif'))


