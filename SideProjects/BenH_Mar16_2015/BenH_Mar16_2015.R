########################################################
## March 16 2014
## MRF
## Zipping files and making available to collaborators (Byrnes)
#######################################################
library(raster)
library(dplyr)

source('../ohiprep/src/R/common.R') 
# set temporary directory to folder on neptune disk big enough to handle it
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

rasters = file.path(dir_halpern2008, 
                    'mnt/storage/marine_threats/impact_layers_2013_redo/global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/by_threat')
rast_save_ci <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/BenH_Mar16_2015")

saveLoc <- file.path(dir_halpern2008, "var/www/ebm-site/GlobalMarine2013/SupplementalData")

setwd('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/new_layers/invasives/moll_nontrans_unclipped_1km')
zip(zipfile= file.path(saveLoc, "invasives_raw"), 
    files = dir())

setwd('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/new_layers/ocean_pollution/moll_nontrans_clipped_1km')
zip(zipfile= file.path(saveLoc, "ocean_pollution_raw"), 
    files = dir())

setwd('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/new_layers/plumes_fert/moll_nontrans_unclipped_1km')
zip(zipfile= file.path(saveLoc, "plumes_fertilizer_raw"), 
    files = dir())

setwd('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/new_layers/plumes_pest/moll_nontrans_unclipped_1km')
zip(zipfile= file.path(saveLoc, "plumes_pesticide_raw"), 
    files = dir())

setwd('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/new_layers/sst/moll_nontrans_unclipped_1km')
zip(zipfile= file.path(saveLoc, "sst_raw"), 
    files = dir())

setwd('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/old_layers/inorganic/moll_nontrans_clipped_1km')
zip(zipfile= file.path(saveLoc, "inorganic_raw"), 
    files = dir())

setwd('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/all_layers')
zip(zipfile= file.path(saveLoc, "CI_2013_OneTimePeriod"), 
    files = dir())



