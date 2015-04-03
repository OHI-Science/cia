#########################################
## Jan 27 2015
## MRF
#########################################
library(raster)
library(dplyr)

source('../ohiprep/src/R/common.R') 
# set temporary directory to folder on neptune disk big enough to handle it
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

rasters = file.path(dir_halpern2008, 
                    'mnt/storage/marine_threats/impact_layers_2013_redo')

rast_save_ci <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/BenH_Jan27_2015/Habitat_cumImpact")

cumImp <- raster(file.path(rasters, 
  "global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/all_layers/global_cumul_impact_2013_all_layers.tif"))
sst <- raster(file.path(rasters, 
                        "global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/by_threat/sst_combo.tif"))
uv <- raster(file.path(rasters, 
                        "global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/by_threat/uv_combo.tif"))
acid <- raster(file.path(rasters, 
                        "global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/by_threat/ocean_acidification_combo.tif"))
overlay(cumImp, sst, fun=function(x,y){x-y}, 
        filename=file.path(rast_save_ci, "CumulativeImpact2013_minus_SST.tif"), 
        progress="text")
overlay(cumImp, sst, uv, acid, fun=function(w,x,y,z){w-(x+y+z)},
        filename=file.path(rast_save_ci, "CumulativeImpact2013_minus_SST_UV_acid.tif"),
        progress="text")

saveLoc <- file.path(dir_halpern2008, "var/www/ebm-site/GlobalMarine2013/SupplementalData")
dataFiles <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/BenH_Jan27_2015/FinalData")
setwd(file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/BenH_Jan27_2015/FinalData"))

zip(zipfile= file.path(saveLoc, "ci_no_uv_sst_acid"), 
    files = grep("ci_no_uv_sst_acid", value=TRUE, dir(dataFiles)))

zip(zipfile= file.path(saveLoc, "ci_no_sst"), 
    files = grep("ci_no_sst", value=TRUE, dir(dataFiles)))

zip(zipfile= file.path(saveLoc, "ci_2013"), 
    files = grep("ci_2013", value=TRUE, dir(dataFiles)))





#######################################################
# This method was aborted because I couldn't replicate the 
# results for reasons that I could never figure out.
# But this codes up the methods that would be used to 
# calculate this.
#
#######################################################
## Some set up
source('../ohiprep/src/R/common.R') 
# set temporary directory to folder on neptune disk big enough to handle it
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

rast_save <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/BenH_Jan27_2015/Habitat_Pressure")
rast_save_ci <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/BenH_Jan27_2015/Habitat_cumImpact")

## vulnerability matrix
vuln <- read.csv('SideProjects/BenH_Jan27_2015/vulnerability_weighting_matrix_2008.csv')

## pressure data (check that these are zero to one values - like what I saw on the website)
pressures <- dir(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/impacts/transformed/"))
pressures <- pressures[-(which(pressures %in% c("metadata.sgml",
                                  "metadata.zip",
                                  "tiff_to_asc.sh")))]
#pressures <- c("dem_nd_lb")

## habitat (values are 1 and NA)
habitats <- list.files(file.path(dir_halpern2008, "mnt/storage/marine_threats/impact_layers_2013_redo/supporting_layers/habitats"))
habitats <- unique(sub("_lzw.*", "", habitats))
habitats <- habitats[-which(habitats=="show_rast_stats.sh")]



## loop to get model data
## deep_waters habitat is different...
for(j in pressures){#j = "uv"
if(j=="artisanal_fishing"){
pressure <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/data/completed/impacts/transformed/%s/grid/artisanal", j)))
} else if(j=="ocean_acidification"){pressure <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/data/completed/impacts/transformed/%s/grid/acid", j)))
} else{pressure <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/data/completed/impacts/transformed/%s/grid/%s", j, j)))}

for(i in habitats){#i="suspension_reef"
habitat <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/impact_layers_2013_redo/supporting_layers/habitats/%s_lzw.tif", i)))
vulnerability <- vuln %>%
  filter(pressure==j) %>%
  select(get(i))
vulnerability <- vulnerability[,1]
overlay(habitat, pressure, fun=function(x,y){(x*y*vulnerability)}, 
        filename=file.path(rast_save, sprintf("%s_%s_combo", j, i)), progress="text", overwrite=TRUE)
}
}

plot(pressure, main=j)
# this habitat is strange:
#habitat <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/ecosystems/deep_waters/grid/deep_waters"))
### Full CumImpact model
cumImpact_old <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/models/model/grid/model"))
crs(cumImpact_old) <- crs(raster(file.path(rast_save, 'sst_seamounts_combo')))
plot(cumImpact_old)
hist(cumImpact_old)

### Recalculate full model
## Sum all files
all_files <- unique(substr(list.files(rast_save), 1, nchar(list.files(rast_save))-4))
all_s <- stack()
for(i in all_files){
  all_s <- stack(all_s, raster(file.path(rast_save, i)))
}


calc(all_s, sum, filename=file.path(rast_save_ci, "cumulative_impacts_model"), progress="text", na.rm=TRUE, overwrite=TRUE)
cum_impact_new <- raster(file.path(rast_save_ci, "cumulative_impacts_model"))
plot(cum_impact_new)
hist(cum_impact_new)

raster::crop(cumImpact_old, cum_impact_new, filename=file.path(rast_save_ci, "cumulative_impacts_model_old_crop"), progress='text')

raster:plot(cum_impact_new, raster(file.path(rast_save_ci, "cumulative_impacts_model_old_crop")), ylab="old 2008 scores", xlab="new 2008 scores", maxpixels=1000000, col=rgb(0,0,0,0.2))   
abline(0,1, col="red")

#overlay(cum_impact_new, raster(file.path(rast_save_ci, "cumulative_impacts_model_old_crop")), 
#        fun=function(x,y){(x-y)}, 
#        filename=file.path(rast_save_ci, 'new_minus_old_Cum_Impact'), progress="text", overwrite=TRUE)

#new_minus_old_ci <- raster(file.path(rast_save_ci, 'new_minus_old_Cum_Impact'))
plot(new_minus_old_ci)
click(new_minus_old_ci)

plot(cum_impact_new)
click(cum_impact_new) ## general values in ocean about 10-13

## In the ocean it seems the vaues are about 1.5 higher.
## habitats that could influence this: d_s_benthic, deep_waters, surface_waters
## pressures: "dem_nd_lb", "dem_nd_hb", "ocean_acidification" (~1), pel_lb (~0.1), pollution (~0.2), shipping (~0.2), sst (~.6), uv(~.7)
## possible that uv was excluded from model?  Nope:  did not improve
## check that layers are the same: uv and sst and acid

## Sum all files - except deep_waters 
all_files <- unique(substr(list.files(rast_save), 1, nchar(list.files(rast_save))-4))
grep("deep_waters", all_files)
all_files_minus_dw <- all_files[-grep("deep_waters", all_files)]

all_s_minus_dw <- stack()
for(i in all_files_minus_dw){
  all_s_minus_dw <- stack(all_s_minus_dw, raster(file.path(rast_save, i)))
}

calc(all_s_minus_dw, sum, filename=file.path(rast_save_ci, "cumulative_impacts_model_minus_dw"), progress="text", na.rm=TRUE, overwrite=TRUE)
cum_impact_new_minus_dw <- raster(file.path(rast_save_ci, "cumulative_impacts_model_minus_dw"))

raster:plot(cum_impact_new_minus_dw, raster(file.path(rast_save_ci, "cumulative_impacts_model_old_crop")), ylab="old 2008 scores", xlab="new 2008 scores minus uv", maxpixels=1000000, col=rgb(0,0,0,0.2))   
abline(0,1, col="red")

overlay(cum_impact_new_minus_acid, raster(file.path(rast_save_ci, "cumulative_impacts_model_old_crop")), 
        fun=function(x,y){(x-y)}, 
        filename=file.path(rast_save_ci, 'new_minus_old_Cum_Impact_minus_acid'), progress="text", overwrite=TRUE)

new_minus_old_ci_minus_acid <- raster(file.path(rast_save_ci, 'new_minus_old_Cum_Impact_minus_acid'))
plot(new_minus_old_ci_minus_acid)
click(new_minus_old_ci_minus_acid)

plot(cum_impact_new)
click(cum_impact_new) ## general values in ocean about 10-13



## Minus SST
sst_files <- unique(substr(list.files(rast_save, "sst"), 1, nchar(list.files(rast_save, "sst"))-4))  #get unique files after cutting off extension

sst_s <- stack()
for(i in sst_files){
  sst_s <- stack(sst_s, raster(file.path(rast_save, i)))
}

calc(sst_s, sum, filename=file.path(rast_save_ci, "sst_ci"), progress="text", na.rm=TRUE, overwrite=TRUE)
sst_ci_raster <- raster(file.path(rast_save_ci, "sst_ci"))
extend(sst_ci_raster, cumImpact, 
       filename=file.path(rast_save_ci, "sst_ci_extend"),
       progress="text") 
sst_impact <- raster(file.path(rast_save_ci, "sst_ci_extend"))

mod_minus_sst_stack <- stack(cumImpact, file.path(rast_save_ci, "sst_ci_extend"))
overlay(mod_minus_sst_stack, fun=function(a,b) a-b,
        filename=file.path(rast_save_ci, "mod_minus_sst.tif"),
        progress="text", overwrite=TRUE)

mod_minus_sst <- raster(file.path(rast_save_ci, "mod_minus_sst.tif"))

## Minus SST, UV, Acid
uv_files <- unique(substr(list.files(rast_save, "uv"), 1, nchar(list.files(rast_save, "uv"))-4))
acid_files <- unique(substr(list.files(rast_save, "ocean_acidification"), 1, nchar(list.files(rast_save, "ocean_acidification"))-4))
climate_files <- c(sst_files, uv_files, acid_files)

climate_s <- stack()
for(i in climate_files){
  climate_s <- stack(climate_s, raster(file.path(rast_save, i)))
}

calc(climate_s, sum, filename=file.path(rast_save_ci, "climate_ci"), progress="text", na.rm=TRUE, overwrite=TRUE)
climate_ci_raster <- raster(file.path(rast_save_ci, "climate_ci"))
extend(climate_ci_raster, cumImpact, 
       filename=file.path(rast_save_ci, "climate_ci_extend"),
       progress="text") 

mod_minus_climate_stack <- stack(cumImpact, file.path(rast_save_ci, "climate_ci_extend"))
overlay(mod_minus_climate_stack, fun=function(a,b) a-b,
        filename=file.path(rast_save_ci, "mod_minus_climate.tif"),
        progress="text", overwrite=TRUE)

mod_minus_climate <- raster(file.path(rast_save_ci, "mod_minus_climate.tif"))



### Data check: Compare raster values with the ones on my computer that were downloaded from web
## The main pressures are all 0 to 1, but the cumulative impact model should match
## Yes:  the model from website has min/max of 1.623462e-06, 90.07213 (which is the same here)


### Data check:  make sure that max values make sense given vulnerability matrix
## random sample 3 in each category (looked ok to me)

sample <- sample(acid_files, 3)
k <- 3
raster(file.path(rast_save, sample[k]))
plot(raster(file.path(rast_save, sample[k])))


### Data check for i=beach and j=sst
# This makes the values 0.6 to 0.6 (and mostly NA) when i=beach and j=sst (which is correct)  
calc(habitat, fun=function(x){(x*vulnerability)}, 
        filename=file.path(rast_save, sprintf("%s_%s_combo", j, i)), progress="text", overwrite=TRUE)
tmp <- raster(file.path(rast_save, sprintf("%s_%s_combo", j, i)))

# This is mostly NA, and values range from 0 to 1
overlay(habitat, pressure, fun=function(x,y){(x*y)}, 
        filename=file.path(rast_save, sprintf("%s_%s_combo", j, i)), progress="text", overwrite=TRUE)
tmp <- raster(file.path(rast_save, sprintf("%s_%s_combo", j, i)))

# This is mostly NA, and values should be 0 to 0.6
overlay(habitat, pressure, fun=function(x,y){(x*y*vulnerability)}, 
        filename=file.path(rast_save, sprintf("%s_%s_combo", j, i)), progress="text", overwrite=TRUE)
tmp <- raster(file.path(rast_save, sprintf("%s_%s_combo", j, i)))

