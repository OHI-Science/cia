#########################################
## Jan 27 2015
## MRF
#########################################
library(raster)
library(dplyr)

## Some set up
source('../ohiprep/src/R/common.R') 
# set temporary directory to folder on neptune disk big enough to handle it
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

rast_save <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/BenH_Jan27_2015/Habitat_Pressure")
rast_save_ci <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/BenH_Jan27_2015/Habitat_cumImpact")

## vulnerability matrix
vuln <- read.csv('SideProjects/BenH_Jan27_2015/vulnerability_weighting_matrix.csv')

## pressure data (check that these are zero to one values - like what I saw on the website)
#sst <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/impacts/transformed/sst/grid/sst"))
pressures <- dir(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/impacts/transformed/"))
pressures <- pressures[-(which(pressures %in% c("metadata.sgml",
                                  "metadata.zip",
                                  "tiff_to_asc.sh")))]
#pressures <- c("ocean_acidification", "sst", "uv")
## habitat (values are 1 and NA)
habitats <- list.files(file.path(dir_halpern2008, "mnt/storage/marine_threats/impact_layers_2013_redo/supporting_layers/habitats"))
habitats <- unique(sub("_lzw.*", "", habitats))
habitats <- habitats[-which(habitats=="show_rast_stats.sh")]



## loop to get model data
for(j in pressures){#j = "uv"
if(j=="artisanal_fishing"){
pressure <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/data/completed/impacts/transformed/%s/grid/artisanal", j)))
} else if(j=="ocean_acidification"){pressure <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/data/completed/impacts/transformed/%s/grid/acid", j)))
} else{pressure <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/data/completed/impacts/transformed/%s/grid/%s", j, j)))}

for(i in habitats){#i="beach"
habitat <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/impact_layers_2013_redo/supporting_layers/habitats/%s_lzw.tif", i)))
vulnerability <- vuln %>%
  filter(pressure==j) %>%
  select(get(i))
vulnerability <- vulnerability[,1]
overlay(habitat, pressure, fun=function(x,y){(x*y*vulnerability)}, 
        filename=file.path(rast_save, sprintf("%s_%s_combo", j, i)), progress="text", overwrite=TRUE)
}
}

### Full CumImpact model
cumImpact <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/models/model/grid/model"))
crs(cumImpact) <- crs(raster(file.path(rast_save, 'sst_seamounts_combo')))

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

