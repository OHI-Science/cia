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

## vulnerability matrix
vuln <- read.csv('SideProjects/BenH_Jan27_2015/vulnerability_weighting_matrix.csv')

## pressure data (check that these are zero to one values - like what I saw on the website)
sst <- raster(file.path(dir_halpern2008, "mnt/storage/marine_threats/data/completed/impacts/transformed/sst/grid/sst"))

## habitat (values are 1 and NA)
files <- list.files(file.path(dir_halpern2008, "mnt/storage/marine_threats/impact_layers_2013_redo/supporting_layers/habitats"))
files <- unique(sub("_lzw.*", "", files))
files <- files[-which(files=="show_rast_stats.sh")]

## loop to get model data
j <- "sst"
pressure <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/data/completed/impacts/transformed/%s/grid/%s", j, j)))

for(i in files){#i="beach"
habitat <- raster(file.path(dir_halpern2008, sprintf("mnt/storage/marine_threats/impact_layers_2013_redo/supporting_layers/habitats/%s_lzw.tif", i)))
vulnerability <- vuln %>%
  filter(pressure==j) %>%
  select(get(i))
vulnerability <- vulnerability[,1]
overlay(habitat, pressure, fun=function(x,y){(x*y*vulnerability)}, 
        filename=file.path(rast_save, sprintf("%s_%s_combo", j, i)), progress="text", overwrite=TRUE)
}


### Data check:  make sure that max values make sense given vulnerability matrix

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

