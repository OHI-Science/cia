#######################################################################
### NCEAS/OHI
### R script to map cumulative pressures
### Melanie Frazier Dec 3, 2013
#######################################################################

rm(list = ls())

## Downloading packages
library(rgdal)
library(raster)
library(sp)
library(maptools)
library(rgeos)
library(plotKML)
library(dplyr)
library(tidyr)
library(rasterVis)
library(RColorBrewer)
library(fields)
library(ggplot2)

myTheme <- theme_bw() + theme(axis.text=element_text(size=20), 
                              axis.title=element_text(size=20, vjust=.15),
                              plot.margin=unit(c(1,1,1,1), "lines"),
                              legend.title = element_text(size=20),
                              legend.text= element_text(size=20),
                              plot.title = element_text(lineheight=.8, size=20),
                              strip.text.x = element_text(size = 18)) 

#paths:
path <- '/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013'
path_trim <- "/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/TrimmedPressureLayers"
path_save <- "/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ResultMaps"

# land layer for plots ----
rgn_ocn_cntry <- readOGR("/var/data/ohi/model/GL-NCEAS-OceanRegions_v2013a/data", layer="rgn_ocean_cntry_mol")
land <- rgn_ocn_cntry[!is.na(rgn_ocn_cntry@data$ISO_3digit) & rgn_ocn_cntry@data$rgn_id==0,]

######################################################
## Paper figures
######################################################
## Figure 1: Absolute per-pixel difference in cumulative impact score (2013-2008) ----
# "Change in cumulative pressures, 2013 minus 2008"
diff_noLand <- raster(file.path(path_trim, "Pressures2013minus2008/global_cumulative_impact_dif"))
#plot(diff_noLand)
diff_noLand_subsample <- sampleRegular(diff_noLand, size=500000, asRaster = TRUE)

png(file.path(path_save, "Fig1.CumImpDiff.png"), res=500, width=7, height=7, units="in")  
#pdf(file.path(path_save, "Fig1.CumImpDiff.pdf"))  #, width=1200, height=1000, res=150, pointsize=14)
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
plot(diff_noLand, col=rev(cols), axes=FALSE, box=FALSE, legend.shrink=0.5, legend.width=0.6, 
     axis.args=list(cex.axis=1.3))
plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
dev.off()


## Figure 2 ----
# Part a: Global 2013 scores
Cum2013 <- raster(file.path(path_trim, "Pressures2013/global_cumulative_impact_2013_all_layers"))

#pdf(file.path(path_save, 'Fig2a.CumImp2013.pdf'))  #, width=1200, height=1000, res=150, pointsize=14)
png(file.path(path_save, 'Fig2a.CumImp2013.png'), res=500, width=7, height=7, units="in")
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
plot(Cum2013, col=rev(cols), axes=FALSE, box=FALSE, legend.shrink=0.5, legend.width=0.6,
     axis.args=list(cex.axis=1.3))
plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
dev.off()

# part b: figure out high and low regions and high levels of increase/decrease.
# # high low regions:
# histogram(Cum2013)
# quantile(Cum2013)
# 
# # reclassify(Cum2013, c(0,2.738833,-1, 
# #                       2.738833, 4.020329, 0,
# #                       4.020329, 15.418009, 1),
# #            filename= file.path(path, "ModifiedPressureMaps/Cum2013highlow"), 
# #            progress="text")
# 
# Cum2013highlow <- raster(file.path(path, "ModifiedPressureMaps/Cum2013highlow"))
# 
# Cum2013low <- Cum2013<2.7
# 
# # increase/decrease
# diff_noLand <- raster(file.path(path_trim, "Pressures2013minus2008/global_cumulative_impact_dif"))
# histogram(diff_noLand)
# quantile(diff_noLand)
# 
# # reclassify(diff_noLand, c(-3.55101061,-0.04539633,-1, 
# #                           -0.04539633, 0.60181093, 0,
# #                           0.60181093, 3.66151476, 1),
# #            filename=file.path(path, "ModifiedPressureMaps/Diffhighlow"), 
# #            progress="text")
# 
# Diffhighlow <- raster(file.path, "ModifiedPressureMaps/Diffhighlow")
# plot(Diffhighlow)
# 
# # combining the two
# # combined <- stack(Cum2013highlow, Diffhighlow)
# # overlay(Cum2013highlow, Diffhighlow, fun=function(Cum2013highlow, Diffhighlow){ifelse(Cum2013highlow==1 & Diffhighlow==1, 2, 
# #                                         ifelse(Cum2013highlow==1 & Diffhighlow==-1, 1,
# #                                                ifelse(Cum2013highlow==-1 & Diffhighlow==1, -1,
# #                                                       ifelse(Cum2013highlow==-1 & Diffhighlow==-1, -2, 0))))},
# #      filename = file.path(path, "ModifiedPressureMaps/DiffHighLowScore"),
# #      progress = "text",
# #         overwrite=TRUE)

DiffHighLowScore <- raster(file.path(path, "ModifiedPressureMaps/DiffHighLowScore"))
plot(DiffHighLowScore)
freq(DiffHighLowScore)

#pdf(file.path(path_save, 'Fig2b.value_trend.pdf')) #, width=1200, height=1000)
png(file.path(path_save, 'Fig2b.value_trend.png'), res=500, width=7, height=7, units="in")
par(mar=c(2,2,2,2))
par(oma=c(0,0,0,4))
arg <- list(at=c(-2,-1,0,1,2), 
            labels=c("low/\n decreasing", "low/\n increasing", "neither", "high/\n decreasing", "high/\n increasing"), 
            cex.axis=1)
color <- c("#023FA5", "#BEC1D4", "#FDF5E6", "#D6BCC0", "#8E063B")
#color <- rev(brewer.pal(5, "Spectral"))
plot(DiffHighLowScore, col=color, axis.arg=arg, axes=FALSE, box=FALSE, legend.shrink=0.3, 
     legend.args=list(text="score/trend", font=2, line=1, cex=1))
plot(land, add=TRUE, border="gray70", col="gray90", lwd=0.5)
dev.off()


# plot example 2
# library(raster)
# library(rasterVis)
# 
# ## Example data
# r <- raster(ncol=4, nrow=2)
# r[] <- sample(1:4, size=ncell(r), replace=TRUE)
# r <- as.factor(r)
# 
# ## Add a landcover column to the Raster Attribute Table
# rat <- levels(r)[[1]]
# rat[["landcover"]] <- c("land","ocean/lake", "rivers","water bodies")
# levels(r) <- rat
# 
# ## Plot
# levelplot(r, col.regions=rev(terrain.colors(4)), xlab="", ylab="")


## Figure 3 ----

# a: SST_diff
sst_diff <- raster(file.path(path_trim, "Pressures2013minus2008/sst_combo_dif"))
histogram(sst_diff, main="sst: 2013 minus 2008")

#pdf(file.path(path_save, 'Fig3a.SST_diff.pdf'))  #, width=1200, height=1000, res=150, pointsize=14)
png(file.path(path_save, 'Fig3a.SST_diff.png'), res=500, width=7, height=7, units="in")
my_breaks <- c(-2.6,-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.6)
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)-1)
plot(sst_diff, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)
#     axis.args=list(at=c(-2.6, -2, -1, 0, 1, 2, 2.6), labels=c(-2.6, -2, -1, 0, 1, 2, 2.6)))

# add axis with fields package function:
def_breaks <- seq(min(my_breaks), max(my_breaks), length.out=length(my_breaks))
image.plot(sst_diff, zlim = c(min(my_breaks), max(my_breaks)), 
           legend.only = TRUE, 
           legend.shrink=0.5,
           legend.width=0.6,
           col = rev(cols),
           axis.args = list(at = def_breaks, labels = my_breaks, cex.axis=1))
plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
dev.off()


#b:  nutrient_diff
nutrient_diff <- raster(file.path(path_trim, "Pressures2013minus2008/plumes_fert_combo_dif"))
histogram(nutrient_diff, main="nutrients: 2013 minus 2008")
nutrient_diff

## Get rid of zeroes (outside of shoreline, the zero values are actually NA - check on this)
# reclassify(nutrient_diff, cbind(0, NA), progress="text", 
#            filename = file.path(path, "ModifiedPressureMaps/nutrient_zerocut"),
#            overwrite=TRUE)
# plot(nutrient_zeroCut)
# histogram(nutrient_zeroCut, main="nutrients: 2013 minus 2008 (zeros removed)")

nutrient_zeroCut <- raster(file.path(path, "ModifiedPressureMaps/nutrient_zerocut"))


## crop different regions
# North America
#extent <- drawExtent()
na_extent <- c(-10800000, -3300000, 2340000, 8260000)
na <- crop(nutrient_zeroCut, na_extent)
plot(na)

#pdf(file.path(path_save, 'Fig3B.nutrient_NA_diff.pdf')  #, width=1200, height=1000, res=150, pointsize=14)
png(file.path(path_save, 'Fig3b.nutrient_NA_diff.png'), res=500, width=7, height=7, units="in")
my_breaks <- c(-0.46 , -0.10, -0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.69)
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)-1)
plot(na, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)
#plot(land, add=TRUE, border="gray95", col="gray95", lwd=0.5)
#plot(na, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)
# add axis with fields package function:
def_breaks <- seq(min(my_breaks), max(my_breaks), length.out=length(my_breaks))
image.plot(sst_diff, zlim = c(min(my_breaks), max(my_breaks)), 
           legend.only = TRUE, 
           legend.shrink=0.5,
           legend.width=0.6,
           col = rev(cols),
           axis.args = list(at = def_breaks, labels = my_breaks, cex.axis=1))
dev.off()


## crop different regions
# Florida
extent <- drawExtent()
fl_extent <- c(-8000000, -7000000, 3000000, 4000000)
fl <- crop(nutrient_zeroCut, fl_extent)
plot(fl)

#pdf(file.path(path_save, 'Fig3B.nutrient_NA_diff.pdf')  #, width=1200, height=1000, res=150, pointsize=14)
png(file.path(path_save, 'Fig3b.nutrient_FL_diff.png'), res=500, width=7, height=7, units="in")
my_breaks <- c(-0.46 , -0.10, -0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.69)
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)-1)
plot(fl, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)
plot(land, add=TRUE, border="gray95", col="gray95", lwd=0.5)
#plot(na, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)
# add axis with fields package function:
def_breaks <- seq(min(my_breaks), max(my_breaks), length.out=length(my_breaks))
image.plot(sst_diff, zlim = c(min(my_breaks), max(my_breaks)), 
           legend.only = TRUE, 
           legend.shrink=0.5,
           legend.width=0.6,
           col = rev(cols),
           axis.args = list(at = def_breaks, labels = my_breaks, cex.axis=1))
dev.off()



# SA
#extent <- drawExtent()
sa_extent <- c(-8780000, -2790000, -6800000, 1490000)
sa <- crop(nutrient_zeroCut, sa_extent)
plot(sa)

pdf('/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ResultMaps/Fig3a.nutrient_SA_diff.pdf')  #, width=1200, height=1000, res=150, pointsize=14)
my_breaks <- c(-0.46 , -0.10, -0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.69)
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)-1)
plot(sa, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)

#plot(land, add=TRUE, border="gray85", col="gray90")
dev.off()

# Aus
extent <- drawExtent()
aus_extent <- c(6040000, 15100000, -4910000, 3520000)
aus <- crop(nutrient_zeroCut, aus_extent)
plot(aus)

pdf('/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ResultMaps/Fig3a.nutrient_aus_diff.pdf')  #, width=1200, height=1000, res=150, pointsize=14)
my_breaks <- c(-0.46 , -0.10, -0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.69)
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)-1)
plot(aus, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)

#plot(land, add=TRUE, border="gray85", col="gray90")
dev.off()

## global
png(file.path(path_save, 'Fig3b.nutrient_global.png'), res=500, width=7, height=7, units="in")
#my_breaks <- c(-0.46 , -0.10, -0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.69)
#cols = colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)-1)
#plot(nutrient_zeroCut, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)
plot(nutrient_diff, col="slategray1", axes=FALSE, box=FALSE, legend=FALSE)

# add axis with fields package function:
# def_breaks <- seq(min(my_breaks), max(my_breaks), length.out=length(my_breaks))
# image.plot(dem_fishing_diff, zlim = c(min(my_breaks), max(my_breaks)), 
#            legend.only = TRUE, 
#            legend.shrink=0.5,
#            legend.width=0.6,
#            col = rev(cols),
#            axis.args = list(at = def_breaks, labels = my_breaks, cex.axis=1))
# 
# # add land layer
# plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
plot(extent(na_extent), col="red", add=TRUE)
plot(extent(sa_extent), col="red", add=TRUE)
plot(extent(aus_extent), col="red", add=TRUE)
dev.off()

#plot(land, add=TRUE, border="gray85", col="gray90")
dev.off()




#c:  demersal destructive fishing
dem_fishing_diff <- raster(file.path(path_trim, "Pressures2013minus2008/demersal_destructive_fishing_combo_dif"))
histogram(dem_fishing_diff, main="demersal destructive fishing: 2013 minus 2008")
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
plot(dem_fishing_diff, col=rev(cols), axes=FALSE, box=FALSE)

png(file.path(path_save, 'Fig3c.demersal_diff.png'), res=500, width=7, height=7, units="in")
#pdf('/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ResultMaps/Fig3c.demersal_destructive_fishing_diff.pdf')  #, width=1200, height=1000, res=150, pointsize=14)
my_breaks <- c(-0.5, -0.2, -0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15, 0.2, 0.7)
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)-1)
plot(dem_fishing_diff, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)
#     axis.args=list(at=c(-0.4, -0.2, -0.1, 0, 0.1, 0.2, 0.4), labels=c("<-0.4", "-0.2", "-0.1", "0", "0.1", "0.2", ">0.4")))

# add axis with fields package function:
def_breaks <- seq(min(my_breaks), max(my_breaks), length.out=length(my_breaks))
image.plot(dem_fishing_diff, zlim = c(min(my_breaks), max(my_breaks)), 
           legend.only = TRUE, 
           legend.shrink=0.5,
           legend.width=0.6,
           col = rev(cols),
           axis.args = list(at = def_breaks, labels = my_breaks, cex.axis=1))

# add land layer
plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
dev.off()


#d:  pelagic fishing
pel_fishing_diff <- raster(file.path(path_trim, "Pressures2013minus2008/pelagic_high_bycatch_combo_dif"))
histogram(pel_fishing_diff, main="pelagic high-bycatch: 2013 minus 2008")
plot(pel_fishing_diff)

png(file.path(path_save, 'Fig3d.pelagic_diff.png'), res=500, width=7, height=7, units="in")
my_breaks <- c(-0.53, -0.2, -0.10, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.10, 0.2, 0.31)
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)-1)
plot(pel_fishing_diff, col=rev(cols), axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)
# add axis with fields package function:
def_breaks <- seq(min(my_breaks), max(my_breaks), length.out=length(my_breaks))
image.plot(dem_fishing_diff, zlim = c(min(my_breaks), max(my_breaks)), 
           legend.only = TRUE, 
           legend.shrink=0.5,
           legend.width=0.6,
           col = rev(cols),
           axis.args = list(at = def_breaks, labels = my_breaks, cex.axis=1))
plot(land, add=TRUE, border="gray85", col="gray90")
dev.off()


## SOM Fig. 1: Histogram of per pixel difference scores ----

## global combined pressures
pressure_diff <- raster(file.path(path_trim, "Pressures2013minus2008/global_cumulative_impact_dif")) 

pdf(file.path(path_save, 'SOMFig1a.global_hist_diff.pdf'))  #, width=1200, height=1000, res=150, pointsize=14))    
    histogram(pressure_diff, scales=list(y=list(at=NULL)), 
              main="Global cumulative impact score (2013 minus 2008)")
    dev.off()
    
## individual pressures    
layers <- dir(file.path(path_trim, "Pressures2013minus2008"))
layers <- gsub(".grd", "", layers)
layers <- gsub(".gri", "", layers)
layers <- unique(layers)
layers <- layers[-which(layers == "global_cumulative_impact_dif")]

no <- length(layers) # amount of files found
imagestack <- stack() # you initialize your raster stack

for (i in 1:no){
  #i=2
  image <- raster(file.path(path_trim, "Pressures2013minus2008", layers[i])) # fill up raster stack with only the tiffs   
  imagestack <- addLayer(imagestack,image)
}

layers_2 <- gsub("_combo_dif", "", layers)
layers_2 <- gsub("_", " ", layers)
names(imagestack) <- layers_2

pdf(file.path(path_save, 'SOMFig1b.pressures_diff.pdf'))  #, width=1200, height=1000, res=150, pointsize=14))
histogram(imagestack, layout=c(3,4), cex=.7)
dev.off() 

# plotting each individually:
for(i in 1:length(layers)){
  #i <- 2
  dif <- raster(file.path(path_trim, "Pressures2013minus2008", layers[i]))
  my_name <- gsub("_combo_dif", "", layers[i])
  my_name <- gsub("_", " ", my_name)
  
  pdf(file.path(path_save, sprintf('Pressures2013minus2008Hists/%s.pdf', my_name)))      
  histogram(dif, main=my_name, xlab="2013 minus 2008 pressure score")
  dev.off()
}

## SOM Fig. 2: Historic 2008 scores vs. new calculation----
new2008 <- raster(file.path(path, "TrimmedPressureLayers/Pressures2008/global_cumulative_impact_2008_all_layers"))
old2008 <- raster(file.path(path, "Historical2008_fromWebsite/model/model"))

old2008_crop <- crop(old2008, new2008, progress="text")

compare2008 <- stack(new2008, old2008_crop) 
names(compare2008) <- c("new2008", "old2008")
pairs(compare2008)    

png(file.path(path_save, 'SOMFig2.2008compare_scatter.png'))  #, width=1200, height=1000, res=150, pointsize=14))
raster:plot(new2008, old2008_crop, ylab="old 2008 scores", xlab="new 2008 scores", maxpixels=10000000, col=rgb(0,0,0,0.2))   
dev.off()    

## SOM Fig. 3a: Scatter plot of average per-pixel ci scores for each eez----
  # note: need to label outliers 

eez_2008 <- read.csv("ZonalExtractionData/twoYearNorm_2008_eez.csv") %>%
  select(eez_id, eez_key, eez_nam, sov_id, sov_nam, eez_iso3, 
         ci_2008=global_cumul_impact_2008_all_layers_except_shipping_oceanpollution_invasives)

eez_2013 <- read.csv("ZonalExtractionData/twoYearNorm_2013_eez.csv") %>%
  select(eez_id, eez_key, eez_nam, sov_id, sov_nam, eez_iso3, 
         ci_2013=global_cumul_impact_2013_all_layers_except_shipping_oceanpollution_invasives_slr) %>%
  left_join(eez_2008)

p <- ggplot(eez_2013, aes(y=ci_2013, x=ci_2008)) +
  geom_point(shape=19) +
  geom_text(aes(label=eez_nam), size=1, hjust=1.1) +
  geom_rug(alpha=0.2) +
  geom_abline(slope=1, intercept=0, linetype=2, color="orange") +
  myTheme +
  labs(x="2008 Cumulative Impact Scores", y="2013 Cumulative Impact Scores")
print(p) 
ggsave(file.path(path_save, 'EEZ_2008vs2013_labels.pdf'))


## SOM Fig. 3b: Scatter plot of average per-pixel ci scores for each meow----

meow_2008 <- read.csv("ZonalExtractionData/twoYearNorm_2008_meow.csv") %>%
  select(ECOREGION,  
         ci_2008=global_cumul_impact_2008_all_layers_except_shipping_oceanpollution_invasives)

meow_2013 <- read.csv("ZonalExtractionData/twoYearNorm_2013_meow.csv") %>%
  select(ECOREGION, 
         ci_2013=global_cumul_impact_2013_all_layers_except_shipping_oceanpollution_invasives_slr) %>%
  left_join(meow_2008)

p <- ggplot(meow_2013, aes(y=ci_2013, x=ci_2008)) +
  geom_point(shape=19) +
  geom_text(aes(label=ECOREGION), size=1, hjust=1.1) +
  geom_rug(alpha=0.2) +
  geom_abline(slope=1, intercept=0, linetype=2, color="orange") +
  myTheme +
  labs(x="2008 Cumulative Impact Scores", y="2013 Cumulative Impact Scores")
print(p) 
ggsave(file.path(path_save, 'meow_2008vs2013_labels.pdf'))


## SOM Fig. 4: stacked horizontal bar graphs for each eez showing contribution of each stressor type to CI score----

eez_2013 <- read.csv("ZonalExtractionData/oneYearNorm_2013_eez.csv") 

eez_2013  <- eez_2013 %>%
  mutate(region_id = sprintf("%s (%s)",eez_nam, eez_id)) %>%
  select(-eez_id, -eez_key, -eez_nam, -sov_id, -sov_nam, -eez_iso3) %>% 
  gather(pressure, value, c(-region_id, -global_cumul_impact_2013_all_layers)) %>%
  mutate(pressure = gsub("_combo", "", pressure))
  
eez_2013_means  <- eez_2013 %>%
  group_by(pressure) %>%
  summarize(mean=mean(value)) %>%
  arrange(mean)
 
eez_2013 <- eez_2013 %>%
  mutate(pressure = factor(pressure, levels=eez_2013_means$pressure))

myPalette <- colorRampPalette(brewer.pal(11, "Spectral"), space="Lab")

ggplot(eez_2013, aes(x=factor(region_id, levels=(region_id)[order(global_cumul_impact_2013_all_layers)]), y=value, fill=pressure, order=desc(pressure))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=myPalette(19)) +
  coord_flip() +
  theme_bw() +
  labs(y="Pressure", x="") + 
  guides(fill=guide_legend(reverse=TRUE)) +
  theme(axis.text.y=element_text(size=5),
        legend.justification=c(1,0), legend.position=c(1,0))
ggsave(file.path(path_save, 'Impacts4EEZs.pdf'), height=20, width=10)
  



         # Code for rasterVis plots....see printouts for understanding how to label histograms
        cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
        myTheme = rasterTheme(region=rev(cols))
        myTheme$layout.heights$main=12
        levelplot(diff_noLand_subsample, par.settings=myTheme, margin=FALSE)
        
        , 
        main="Change in cumulative pressure, 2013 minus 2008",
        FUN.margin=mean, scales=list(draw=FALSE)) 
    
    +
      layer(sp.polygons(land, lwd=0.8, fill="gray80", col="gray85"))
    
    ## to get values across rows:
    mean(getValues(cum2013_noLand, 50), na.rm=TRUE)
    
    ## potentially useful commands:
    ## Define the legend breaks
    #my.at <- seq(100, 1850, 500)
    #levelplot(r, at=my.at)
    #myColorkey <- list(at=my.at, ## where the colors change
    #                   labels=list(
    #                     at=my.at ## where to print labels
    #                   ))
    levelplot(r, at=my.at, colorkey=myColorkey)
    ## Get rid of scales
    # scales=list(draw=FALSE) # although centered everything else oddly.
    
    
    ### Original from Ben's (might be helpful to understand this)
    #png(sprintf('figure/levelplot_%s_again.png',f), width=1200, height=1000, res=150, pointsize=14)
    pdf(sprintf('figure/levelplot_%s_again.pdf',f)) # , width=1200, height=1000, res=150, pointsize=14)
    rng = extendrange(r[[f]],f=0.0001)
    p = levelplot(r.f, par.settings=myTheme, FUN.margin=mean, # main=f, 
                  at=seq(rng[1], rng[2], length.out=10)) + 
      layer(sp.polygons(wrld.sp, lwd=0.8, fill='gray95', col='gray80'))
    print(p)
    dev.off()
    
    
    
    
    
    
    
    
    
    
    
    
    
    #-----------------------------------------------------------
    # 2013 data
    #-----------------------------------------------------------
    # read in 2013 clipped rasters (i.e., no land or border)
    setwd("C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters")
    
    stack2013 <- stack(artisanal_fishing_combo <- raster("artisanal_fishing_combo_clip"),
                       demersal_destructive_fishing_combo <- raster("demersal_destructive_fishing_combo_clip"),
                       demersal_nondest_high_bycatch_combo <- raster("demersal_nondest_high_bycatch_combo_clip"),
                       demersal_nondest_low_bycatch_combo <- raster("demersal_nondest_low_bycatch_combo_clip"),
                       inorganic_combo <- raster("inorganic_combo_clip"),
                       invasives_combo <- raster("invasives_combo_clip"),
                       night_lights_combo <- raster("night_lights_combo_clip"),
                       ocean_acidification_combo <- raster("ocean_acidification_combo_clip"),
                       ocean_pollution_combo <- raster("ocean_pollution_combo_clip"),
                       oil_rigs_combo <- raster("oil_rigs_combo_clip"),
                       pelagic_high_bycatch_combo <- raster("pelagic_high_bycatch_combo_clip"),
                       pelagic_low_bycatch_combo <-  raster("pelagic_low_bycatch_combo_clip"),
                       plumes_fert_combo <- raster("plumes_fert_combo_clip"),
                       plumes_pest_combo <- raster("plumes_pest_combo_clip"),
                       population_combo <- raster("population_combo_clip"),
                       shipping_combo <- raster("shipping_combo_clip"),
                       slr_combo <- raster("slr_combo_clip"),
                       sst_combo <- raster("sst_combo_clip"),
                       uv_combo <- raster("uv_combo_clip"))
    
    names(stack2013) <- c("artisanal fishing",
                          "demersal destructive fishing",
                          "demersal nondest high bycatch",
                          "demersal nondest low bycatch",
                          "inorganic",
                          "invasives",
                          "night lights",
                          "ocean acidification",
                          "ocean pollution",
                          "oil rigs",
                          "pelagic high bycatch",
                          "pelagic low bycatch",
                          "plumes fertilyzer",
                          "plumes pesticide",
                          "population",
                          "shipping",
                          "slr",
                          "sst",
                          "uv")
    
    
    cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
    myTheme = rasterTheme(region=rev(cols))
    levelplot(stack2013, par.settings=myTheme)+
      layer(sp.polygons(land, lwd=0.8, fill="gray80", col="gray85")) 
    
    #-----------------------------------------------------------
    # Percent contribution of each layer
    #-----------------------------------------------------------
    stack2013_percent <- stack(artisanal_fishing_combo <- raster("artisanal_fishing_combo_percent"),
                               demersal_destructive_fishing_combo <- raster("demersal_destructive_fishing_combo_percent"),
                               demersal_nondest_high_bycatch_combo <- raster("demersal_nondest_high_bycatch_combo_percent"),
                               demersal_nondest_low_bycatch_combo <- raster("demersal_nondest_low_bycatch_combo_percent"),
                               inorganic_combo <- raster("inorganic_combo_percent"),
                               invasives_combo <- raster("invasives_combo_percent"),
                               night_lights_combo <- raster("night_lights_combo_percent"),
                               ocean_acidification_combo <- raster("ocean_acidification_combo_percent"),
                               ocean_pollution_combo <- raster("ocean_pollution_combo_percent"),
                               oil_rigs_combo <- raster("oil_rigs_combo_percent"),
                               pelagic_high_bycatch_combo <- raster("pelagic_high_bycatch_combo_percent"),
                               pelagic_low_bycatch_combo <-  raster("pelagic_low_bycatch_combo_percent"),
                               plumes_fert_combo <- raster("plumes_fert_combo_percent"),
                               plumes_pest_combo <- raster("plumes_pest_combo_percent"),
                               population_combo <- raster("population_combo_percent"),
                               shipping_combo <- raster("shipping_combo_percent"),
                               slr_combo <- raster("slr_combo_percent"),
                               sst_combo <- raster("sst_combo_percent"),
                               uv_combo <- raster("uv_combo_percent"))
    
    names(stack2013_percent) <- c("artisanal fishing",
                                  "demersal destructive fishing",
                                  "demersal nondest high bycatch",
                                  "demersal nondest low bycatch",
                                  "inorganic",
                                  "invasives",
                                  "night lights",
                                  "ocean acidification",
                                  "ocean pollution",
                                  "oil rigs",
                                  "pelagic high bycatch",
                                  "pelagic low bycatch",
                                  "plumes fertilyzer",
                                  "plumes pesticide",
                                  "population",
                                  "shipping",
                                  "slr",
                                  "sst",
                                  "uv")
    
    
    cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
    myTheme = rasterTheme(region=rev(cols))
    levelplot(stack2013_percent, par.settings=myTheme)+
      layer(sp.polygons(land, lwd=0.8, fill="gray80", col="gray85")) 
    
    #-----------------------------------------------------------
    # Total pressure of specific groups
    #-----------------------------------------------------------
    stack2013_groups <- stack(raster("FishingPressures"),
                              raster("LandBasedPollutionPressures"),
                              raster("ClimateChangePressures"),
                              raster("OtherPressures"))
    
    names(stack2013_groups) <- c("Fishing pressures (N=6)",
                                 "Land-based pollution pressures (N=3)",
                                 "Climate change pressures (N=4)",
                                 "Other pressures (N=6)")
    cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
    myTheme = rasterTheme(region=rev(cols))
    levelplot(stack2013_groups, par.settings=myTheme)+
      layer(sp.polygons(land, lwd=0.8, fill="gray80", col="gray85")) 
    
    
    #-----------------------------------------------------------
    # 2013 minus 2008 data
    #-----------------------------------------------------------
    # read in 2013 clipped rasters (i.e., no land or border)
    setwd("C:\\Users\\frazier\\Desktop\\pressures\\PressureRasters")
    
    stack2013_2008 <- stack(demersal_destructive_fishing_combo <- raster("demersal_destructive_fishing_combo_2013_minus_2008_clip"),
                            demersal_nondest_high_bycatch_combo <- raster("demersal_nondest_high_bycatch_combo_2013_minus_2008_clip"),
                            demersal_nondest_low_bycatch_combo <- raster("demersal_nondest_low_bycatch_combo_2013_minus_2008_clip"),
                            night_lights_combo <- raster("night_lights_combo_2013_minus_2008_clip"),
                            ocean_pollution_combo <- raster("ocean_pollution_combo_2013_minus_2008_clip"),
                            oil_rigs_combo <- raster("oil_rigs_combo_2013_minus_2008_clip"),
                            pelagic_high_bycatch_combo <- raster("pelagic_high_bycatch_combo_2013_minus_2008_clip"),
                            pelagic_low_bycatch_combo <-  raster("pelagic_low_bycatch_combo_2013_minus_2008_clip"),
                            plumes_fert_combo <- raster("plumes_fert_combo_2013_minus_2008_clip"),
                            plumes_pest_combo <- raster("plumes_pest_combo_2013_minus_2008_clip"),
                            population_combo <- raster("population_combo_2013_minus_2008_clip"),
                            shipping_combo <- raster("shipping_combo_2013_minus_2008_clip"),
                            sst_combo <- raster("sst_combo_2013_minus_2008_clip"),
                            uv_combo <- raster("uv_combo_2013_minus_2008_clip"))
    
    stack2013_2008_names <- c("demersal destructive fishing",
                              "demersal nondest high bycatch",
                              "demersal nondest low bycatch",
                              "night lights",
                              "ocean pollution",
                              "oil rigs",
                              "pelagic high bycatch",
                              "pelagic low bycatch",
                              "plumes fertilyzer",
                              "plumes pesticide",
                              "population",
                              "shipping",
                              "sst",
                              "uv")
    
    names(stack2013_2008) <- stack2013_2008_names
    
    cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
    myTheme = rasterTheme(region=rev(cols))
    myTheme$layout.heights$main=12
    
    for(i in 1:length(stack2013_2008_names)){
      #i <- 1
      #pdf(sprintf('C:\\Users\\frazier\\Desktop\\pressures\\PressuresMaps\\%s_2013minus2008.pdf', 
      #            stack2013_2008_names[i])) # , width=1200, height=1000, res=150, pointsize=14)
      png(sprintf('C:\\Users\\frazier\\Desktop\\pressures\\PressuresMaps\\%s_2013minus2008.png', 
                  names(stack2013_2008)[i]), res=150, width=1200, height=1000) # , pointsize=14)
      #rng = extendrange(r[[f]],f=0.0001)
      p = levelplot(stack2013_2008, par.settings=myTheme, layers=i, 
                    main=stack2013_2008_names[i],
                    FUN.margin=mean) +
        layer(sp.polygons(land, lwd=0.8, fill="gray80", col="gray85"))
      print(p)
      dev.off()
    }
    
    ### using a consistent color scale:
    for(i in 1:length(stack2013_2008_names)){
      #i <- 10
      png(sprintf('C:\\Users\\frazier\\Desktop\\pressures\\PressuresMaps\\%s_2013minus2008_v2.png', 
                  names(stack2013_2008)[i]), res=150, width=1200, height=1000) # , pointsize=14)
      p=levelplot(stack2013_2008, par.settings=myTheme, layers=i, 
                  main=stack2013_2008_names[i],
                  FUN.margin=mean, 
                  at=seq(-2, 2, length=50),
                  colorkey=list(at=seq(-2, 2, length=50), col=rev(colorRampPalette(brewer.pal(11, "Spectral"))(255)))) +
        layer(sp.polygons(land, lwd=0.8, fill="gray80", col="gray85"))
      print(p)
      dev.off()
    }
    
    
    ### Histograms of the data:
    for(i in 1:length(stack2013_2008_names)){
      #  i=1
      png(sprintf('C:\\Users\\frazier\\Desktop\\pressures\\PressuresMaps\\%s_2013minus2008_histogram.png', 
                  names(stack2013_2008)[i]), res=150, width=1200, height=1000) # , pointsize=14)
      p <- histogram(stack2013_2008, layers=i, main=stack2013_2008_names[i], xlab="2013 minus 2008 pressure score")
      print(p)
      dev.off()
    }
    ############################################################################
    ## Make the maps, this code is modified from Ben's script: spp_grp_var.Rmd
    ## This script contains additional code that may be handy
    ############################################################################
    
    # To do: label the axes
    
    ### 2013 data:
    cum2013_noLand <- raster("D:\\PressureSummary\\cum2013_clip")
    cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
    myTheme = rasterTheme(region=rev(cols))
    myTheme$layout.heights$main=12
    levelplot(cum2013_noLand, par.settings=myTheme, 
              main="Absolute cumulative pressures, 2013",
              FUN.margin=mean) +
      layer(sp.polygons(land, lwd=0.8, fill="gray80", col="gray85"))
    
    # "Change in cumulative pressures, 2013 minus 2008"
    diff_noLand <- raster("D:\\PressureSummary\\diff2013_2008_clip")
    cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
    myTheme = rasterTheme(region=rev(cols))
    myTheme$layout.heights$main=12
    levelplot(diff_noLand, par.settings=myTheme, 
              main="Change in cumulative pressure, 2013 minus 2008",
              FUN.margin=mean, scales=list(draw=FALSE)) +
      layer(sp.polygons(land, lwd=0.8, fill="gray80", col="gray85"))
    
    ## to get values across rows:
    mean(getValues(cum2013_noLand, 50), na.rm=TRUE)
    
    ## potentially useful commands:
    ## Define the legend breaks
    #my.at <- seq(100, 1850, 500)
    #levelplot(r, at=my.at)
    #myColorkey <- list(at=my.at, ## where the colors change
    #                   labels=list(
    #                     at=my.at ## where to print labels
    #                   ))
    levelplot(r, at=my.at, colorkey=myColorkey)
    ## Get rid of scales
    # scales=list(draw=FALSE) # although centered everything else oddly.
    
    
    ### Original from Ben's (might be helpful to understand this)
    #png(sprintf('figure/levelplot_%s_again.png',f), width=1200, height=1000, res=150, pointsize=14)
    pdf(sprintf('figure/levelplot_%s_again.pdf',f)) # , width=1200, height=1000, res=150, pointsize=14)
    rng = extendrange(r[[f]],f=0.0001)
    p = levelplot(r.f, par.settings=myTheme, FUN.margin=mean, # main=f, 
                  at=seq(rng[1], rng[2], length.out=10)) + 
      layer(sp.polygons(wrld.sp, lwd=0.8, fill='gray95', col='gray80'))
    print(p)
    dev.off()
    
    
    
    
    
    ######################################################
    ## Code from Ben's script
    ## spp_grp_var.Rmd
    #####################################################
    # set rainbow color scheme
    cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255))
    sprintf('\n%02d: %s\n', which(flds==f), f)
    # color palettes
    pal = col.RdYlBu = brewer.pal(10, 'RdYlBu')
    
    # setup margins
    pdf(sprintf('figure/map_%s.pdf', f)) #, width=1000, height=800, res=150, pointsize=12)
    op = par(oma=c(0,0,0,0), mar=c(0,0,0,0), bg='transparent', cex=1.5) # c(bottom, left, top, right) 
    
    # plot polygons
    plot(ocean, col='gray90', border=NA)
    rng = seq(50,100,length.out=11)
    plot(eez, border=NA, add=T, col=pal[cut(v, rng, labels=1:10)])
    plot(land, col='gray80', border='grey75', add=T)
    title(f,lin=-1)  
    
    # plot legend
    p=par('usr'); px=diff(p[1:2]); py=diff(p[3:4]) # c(x1, x2, y1, y2)
    yh=0.03; x1=p[1]+px*0.2; x2=p[2]-px*0.1; y1=p[3]*0.85; y2=p[3]*0.85+py*yh
    ix = seq(x1, x2, length.out=10)
    ixd = diff(ix)[1]/2
    par(xpd=TRUE) # turn off clipping to plot region
    image(x = ix,
          y = c(y1, y2),
          z = matrix(1:10), col=pal, add=T)
    rect(x1-ixd,y1,x2+ixd,y2, border='gray20')
    rect(x1-ixd*3,y1,x1-ixd,y2, border='gray20')
    text(x = c(x1-ixd*2, seq(x1-ixd, x2+ixd, length.out=6)),
         y = y1, cex=0.6, pos=1,
         labels=c('NA',as.character(rng[seq(1,11,by=2)])))  # adj=c(0.5,0), # , offset=0.1,
    par(xpd=F) # turn back on clipping to plot region
    
    par(op)
    dev.off()
    
    pdf(file='figure/taxabias.pdf', width=11,height=8.5)
    g = ggplot(mlt.taxon, aes(x=grp, y=count, group=Taxon_category, colour=category)) + 
      geom_point(aes(colour=category, shape=category), size=10) +
      #scale_colour_manual(values=cols.rbg[c(3,2,1)]) + 
      scale_shape_discrete(solid=F) +
      geom_line() +
      geom_point(data=mlt.grp, aes(x=grp, y=count, colour=category, shape=category), size=10) + 
      scale_y_log10(breaks=10^c(1:6),labels=c('10','100','1,000','10,000','100,000','1,000,000')) + 
      xlab('taxon') + coord_flip() + 
      theme(legend.justification=c(1,1), legend.position=c(1,1), 
            plot.margin = unit(c(1, 1, 1, 4), 'lines')) # c(top,right,bottom,left)
    print(g)
    dev.off()
    
    plot(land, col='gray80', border='grey75', add=T)
    
    ################################################################
    ## end code from Ben's script
    ################################################################
    
    
    