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
#library(maptools)
#library(rgeos)
#library(dplyr)
library(tidyr)
library(rasterVis)
library(RColorBrewer)
library(fields)
#library(ggplot2)
#library(spatial.tools)
#library(classInt)

# set temporary directory to folder on neptune disk big enough to handle it
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

myTheme <- theme_bw() + theme(axis.text=element_text(size=20), 
                              axis.title=element_text(size=20, vjust=.15),
                              plot.margin=unit(c(1,1,1,1), "lines"),
                              legend.title = element_text(size=20),
                              legend.text= element_text(size=20),
                              plot.title = element_text(lineheight=.8, size=20),
                              strip.text.x = element_text(size = 18)) 

source('~/ohiprep/src/R/common.R')

#paths:
path <- file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Pressures-Summaries_frazier2013')
path_trim <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/TrimmedPressureLayers")
path_save <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ResultMaps")

rasters = file.path(dir_halpern2008, 
                    'mnt/storage/marine_threats/impact_layers_2013_redo')


# land layer for plots ----
rgn_ocn_cntry <- readOGR("/var/data/ohi/model/GL-NCEAS-OceanRegions_v2013a/data", layer="rgn_ocean_cntry_mol")
land <- rgn_ocn_cntry[!is.na(rgn_ocn_cntry@data$ISO_3digit) & rgn_ocn_cntry@data$rgn_id==0,]

legend.shrink <- 0.4
legend.width <- 0.6

# raster functions ----
raster_defaultLegend <- function(raster_data, saveLoc, title_legend=NA, title_plot=NA, cols){
#   par(mar=c(2,2,2,2))
#   par(oma=c(0,0,0,4))
  png(file.path(path_save, saveLoc), res=500, width=7, height=7, units="in")  
  #pdf(file.path(path_save, saveLoc))  #, width=1200, height=1000, res=150, pointsize=14)
  plot(raster_data, col=cols, axes=FALSE, box=FALSE, legend.shrink=legend.shrink, legend.width=legend.width, 
       axis.args=list(cex.axis=.8), 
       legend.args=list(text=title_legend, font=2, line=1, cex=1))
  title(main=title_plot, line=-5)
  plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
  dev.off()
}

raster_breaks <- function(raster_data, saveLoc, title, myBreaks, cols){
#   par(mar=c(2,2,2,2))
#   par(oma=c(0,0,0,4))
png(file.path(path_save, saveLoc), res=500, width=7, height=7, units="in")
plot(raster_data, col=cols, axes=FALSE, box=FALSE, breaks=myBreaks, legend=FALSE)
# add axis with fields package function:
def_breaks <- seq(min(myBreaks), max(myBreaks), length.out=length(myBreaks))
image.plot(raster_data, zlim = c(min(myBreaks), max(myBreaks)), 
           legend.only = TRUE, 
           legend.shrink=legend.shrink,
           legend.width=legend.width,
           col = cols,
           axis.args = list(at = def_breaks, labels = round(myBreaks, 1), cex.axis=.8))
plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
dev.off()
}


difPlots <- function(rast_data, my_breaks, file_name, legend=TRUE, extent=FALSE, title=NA){
  #pdf(file.path(path_save, 'Fig2a.SST_diff.pdf'))  #, width=1200, height=1000, res=150, pointsize=14)
  png(file.path(path_save, file_name), res=500, width=7, height=7, units="in")
  cols_low = colorRampPalette(c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598", "#FFFFBF"))((length(my_breaks)-1)/2)
  cols_high = colorRampPalette(c("#FEE08B", "#FDAE61",  "#F46D43", "#D53E4F", "#9E0142"))((length(my_breaks)-1)/2)
  cols <- c(cols_low, cols_high)  
  plot(rast_data, col=cols, axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE)
  if(!is.na(title)){title(main=title, line=-5)}
    
  if(legend){
  # add axis with fields package function:
  which(my_breaks==0)
  def_breaks <- c(my_breaks[1:which(my_breaks==0)], 0, my_breaks[(which(my_breaks==0)+1):length(my_breaks)])
  locs <- seq(min(def_breaks), max(def_breaks), length=length(def_breaks))
  locs[c(length(locs)/2, (length(locs)/2)+1)] <- 0 
  
  image.plot(rast_data, zlim = c(min(my_breaks), max(my_breaks)), 
             legend.only = TRUE, 
             legend.shrink=0.3,
             legend.width=0.6,
             col = c(cols_low, "#FFFFFF", cols_high),
             axis.args = list(at = locs, 
                              labels = round(def_breaks, 2), cex.axis=0.8))}
  
  plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
  
  if(extent){
    plot(extent(fl_extent), col="red", add=TRUE)
    plot(extent(uk_extent), col="red", add=TRUE)
    plot(extent(cn_extent), col="red", add=TRUE)
  #  plot(extent(med_extent), col="red", add=TRUE)
  }
  
  dev.off()  
}


### regions for coastal pressures:
# find extents
# plot(nutrient_zeroCut)
# extent <- drawExtent()

## crop different regions
# Florida
fl_extent <- c(-7900000, -7300000, 3000000, 3800000)
# ches_extent <- c(-6700000, -6400000, 4400000, 4800000)
# miss_extent <- c(-8500000, -8100000, 3450000, 3850000)
# gom_extent <- c(-8300000, -7400000, 2900000, 3850000)
med_extent <- c(1900000, 3300000, 3700000, 4900000)
uk_extent <- c(-800000, 200000, 5800000, 6900000)
cn_extent <- c(10000000, 11400000, 2500000, 4400000)

difference_data <- data.frame(layer=c("demersal_destructive_fishing", 
                                   "demersal_nondest_high_bycatch", 
                                   "demersal_nondest_low_bycatch",
                                   "night_lights",
                                   "oil_rigs",
                                   "pelagic_high_bycatch",
                                   "pelagic_low_bycatch",
                                   "plumes_fert",
                                   "plumes_pest",
                                   "population",
                                   "sst",
                                   "uv"),
                           newName=c("Demersal destructive fishing",
                                     "Demersal nondestructive high bycatch fishing",
                                     "Demersal nondestructive low bycatch fishing",
                                     "Light pollution",
                                     "Oil rigs", 
                                     "Pelagic high bycatch fishing",
                                     "Pelagic low bycatch fishing",
                                     "Nutrient pollution",
                                     "Organic pollution",
                                     "Direct human impact",
                                     "Sea surface temperature",
                                     "UV"))


coastal_pressures <- data.frame(layers = c("artisanal_fishing", 
                                          "inorganic",
                                          "invasives",
                                          "night_lights",
                                          "oil_rigs",
                                          "plumes_fert",
                                          "plumes_pest",
                                          "population",
                                          "slr"),
                                newName = c("Artisanal fishing",
                                           "Inorganic pollution",
                                           "Invasive species",
                                           "Light pollution",
                                           "Oil rigs",
                                           "Nutrient pollution",
                                           "Organic pollution",
                                           "Direct human impact",
                                           "Sea level rise"))

ocean_pressures <- data.frame(layer = c("demersal_destructive_fishing",
                                    "demersal_nondest_high_bycatch", 
                                    "demersal_nondest_low_bycatch",  
                                    "ocean_acidification",
                                    "ocean_pollution", 
                                    "pelagic_high_bycatch",
                                    "pelagic_low_bycatch",
                                    "shipping",
                                    "sst", 
                                    "uv"),
                          newName = c("Demersal destructive fishing",
                                    "Demersal nondestructive high bycatch fishing",
                                    "Demersal nondestructive low bycatch fishing",
                                    "Ocean acidification",
                                    "Ocean-based pollution",
                                    "Pelagic high bycatch fishing",
                                    "Pelagic low bycatch fishing",
                                    "Shipping",
                                    "Sea surface temperature",
                                    "UV"))

ocean_pressures_diff <- data.frame(layer = c("demersal_destructive_fishing",
                                        "demersal_nondest_high_bycatch", 
                                        "demersal_nondest_low_bycatch",  
                                        "pelagic_high_bycatch",
                                        "pelagic_low_bycatch",
                                        "sst", 
                                        "uv"),
                              newName = c("Demersal destructive fishing",
                                          "Demersal nondestructive high bycatch fishing",
                                          "Demersal nondestructive low bycatch fishing",
                                          "Pelagic high bycatch fishing",
                                          "Pelagic low bycatch fishing",
                                          "Sea surface temperature",
                                          "UV"))

coastal_pressures_diff <- data.frame(layers = c("night_lights",
                                           "oil_rigs",
                                           "plumes_fert",
                                           "plumes_pest",
                                           "population"),
                                newName = c("Light pollution",
                                            "Oil rigs",
                                            "Nutrient pollution",
                                            "Organic pollution",
                                            "Direct human impact"))

######################################################
## Paper figures
######################################################
## Figure 1A: Absolute per-pixel difference in cumulative impact score (2013-2008) ----
# "Change in cumulative pressures, 2013 minus 2008"
diff_noLand <- raster(file.path(path_trim, "Pressures2013minus2008/global_cumulative_impact_dif"))
#plot(diff_noLand)
#diff_noLand_subsample <- sampleRegular(diff_noLand, size=500000, asRaster = TRUE)

raster_defaultLegend(raster_data=diff_noLand, 
                     saveLoc="Fig1.CumImpDiff.png", 
                     title_legend="Change in\ncumulative impact",
                     cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(250)))


## determine cells >0
reclassify(diff_noLand, c(-Inf,0, 0, 
                          0, Inf, 1),
           filename=file.path(path, "ModifiedPressureMaps/DiffCounts"), 
           progress="text")

tmp <- raster(file.path(path, "ModifiedPressureMaps/DiffCounts"))
freq(tmp, value=1, useNA="no", progress="text")
freq(tmp, value=0, useNA="no", progress="text")

#check:
freq(diff_noLand, value=0, progress="text")

# part b: figure out high and low regions and high levels of increase/decrease----
# high low regions:
# Cum2013 <- raster(file.path(path_trim, "Pressures2013/global_cumulative_impact_2013_all_layers"))
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
# plot(DiffHighLowScore)
# freq(DiffHighLowScore)

#pdf(file.path(path_save, 'Fig2b.value_trend.pdf')) #, width=1200, height=1000)
png(file.path(path_save, 'Fig1b.value_trend_v2.png'), res=500, width=7, height=7, units="in")
par(mar=c(2,2,2,2))
par(oma=c(0,0,0,4))
arg <- list(at=c(-2,-1,0,1,2), 
            labels=c("low/\n decreasing", "low/\n increasing", "neither", "high/\n decreasing", "high/\n increasing"), 
            cex.axis=0.9)

color <- c("#8BB4EC", "#B2D698", "#F7EE3B0D", "#E77D27", "#991219") # trying rainbow color scheme
#color <- c("#023FA5", "#BEC1D4", "#D1D1D10D", "#D6BCC0", "#8E063B") #red/blue color scheme

plot(DiffHighLowScore, col=color, axis.arg=arg, axes=FALSE, box=FALSE, legend.shrink=0.3,
     legend.width=0.6,
     legend.args=list(text="score/trend", font=2, line=1, cex=1))
plot(land, add=TRUE, border="gray60", col="gray90", lwd=0.4)
dev.off()


## Fig. 2: EEZ change in CHI vs. absolute CHI ----

eez2ohi <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-OceanRegions_v2013a/data/rgn_2013master.csv"), stringsAsFactors=FALSE) %>%
  select(rgn_id_2013, eez_id, eez_nam) 

eezArea <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-OceanRegions_v2013a/data/eez_fao_gcs_area.csv"), stringsAsFactors=FALSE)%>%
  mutate(eez_id = as.character(eez_id))

coastal_pop <- read.csv("../ohi-global/eez2013/layers/mar_coastalpopn_inland25mi.csv", stringsAsFactors=FALSE) %>%
  filter(year==2015) %>%
  select(rgn_id_2013=rgn_id, popsum)

coastal_pop_trend <- read.csv("../ohi-global/eez2013/layers/mar_coastalpopn_inland25mi.csv", stringsAsFactors=FALSE) %>%
  arrange(rgn_id, year) %>%
  group_by(rgn_id) %>%
  do(mdl = lm(log(popsum+1) ~ year, data=.)) %>%
  do(data.frame(
    rgn_id_2013=.$rgn_id,
    pop_trend=coef(.$mdl)[['year']]))

# test <- read.csv("../ohi-global/eez2013/layers/mar_coastalpopn_inland25mi.csv", stringsAsFactors=FALSE) %>%
#   filter(rgn_id==1)
# mod <- lm(log(popsum+1)~year, data=test)
# summary(mod)


data2013 <- read.csv("ZonalExtractionData/oneYearNorm_2013_eez.csv", stringsAsFactors=FALSE) %>%
  select(eez_id, global_cumul_impact_2013_all_layers) 

datadiff <- read.csv("ZonalExtractionData/diff_2013minus2008_eez.csv", stringsAsFactors=FALSE) %>%
  select(eez_id, global_cumul_impact_2013_minus_2008) %>%
  left_join(data2013, by=c("eez_id")) %>%
  mutate(eez_id=as.character(eez_id)) %>%
  left_join(eez2ohi) %>%
  filter(!(rgn_id_2013 %in% 255),
         !is.na(rgn_id_2013)) %>%
  left_join(coastal_pop, by=c("rgn_id_2013")) %>%
  mutate(log_population=log(popsum + 1)) %>%
  left_join(coastal_pop_trend, by=c("rgn_id_2013")) %>%
  left_join(eezArea, by=c("eez_id")) %>%
  mutate(log_area_km2 = log(area_km2))

## version 1
ggplot(datadiff, aes(x=global_cumul_impact_2013_minus_2008, y=global_cumul_impact_2013_all_layers, size=log_population)) +
  geom_point(shape=19, fill="gray", alpha=.6, color="gray") +
  labs(x=expression(gamma ~ "CHI"), y='2013 CHI', size="ln population") +
  geom_hline(yintercept=median(datadiff$global_cumul_impact_2013_all_layers), color="red", linetype=2) +
  geom_vline(xintercept=0, color="red", linetype=2) +
  scale_size(range=c(2,8)) +
  myTheme + 
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(colour = NA))

#ggsave(file.path(path_save, 'CHI2013vsCHIGamma.pdf'))

## version 2
my_cols <- diverge_hcl(3, c=100, l=c(50, 90), power=1)
ggplot(datadiff, aes(x=global_cumul_impact_2013_minus_2008, y=global_cumul_impact_2013_all_layers, size=area_km2)) +
  geom_point(aes(color=pop_trend), shape=19, alpha=.8) +
  labs(x=expression(gamma ~ "CHI"), y='2013 CHI', size=expression("eez area, km"^2), color="ln pop tren") +
  geom_hline(yintercept=median(datadiff$global_cumul_impact_2013_all_layers), color="red", linetype=2) +
  geom_vline(xintercept=0, color="red", linetype=2) +
  scale_size(range=c(3,15)) +
  scale_color_gradient(low="yellow", high="brown") +
  myTheme + 
  theme(legend.justification=c(1,1),
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(colour = NA))

ggsave(file.path(path_save, 'CHI2013vsCHIGamma_area.pdf'))

## Figure 3 ----
# Part a: Global 2013 scores
Cum2013 <- raster(file.path(path_trim, "Pressures2013/global_cumulative_impact_2013_all_layers"))

## most breaks are 0.1 quantiles, but the last one is 0.5
# quantile(Cum2013, probs=c(seq(0,1, by=0.1)))
# quantile(Cum2013, probs=c(seq(0,1, by=0.05)))

my_breaks <- c(0, 1.781894, 2.377814, 2.986494, 3.316144, 3.558642, 3.750878, 
               3.923132, 4.127960, 4.384074, 4.571275, 16)
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)+2))[2:12] #stripping extreme ends of the color spectrum
#cols = rev(brewer.pal(11, 'Spectral')) #full spectrum

raster_breaks(raster_data=Cum2013, saveLoc="Fig3.CumImp2013.png", myBreaks=my_breaks, cols=cols)
  


## Figure 4 ----

# a: SST_diff

sst_zeroCut <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/sst_combo_2013_minus_2008.tif"))
# histogram(sst_zeroCut) #where to place breaks
# plot(sst_zeroCut)

## trying natural jenks - looks good but going with regular intervals:
## NOTE: natural jenks takes a long time to run.
# cuts <- classIntervals(na.omit(sampleRegular(sst_zeroCut, 100000)), n=10,style="jenks")
# my_breaks  <- c(-2.6, -1.19, -0.676, -0.2814, 0, 0.2289, 0.4765, 0.7906, 1.1543, 1.5390, 2.6) #natural jenks
## This goes with the natural jenks (lots of effort to get uneven breaks around zero): 
# low_cols = rev(brewer.pal(length(my_breaks[my_breaks<0])*2, 'Spectral'))[1:length(my_breaks[my_breaks<0])]
# high_cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks[my_breaks>0])*2))
# high_cols = high_cols[(length(my_breaks[my_breaks>0])+1):length(high_cols)]
# cols <- c(low_cols, high_cols)
# locs <- seq(min(def_breaks), max(def_breaks), length=length(def_breaks))
# locs[c(length(low_cols)+1, length(low_cols)+2)] <- mean(c(locs[length(low_cols)+1], locs[length(low_cols)+2]))  
# col = c(low_cols, '#FFFFFF', high_cols),

my_breaks <- c(-2.6,-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.6) # for plot
difPlots(sst_zeroCut, my_breaks=my_breaks, file_name="Fig4a.SST_diff.png")


#c:  demersal destructive fishing
dem_fishing_zerocut <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/demersal_destructive_fishing_combo_2013_minus_2008.tif"))
#plot(dem_fishing_zerocut)
#histogram(dem_fishing_zerocut)

# ## jenks
# cuts <- classIntervals(na.omit(sampleRegular(dem_fishing_zerocut, 50000)), n=10,style="jenks")
# my_breaks <- c(-0.200318, -0.1358308, -0.09420618, -0.0626831, -0.03774771, -0.01757202, 0,
#                0.01724225, 0.05912814) #natural jenks

my_breaks <- c(-0.7, -0.2, -0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15, 0.2, 0.7)
difPlots(dem_fishing_zerocut, my_breaks=my_breaks, file_name="Fig4c.dem_diff.png")


#d:  pelagic fishing
pel_fishing_diff <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/pelagic_high_bycatch_combo_2013_minus_2008.tif"))
# histogram(pel_fishing_diff, main="pelagic high-bycatch: 2013 minus 2008")
# plot(pel_fishing_diff)

my_breaks <- c(-0.53, -0.2, -0.10, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.10, 0.2, 0.53)
difPlots(pel_fishing_diff, my_breaks=my_breaks, file_name="Fig4d.pel_diff.png")


#b:  nutrient_diff
nutrient_zeroCut <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/plumes_fert_combo_2013_minus_2008.tif"))
my_breaks <- c(-0.69 , -0.10, -0.08, -0.06, -0.04, -0.02, 0,
               0.02, 0.04, 0.06, 0.08, 0.1, 0.69)
difPlots(nutrient_zeroCut, my_breaks=my_breaks, file_name="Fig4b.nutrients_global.png", extent=TRUE)


fl <- crop(nutrient_zeroCut, fl_extent)
difPlots(fl, my_breaks=my_breaks, file_name="Fig4b.nutrients_fl_diff.png", legend=FALSE)

med <- crop(nutrient_zeroCut, med_extent)
difPlots(med, my_breaks=my_breaks, file_name="Fig4b.nutrients_med_diff.png", legend=FALSE)

uk <- crop(nutrient_zeroCut, uk_extent)
difPlots(uk, my_breaks=my_breaks, file_name="Fig4b.nutrients_uk_diff.png", legend=FALSE)

cn <- crop(nutrient_zeroCut, cn_extent)
difPlots(cn, my_breaks=my_breaks, file_name="Fig4b.nutrients_cn_diff.png", legend=FALSE)



## SOM Fig. 1: Histogram of per pixel difference scores ----

## global combined pressures

# Figure S1A: 2013 minus 2008
pressure_diff <- raster(file.path(path_trim, "Pressures2013minus2008/global_cumulative_impact_dif")) 

pdf(file.path(path_save, 'Histograms/SOMFig1a.global_hist_diff.pdf'))  #, width=1200, height=1000, res=150, pointsize=14))    
    histogram(pressure_diff, scales=list(y=list(at=NULL)), 
              main="Global cumulative impact score (2013 minus 2008)")
    dev.off()

# Figure S1C: 2013 scores
Cum2013 <- raster(file.path(path_trim, "Pressures2013/global_cumulative_impact_2013_all_layers"))
pdf(file.path(path_save, 'Histograms/SOMFig1c.global_hist_2013.pdf'))  #, width=1200, height=1000, res=150, pointsize=14))    
histogram(Cum2013, scales=list(y=list(at=NULL)), 
          main="2013 global cumulative impact score")
dev.off()


## Figure S2B: individual pressures    
layers <- dir(file.path(path_trim, "Pressures2013minus2008"))
layers <- gsub(".grd", "", layers)
layers <- gsub(".gri", "", layers)
layers <- unique(layers)
layers <- layers[-which(layers == "global_cumulative_impact_dif")]

no <- length(layers) # amount of files found
imagestack <- stack() # you initialize your raster stack

for (i in 1:no){
  #i=2
  image <- raster(file.path(path_trim, "Pressures2013minus2008", layers[i])) 
  imagestack <- addLayer(imagestack,image)
}

#getting cell status
rasterMean <- cellStats(imagestack, stat="mean")
rasterMin <- cellStats(imagestack, stat="min")
rasterMax <- cellStats(imagestack, stat="max")

rasterData <- cbind(rasterMean, rasterMin, rasterMax)

write.csv(rasterData, "DataSummary/2013minus2008_data.csv")

## making figures:

## Can't get the x-axis to display independently using rasterVis:
# layers_2 <- gsub("_combo_dif", "", layers)
# layers_2 <- gsub("_", " ", layers)
# names(imagestack) <- layers_2

# pdf(file.path(path_save, 'SOMFig1b.pressures_diff.pdf'))  #, width=1200, height=1000, res=150, pointsize=14))
# histogram(imagestack, layout=c(3,4), cex=.7, scales = list(relation = "free"))
# dev.off() 


# plotting each individually:
for(i in 1:length(difference_data$layer)){
  #i <- 1
  dif <- raster(file.path(path_trim, "Pressures2013minus2008", 
                          paste(difference_data$layer[i], "_combo_dif", sep="")))
  
  png(file.path(path_save, sprintf('Histograms/%s.png', 
                                   difference_data$layer[i])), res=500, width=7, height=7, units="in")      
  print(histogram(dif, main=list(as.character(difference_data$newName[i]), cex=1.5), 
                  scales=list(y=list(at=NULL), cex=1.3)))
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

#################################################
## determine where high values are located:
reclassify(old2008, c(-Inf, 19, 0,
                          19, Inf, 1), 
           filename=file.path(path, 'ModifiedPressureMaps/old2008_greaterthan19'),
           progress="text", overwrite=TRUE)

rasterOutliers <- raster(file.path(path, 'ModifiedPressureMaps/old2008_greaterthan19'))
plot(rasterOutliers)

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


clipLayer("rasterOutliers", file.path(path, 'ModifiedPressureMaps/old2008_greaterthan19_clipped'))


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

eez_2013 <- read.csv("ZonalExtractionData/oneYearNorm_2013_eez.csv") %>%
  filter(eez_nam != "Conflict Zone") %>%
  mutate(eez_nam = gsub("Cura\x8dao", "Curacao", eez_nam),
         eez_nam = gsub("R\x8epublique du Congo", "Republique du Congo", eez_nam),
         eez_nam = gsub("R\x8eunion", "Reunion", eez_nam))


ranks <- eez_2013 %>%
  mutate(region_id = sprintf("%s (%s)",eez_nam, eez_id)) %>%
  select(region_id, global_cumul_impact_2013_all_layers) %>%
  arrange(-global_cumul_impact_2013_all_layers)

batch1 <- ranks$region_id[1:79]
batch2 <- ranks$region_id[80:159]
batch3 <- ranks$region_id[160:length(ranks$region_id)]

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


library(ggplot2)
p <- ggplot(subset(eez_2013, region_id %in% batch1), aes(x=factor(region_id, levels=(region_id)[order(global_cumul_impact_2013_all_layers)]), y=value, fill=pressure, order=desc(pressure))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=myPalette(19)) +
  coord_flip() +
  theme_bw() +
  labs(y="Pressure", x="") + 
  ylim(0, 9) + 
  guides(fill=guide_legend(reverse=TRUE)) +
  theme(axis.text.y=element_text(size=10),
        legend.justification=c(1,0), legend.position=c(1,0))
p
ggsave(file.path(path_save, 'Impacts4EEZs_p1.pdf'), height=20, width=10)
  
p %+% subset(eez_2013, region_id %in% batch2)
ggsave(file.path(path_save, 'Impacts4EEZs_p2.pdf'), height=20, width=10)

p %+% subset(eez_2013, region_id %in% batch3)
ggsave(file.path(path_save, 'Impacts4EEZs_p3.pdf'), height=20, width=10)

##############################################
## SOM Fig. 5: individual pressures----
# 2013/normalized one time period/averaged by num ecosystems ---- 
# NOTE:  This is the most complete data for 2013, but can't be compared to 2008 data

for(i in 1:length(ocean_pressures$layer)){
# i <- 2
  rast_data <- raster(file.path(path, 
              sprintf("ModifiedPressureMaps/Zero_cut/Pressures2013_oneyear/%s_combo.tif", ocean_pressures$layer[i])))
  save_to <- sprintf("Pressures2013_OneYear/%s.png", ocean_pressures$layer[i])
  
  raster_defaultLegend(rast_data, save_to, title_plot=ocean_pressures$newName[i], 
                        cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(250)))
  
}

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(250))

region_plot <- function(region_extent, saveFile, cols){
#   region_extent <- fl_extent
#   saveFile <- "FL"

  for(i in 1:length(coastal_pressures$layers)){
#   i <- 1  
pressure <- coastal_pressures$layer[i]
raster_data <- raster(file.path(path, 
            sprintf("ModifiedPressureMaps/Zero_cut/Pressures2013_oneyear/%s_combo.tif", pressure)))

## crop region
region <- crop(raster_data, region_extent)

png(file.path(path_save, sprintf("Pressures2013_OneYear/%s/%s.png",  saveFile, coastal_pressures$layer[i])), 
    res=500, width=7, height=7, units="in")  
  plot(region, col=cols, 
       breaks=seq(0,
                  maxValue(raster_data), length.out=250),
         axes=FALSE, box=FALSE, legend.shrink=legend.shrink, legend.width=legend.width, 
       axis.args=list(cex.axis=1.1,
                      at=seq(0,
                             maxValue(raster_data), .2),
                      labels=seq(0,
                                 maxValue(raster_data), .2)))
  title(main=gsub("_", " ", coastal_pressures$newName[i]))
  plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
  dev.off()
}
}

region_plot(fl_extent, "FL", cols=cols)
region_plot(med_extent, "MED", cols=cols)
region_plot(uk_extent, "UK", cols=cols)
region_plot(cn_extent, "CN", cols=cols)


# tried to use quantiles for breakpoints, but didn't work because
# so many zeros for many pressures
#test <- quantile(raster(stack_2013_one, 1), probs=seq(0,1,0.1))


## SOM Fig. 6: Global map of summed stressor intensity (unweighted by habitat vulnerability)----
stress_path <- file.path(rasters, 
                         'impact_layers/final_impact_layers/threats_2013_final/normalized_by_one_time_period')

tifs = list.files(stress_path, 
                  pattern=glob2rx('*.tif'))

# visualize:
tmp <- raster(file.path(rasters, 
                         'impact_layers/final_impact_layers/threats_2013_final/normalized_by_one_time_period/ocean_pollution.tif'))
plot(tmp)

tifs_1 <- c("demersal_destructive_fishing.tif", "demersal_nondest_high_bycatch.tif", "demersal_nondest_low_bycatch.tif",
            "ocean_acidification.tif", "pelagic_high_bycatch.tif", "pelagic_low_bycatch.tif", "shipping.tif",
            "slr.tif", "sst.tif", "uv.tif")  # correct resolution

tifs_2 <- c("artisanal_fishing.tif", "inorganic.tif", "plumes_fert.tif", "plumes_pest.tif", 
            "invasives.tif", "night_lights.tif", "oil_rigs.tif", "population.tif") # need to fix_extent

tifs_3 <- c("ocean_pollution.tif")

stack_2013_one_raw <- stack()
for(i in 1:length(tifs_1)){
  tmp <- raster(file.path(stress_path, tifs_1[i]))
  stack_2013_one_raw <- stack(stack_2013_one_raw, tmp )
}

for(i in 1:length(tifs_2)){
#  i <- 1
  tmp <- raster(file.path(stress_path, tifs_2[i]))
  tmp = extend(tmp, raster(stack_2013_one_raw, 1)) 
  extent(tmp) = extent(raster(stack_2013_one_raw, 1))
  stack_2013_one_raw <- stack(stack_2013_one_raw, tmp)  
}

for(i in 1:length(tifs_3)){
  tmp <- raster(file.path(stress_path, tifs_3[i]))
  tmp <- modify_raster_margins(tmp, extent_delta=c(1,0,1,0))
  extent(tmp) = extent(raster(stack_2013_one_raw, 1))
  stack_2013_one_raw <- stack(stack_2013_one_raw, tmp)  
}

calc(stack_2013_one_raw, sum, filename = file.path(path_save, "Pressures2013_raw_2013_OneYear/SumRawPressures"), progress="text")
calc(stack_2013_one_raw, sum, na.rm=TRUE, filename = file.path(path_save, "Pressures2013_raw_2013_OneYear/SumRawPressures_na_rm"), progress="text")

total <- raster(file.path(path_save, "Pressures2013_raw_2013_OneYear/SumRawPressures_na_rm"))
plot(total)

# clip outter parts
clip_raster <- raster(file.path(path, "clip_raster"))

s <- stack(total, clip_raster)
overlay(s, fun=function(x,y) x*y, 
        filename=file.path(path_save, "Pressures2013_raw_2013_OneYear/SumRawPressures_na_rm_clipped"),
        progress="text", overwrite=TRUE)


total <- raster(file.path(path_save, "Pressures2013_raw_2013_OneYear/SumRawPressures_na_rm_clipped"))


fl <- crop(nutrient_zeroCut, fl_extent)



png(file.path(path_save, "Pressures2013_raw_2013_OneYear/SumRawPressures_na_rm_clipped.png"), res=500, width=7, height=7, units="in")  
  cols = colorRampPalette(brewer.pal(11, 'Spectral'))(250)
  plot(total, col=rev(cols), axes=FALSE, box=FALSE, legend.shrink=0.5, legend.width=0.6, 
       breaks=seq(minValue(total), maxValue(total), length.out=250),
       axis.args=list(cex.axis=1.3,
       at=seq(minValue(total), maxValue(total), 2),
      labels=seq(minValue(total), maxValue(total), 2))
       )
  title(main="Total stressor intensity", line=-5)
  plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
plot(extent(fl_extent), col="red", add=TRUE)
plot(extent(uk_extent), col="red", add=TRUE)
plot(extent(cn_extent), col="red", add=TRUE)
plot(extent(med_extent), col="red", add=TRUE)
  dev.off()

region_plot <- function(raster_name, name){
#   raster_name <- fl
#   name <- "FL_SOM6a"
  png(file.path(path_save, sprintf("Pressures2013_raw_2013_OneYear/%s.png",  name)), res=500, width=7, height=7, units="in")  
  plot(raster_name, col=c(rev(cols)), 
       breaks=seq(minValue(total), maxValue(total),length.out=250),
       axes=FALSE, box=FALSE, legend=FALSE,
#       legend.shrink=0.5, legend.width=0.6, 
#        axis.args=list(cex.axis=1.3,
#                       at=seq(minValue(total),
#                              maxValue(total), 2),
#                       labels=seq(minValue(total),
#                                  maxValue(total), 2))
)
  plot(land, add=TRUE, border="gray60", col="gray90", lwd=0.5)
dev.off()
}  

fl <- crop(total, fl_extent)
region_plot(fl, "FL_SOM6a")

med <- crop(total, med_extent)
region_plot(med, "med_SOM6b")

uk <- crop(total, uk_extent)
region_plot(uk, "uk_SOM6c")

cn <- crop(total, cn_extent)
region_plot(cn, "cn_SOM6d")

#####################################################3
## SOM Fig. 7: Global map of % contribution of each of 19 stressors to cumulative impact score----
######################################################

tifs = list.files(file.path(path, 'ModifiedPressureMaps/Zero_cut/Pressures2013_oneyear'), pattern=glob2rx('*.tif'))

stack_2013_one <- stack()
for(i in 1:length(tifs)){
  tmp <- raster(file.path(file.path(path, 'ModifiedPressureMaps/Zero_cut/Pressures2013_oneyear'), tifs[i]))
  names(tmp) <- tifs[i]
  stack_2013_one <- stack(stack_2013_one, tmp )
}

ci <- raster(file.path(rasters, "global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/all_layers/global_cumul_impact_2013_all_layers.tif"))

for(i in 1:length(names(stack_2013_one))){
#i <- 2
  s <- stack(raster(stack_2013_one, i),
             ci)
  overlay(s, fun=function(a,b) (a/b)*100, 
                filename=file.path(path, sprintf("ModifiedPressureMaps/PercentContribution2CI/%s_pctContribution
                                                 ", names(stack_2013_one)[i])),
                progress="text", overwrite=TRUE)
}


tifs = list.files(file.path(path, "ModifiedPressureMaps/PercentContribution2CI/ZeroAsNA"), pattern=glob2rx('*.gri'))
tifs <- gsub(".gri", '', tifs)


cols = colorRampPalette(brewer.pal(11, 'Spectral'))(250)[1:250]

for(i in 1:length(ocean_based$layer)){
  #i=1
  png(file.path(path_save, sprintf("PercentContributions2013_oneYear/%s_legend.png",  
                                   ocean_based$layer[i])), res=500, width=7, height=7, units="in")  
  plot(raster(file.path(path, "ModifiedPressureMaps/PercentContribution2CI/ZeroAsNA", 
                        sprintf('%s_pctContribution', ocean_based$layer[i]))), 
       col=c(rev(cols)), 
       breaks=seq(0, 100, length.out=250),
       axes=FALSE, box=FALSE, legend=FALSE)
#       legend.shrink=0.5, legend.width=0.6, 
#        axis.args=list(cex.axis=1.3,
#                       at = seq(0, 100, 20),
#                       labels = seq(0,100, 20)))
  title(main=ocean_based$newName[i], line=-5)
  plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
  dev.off()
}

# get a legend:
  i=1
  png(file.path(path_save, sprintf("PercentContributions2013_oneYear/legend.png",  
                                   ocean_based$layer[i])), res=500, width=7, height=7, units="in")  
  plot(raster(file.path(path, "ModifiedPressureMaps/PercentContribution2CI/ZeroAsNA", 
                        sprintf('%s_pctContribution', ocean_based$layer[i]))), 
       col=c(rev(cols)), 
       breaks=seq(0, 100, length.out=250),
       axes=FALSE, box=FALSE, 
        legend.shrink=0.5, legend.width=0.6, 
         axis.args=list(cex.axis=1.3,
                        at = seq(0, 100, 20),
                        labels = seq(0,100, 20)))
  title(main=ocean_based$newName[i], line=-5)
  plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
  dev.off()


# ##if the layer errors out it is due to interactions with ggplot2. 
# ## can be remedied by removing ggplot2 for this plot
# p.strip <- list(cex=.6, lines=2) 
# cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
# myTheme = rasterTheme(region=rev(cols))
# png(file.path(path_save, "PercentContributions2013_oneYear/pct_contribution_v2.png"), 
#     width=6, height=9, units='in', res=2500)
# p=levelplot(stack_ocean_based, par.settings=myTheme, 
#             scales=list(draw=FALSE), layout=c(2,5), 
#             labels=list(cex=0.2),
#             par.strip.text=p.strip,
#             names.attr=ocean_based$newName)  +
#    layer(sp.polygons(land, lwd=0.8, fill="gray80", col="gray85")) 
# print(p)
# dev.off()


stack_2013_coastal <- stack()
for(i in 1:length(coastal_pressures$layer)){
  tmp <- raster(file.path(path, "ModifiedPressureMaps/PercentContribution2CI/ZeroAsNA", 
                          sprintf("%s_pctContribution",coastal_pressures$layer[i])))
  names(tmp) <- coastal_pressures$newName[i]
  stack_2013_coastal <- stack(stack_2013_coastal, tmp )
}


## plot function for regions:
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(250)[1:250]
region_plots <- function(raster, save_file){
  for(i in 1:dim(raster)[3]){
    #save_file <- "fl"
    #raster <- fl
    #i=1
    png(file.path(path_save, sprintf("PercentContributions2013_oneYear/%s/%s.png",
                                     save_file,
                                     names(raster(raster, i)))), res=500, width=7, height=7, units="in")  
    plot(raster(raster, i), 
         col=c(rev(cols)), 
         breaks=seq(0, 100, length.out=250),
         axes=FALSE, box=FALSE, legend=FALSE)
    #       legend.shrink=0.5, legend.width=0.6, 
    #        axis.args=list(cex.axis=1.3,
    #                       at = seq(0, 100, 20),
    #                       labels = seq(0,100, 20)))
    title(main=gsub("\\.", " ", names(raster(raster, i))), cex.main=1.5)
    plot(land, add=TRUE, border="gray70", col="gray90", lwd=0.5)
    dev.off()
  }
}



## crop different regions
fl <- crop(stack_2013_coastal, fl_extent)
region_plots(fl, "fl")

# med
med <- crop(stack_2013_coastal, med_extent)
region_plots(med, "med")

uk <- crop(stack_2013_coastal, uk_extent)
region_plots(uk, "uk")

cn <- crop(stack_2013_coastal, cn_extent)
region_plots(cn, "cn")

##########################################################################
## SOM Fig. 8: Global map of # of non-zero stressors in each pixel----
##########################################################################

tifs = list.files(file.path(path_trim, 'Pressures2013'), pattern=glob2rx('*.gri'))
tifs <- gsub(".gri", '', tifs)

stack_2013_one <- stack()
for(i in 1:length(tifs)){
  tmp <- raster(file.path(file.path(path_trim, 'Pressures2013'), tifs[i]))
  names(tmp) <- tifs[i]
  stack_2013_one <- stack(stack_2013_one, tmp )
}

plot(raster(stack_2013_one, 2))

for(i in 1:length(tifs)){
  #i <- 1
  reclassify(raster(stack_2013_one, i), c(-Inf, 0, 0,
                                          0, Inf, 1), 
             filename=file.path(path, 
              sprintf('ModifiedPressureMaps/ZeroOneClassification/%s_ZeroClassify', tifs[i])),
             progress="text")
}

tifs = list.files(file.path(path, 'ModifiedPressureMaps/ZeroOneClassification'), pattern=glob2rx('*.gri'))

stack_2013_ZeroOne <- stack()
for(i in 1:length(tifs)){
  tmp <- raster(file.path(file.path(path, 'ModifiedPressureMaps/ZeroOneClassification'), tifs[i]))
  names(tmp) <- tifs[i]
  stack_2013_ZeroOne <- stack(stack_2013_ZeroOne, tmp )
}

plot(raster(stack_2013_ZeroOne, 19))

calc(stack_2013_ZeroOne, sum, na.rm=TRUE, 
     filename = file.path(path, "ModifiedPressureMaps/SumZeroOneData"), 
     progress="text", overwrite=TRUE)

tmp <- raster(file.path(path, "ModifiedPressureMaps/SumZeroOneData"))

# clip outter parts
clip_raster <- raster(file.path(path, "clip_raster"))

s <- stack(tmp, clip_raster)
overlay(s, fun=function(x,y) x*y, 
        filename=file.path(path, "ModifiedPressureMaps/SumZeroOneData_clipped"),
        progress="text", overwrite=TRUE)

tmp <- raster(file.path(path, "ModifiedPressureMaps/SumZeroOneData_clipped"))
freq(tmp, value=0, useNA="no", progress="text")  #2388249
freq(tmp, value=1, useNA="no", progress="text")  #7212563

png(file.path(path_save, "SumNonZeroPressures.png"), res=500, width=7, height=7, units="in")  
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(255)
plot(tmp, col=rev(cols), axes=FALSE, box=FALSE, legend.shrink=0.5, legend.width=0.6, 
     axis.args=list(cex.axis=1.3))
title(main="Sum of non-zero cells", line=-5)
plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
dev.off()

###################################
# S9: no longer doing
###################################


####################################
## S10: Change in stressor impact----
####################################

dem_fishing_zerocut <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/demersal_destructive_fishing_combo_2013_minus_2008.tif"))
my_breaks <- c(-0.7, -0.2, -0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15, 0.2, 0.7)
difPlots(dem_fishing_zerocut, my_breaks=my_breaks, 
         file_name=sprintf("Pressures2013minus2008/%s.png", names(dem_fishing_zerocut)), 
         title="Demersal destructive fishing")

dem_nd_high <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/demersal_nondest_high_bycatch_combo_2013_minus_2008.tif"))
#hist(dem_nd_high)
my_breaks <- c(-0.6, -0.2, -0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15, 0.2, 0.6)
difPlots(dem_nd_high, my_breaks=my_breaks, 
         file_name=sprintf("Pressures2013minus2008/%s.png", names(dem_nd_high)), 
         title="Demersal nondestructive high bycatch fishing")

dem_nd_low <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/demersal_nondest_low_bycatch_combo_2013_minus_2008.tif"))
#hist(dem_nd_low)
my_breaks <- c(-0.27, -0.10, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.10, 0.22)
difPlots(dem_nd_low, my_breaks=my_breaks, 
         file_name=sprintf("Pressures2013minus2008/%s.png", names(dem_nd_low)), 
         title="Demersal nondestructive low bycatch fishing")

pel_fishing_diff <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/pelagic_high_bycatch_combo_2013_minus_2008.tif"))
my_breaks <- c(-0.53, -0.2, -0.10, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.10, 0.2, 0.53)
difPlots(pel_fishing_diff, my_breaks=my_breaks, 
         file_name=sprintf("Pressures2013minus2008/%s.png", names(pel_fishing_diff)), 
         title="Pelagic high bycatch fishing")

pel_fishing_low <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/pelagic_low_bycatch_combo_2013_minus_2008.tif"))
#hist(pel_fishing_low)
my_breaks <- c(-0.60, -0.2, -0.10, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.10, 0.2, 0.22)
difPlots(pel_fishing_low, my_breaks=my_breaks, 
         file_name=sprintf("Pressures2013minus2008/%s.png", names(pel_fishing_low)), 
         title="Pelagic low bycatch fishing")


sst_zeroCut <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/sst_combo_2013_minus_2008.tif"))
my_breaks <- c(-2.6,-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.6) # for plot
difPlots(sst_zeroCut, my_breaks=my_breaks, 
         file_name=sprintf("Pressures2013minus2008/%s.png", names(sst_zeroCut)), 
         title="Sea surface temperature")


uv_zeroCut <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/uv_combo_2013_minus_2008.tif"))
#histogram(uv_zeroCut)
my_breaks <- c(-1.2, -0.3 ,-0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 0.3, 1.2) # for plot
difPlots(uv_zeroCut, my_breaks=my_breaks, 
         file_name=sprintf("Pressures2013minus2008/%s.png", names(uv_zeroCut)), 
         title="UV")


#b:  nutrient_diff
nutrient_zeroCut <- raster(file.path(path, "ModifiedPressureMaps/Zero_cut/Difference2013minus2008/plumes_fert_combo_2013_minus_2008.tif"))
my_breaks <- c(-0.69 , -0.10, -0.08, -0.06, -0.04, -0.02, 0,
               0.02, 0.04, 0.06, 0.08, 0.1, 0.69)
difPlots(nutrient_zeroCut, my_breaks=my_breaks, file_name="Fig4b.nutrients_global.png", extent=TRUE)


fl <- crop(nutrient_zeroCut, fl_extent)
difPlots(fl, my_breaks=my_breaks, file_name="Fig4b.nutrients_fl_diff.png", legend=FALSE)

med <- crop(nutrient_zeroCut, med_extent)
difPlots(med, my_breaks=my_breaks, file_name="Fig4b.nutrients_med_diff.png", legend=FALSE)

uk <- crop(nutrient_zeroCut, uk_extent)
difPlots(uk, my_breaks=my_breaks, file_name="Fig4b.nutrients_uk_diff.png", legend=FALSE)

cn <- crop(nutrient_zeroCut, cn_extent)
difPlots(cn, my_breaks=my_breaks, file_name="Fig4b.nutrients_cn_diff.png", legend=FALSE)




################################################################################
# Table S??: Change in pixels effected for each stressor from 2013 to 2008----
################################################################################

#first convert to zero/one

raster_list <- c(
  "sst_combo",
                            ))
file_list <- data.frame(locs=c("Pressures2008", "Pressures2013_two_years"))
stressor_num <- 3

raster_2008 <- raster(file.path(path_trim, file_list$locs[1], raster_list$stressor[stressor_num]))
raster_2013 <- raster(file.path(path_trim, file_list$locs[2], raster_list$stressor[stressor_num]))

reclassify(raster_2013, c(-Inf, 0, 0,
                          0, Inf, 1), 
           filename=file.path(path, 
                              sprintf('ModifiedPressureMaps/ZeroOneClassification/2013_two_year/%s', raster_list$stressor[stressor_num])),
           progress="text", overwrite=TRUE)

reclassify(raster_2008, c(-Inf, 0, 0,
                          0, Inf, 1), 
           filename=file.path(path, 
                              sprintf('ModifiedPressureMaps/ZeroOneClassification/2008_two_year/%s', raster_list$stressor[stressor_num])),
           progress="text", overwrite=TRUE)

dem_2008 <- raster(file.path(path, 
                             sprintf('ModifiedPressureMaps/ZeroOneClassification/2008_two_year/%s', raster_list$stressor[stressor_num])))
freq(dem_2008, value=1, useNA="no", progress="text")
freq(dem_2008, value=0, useNA="no", progress="text")

dem_2013 <- raster(file.path(path, 
                             sprintf('ModifiedPressureMaps/ZeroOneClassification/2013_two_year/%s', raster_list$stressor[stressor_num])))
freq(dem_2013, value=1, useNA="no", progress="text")
freq(dem_2013, value=0, useNA="no", progress="text")

s <- stack(dem_2008, dem_2013)
overlay(s, fun=function(x,y) x*y, 
        filename=file.path(path, 
                           sprintf('ModifiedPressureMaps/ZeroOneClassification/two_year_2013times2008/', raster_list$stressor[stressor_num])), 
        progress="text")

diff <- raster(file.path(path, 
                           sprintf('ModifiedPressureMaps/ZeroOneClassification/two_year_2013times2008/%s', raster_list$stressor[stressor_num])))
freq(diff, value=1, useNA="no", progress="text")
freq(diff, value=0, useNA="no", progress="text")


tifs = list.files(file.path(path_trim, 'Pressures2013'), pattern=glob2rx('*.gri'))
tifs <- gsub(".gri", '', tifs)

stack_2013_one <- stack()
for(i in 1:length(tifs)){
  tmp <- raster(file.path(file.path(path_trim, 'Pressures2013'), tifs[i]))
  names(tmp) <- tifs[i]
  stack_2013_one <- stack(stack_2013_one, tmp )
}

plot(raster(stack_2013_one, 2))

for(i in 1:length(tifs)){
  #i <- 1
  reclassify(raster(stack_2013_one, i), c(-Inf, 0, 0,
                                          0, Inf, 1), 
             filename=file.path(path, 
                                sprintf('ModifiedPressureMaps/ZeroOneClassification/%s_ZeroClassify', tifs[i])),
             progress="text")
}



## No longer used: Global map of # of non-NA stressors in each pixel----
# NOTE: this one does not work - only night lights and oil rigs (based on night lights)
# have tiny slivers of missing data in the north and south poles - but these are 
# probably also reasonably estimated to be zero.

tifs = list.files(file.path(path_trim, 'Pressures2013'), pattern=glob2rx('*.gri'))
tifs <- gsub(".gri", '', tifs)

stack_2013_one <- stack()
for(i in 1:length(tifs)){
  tmp <- raster(file.path(file.path(path_trim, 'Pressures2013'), tifs[i]))
  names(tmp) <- tifs[i]
  stack_2013_one <- stack(stack_2013_one, tmp )
}

plot(raster(stack_2013_one, 18))

for(i in 1:length(tifs)){  
  #  i <- 2
  reclassify(raster(stack_2013_one, i), c(-Inf, Inf, 1),
             filename=file.path(path, 
                                sprintf('ModifiedPressureMaps/NAClassification/%s_NAclassify', tifs[i])),
             progress="text", overwrite=TRUE)
}

tifs = list.files(file.path(path, 'ModifiedPressureMaps/NAClassification'), pattern=glob2rx('*.gri'))

stack_2013_NAs <- stack()
for(i in 1:length(tifs)){
  tmp <- raster(file.path(file.path(path, 'ModifiedPressureMaps/NAClassification'), tifs[i]))
  names(tmp) <- tifs[i]
  stack_2013_NAs <- stack(stack_2013_NAs, tmp )
}


plot(raster(stack_2013_NAs, 1))


calc(stack_2013_NAs, sum, na.rm=TRUE, 
     filename = file.path(path, "ModifiedPressureMaps/SumNonNAs"), 
     progress="text", overwrite=TRUE)

tmp <- raster(file.path(path, "ModifiedPressureMaps/SumNonNAs"))
plot(tmp)

# clip outter parts
clip_raster <- raster(file.path(path, "clip_raster"))

s <- stack(tmp, clip_raster)
overlay(s, fun=function(x,y) x*y, 
        filename=file.path(path, "ModifiedPressureMaps/SumZeroOneData_clipped"),
        progress="text", overwrite=TRUE)

SumRaster <- raster(file.path(path, "ModifiedPressureMaps/SumZeroOneData_clipped"))

png(file.path(path_save, "SumNonZeroPressures.png"), res=500, width=7, height=7, units="in")  
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(250)
plot(SumRaster, col=rev(cols), axes=FALSE, box=FALSE, legend.shrink=0.5, legend.width=0.6, 
     breaks=seq(minValue(SumRaster), maxValue(SumRaster), length.out=250),
     axis.args=list(cex.axis=1.3,
                    at=seq(minValue(SumRaster), maxValue(SumRaster), 2),
                    labels=seq(minValue(SumRaster), maxValue(SumRaster), 2))
)
title(main="Sum of non-zero cells", line=-5)
plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.3)
plot(extent(fl_extent), col="red", add=TRUE)
plot(extent(uk_extent), col="red", add=TRUE)
plot(extent(cn_extent), col="red", add=TRUE)
plot(extent(med_extent), col="red", add=TRUE)
dev.off()

region_plot <- function(raster_name, name){
  #   raster_name <- fl
  #   name <- "FL_SOM6a"
  png(file.path(path_save, sprintf("SumNonZeroPressures_%s.png",  name)), res=500, width=7, height=7, units="in")  
  plot(raster_name, col=c(rev(cols)), 
       breaks=seq(minValue(SumRaster), maxValue(SumRaster),length.out=250),
       axes=FALSE, box=FALSE, legend=FALSE,
       #       legend.shrink=0.5, legend.width=0.6, 
       #        axis.args=list(cex.axis=1.3,
       #                       at=seq(minValue(total),
       #                              maxValue(total), 2),
       #                       labels=seq(minValue(total),
       #                                  maxValue(total), 2))
  )
  plot(land, add=TRUE, border="gray60", col="gray90", lwd=0.5)
  dev.off()
}  

fl <- crop(SumRaster, fl_extent)
region_plot(fl, "FL_SOM6a")

med <- crop(SumRaster, med_extent)
region_plot(med, "med_SOM6b")

uk <- crop(SumRaster, uk_extent)
region_plot(uk, "uk_SOM6c")

cn <- crop(SumRaster, cn_extent)
region_plot(cn, "cn_SOM6d")






#### Extra code ----


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
    
    


