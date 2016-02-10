#######################################################################
### NCEAS/OHI
### Figures for Ted Talk
### Melanie Frazier Sep 14 2015
#######################################################################
rm(list = ls())

## Downloading packages
library(tidyr)
library(RColorBrewer)
library(ggplot2) #don't load this unless it is needed for a particular plot (messes things up)
library(grid) #needed for myTheme function
library(dplyr)
library(fields)
library(raster)
library(sp)
library(rgdal)

legend.shrink <- 0.4
legend.width <- 0.6


myTheme <- theme_bw() + theme(axis.text=element_text(size=30), 
                              axis.title=element_text(size=30, vjust=.75),
                              plot.margin=unit(c(1,1,1,1), "lines"),
                              legend.title = element_text(size=20),
                              legend.text= element_text(size=20),
                              plot.title = element_text(lineheight=.8, size=20),
                              strip.text.x = element_text(size = 18)) 

source('~/ohiprep/src/R/common.R')

#paths:
path <- file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Pressures-Summaries_frazier2013')

#### Bubble plots ----
### getting the data:

## eez/ohi translator
eez2ohi <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-OceanRegions_v2013a/data/rgn_2013master.csv"), stringsAsFactors=FALSE) %>%
  select(rgn_id_2013, eez_id, eez_nam) 

eez2ohi$eez_id[eez2ohi$rgn_id_2013==137] <- 137
eez2ohi <- rbind(eez2ohi, data.frame(rgn_id_2013=137, eez_id=109, eez_nam="Ecuador|Galapagos Islands"))

eez2ohi$eez_nam[eez2ohi$rgn_id_2013==163] <- "United States"
eez2ohi <- rbind(eez2ohi, data.frame(rgn_id_2013=163, eez_id=160, eez_nam="United States"))
eez2ohi <- rbind(eez2ohi, data.frame(rgn_id_2013=163, eez_id=163, eez_nam="United States"))

eez2ohi <- rbind(eez2ohi, data.frame(rgn_id_2013=171, eez_id=87, eez_nam="Brazil|Trindade"))

eez2ohi$eez_id[eez2ohi$rgn_id_2013==224] <- 224
eez2ohi <- rbind(eez2ohi, data.frame(rgn_id_2013=224, eez_id=225, eez_nam="Chile|Easter Island"))


eez2ohi$eez_nam[eez2ohi$rgn_id_2013==32] <- "Reunion"
eez2ohi$eez_nam[eez2ohi$rgn_id_2013==100] <- "Republique du Congo"

eez2ohi <- mutate(eez2ohi, eez_id = as.numeric(eez_id))

# eez Area (for weighted mean of some regions)
eezArea <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-OceanRegions_v2013a/data/eez_area.csv"), stringsAsFactors=FALSE)%>%
  mutate(eez_id = as.numeric(eez_id)) %>%
  select(eez_id, area_km2)


## chi data
chi_scores <- read.csv("ZonalExtractionData/withZero2NA/oneYearNorm_2013_eez_zeroData.csv", stringsAsFactors=FALSE) %>%
  select(eez_id, global_cumul_impact_2013_all_layers=global_cumulative_impact_2013_all_layers.gri) 
chi_change <- read.csv("ZonalExtractionData/withZero2NA/diff_2013minus2008_eez_zeroData.csv", stringsAsFactors=FALSE) %>%
  select(eez_id, global_cumul_impact_2013_minus_2008=global_cumulative_impact_dif.gri) 


chi <- chi_scores %>%
  left_join(chi_change) %>%
  left_join(eez2ohi) %>%
  filter(!(rgn_id_2013 %in% 255),
         !is.na(rgn_id_2013)) %>%
  left_join(eezArea, by=c("eez_id")) %>%
  group_by(rgn_id_2013, eez_nam) %>%
  summarize(global_cumul_impact_2013_all_layers=weighted.mean(global_cumul_impact_2013_all_layers, area_km2),  
            global_cumul_impact_2013_minus_2008=weighted.mean(global_cumul_impact_2013_minus_2008, area_km2),
            area_km2=sum(area_km2)) %>%
  ungroup()


### ohi 2015 results
ohi <- read.csv('../ohi-global/global2015/radical_2015-09-11.csv') %>%
  filter(dimension == "score") %>%
  filter(region_id != 0) %>%
  filter(region_id < 255) %>%
  filter(goal == "Index") %>%
  mutate(scenario = paste0('OHI_', scenario))

ohi <- spread(ohi, scenario, value) 

ohi$ohi_change <- ohi$'OHI_2015' - ohi$'OHI_2012' 

ohi <- ohi %>%
  select(-goal, -dimension) %>%
  rename(rgn_id_2013 = region_id)

## coastal population and trend
## add in data for uninhabitated islands:
zeros <- read.csv('../ohiprep/src/LookupTables/rgn_uninhabited_islands.csv') 

coastal_pop <- read.csv("../ohi-global/eez2013/layers/mar_coastalpopn_inland25mi.csv", stringsAsFactors=FALSE) %>%
  filter(year==2015) %>%
  select(rgn_id_2013=rgn_id, popsum) %>%
  rbind(data.frame(rgn_id_2013 = c(zeros$rgn_id, 213), popsum=0)) %>%
  rbind(data.frame(rgn_id_2013 = c(38), popsum=4000)) %>%  # https://en.wikipedia.org/wiki/British_Indian_Ocean_Territory
  unique()

coastal_pop_trend <- read.csv("../ohi-global/eez2013/layers/mar_coastalpopn_inland25mi.csv", stringsAsFactors=FALSE) %>%
  arrange(rgn_id, year) %>%
  group_by(rgn_id) %>%
  do(mdl = lm(log(popsum+1) ~ year, data=.)) %>%
  do(data.frame(
    rgn_id_2013=.$rgn_id,
    pop_trend=coef(.$mdl)[['year']])) %>%
  ungroup() %>%
  rbind(data.frame(rgn_id_2013 = c(zeros$rgn_id, 213, 38), pop_trend=0)) %>%
  unique()

## region data
r1 <- read.csv('../ohiprep/src/LookupTables/rgn_georegions_wide_2013b.csv') %>%
  select(rgn_id_2013=rgn_id, r1_label)


## combining data:

data <- chi %>%
  left_join(ohi) %>%
  left_join(coastal_pop) %>%
  left_join(coastal_pop_trend) %>%
  left_join(r1) %>%
  mutate(lnPop=log(popsum + 1)) %>%
  mutate(labels1 = (ifelse(global_cumul_impact_2013_all_layers>6 | 
                             OHI_2015 > 85 | 
                             global_cumul_impact_2013_all_layers<2 |
                             OHI_2015 < 50, eez_nam, NA))) %>%
mutate(labels2 = (ifelse(ohi_change > 5 | 
                           ohi_change < -5 | 
                           global_cumul_impact_2013_minus_2008 < -0.5 |
                           global_cumul_impact_2013_minus_2008 > 1, eez_nam, NA)))


data <- data %>%
  mutate(ln_CHI = log(global_cumul_impact_2013_all_layers)) %>%
  mutate(ln_scale_CHI = 1 - (ln_CHI - min(ln_CHI, na.rm=TRUE))/(max(ln_CHI, na.rm=TRUE) - min(ln_CHI, na.rm=TRUE))) %>%
  mutate(scale_change_CHI = 1 - (global_cumul_impact_2013_minus_2008 - min(global_cumul_impact_2013_minus_2008, na.rm=TRUE))/
           (max(global_cumul_impact_2013_minus_2008, na.rm=TRUE) - min(global_cumul_impact_2013_minus_2008, na.rm=TRUE))) %>%
  mutate(scale_OHI_2015 = (OHI_2015 - min(OHI_2015, na.rm=TRUE))/(max(OHI_2015, na.rm=TRUE) - min(OHI_2015, na.rm=TRUE))) %>%
  mutate(scale_ohi_change = (ohi_change - min(ohi_change, na.rm=TRUE)) /(max(ohi_change, na.rm=TRUE) - min(ohi_change, na.rm=TRUE))) %>%
  mutate(ohivschi_color = (ln_scale_CHI + scale_OHI_2015) / 2) %>%
  mutate(change_color = (scale_change_CHI + scale_ohi_change) / 2)

data <- data.frame(data)

# Ben request 2/10/16
tmp <- filter(data, global_cumul_impact_2013_minus_2008<0 & ohi_change > 0) %>%
  select(eez_nam) %>%
  arrange(eez_nam)
write.csv(tmp, "SideProjects/TedTalkSep2015/Countries_lessCHI_moreOHI.csv", row.names=FALSE)


#   select(-rgn_id_2013, -labels1, -labels2) %>%
#   rename(CHI_2013=global_cumul_impact_2013_all_layers, CHI_change=global_cumul_impact_2013_minus_2008) %>%
#   mutate(time = 2015)
# 
# Motion=gvisMotionChart(gV_data, 
#                        idvar="eez_nam", 
#                        timevar="time")
# plot(Motion)
# 
# print(Motion, file='SideProjects/TedTalkSep2015/GoogleVisScores.html')


p <- ggplot(data, aes(y=global_cumul_impact_2013_all_layers, x=OHI_2015)) +
     geom_point(aes(fill=ohivschi_color), shape=21, color="gray30", alpha=0.7, size=12) +
#  stat_smooth(method=lm, show_guide = FALSE)  +
   #geom_text(aes(label=as.character(labels1)), size=4, hjust=1) +
#  geom_text(aes(label=as.character(eez_nam)), size=4, hjust=1) +
  labs(y="", x= "") +
#  scale_size(range=c(3,18)) +
   scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
#    scale_fill_brewer(palette="Set1") +
#   scale_fill_gradientn(colours=rev(brewer.pal(11, "Spectral")), values=seq(-0.05, 0.05, length=11),  
#                        rescaler = function(x, ...) x, oob = identity, na.value="white") +
  #scale_color_gradient(low="yellow", high="red3") +
  myTheme + 
  theme(legend.position="none")
#   theme(legend.justification=c(1,1),
#         legend.background = element_rect(colour = "gray"),
#         legend.key = element_rect(colour = NA),
#         axis.title.y = element_text(vjust=1),
#         legend.position=c(1,1)) +
#   guides(fill = guide_legend(override.aes = list(size=10)))
plot(p)
ggsave('SideProjects/TedTalkSep2015/CHIvsOHI_Sep25.png', width=15, height=13, dpi=300)

mod <- lm(ohi_2015 ~ global_cumul_impact_2013_all_layers, data=data)
summary(mod)

#Lebanon
p <- ggplot(data, aes(y=global_cumul_impact_2013_all_layers, x=OHI_2015)) +
  geom_point(aes(fill=ohivschi_color), shape=21, color="gray80", alpha=0.2, size=12) +
  geom_point(data=subset(data, eez_nam == "Lebanon"), aes(fill=ohivschi_color), shape=21, color="black", size=12) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
    labs(y="", x= "") +
  myTheme + 
  theme(legend.position="none")
plot(p)
ggsave('SideProjects/TedTalkSep2015/CHIvsOHI_Lebanon_Sep25.png', width=15, height=13, dpi=300)

#New Caledonia
p <- ggplot(data, aes(y=global_cumul_impact_2013_all_layers, x=OHI_2015)) +
  geom_point(aes(fill=ohivschi_color), shape=21, color="gray80", alpha=0.2, size=12) +
  geom_point(data=subset(data, eez_nam == "New Caledonia"), fill=brewer.pal(11, "RdYlBu")[9], shape=21, color="black", size=12, alpha=1) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
  labs(y="", x= "") +
  myTheme + 
  theme(legend.position="none")
plot(p)
ggsave('SideProjects/TedTalkSep2015/CHIvsOHI_NewCaldedonia_Sep25.png', width=15, height=13, dpi=300)


#Peru
p <- ggplot(data, aes(y=global_cumul_impact_2013_all_layers, x=OHI_2015)) +
  geom_point(aes(fill=ohivschi_color), shape=21, color="gray80", alpha=0.2, size=12) +
  geom_point(data=subset(data, eez_nam == "Peru"), aes(fill=ohivschi_color), shape=21, color="black", size=12) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
  labs(y="", x= "") +
  myTheme + 
  theme(legend.position="none")
plot(p)
ggsave('SideProjects/TedTalkSep2015/CHIvsOHI_Peru_Sep25.png', width=15, height=13, dpi=300)


"Northern Saint-Martin"
p <- ggplot(data, aes(y=global_cumul_impact_2013_all_layers, x=OHI_2015)) +
  geom_point(data=subset(data, eez_nam == "Northern Saint-Martin"), fill=brewer.pal(11, "RdYlBu")[8], shape=21, color="black", size=12, alpha=1) +
    geom_point(aes(fill=ohivschi_color), shape=21, color="gray80", alpha=0.2, size=12) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
  labs(y="", x= "") +
  myTheme + 
  theme(legend.position="none")
plot(p)
ggsave('SideProjects/TedTalkSep2015/CHIvsOHI_NSMartin_Sep25.png', width=15, height=13, dpi=300)


ggplot(data, aes(x=ohi_change, y=global_cumul_impact_2013_minus_2008)) +
  geom_point(aes(fill=change_color), shape=21, alpha=0.7, color="gray30", size=12) +
#  stat_smooth(method=lm, show_guide = FALSE)  +
#  geom_text(aes(label=as.character(labels2)), size=4, hjust=1) +
  labs(y="", x= "") +
#  scale_size(range=c(3,18)) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
#  scale_fill_brewer(palette="Set1") +
  myTheme + 
  theme(legend.position="none")
#   theme(legend.justification=c(1,1),
#         legend.background = element_rect(colour = "gray"),
#         legend.key = element_rect(colour = NA),
#         axis.title.y = element_text(vjust=1),
#         legend.position=c(1,1)) +
#   guides(fill = guide_legend(override.aes = list(size=7)))

ggsave('SideProjects/TedTalkSep2015/changeInCHIvsOHI_Sept25.png', width=15, height=13, dpi=300)

# Mozambique
ggplot(data, aes(x=ohi_change, y=global_cumul_impact_2013_minus_2008)) +
  geom_point(aes(fill=change_color), shape=21, alpha=0.2, color="gray30", size=12) +
  geom_point(data=subset(data, eez_nam== "Mozambique"), fill=brewer.pal(11, "RdYlBu")[8], shape=21, alpha=1, color="black", size=12) +
  labs(y="", x= "") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
  myTheme + 
  theme(legend.position="none")
ggsave('SideProjects/TedTalkSep2015/changeInCHIvsOHI_Mozambique_Sept25.png', width=15, height=13, dpi=300)

# OHI>0 & CHI>0
ggplot(data, aes(x=ohi_change, y=global_cumul_impact_2013_minus_2008)) +
  geom_point(aes(fill=change_color), shape=21, alpha=0.2, color="gray30", size=12) +
  geom_point(data=subset(data, ohi_change>0 & global_cumul_impact_2013_minus_2008<0), aes(fill=change_color), shape=21, alpha=1, color="black", size=12) +
  labs(y="", x= "") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
  myTheme + 
  theme(legend.position="none")

ggsave('SideProjects/TedTalkSep2015/changeInCHIvsOHI_greater0_Sept25.png', width=15, height=13, dpi=300)

# US/UK/Iceland
ggplot(data, aes(x=ohi_change, y=global_cumul_impact_2013_minus_2008)) +
  geom_point(aes(fill=change_color), shape=21, alpha=0.2, color="gray30", size=12) +
  geom_point(data=subset(data, eez_nam %in% c("United States", "United Kingdom", "Iceland")), aes(fill=change_color), shape=21, alpha=1, color="black", size=12) +
  labs(y="", x= "") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
  myTheme + 
  theme(legend.position="none")

ggsave('SideProjects/TedTalkSep2015/changeInCHIvsOHI_UK_US_Ice_Sept25.png', width=15, height=13, dpi=300)


# Croatio
ggplot(data, aes(x=ohi_change, y=global_cumul_impact_2013_minus_2008)) +
  geom_point(aes(fill=change_color), shape=21, alpha=0.2, color="gray30", size=12) +
  geom_point(data=subset(data, eez_nam %in% c("Croatia")), aes(fill=change_color), shape=21, alpha=1, color="black", size=12) +
  labs(y="", x= "") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdYlBu")) +
  myTheme + 
  theme(legend.position="none")

ggsave('SideProjects/TedTalkSep2015/changeInCHIvsOHI_Croatia_Sept25.png', width=15, height=13, dpi=300)

############################################################

### Mapping data



#### Old 2008 CHI map ----

rasters = file.path('/var/cache/halpern-et-al', 
                    'mnt/storage/marine_threats/impact_layers_2013_redo')
myFiles = file.path("/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013")


# set temporary directory to folder on neptune disk big enough to handle it
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)


# land layer for plots ----
rgn_ocn_cntry <- readOGR("/var/data/ohi/model/GL-NCEAS-OceanRegions_v2013a/data", layer="rgn_ocean_cntry_gcs")
land <- rgn_ocn_cntry[!is.na(rgn_ocn_cntry@data$ISO_3digit) & rgn_ocn_cntry@data$rgn_id==0,]

### load this function:
raster_breaks <- function(raster_data, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=TRUE){
#   par(mar=c(2,2,2,2))
#   par(oma=c(0,0,0,4))
  width = 15
  png(saveLoc, res=500, width=width, height=width*nrow(raster_data)/ncol(raster_data), units="in")
  plot(raster_data, col=cols, axes=FALSE, box=FALSE, breaks=myBreaks, legend=FALSE, useRaster=FALSE)
  # add axis with fields package function:
  def_breaks <- seq(0, length(myBreaks), length.out=length(myBreaks))
 
  if(legend){
   image.plot(raster_data, #zlim = c(min(myBreaks), max(myBreaks)), 
             legend.only = TRUE, 
             legend.shrink=legend.shrink,
            legend.width=legend.width,
             col = cols,
             legend.lab=title_legend,
             breaks=def_breaks,
             lab.breaks=round(myBreaks, 1),
             axis.args = list(cex.axis = 0.8))
  }
  
  if(ice_over){
    plot(ice_max, add=TRUE, border=NA, col="#00000059") #35% transparent
    plot(ice_min, add=TRUE, border="gray80", col="white")
    plot(ice_min, add=TRUE, border=NA, col="white")
  }
  plot(land, add=TRUE, border="gray100", col="gray90", lwd=0.5)
  
  dev.off()
}


raster_breaks_midRes <- function(raster_data, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=TRUE){
  #   par(mar=c(2,2,2,2))
  #   par(oma=c(0,0,0,4))
  width = 15
  png(saveLoc, res=1500, width=width, height=width*nrow(raster_data)/ncol(raster_data), units="in")
  plot(raster_data, col=cols, axes=FALSE, box=FALSE, breaks=myBreaks, legend=FALSE, useRaster=FALSE)
  # add axis with fields package function:
  def_breaks <- seq(0, length(myBreaks), length.out=length(myBreaks))
  
  if(legend){
    image.plot(raster_data, #zlim = c(min(myBreaks), max(myBreaks)), 
               legend.only = TRUE, 
               legend.shrink=legend.shrink,
               legend.width=legend.width,
               col = cols,
               legend.lab=title_legend,
               breaks=def_breaks,
               lab.breaks=round(myBreaks, 1),
               axis.args = list(cex.axis = 0.8))
  }
  
  if(ice_over){
    plot(ice_max, add=TRUE, border=NA, col="#00000059") #35% transparent
    plot(ice_min, add=TRUE, border="gray80", col="white")
    plot(ice_min, add=TRUE, border=NA, col="white")
  }
  plot(land, add=TRUE, border="gray100", col="gray90", lwd=0.5)
  
  dev.off()
}




### old 2008 CHI
raster_data = raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_CI_wgs.tif'))
cols=c('#8BB4EB', '#B0D598', '#F8EE39', '#EFB51D', '#E87C23', '#991017')
myBreaks=c(0,1.4, 4.95, 8.47, 12, 15.52, maxValue(raster_data))
saveLoc= 'SideProjects/TedTalkSep2015/old2008_chi.png'

raster_breaks(raster_data, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)
  
### old 2008 CHI - highs highlighted
raster_data = raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_CI_wgs.tif'))
cols=c('#8BB4EB26', '#B0D59826', '#F8EE3926', '#EFB51D26', '#E87C2326', '#991017')
myBreaks=c(0,1.4, 4.95, 8.47, 12, 15.52, maxValue(raster_data))
saveLoc= 'SideProjects/TedTalkSep2015/old2008_chi_highs.png'

raster_breaks(raster_data, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)


### old 2008 CHI - lows highlighted
raster_data = raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_CI_wgs.tif'))
cols=c('#8BB4EB', '#B0D59826', '#F8EE3926', '#EFB51D26', '#E87C2326', '#99101726')
myBreaks=c(0,1.4, 4.95, 8.47, 12, 15.52, maxValue(raster_data))
saveLoc= 'SideProjects/TedTalkSep2015/old2008_chi_lows.png'

raster_breaks(raster_data, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)


#### Individual 2008 pressures ----

# load in this function:
raster_defaultLegend <- function(raster_data, saveLoc, title_plot=NA, cols, ice_over=TRUE){
  #   par(mar=c(2,2,2,2))
  #   par(oma=c(0,0,0,4))
  width=15
  png(saveLoc, res=500, width=width, height=width*nrow(raster_data)/ncol(raster_data), units="in")  
  #pdf(file.path(path_save, saveLoc))  #, width=1200, height=1000, res=150, pointsize=14)
  plot(raster_data, col=cols, axes=FALSE, box=FALSE, legend=FALSE, useRaster=FALSE)
  title(main=title_plot, line=-5)
  if(ice_over){
    plot(ice_max, add=TRUE, border=NA, col="#00000033")
    plot(ice_min, add=TRUE, border="gray80", col="white")
    plot(ice_min, add=TRUE, border=NA, col="white")
  }
  plot(land, add=TRUE, border="gray100", col="gray90", lwd=0.5)  
  dev.off()
}

raster_defaultLegend_midRes <- function(raster_data, saveLoc, title_plot=NA, cols, ice_over=TRUE){
  #   par(mar=c(2,2,2,2))
  #   par(oma=c(0,0,0,4))
  width=15
  png(saveLoc, res=1800, width=width, height=width*nrow(raster_data)/ncol(raster_data), units="in")  
  #pdf(file.path(path_save, saveLoc))  #, width=1200, height=1000, res=150, pointsize=14)
  plot(raster_data, col=cols, axes=FALSE, box=FALSE, legend=FALSE, useRaster=FALSE)
  title(main=title_plot, line=-5)
  if(ice_over){
    plot(ice_max, add=TRUE, border=NA, col="#00000033")
    plot(ice_min, add=TRUE, border="gray80", col="white")
    plot(ice_min, add=TRUE, border=NA, col="white")
  }
  plot(land, add=TRUE, border="gray100", col="gray90", lwd=0.3)  
  dev.off()
}


## Zoom in on change in CHI 2013 data ----
### crop different regions
raster_data = raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_CI_wgs.tif'))
plot(raster_data, col=cols, axes=FALSE, box=FALSE, breaks=myBreaks, legend=FALSE, useRaster=FALSE)
extent <- drawExtent()  # use this to find extent after plotting map

# Europe
europe_crop <- c(-11, 28, 50, 69)
europe <- crop(raster_data, europe_crop)
plot(europe)

cols=c('#8BB4EB', '#B0D598', '#F8EE39', '#EFB51D', '#E87C23', '#991017')
myBreaks=c(0.00001, 0.05, 0.1, 0.25, 0.5, .75, maxValue(raster_data))

shipping <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_shipping_wgs.tif'))
saveLoc= 'SideProjects/TedTalkSep2015/old2008_shipping.png'
raster_breaks(raster_data=shipping, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)

shipping_crop <- crop(shipping, europe_crop)
saveLoc= 'SideProjects/TedTalkSep2015/old2008_shipping_europe.png'
raster_breaks(raster_data=shipping_crop, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)

#cols = colorRampPalette(c('white', '#8BB4EB', '#B0D598', '#F8EE39', '#EFB51D', '#E87C23', '#991017'))(250)
# raster_defaultLegend(raster_data=shipping, 
#                      saveLoc=saveLoc,
#                      cols=cols,
#                      ice_over = FALSE)


sst <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_sst_wgs.tif'))
saveLoc= 'SideProjects/TedTalkSep2015/old2008_sst.png'
raster_breaks(raster_data=sst, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)

sst_crop <- crop(sst, europe_crop)
saveLoc= 'SideProjects/TedTalkSep2015/old2008_sst_europe.png'
raster_breaks(raster_data=sst_crop, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)


# raster_defaultLegend(raster_data=sst, 
#                      saveLoc=saveLoc,
#                      cols=cols,
#                      ice_over = FALSE)


nutrients <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_nutrient_wgs.tif'))
saveLoc= 'SideProjects/TedTalkSep2015/old2008_nutrients.png'
raster_breaks(raster_data=nutrients, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)

nutrient_crop <- crop(nutrients, europe_crop)
saveLoc= 'SideProjects/TedTalkSep2015/old2008_nutrients_europe.png'
raster_breaks(raster_data=nutrient_crop, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)


# raster_defaultLegend(raster_data=nutrients, 
#                      saveLoc=saveLoc,
#                      cols=cols,
#                      ice_over = FALSE)


dem_nd_hb <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_dem_nd_hb_wgs.tif'))
saveLoc= 'SideProjects/TedTalkSep2015/old2008_dem_nd_hb.png'
raster_breaks(raster_data=dem_nd_hb, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)

dem_nd_hb_crop <- crop(dem_nd_hb, europe_crop)
saveLoc= 'SideProjects/TedTalkSep2015/old2008_dem_nd_hb_europe.png'
raster_breaks(raster_data=dem_nd_hb_crop, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)


# raster_defaultLegend(raster_data=dem_nd_hb, 
#                      saveLoc=saveLoc,
#                      cols=cols,
#                      ice_over = FALSE)


dem_nd_lb <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2008old_wgs/2008old_dem_nd_lb_wgs.tif'))
saveLoc= 'SideProjects/TedTalkSep2015/old2008_dem_nd_lb.png'
raster_breaks(raster_data=dem_nd_lb, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)

dem_nd_lb_crop <- crop(dem_nd_hb, europe_crop)
saveLoc= 'SideProjects/TedTalkSep2015/old2008_dem_nd_lb_europe.png'
raster_breaks(raster_data=dem_nd_lb_crop, saveLoc, title, title_legend=NULL, myBreaks, cols, ice_over=FALSE, legend=FALSE)

# raster_defaultLegend(raster_data=dem_nd_lb, 
#                      saveLoc=saveLoc,
#                      cols=cols,
#                      ice_over = FALSE)

#####  New 2013 CHI and change ----

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(250))

changeCHI_2013 <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2013minus2008_wgs/global_cumulative_impact_dif_wgs.tif'))
saveLoc= '/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/TedTalkSep2015/CHI2013minus2008_midres.png'

breaks=seq(minValue(changeCHI_2013), maxValue(changeCHI_2013), length.out=251)

raster_breaks_midRes(raster_data=changeCHI_2013, 
                     saveLoc=saveLoc,
                     myBreaks=breaks,
                     cols=cols,
                     legend=FALSE,
                     title_legend=NULL)

northSea_crop <- c(-11, 10, 50, 62)
NScrop <- crop(changeCHI_2013, northSea_crop)
plot(NScrop)

saveLoc= '/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/TedTalkSep2015/CHI2013minus2008_NScrop_midres.png'

raster_breaks_midRes(raster_data=NScrop, 
                     saveLoc=saveLoc,
                     myBreaks=breaks,
                     cols=cols,
                     legend=FALSE,
                     title_legend=NULL)



## Zoom in on change in CHI 2013 data ----
### crop different regions
# plot(CHI2013, col=cols, axes=FALSE, box=FALSE, breaks=my_breaks, legend=FALSE, useRaster=FALSE)
# extent <- drawExtent()  # use this to find extent after plotting map

breaks=seq(minValue(changeCHI_2013), maxValue(changeCHI_2013), length.out=251)
# Europe
europe_crop <- c(-6, 17, 50, 60)
europe <- crop(changeCHI_2013, europe_crop)

saveLoc= 'SideProjects/TedTalkSep2015/CHI2013minus2008_neurope.png'
raster_breaks(europe, saveLoc=saveLoc, myBreaks=breaks, cols=cols, legend=FALSE, title_legend=NULL)

# Adriatic
adriatic_crop <- c(11, 21, 37, 46)
adriatic <- crop(changeCHI_2013, adriatic_crop)

saveLoc= 'SideProjects/TedTalkSep2015/CHI2013minus2008_adriatic.png'
raster_breaks(adriatic, saveLoc=saveLoc, myBreaks=breaks, cols=cols, legend=FALSE, title_legend=NULL)



# blues and greens hightlighted:
cols = rev(colorRampPalette(c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", 
                              "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"))(250))
cols <- c(cols[1:100], paste0(cols[101:250], "26")) 

changeCHI_2013 <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2013minus2008_wgs/global_cumulative_impact_dif_wgs.tif'))
saveLoc= 'SideProjects/TedTalkSep2015/CHI2013minus2008_decreasedDominant.png'

raster_defaultLegend(raster_data=changeCHI_2013, 
                     saveLoc=saveLoc,
                     cols=cols,
                     ice_over = FALSE)


##### 2013 CHI ----
CHI2013 <- raster(file.path(myFiles, 'TrimmedPressureLayers/Pressures2013_wgs/global_cumulative_impact_2013_all_layers_wgs.tif'))
my_breaks <- c(0, 1.781894, 2.377814, 2.986494, 3.316144, 3.558642, 3.750878, 
               3.923132, 4.127960, 4.384074, 4.571275, 16)
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(length(my_breaks)+2))[2:12] #stripping extreme ends of the color spectrum
saveLoc= 'SideProjects/TedTalkSep2015/CHI2013.png'

raster_breaks(raster_data=CHI2013, saveLoc=saveLoc, myBreaks=my_breaks, cols=cols, legend=FALSE, title_legend=NULL)

#### OHI 2015 scores mapped ----
## see OHI2015_maps.R script

### Summary of index/goal scores ----
require(gdata)

goals <- c('Index', 'AO', 'SPP', 'BD', 'HAB', 'CP', 'CS', 'CW', 'ECO', 'LE', 'LIV', 'FIS', 'FP', 'MAR', 'ICO', 'SP', 'LSP', 'NP', 'TR')


## summary of index/goal scores
ohi2 <- read.csv('../ohi-global/global2015/radical_2015-09-11.csv') %>%
  filter(dimension == "score") %>%
  filter(region_id == 300)

ohi2 <- spread(ohi2, scenario, value) 

ohi2$goal <- reorder.factor(ohi2$goal, new.order=goals)

ohi2 <- ohi2 %>%
  arrange(goal) %>%
  select(-dimension, -region_id)

write.csv(ohi2, 'SideProjects/TedTalkSep2015/IndexSummary.csv', row.names=FALSE)

## Target country explore----

regions <- chi %>%
  select(region_id=rgn_id_2013, eez_nam)

ohi3 <- read.csv('../ohi-global/global2015/radical_2015-09-11.csv') %>%
  filter(dimension == "score") %>%
  filter(region_id != 0) %>%
  filter(region_id < 255) %>%
  left_join(regions) %>%
  select(dimension, scenario, goal, eez_nam, region_id, value) 

ohi_v1 <-  ohi3 %>%
  filter(eez_nam %in% c("Peru", "Northern Saint-Martin", "Mozambique", "Republique du Congo", "United Kingdom", "Nauru", "Croatia"))
write.csv(ohi_v1, 'SideProjects/TedTalkSep2015/OHIscores_v1.csv', row.names=FALSE)


## get expanded version
tmp <- expand.grid(scenario=2012:2015, goals=goals)

scenario_goal <- paste(tmp$scenario, tmp$goals, sep="_")

ohi4 <- ohi3 %>%
  mutate(year_goal = paste(scenario, goal, sep="_")) %>%
  select(region_id, eez_nam, year_goal, value) 
  
  ohi4$year_goal <- reorder.factor(ohi4$year_goal, new.order=scenario_goal)  
  
ohi4 <- spread(ohi4, year_goal, value) %>%
  filter(eez_nam %in% c("Peru", "Northern Saint-Martin", "Mozambique", "Republique du Congo", "United Kingdom", "Nauru", "Croatia"))

write.csv(ohi4, 'SideProjects/TedTalkSep2015/OHIscores_v2.csv', row.names=FALSE)

### NOTE: in these cases, I don't need to worry about summarizing by eez (there are some, such as US, that have multiple EEZs)
## get changes in CHI:
chi_diffs <- chi_change <- read.csv("ZonalExtractionData/withZero2NA/diff_2013minus2008_eez_zeroData.csv", stringsAsFactors=FALSE) %>%
  filter(eez_nam %in% c("Peru", "Northern Saint-Martin", "Mozambique", "R\x8epublique du Congo", "United Kingdom", "Nauru", "Croatia")) %>%
  select(-eez_id, -eez_key, -sov_id, -sov_nam, -eez_iso3, -sst) %>%
  rename(sst=layer)
write.csv(chi_diffs, 'SideProjects/TedTalkSep2015/CHIscores.csv', row.names=FALSE)

## get CHI 2013 scores:
chi_2013 <- chi_change <- read.csv("ZonalExtractionData/withZero2NA/oneYearNorm_2013_eez_zeroData.csv", stringsAsFactors=FALSE) %>%
  filter(eez_nam %in% c("Peru", "Northern Saint-Martin", "Mozambique", "R\x8epublique du Congo", "United Kingdom", "Nauru", "Croatia")) %>%
  select(-eez_id, -eez_key, -sov_id, -sov_nam, -eez_iso3, -sst) %>%
  rename(sst=layer)
write.csv(chi_2013, 'SideProjects/TedTalkSep2015/CHIscores_2013.csv', row.names=FALSE)


#### Checking into New Zealand----
chi_change <- read.csv("ZonalExtractionData/withZero2NA/diff_2013minus2008_eez_zeroData.csv", stringsAsFactors=FALSE) %>%
  arrange(global_cumulative_impact_dif.gri, layer) %>%
  select(-eez_id, -eez_key, -sov_id, -eez_iso3)


denmark <- filter(chi_change, eez_nam=="Denmark")
netherlands <- filter(chi_change, eez_nam=="Netherlands")




### Interactive plots ----
### A bunch of attempts to create interactive plots
### none of them were exactly what I wanted.  

library(ggvis)

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
} 


data$id <- 1:nrow(data)  # Add an id column to use ask the key

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- data$eez_nam[data$id == x$id]
  paste0("<b>", names(row), "</b>:", format(row), collapse = "<br />")
}

data %>% 
  ggvis(x = ~ohi_2015, y = ~global_cumul_impact_2013_all_layers, fill = ~pop_trend, size = ~lnPop, key := ~id) %>%
  layer_points() %>%
  add_tooltip(all_values, 'hover')



library(googleVis)
bubble <- gvisBubbleChart(data, idvar="eez_nam", xvar="ohi_2015", yvar="global_cumul_impact_2013_all_layers", colorvar = "pop_trend", sizevar = 'lnPop',
                          options=list(height=900,
                                       hAxis='{minValue:50, maxValue:100}'))
plot(bubble)

library(metricsgraphics)
library(htmltools)
library(htmlwidgets)

data %>%
  mjs_plot(x=ohi_2015, y=global_cumul_impact_2013_all_layers, width=600, height=500) %>%
  mjs_point(color_accessor=pop_trend, size_accessor=lnPop) 

devtools::install_github("ropensci/plotly")
library(plotly)
Sys.setenv("plotly_username"="melsteroni")
Sys.setenv("plotly_api_key"="h2uycul2m5")
plotly:::verify("melsteroni")
plotly:::verify("h2uycul2m5")
(gg <- ggplotly(p))  
