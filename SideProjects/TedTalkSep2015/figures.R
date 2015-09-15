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


myTheme <- theme_bw() + theme(axis.text=element_text(size=20), 
                              axis.title=element_text(size=20, vjust=.75),
                              plot.margin=unit(c(1,1,1,1), "lines"),
                              legend.title = element_text(size=20),
                              legend.text= element_text(size=20),
                              plot.title = element_text(lineheight=.8, size=20),
                              strip.text.x = element_text(size = 18)) 

source('~/ohiprep/src/R/common.R')

#paths:
path <- file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Pressures-Summaries_frazier2013')


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
  filter(goal == "Index")

ohi <- spread(ohi, scenario, value) 

ohi$ohi_change <- ohi$'2015' - ohi$'2012' 
names(ohi)[which(names(ohi)=='2015')] <- 'ohi_2015'

ohi <- select(ohi, rgn_id_2013=region_id, ohi_2015, ohi_change)

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

## combining data:

data <- chi %>%
  left_join(ohi) %>%
  left_join(coastal_pop) %>%
  left_join(coastal_pop_trend) %>%
  mutate(lnPop=log(popsum + 1)) %>%
  mutate(labels1 = (ifelse(global_cumul_impact_2013_all_layers>6 | 
                             ohi_2015 > 85 | 
                             global_cumul_impact_2013_all_layers<2 |
                             ohi_2015 < 50, eez_nam, NA))) %>%
mutate(labels2 = (ifelse(ohi_change > 5 | 
                           ohi_change < -5 | 
                           global_cumul_impact_2013_minus_2008 < -0.5 |
                           global_cumul_impact_2013_minus_2008 > 1, eez_nam, NA)))
head(data.frame(data))
data <- data.frame(data)

p <- ggplot(data, aes(y=global_cumul_impact_2013_all_layers, x=ohi_2015, size=lnPop)) +
     geom_point(aes(fill=pop_trend), shape=21, color="black", alpha=0.8) +
  stat_smooth(method=lm, show_guide = FALSE)  +
   #geom_text(aes(label=as.character(labels1)), size=4, hjust=1) +
  geom_text(aes(label=as.character(eez_nam)), size=4, hjust=1) +
  labs(y="Cumulative impact scores", x= "Ocean Health Index scores", size=expression("Ln coastal pop"), fill=expression("pop trend")) +
  scale_size(range=c(3,13)) +
  scale_fill_gradientn(colours=rev(brewer.pal(11, "Spectral")), values=seq(-0.05, 0.05, length=11),  
                       rescaler = function(x, ...) x, oob = identity, na.value="white") +
  #scale_color_gradient(low="yellow", high="red3") +
  myTheme + 
  theme(legend.justification=c(1,1),
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(colour = NA),
        axis.title.y = element_text(vjust=1),
        legend.position=c(1,1))
plot(p)

ggsave('SideProjects/TedTalkSep2015/CHIvsOHI_allNames.png', width=15, height=13, dpi=300)


ggplot(data, aes(x=ohi_change, y=global_cumul_impact_2013_minus_2008, size=lnPop)) +
  geom_point(aes(fill=pop_trend), shape=21, alpha=0.8, color="black") +
#  stat_smooth(method=lm, show_guide = FALSE)  +
  geom_text(aes(label=as.character(labels2)), size=4, hjust=1) +
  labs(y="Change in Cumulative Human Impact", x= "Change in Ocean Health Index", size=expression("Ln coastal pop"), fill=expression("pop trend")) +
  scale_size(range=c(3,13)) +
  scale_fill_gradientn(colours=rev(brewer.pal(11, "Spectral")), values=seq(-0.05, 0.05, length=11),  
                       rescaler = function(x, ...) x, oob = identity, na.value="white") +
  #scale_color_gradient(low="yellow", high="red3") +
  myTheme + 
  theme(legend.justification=c(1,1),
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(colour = NA),
        axis.title.y = element_text(vjust=1),
        legend.position=c(1,1))
ggsave('SideProjects/TedTalkSep2015/changeInCHIvsOHI.png', width=15, height=13, dpi=300)




mod <- lm(ohi_2015 ~ global_cumul_impact_2013_all_layers, data=data)
summary(mod)

ggplot(data, aes(y=global_cumul_impact_2013_minus_2008, x=ohi_change)) +
  myTheme+
  geom_point(size=2.5, shape=19)+
  labs(y="Change in cumulative impact scores", x="Change in Ocean Health Index scores") +
  stat_smooth(method=lm)  
#ggsave(file.path(path_save, 'Supplement_CHI2013vsPopulation.png'), width=7, height=6, dpi=300)

mod <- lm(global_cumul_impact_2013_minus_2008 ~ ohi_change, data=data) 
summary(mod)


#### Checking into New Zealand----
chi_change <- read.csv("ZonalExtractionData/withZero2NA/diff_2013minus2008_eez_zeroData.csv", stringsAsFactors=FALSE) %>%
  arrange(global_cumulative_impact_dif.gri, layer) %>%
  select(-eez_id, -eez_key, -sov_id, -eez_iso3)


denmark <- filter(chi_change, eez_nam=="Denmark")
netherlands <- filter(chi_change, eez_nam=="Netherlands")




## other files
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

labels <- read.csv(file.path(path_save, "Labels_fig2.csv")) %>%
  select(rgn_id_2013, label)

datadiff2 <- datadiff %>%
  left_join(labels) %>%
  left_join(coastal_pop, by=c("rgn_id_2013")) %>%
  mutate(log_population=log(popsum + 1)) %>%
  left_join(coastal_pop_trend, by=c("rgn_id_2013")) %>%
  mutate(log_area_km2 = log(area_km2))


# hdi <- read.csv("../ohiprep/Global/GL-UNDP-HDI_v2012_continuousVals/data/hdi.csv")  %>%
#   select(rgn_id_2013=rgn_id, hdi=HDI)
# # tried this - but was confusing because such a strong negative relationship between hdi and population trend
# 
# datadiff2 <- datadiff2 %>%
#   left_join(hdi)

datadiff2$label <- ifelse(datadiff2$label==1, as.character(datadiff2$eez_nam), NA)


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
