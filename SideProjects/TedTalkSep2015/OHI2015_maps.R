#######################################################################
### NCEAS/OHI
### R script to make maps of score data
### Melanie Frazier Oct 17, 2013
#######################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(hwriter)
library(RColorBrewer)
library(knitr)
library(googleVis)
library(ohicore)
library(sp)
library(rgdal)

radicalFile = '2015-09-11'
saveFile = 'global2015'
scenario = "2015"

goals <- c('Index', 'AO', 'SPP', 'BD', 'HAB', 'CP', 'CS', 'CW', 'ECO', 'LE', 'LIV', 'FIS', 'FP', 'MAR', 'ICO', 'SP', 'LSP', 'NP', 'TR')


## Load the map data:
 p <- readOGR("/var/data/ohi/model/GL-NCEAS-OceanRegions_v2013a/data", "rgn_ocean_cntry_gcs")

 ## This isolates the main map parts: 
 ocean  <-  p[is.na(p@data$ISO_3digit) & p@data$rgn_id==0,]
 land  <-  p[!is.na(p@data$ISO_3digit) & p@data$rgn_id==0,]
 rgnOHI <-  p[p@data$rgn_id>0,]
 
 
 ## read in data:
 data <- read.csv(sprintf('%s/radical_%s.csv', saveFile, radicalFile)) 
 
 ## global score map
 data <- data[data$scenario == scenario, ]
 
 data <- data %>%
   filter(dimension == "score") %>%
   filter(region_id <= 250) %>%
   filter(region_id != 0) %>%
   select(code=region_id, goal, value) %>%
   mutate(goal = factor(goal, levels=goals)) 
 
 PlotData <- spread(data, goal, value) 
 rownames(PlotData) <- PlotData$code
 
 mapCols <- 2:2
 
 ## This loop goes through the columns to be plotted and:
 ## 1. matches the data row names (rgn_id) to the rgn_id of the OHI regions
 ## 2. identifies the break points for colors from: 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100
 ## 3. plots the maps and saves them to the "fig" file in the working directory 
 plotMap <- function(
   fileSave = '../cia/SideProjects/TedTalkSep2015/OHI2015_GerAus.png'
   rgn_id_bright = c(16, 176) #Australia and Germany
   #rgn_id_bright = c(90) #Prince Edward Islands
   ){
 for (i in 1:length(mapCols)){ 
#   i <- 1
   s=mapCols[i]
   # Index map
   fld = names(PlotData)[s]
   v= PlotData[match(rgnOHI@data$rgn_id, row.names(PlotData)), fld]    
   maps = setNames(list(v), fld)  
   

   col.brks = c(0, seq(20,100,by=1))
   
   cols = colorRampPalette(brewer.pal(11, 'RdYlBu'))(length(col.brks))[cut(v, col.brks, include.lowest=TRUE)]
   
   if(sum(!is.na(rgn_id_bright))>0){
        cols = paste0(cols, "26")
        cols[which(rgnOHI@data$rgn_id %in% rgn_id_bright)] = 
          substr(cols[which(rgnOHI@data$rgn_id %in% rgn_id_bright)], 1, 7)
        cols[cols %in% "NA26"] <- NA
   }
   
   # plot map
   png(file=fileSave, width=1200, height=800, res=150, pointsize=18, type='cairo')
   par(oma=c(0,0,0,0),
       mar=c(1.5,1,0,1))
   plot(ocean, col='gray90', border=NA)
   plot(rgnOHI, border="grey75", add=TRUE, col = cols)
   plot(land, col='gray80', border='grey75', add=TRUE)
   
   # finish fig
   dev.off() #; system(sprintf('open %s', fig))
 }
 }
 