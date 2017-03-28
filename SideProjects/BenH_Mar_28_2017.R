## Ben request on Mar 28 2016 for this plot with different color scale

# Supplement figure 6 
png(file.path(path_save, "Pressures2013_raw_2013_OneYear/SumRawPressures_na_rm_clipped_new_color.png"), res=300, width=8, height=5, units="in")  
cols = colorRampPalette(c("#EF4F2D", "#EF4F2D", "#EF4F2D", "#EF4F2D",
                          "#E18629", "#E18629", "#E18629", 
                          "#E0E34A", "#E0E34A", "#E0E34A", 
                          "#66ABBA", "#72858B", 
                          "#2B6096", "#57903F", "#5A5B5D"))(250)
plot(total, col=rev(cols), axes=FALSE, box=FALSE, legend.shrink=0.5, legend.width=0.6, 
     breaks=seq(minValue(total), maxValue(total), length.out=250),
     axis.args=list(cex.axis=1.3,
                    at=seq(minValue(total), maxValue(total), 2),
                    labels=seq(minValue(total), maxValue(total), 2))
)
title(main="Total stressor intensity", line=1)

plot(ice_max, add=TRUE, border=NA, col="#0000004D") #30% transparent
plot(ice_min, add=TRUE, border="gray80", col="white")
plot(ice_min, add=TRUE, border=NA, col="white")

plot(land, add=TRUE, border="gray80", col="gray90", lwd=0.5)
plot(extent(fl_extent), col="red", add=TRUE)
plot(extent(uk_extent), col="red", add=TRUE)
plot(extent(cn_extent), col="red", add=TRUE)
plot(extent(med_extent), col="red", add=TRUE)
dev.off()

region_plot <- function(raster_name, name){
  #   raster_name <- fl
  #   name <- "FL_SOM6a"
  png(file.path(path_save, sprintf("Pressures2013_raw_2013_OneYear/%s.png",  name)), res=300, width=8, height=8, units="in")  
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
region_plot(fl, "FL_SOM6a_new_color")

med <- crop(total, med_extent)
region_plot(med, "med_SOM6b_new_color")

uk <- crop(total, uk_extent)
region_plot(uk, "uk_SOM6c_new_color")

cn <- crop(total, cn_extent)
region_plot(cn, "cn_SOM6d_new_color")


