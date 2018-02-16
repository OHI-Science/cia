## Ben request on Mar 28 2016 for this plot with different color scale

# Supplement figure 8

#######################
## Color scheme variant
#######################

##Get legend
png(file.path(dir_M, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/SumRawPressures_display_legend_Feb16_2018_v1.png"), res=400, width=36, height=22.5, units="in")  
cols = colorRampPalette(c("#EF4F2D", "#EF4F2D", "#EF4F2D", "#EF4F2D",
                          "#E18629", "#E18629", "#E18629", 
                          "#E0E34A", "#E0E34A", "#E0E34A", 
                          "#66ABBA", "#4885A8", 
                          "#2B6096", "#425D79", "#5A5B5D"))(250) #4C6782
plot(total, col=rev(cols), axes=FALSE, box=FALSE, legend.shrink=0.5, legend.width=2, 
     breaks=seq(minValue(total), maxValue(total), length.out=250),
     axis.args=list(cex.axis=2,
                    at=seq(minValue(total), maxValue(total), 2),
                    labels=seq(minValue(total), maxValue(total), 2))
)


plot(land, add=TRUE, border="gray80", col="gray90", lwd=1)
dev.off()

##Figure minus legend
png(file.path(dir_M, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/SumRawPressures_display_Feb16_2018_v1.png"), res=400, width=36, height=22.5, units="in")  
cols = colorRampPalette(c("#EF4F2D", "#EF4F2D", "#EF4F2D", "#EF4F2D",
                          "#E18629", "#E18629", "#E18629", 
                          "#E0E34A", "#E0E34A", "#E0E34A", 
                          "#66ABBA", "#4885A8", 
                          "#2B6096", "#425D79", "#5A5B5D"))(250) #4C6782
plot(total, col=rev(cols), axes=FALSE, box=FALSE, legend=FALSE,  
     breaks=seq(minValue(total), maxValue(total), length.out=250))
)


plot(land, add=TRUE, border="gray80", col="gray90", lwd=1)
dev.off()

#######################
## Original color scheme
#######################

##Get legend
png(file.path(dir_M, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/SumRawPressures_display_legend_Feb16_2018_v2.png"), res=400, width=36, height=22.5, units="in")  
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(250)
plot(total, col=rev(cols), axes=FALSE, box=FALSE, legend.shrink=0.5, legend.width=2, 
     breaks=seq(minValue(total), maxValue(total), length.out=250),
     axis.args=list(cex.axis=2,
                    at=seq(minValue(total), maxValue(total), 2),
                    labels=seq(minValue(total), maxValue(total), 2))
)


plot(land, add=TRUE, border="gray80", col="gray90", lwd=1)
dev.off()

##Figure minus legend
png(file.path(dir_M, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/SideProjects/SumRawPressures_display_Feb16_2018_v2.png"), res=400, width=36, height=22.5, units="in")  
cols = colorRampPalette(brewer.pal(11, 'Spectral'))(250)
plot(total, col=rev(cols), axes=FALSE, box=FALSE, legend=FALSE,  
     breaks=seq(minValue(total), maxValue(total), length.out=250))
)


plot(land, add=TRUE, border="gray80", col="gray90", lwd=1)
dev.off()



