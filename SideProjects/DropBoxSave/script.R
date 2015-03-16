## seeing about saving files to dropbox
## Instructions are from here: https://github.com/karthik/rDrop
## currently not working for large files - will be updated in about 2 weeks to solve these problems

#install_github("duncantl/ROAuth")
library(devtools)
#install_github("karthik/rDrop")
library(rDrop)

## Only need to do this once (for Melanie_Frazier_rDrop2)
# dropbox_credentials <- dropbox_auth("xrdexfos887vlam", "eplql5bmt6p4m85")
# save(dropbox_credentials, file="SideProjects/DropBoxSave/my_dropbox_credentials.rdata")
load('SideProjects/DropBoxSave/my_dropbox_credentials.rdata')

dropbox_acc_info(dropbox_credentials)

pdf("SideProjects/DropBoxSave/dropboxFig.pdf")
x <- sample(100)
hist(x, freq = FALSE, col = "light blue")
tmp <- dev.off()

setwd('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2008_interim/new_layers/plumes_fert/moll_nontrans_unclipped_1km')
zip_list <- list.files(pattern=".tif")
zip_name <- "plumes_fert_test.zip"
zip(zip_name, zip_list)
fq_zipfile <- normalizePath(zip_name)

file.info("plumes_fert_test.zip")$size
dropbox_put(dropbox_credentials, what="plumes_fert_test.zip", filename='CHI/plumes_fert_test.zip')
# clean up
unlink(zip_name)
