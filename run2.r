## Mode 2
#!/usr/bin/Rscript

## NSW Test
library(tidyverse)
library(sf)
library(raster)
library(spatial.tools)
library(foreach)
library(doParallel)
library(mapview)
library(fasterize)
library(velox)
library(lwgeom)
source("global_config.r")
source("../inputs/inputs/config_linux.r")
source("functions.r")
rast_temp = "../inputs/inputs/output"

# Load veg base
print("Getting temprast")
tmprast = raster(paste0(rast_temp,"/roi_mask.tif"))
rex = paste(extent(tmprast)[c(1,3,2,4)],collapse=" ")
rres = res(tmprast)
print("Loading vegbase")

print("Re-rasterizing vegbase")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegcode.tif"),attribute=f_vegid)
system(cmd)
print("Getting mask tif")
mask_tif<-tmprast





#######
vegbase = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
#vegbase = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
print("Getting veg raster")
vegcode = raster(paste0(rast_temp,"/r_vegcode.tif"))
print("Masking")
vegcode = vegcode * mask_tif

print("Generating code table")
codelist = tibble(ID=unique(vegbase[[f_vegid]]))
codelist$category = ""
vt = which(substr(names(vegbase),1,7)=="VEGTEXT")[1]
for(i in seq_along(codelist$ID)){
  thisveg = filter(vegbase,!!rlang::sym(f_vegid)==codelist$ID[i])
  codelist$category[i] = as.character(thisveg[1,vt])[1]
}

codelist = as.data.frame(codelist)



tr <- ratify(vegcode)

rat <- levels(tr)[[1]]

rat <- left_join(rat,codelist)
rat$category[is.na(rat$category)]=""

print("Generating colour table")
col_vec = colorRampPalette(c("white","brown","green","red","blue","yellow","pink","purple"))(nrow(rat))

print("Assigning colours and names")
ids = as.numeric(rownames(rat))
codes = rat$ID

tr = reclassify(tr,cbind(codes,ids))
rat$ID = ids

levels(tr) <- rat

require(foreign)
require(sp)
atable = levels(tr)[[1]]
names(atable)=c("VALUE","CATEGORY")
x = as.data.frame(table(raster::values(tr)))
names(x)=c("VALUE","COUNT")
x$VALUE = as.numeric(as.character(x$VALUE))
a2 = left_join(atable,x)
a2$COUNT[is.na(a2$COUNT)]=0
a2 = dplyr::select(a2,VALUE,COUNT,CATEGORY)
write.dbf(a2,paste0(rast_temp,"/r_vegcode.tif.vat.dbf"))


col_vec[1]="#ffffff"
col_vec=c("#ffffff",col_vec)
colortable(tr) <- col_vec

print("Writing file")
bigWrite(tr,paste0(rast_temp,"/r_vegcode2.tif"))
esri_output("r_vegcode2.tif")

print("Removing old files")
unlink(paste0(rast_temp,"/r_vegcode.tif"))
unlink(paste0(rast_temp,"/r_vegcode.tif.aux.xml"))
file.rename(paste0(rast_temp,"/r_vegcode2.tif"),paste0(rast_temp,"/r_vegcode.tif"))
file.rename(paste0(rast_temp,"/r_vegcode2.tif.aux.xml"),paste0(rast_temp,"/r_vegcode.tif.aux.xml"))

in_file = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_vegBase.shp"))


## Shapefile test

file.copy("3308.prj",paste0(rast_temp,"/v_fmz_sfaz_threshold_status.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_fmz_threshold_status.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_heritage_fmz_threshold_status.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_heritage_threshold_status.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_heritage_fmz_sfaz_threshold_status.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_region.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_tsl.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_timesburnt.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_tsl_sfaz.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_sfaz_candidate_blocks.prj"),overwrite=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_vegBase.prj"),overwrite=TRUE)