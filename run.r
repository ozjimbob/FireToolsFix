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
source("../inputs/config_linux.r")
source("functions.r")
rast_temp = "../inputs/output"
dir = list.files(rast_temp)

# Load region and rasterize
print("Loading region")
region = read_sf(paste0(rast_temp,"/v_region.shp"))
region$flag = 1
print("Rasterizing")
tmprast = raster(paste0(rast_temp,"/r_TimeSinceLast.tif"))
mask_tif = fasterize(region,tmprast,field="flag")
print("Writing Region")
bigWrite(mask_tif,paste0(rast_temp,"/roi_mask.tif"))

print("Fix heritage")
rx_write("r_heritage_threshold_status.tif")
esri_output("r_heritage_threshold_status.tif")

print("Fix fmz")
rx_write("r_fmz_threshold_status.tif")
esri_output("r_fmz_threshold_status.tif")

print("Fix heritage + fmz")
rx_write("r_heritage_fmz_threshold_status.tif")
esri_output("r_heritage_fmz_threshold_status.tif")

print("Fix fmz + sfaz")
rx_write("r_fmz_sfaz_threshold_status.tif")
esri_output("r_fmz_sfaz_threshold_status.tif")

print("Fix combined")
rx_write("r_heritage_fmz_sfaz_threshold_status.tif")
esri_output("r_heritage_fmz_sfaz_threshold_status.tif")

### Veg

print("Fixing Veg")
vegbase = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
vegcode = raster(paste0(rast_temp,"/r_vegcode.tif"))
print("Masking veg")
vegcode = vegcode * mask_tif

print("Making codelist")
codelist = tibble(ID=unique(vegbase[[f_vegid]]))
codelist$category = ""
for(i in seq_along(codelist$ID)){
  thisveg = filter(vegbase,!!rlang::sym(f_vegid)==codelist$ID[i])
  codelist$category[i] = thisveg$VEGTEXT[1]
}

codelist = as.data.frame(codelist)


print("Ratify")
tr <- ratify(vegcode)

rat <- levels(tr)[[1]]

print("Joining")
rat <- left_join(rat,codelist)
rat$category[is.na(rat$category)]=""

print("Making colour vector")
col_vec = colorRampPalette(c("white","brown","green","red","blue","yellow","pink","purple"))(nrow(rat))


ids = as.numeric(rownames(rat))
codes = rat$ID
print("Reclassify")
tr = reclassify(tr,cbind(codes,ids))
rat$ID = ids

print("Setting levels")
levels(tr) <- rat

require(foreign)
require(sp)
print("Making DBF table")
atable = levels(tr)[[1]]
names(atable)=c("VALUE","CATEGORY")
x = as.data.frame(table(raster::values(tr)))
names(x)=c("VALUE","COUNT")
x$VALUE = as.numeric(as.character(x$VALUE))
a2 = left_join(atable,x)
a2$COUNT[is.na(a2$COUNT)]=0
a2 = dplyr::select(a2,VALUE,COUNT,CATEGORY)
print("Writing DFB Table")
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

### Fire times


print("Renaming and masking files")
print("Last burnt")
temp_d = raster(paste0(rast_temp,"/r_LastYearBurnt.tif"))
temp_d = temp_d * mask_tif
bigWrite(temp_d,paste0(rast_temp,"/r_LastYearBurnt.tif"))
esri_output("r_LastYearBurnt.tif")


print("times burnt")
temp_d = raster(paste0(rast_temp,"/r_NumTimesBurnt.tif"))
temp_d = temp_d * mask_tif
bigWrite(temp_d,paste0(rast_temp,"/r_NumTimesBurnt.tif"))
esri_output("r_NumTimesBurnt.tif")

print("Time since last")
temp_d = raster(paste0(rast_temp,"/r_TimeSinceLast.tif"))
temp_d = temp_d * mask_tif
bigWrite(temp_d,paste0(rast_temp,"/r_TimeSinceLast.tif"))
esri_output("r_TimeSinceLast.tif")