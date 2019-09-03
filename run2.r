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
tmprast = raster(paste0(rast_temp,"/roi_mask.tif"))
rex = paste(extent(tmprast)[c(1,3,2,4)],collapse=" ")
rres = res(tmprast)
vegbase = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegcode.tif"),attribute=f_vegid)
system(cmd)
mask_tif<-raster(paste0(rast_temp,"/roi_mask.tif"))



rx_write=function(file,outfile){
  require(foreign)
  require(sp)
  require(raster)
  rtable = data.frame(ID = c(1,2,3,4,5,6,7,8,9),
                      Status = c("NoFireRegime",
                                 "TooFrequentlyBurnt",
                                 "Vulnerable",
                                 "LongUnburnt",
                                 "WithinThreshold",
                                 "Recently Treated",
                                 "Monitor OFH In the Field",
                                 "Priority for Assessment and Treatment",
                                 "Unknown"))
  col_vec = c("#ffffff",
              "#ffffff","#ff0000","#ff6600","#00ffff","#999999","#99FF99","#226622","#00ff00","#cccccc")
  
  tr <- raster(paste0(rast_temp,"/",file))
  tr = tr * mask_tif
  tr <- ratify(tr)
  rat <- levels(tr)[[1]]
  rat <- left_join(rat,rtable)
  levels(tr) <- rat
  colortable(tr) <- col_vec
  
  # Write ESRI DB
  
  atable = levels(tr)[[1]]
  names(atable)=c("VALUE","CATEGORY")
  x = as.data.frame(table(raster::values(tr)))
  names(x)=c("VALUE","COUNT")
  x$VALUE = as.numeric(as.character(x$VALUE))
  a2 = left_join(atable,x)
  a2$COUNT[is.na(a2$COUNT)]=0
  a2 = dplyr::select(a2,VALUE,COUNT,CATEGORY)
  write.dbf(a2,paste0(rast_temp,"/",outfile,".vat.dbf"))
  
  # Fix projection
  
  crs(tr) <- CRS('+init=epsg:3308')
  
  bigWrite(tr,paste0(rast_temp,"/",outfile))
  unlink(paste0(rast_temp,"/",file))
  
}

# Insert 3308 projection into TIF output
esri_output = function(tfile){
  log_it("Generating ESRI projection")
  infile = paste0(rast_temp,"/",tfile)
  tempfile = paste0(rast_temp,"/",tfile,".tmp")
  gt = Sys.which("gdal_translate")
  cmd=paste0(gt," ",infile," -a_srs 3308.prj -co COMPRESS=LZW ",tempfile)
  cout = system(cmd,intern=TRUE)
  log_it(cout)
  unlink(infile)
  file.rename(tempfile,infile)
}


#######

vegbase = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
vegcode = raster(paste0(rast_temp,"/r_vegcode.tif"))
vegcode = vegcode * mask_tif

log_it("Generating code table")
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

log_it("Generating colour table")
col_vec = colorRampPalette(c("white","brown","green","red","blue","yellow","pink","purple"))(nrow(rat))

log_it("Assigning colours and names")
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

log_it("Writing file")
bigWrite(tr,paste0(rast_temp,"/r_vegcode2.tif"))
esri_output("r_vegcode2.tif")

log_it("Removing old files")
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