# Functions
bigWrite <- function(r,out){
  s2 <- writeStart(r, filename=out, format='GTiff', overwrite=TRUE)
  tr <- blockSize(r)
  for (i in tr$n:1) {
    v <- getValuesBlock(r, row=tr$row[i], nrows=tr$nrows[i])
    s2 <- writeValues(s2, v, tr$row[i])
  }
  s2 <- writeStop(s2)
}


rx_write=function(file){
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
  write.dbf(a2,paste0(rast_temp,"/",file,".vat.dbf"))
  
  # Fix projection
  
  crs(tr) <- CRS('+init=epsg:3308')
  
  bigWrite(tr,paste0(rast_temp,"/",file,".tmp"))
  file.rename(paste0(rast_temp,"/",file,".tmp"),paste0(rast_temp,"/",file))
  
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
