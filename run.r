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

dir = list.files("../inputs/")
print(dir)
#print(list.files(paste0("~/inputs/",dir))
      