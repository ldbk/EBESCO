# 
# 
# 
# 
# install_or_update_packages <- function(packages_list){
#   
#   installed <- rownames(installed.packages())
#   
#   # Install missing packages
#   missing <- packages_list[!packages_list %in% installed]
#   if(length(missing) > 0){
#     install.packages(missing, dependencies = TRUE, quiet = TRUE)
#   }
#   
#   # Update all installed packages 
#   update.packages(ask = FALSE, quiet = TRUE)
#   
#   # Load packages
#   invisible(lapply(packages_list, function(packages){
#     suppressPackageStartupMessages(library(packages, character.only = TRUE))
#   }))
# }
# 
# 
# packages <- c(
#   "tidyverse",        
#   "here",
#   "readxl",
#   "devtools",
#   "TMBhelper",
#   "TMB",
#   "pak",
#   "sf",
#   "sdmTMB","sdmTMBextra","ncdf4","terra",
#   "raster","ggOceanMaps","ggspatial","rnaturalearth","inlabru",
#   "rmdformats","viridis","visreg","fitdistrplus",#"tweedie",
#   "scales","rlang","ape"
# )
# 
# install_or_update_packages(packages)






library(dplyr)
library(here)
library(devtools)
library(TMBhelper)
library(TMB)
library(tidyverse)
library(pak)
library(readxl)
library(readr)
library(sf)
library(sdmTMB)
library(sdmTMBextra)
library(ncdf4)
library(terra)
library(raster)
library(ggOceanMaps)
library(ggspatial)
library(rnaturalearth)
library(inlabru)
library(rmdformats)
library(viridis)
library(visreg)
library(fitdistrplus)
library(tweedie)
library(scales)
library(rlang)
library(ape)
