
# Automatic installation, update and loading of required packages
# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#


install_or_update_packages <- function(packages_list){
  
  # Get names of currently installed packages
  installed <- rownames(installed.packages())
  
  # Identify packages that are not yet installed
  missing <- packages_list[!packages_list %in% installed]
  
  # Install missing packages 
  if(length(missing) > 0){
    install.packages(missing, dependencies = TRUE)
  }
  
  # Install sdmTMBextra from GitHub if not installed (requires remotes package)
  if(!"sdmTMBextra" %in% installed){
    if(!"remotes" %in% installed) install.packages("remotes")
    remotes::install_github("pbs-assess/sdmTMBextra", dependencies = TRUE)
  }
  
  # Update only the packages listed (not the entire library)
  update.packages(oldPkgs = packages_list, ask = FALSE)
  
  # Load all requested packages silently
  invisible(lapply(packages_list, function(package){
    suppressPackageStartupMessages(library(package, character.only = TRUE))
  }))
}


packages_list <- c(
  "ape",
  "dplyr",
  "ggOceanMaps",
  "ggspatial",
  "here",
  "inlabru",
  "ncdf4",
  "pak",
  "raster",
  "readr",
  "readxl",
  "remotes",
  "rlang",
  "rmdformats",
  "rnaturalearth",
  "scales",
  "sdmTMB",
  "sdmTMBextra",
  "sf",
  "terra",
  "tidyverse",
  "tweedie",
  "viridis"
)

install_or_update_packages(packages_list)