# a235
# Prepare model inputs for California land carbon stsim-sf model
# Copy national-assessment data and crop spatial data to California
# Write California-extent rasters over CONUS rasters to maintain original data file structure
# Script by Bronwyn Rayfield, ApexRMS

library(raster)
library(tidyverse)

# working dir is assumed to be the a235 github repo
# National assessment githhub repo is at the same level as the a235 repo 
dataDir <- "data"
nationalAssessmentDir <- "../national-assessment"

# Copy over national assessment data directory
file.copy(file.path(nationalAssessmentDir, "data"), ".", recursive=TRUE)

# Read in study area layer to be used as clipping mask
studyArea <- raster(file.path(dataDir, "study-area.tif"))

# Get number of cells in original CONUS study area
icStates <- raster(file.path(nationalAssessmentDir, "data", "initial-conditions", "final", "ic-states.tif"))
CONUSNumCells <- ncell(icStates)

# Select all the tiff files in the data directory and keep those that have the correct number of raster cells
# 1770 tif files meet this criterion
files <- tibble(filename = list.files(path="data", pattern="tif$", full.names=TRUE, recursive=TRUE)) %>%
  rowwise() %>%
  mutate(keep = ifelse(ncell(raster(filename))==CONUSNumCells,1,0)) %>%
  filter(keep == 1) %>%
  select(filename) %>%
  pull()

# Crop and mask all rasters
# This takes ~ 30 mins
# user  system elapsed 
# 1605.25  302.24 1917.95 
ptm <- proc.time() #start the clock
for(i in 1:length(files)){
  s <- raster(files[i])
  print(paste("Working on", files[i]))
  s_crop <- crop(s, studyArea) %>%
    mask(., studyArea)
  
  writeRaster(s_crop, filename=files[i], bylayer=TRUE, format="GTiff", overwrite=TRUE)
}
proc.time() - ptm #stop the clock


