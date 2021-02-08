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

# State ID for California is 6
stateId <- 6

# Copy over national assessment data directory
file.copy(file.path(nationalAssessmentDir, "data"), ".", recursive=TRUE)

# Create California study area
icStates <- raster(file.path(dataDir, "initial-conditions", "final", "ic-states.tif"))
studyArea <- Which(icStates==stateId) %>%
  reclassify(., c(-0.5, 0.5, NA)) %>%
  trim(.)

# Get number of cells in original CONUS study area
CONUSNumCells <- ncell(icStates)

# Select all the tiff files in the data directory and keep those that have the correct number of raster cells
# 1770 tif files meet this criterion
files <- tibble(filename = list.files(path="data", pattern="tif$", full.names=TRUE, recursive=TRUE)) %>%
  rowwise() %>%
  mutate(keep = ifelse(ncell(raster(filename))==CONUSNumCells,1,0)) %>%
  filter(keep == 1) %>%
  select(filename) %>%
  pull()

# Stack, crop and mask all rasters
# WARNING this takes ~ 12 hrs!!! Consider rewriting to make this faster
# user   system  elapsed 
# 44290.14  1273.54 46169.14 
ptm <- proc.time() #start the clock
s <- stack(files) 
s_crop <- crop(s, studyArea) %>%
  mask(., studyArea)
writeRaster(s_crop, filename=files, bylayer=TRUE, format="GTiff", overwrite=TRUE)
proc.time() - ptm #stop the clock
