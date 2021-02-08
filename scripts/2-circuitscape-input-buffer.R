# 235
# Create an analysis buffer for California to remove edge effect during Circuitscape analyses


library(raster)
library(sf)
library(tidyverse)

# working dir is assumed to be the a235 github repo
dataDir <- "data"

# Read in states
icStates <- raster(file.path(dataDir, "initial-conditions", "final", "ic-states.tif"))

# Convert raster to polygon
studyArea <- rasterToPolygons(icStates)


# Create bounding box around study area
# Calculate 20% of the average of the max length and width of the bounding box to be the study area buffer width
# Connectivity analyses will be run in this buffered study area to minimize edge effects
studyAreaBBox <- st_bbox(studyArea)
studyAreaBufferWidth <- round(((studyAreaBBox$ymax - studyAreaBBox$ymin) + (studyAreaBBox$xmax - studyAreaBBox$xmin)) / 2 * 0.2, digits = 0) # in m
studyAreaBuffer <- st_buffer(studyArea, studyAreaBufferWidth)

## Crop LULC data to STUDY AREA BUFFER region and standardize rasters ---------------------

# SOLRIS data
# Crop to buffered study area
SOLRIS_buffer <- SOLRISRaw %>%
  crop(., extent(studyAreaBuffer), snap="out") %>% # Crop SOLRIS to study area buffer extent
  mask(., mask=studyAreaBuffer) %>% # Clip to buffered study area
  trim(.) # Trim extra white spaces
