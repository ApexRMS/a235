# 235
# Create an analysis buffer for California to remove edge effect during Circuitscape connectivity analyses

library(raster)
library(tidyverse)

# working dir is assumed to be the a235 github repo
dataDir <- "data"

# Read in initial LULC for CONUS
sc <- raster(file.path("../", "national-assessment", "data", "initial-conditions", "final", "ic-state-class.tif"))

# Read in states and create binary study area
icStates <- raster(file.path(dataDir, "initial-conditions", "final", "ic-states.tif"))
studyArea <- Which(icStates>0)
studyArea[studyArea==0] <- NA

# Create bounding box around study area
# Calculate 20% of the average of the max length and width of the bounding box to be the study area buffer width
# Connectivity analyses will be run in this buffered study area to minimize edge effects
studyAreaBufferWidth <- round(((ymax(icStates) - ymin(icStates)) + (xmax(icStates) - xmin(icStates))) / 2 * 0.2, digits = 0) # in m
studyArea <- extend(studyArea, extent(xmin(studyArea) - studyAreaBufferWidth, 
                                      xmax(studyArea) + studyAreaBufferWidth, 
                                      ymin(studyArea) - studyAreaBufferWidth, 
                                      ymax(studyArea) + studyAreaBufferWidth))
studyAreaBuffer <- buffer(studyArea, studyAreaBufferWidth, doEdge=T)


# Create a raster with values of open water (value = 11)
waterBuffer <- studyAreaBuffer
values(waterBuffer) <- 11
waterBuffer <- mask(waterBuffer, studyAreaBuffer)

## Crop LULC data to study area buffer region
scCrop <- sc %>%
  crop(., studyAreaBuffer) %>%
  extend(., studyAreaBuffer) %>% #Note: need to extend original CONUS extent into water
  mask(., studyAreaBuffer)

# Merge CONUS and water buffers  
scBuffered <- merge(scCrop, waterBuffer)

# Write raster
writeRaster(studyAreaBuffer, file.path(dataDir, "study-area-buffered.tif"))
writeRaster(scCrop, file.path(dataDir, "ic-state-class-buffered.tif"))
writeRaster(scBuffered, file.path(dataDir, "ic-state-class-buffered-with-water.tif"))
writeRaster(waterBuffer, file.path(dataDir, "water-buffer.tif"))
