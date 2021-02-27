# 235
# Create an analysis buffer for California to remove edge effect during Circuitscape connectivity analyses

library(raster)
library(tidyverse)

# Create data dir
dir.create("data")

# working dir is assumed to be the a235 github repo
dataDir <- "data"
nationalAssessmentDir <- "../national-assessment"

# State ID for California is 6
stateId <- 6

# Read in states for CONUS
icStates <- raster(file.path(nationalAssessmentDir, "data", "initial-conditions", "final", "ic-states.tif"))

# Create California study area
studyArea <- Which(icStates==stateId) %>%
  reclassify(., c(-0.5, 0.5, NA)) %>%
  trim(.)

# Read in states and create binary study area
studyArea <- Which(icStates>0)
studyArea[studyArea==0] <- NA

# Create bounding box around study area
# Calculate 20% of the average of the max length and width of the bounding box to be the study area buffer width
# Connectivity analyses will be run in this buffered study area to minimize edge effects
studyAreaBufferWidth <- round(((ymax(studyArea) - ymin(studyArea)) + (xmax(studyArea) - xmin(studyArea))) / 2 * 0.2, digits = 0) # in m
studyAreaExtend <- extend(studyArea, extent(xmin(studyArea) - studyAreaBufferWidth, 
                                      xmax(studyArea) + studyAreaBufferWidth, 
                                      ymin(studyArea) - studyAreaBufferWidth, 
                                      ymax(studyArea) + studyAreaBufferWidth))
studyAreaBuffer <- buffer(studyAreaExtend, studyAreaBufferWidth, doEdge=T)


# Create a raster with values of 0 in open water
waterBuffer <- studyAreaBuffer
values(waterBuffer) <- 0
waterBuffer <- mask(waterBuffer, studyAreaBuffer)

# Write raster
writeRaster(studyArea, file.path(dataDir, "study-area.tif"))
writeRaster(studyAreaBuffer, file.path(dataDir, "study-area-buffered.tif"))
writeRaster(waterBuffer, file.path(dataDir, "water-buffered.tif"))
