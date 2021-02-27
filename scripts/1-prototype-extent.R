# 235
# Create a prototype study area within California

library(raster)
library(tidyverse)

# Create data dir
dir.create("data")

# working dir is assumed to be the a235 github repo
dataDir <- "data"
nationalAssessmentDir <- "../national-assessment"

# State ID for California is 6
stateId <- 6

# Width and Height of prototype study area (in m)
width <- 200000
height <- 200000

# Read in states for CONUS
icStates <- raster(file.path(nationalAssessmentDir, "data", "initial-conditions", "final", "ic-states.tif"))

# Create California study area
studyAreaCA <- Which(icStates==stateId) %>%
  reclassify(., c(-0.5, 0.5, NA)) %>%
  trim(.)

# Set prototype extent in from the coast and from the northern state border
xminNew <- xmin(studyAreaCA) + 60000
xmaxNew <- xminNew + width
ymaxNew <- ymax(studyAreaCA) - 60000
yminNew <- ymaxNew - height

            
prototypeExtent <- extent(c(xminNew, xmaxNew, yminNew, ymaxNew))

studyAreaPrototype <- crop(studyAreaCA, prototypeExtent)

# Write raster
writeRaster(studyAreaPrototype, file.path(dataDir, "study-area.tif"), overwrite=T)
