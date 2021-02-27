#a235
# Create focal nodes arond analysis buffer for circuitscape

## Workspace ---------------------------------------------------------
# Packages
library(tidyverse)
library(raster)

# Settings
options(stringsAsFactors=FALSE, SHAPE_RESTORE_SHX=T, useFancyQuotes = F, digits=10)

# Directories
rawDataDir <- "Data/Raw"
procDataDir <- "Data/Processed"

## Input parameters
source(file.path("Data", "a233_InputParams.R")) # project level parameters
polygonBufferWidth


## Read in data---------------------------------------------------------
# Resistance layer
studyArea <- raster(
  file.path(procDataDir, 
            paste0("Generic_Resistance_", polygonBufferWidth, "km_buffer.tif")))


## Generate nodes ---------------------------------------------------------

# Create binary study area raster
studyArea[studyArea > 0] <- 1

#ID boundary 
boundary <- boundaries(studyArea, type='inner', directions=8, asNA=T)

focalNodes50 <- sampleRandom(boundary, size=50, asRaster=T)
focalNodesID50 <- clump(focalNodes50)


writeRaster(focalNodesID50, file.path(procDataDir, paste0("FocalNode_50_", polygonBufferWidth, "km_buffered.tif")), overwrite=T)