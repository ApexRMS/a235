# a235
# Extra code related to processing the buffer around the study area

# Treat initial state class separately in order to give "Water" pixels a value of 11
# Create a raster with values of open water (value = 11)
waterBuffer[waterBuffer == 0] <- 11

## Crop LULC data to study area buffer region
scCrop <- raster(file.path(nationalAssessmentDir, "data", "initial-conditions", "final", "ic-state-class.tif")) %>%
  crop(., studyArea) %>%
  extend(., studyArea) %>% #Note: need to extend original CONUS extent into water
  mask(., studyArea)

# Merge CONUS and water buffers  
scBuffered <- merge(scCrop, waterBuffer)

writeRaster(s_crop0, filename=file.path(dataDir, "initial-conditions", "final", "ic-state-class.tif"), bylayer=TRUE, format="GTiff", overwrite=TRUE)


