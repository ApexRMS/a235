library(tidyverse)
library(rsyncrosim)
library(raster)


options(tibble.width = Inf, tibble.print_min = Inf)

# Input parameters -----------------------------------------------------------
numScenarios <- 1
numIterations <- 1
timeInterval <- 10
timesteps <- seq(from=2000, to=2020, by=timeInterval)
numTimesteps = length(timesteps)


speciesDatasheet <- read_csv(file.path("data/st-connect/species.csv"))
speciesCodes <- speciesDatasheet %>% select('Code') %>% pull()
numSpecies <- length(speciesCodes)

resistanceDatasheet <- read_csv(file.path("data/st-connect/resistance.csv"))

buffer <- raster()

focalNodes <- raster()

# Input files and folders
SyncroSimDir <- "C:/Users/bronw/Documents/Apex/SyncroSim/2-2-26/"
stsimModel <- "../models/Spatial Model Conus.ssim"
resultScenarioNumber <- c(70) 

# Resistance all species -----------------------------------------------------
for(scn in 1:numScenarios){
  print(paste("Working on scenario", resultScenarioNumber[scn]))

  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(stsimModel, session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  for(year in timesteps){
    print(paste("Working on scenario", resultScenarioNumber[scn], "year", year))
    #### Create maps of habitat suitability summed across species and iterations ####
    mapName <- "stsim_HSOutputHabitatSuitability"
    
    # Get habitat suiability maps for year and all other iterations
    for(i in 1:numIterations){
      # Add the buffer
      
      # Reclassify LULC to resistance
      
      
      
      
      
      
      # Read in habitat suitability map for all iterations for all species 
      habSuitTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
      # Sum all species
      habSuitTemp <- calc(habSuitTemp, sum)
      
      habSuit <- habSuit + habSuitTemp
    }    
    
    # Divide by num iterations to get probability
    habSuit <- habSuit / (numIterations * numSpecies * 100)    
    
    # Set NA outside study area
    habSuit<-habSuit*clippingMask
    
    # Set NA outside BTSL
    habSuit_btsl<-habSuit*btslMask
    
    # Write rasters
    # writeRaster(habSuit, paste0(workingDir, resultTag[scn], "_", year, "_habitatSuitabilityALL.tif"), overwrite=T)
    # BTSL
    writeRaster(habSuit_btsl, paste0(workingDir, resultTag[scn], "_", year, "_habitatSuitabilityALL_btsl.tif"), overwrite=T)
  }
}







# Maps of habitat suitability for each focal species -------------------------------------------
for(scn in 1:numScenarios){
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  
  for(year in timesteps){
    print(paste("Working on scenario", resultTag[scn], "year", year))
    mapName <- "stconnect_HSOutputHabitatSuitability"
    
    for(speciesIndex in numSpecies){
      
      # Get habitat suitability map for year, iteration 1 for all species
      habSuit <- datasheetRaster(myScenario, mapName, iteration = 1, timestep = year)
      # Get map for focal species
      habSuit <- habSuit[[speciesIndex]]
      # Sum all species
      habSuit <- calc(habSuit, sum)
      # Crop to focal stratum
      #habSuit <- habSuit * clippingMask
      
      # Get habitat suitability maps for year and all other iterations
      for(i in 2:numIterations){
        # Read in habitat suitability map for all iterations for all species 
        habSuitTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
        # Get map for focal species
        habSuitTemp <- habSuitTemp[[speciesIndex]]
        # Sum all species
        habSuitTemp <- calc(habSuitTemp, sum)
        # Crop to focal stratum
        #habSuitTemp <- habSuitTemp * clippingMask
        
        habSuit <- habSuit + habSuitTemp
      }    
      
      # Divide by num iterations to get probability
      habSuit <- habSuit / (numIterations * 100)   
      
      # Set NA outside study area
      habSuit<-habSuit*clippingMask
      
      # Write rasters
      writeRaster(habSuit, paste0(workingDir, resultTag[scn], "_", mapName, "_", speciesRow, "_", year, ".tif"), overwrite=T)
    }
  }
}




# Habitat patch all species all years ---------------------------------------------
for(scn in 1:numScenarios){
  print(paste("Working on scenario", resultTag[scn]))
  
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
#  for(year in timesteps){
    print(paste("Working on scenario", resultTag[scn]))
    #### Create maps of habitat suitability summed across species and iterations ####
    mapName <- "stconnect_HSOutputHabitatPatch"
    
    # Get habitat patch map for year, iteration 1 for all species
    habPatch <- datasheetRaster(myScenario, mapName, iteration = 1)#, timestep = year)
    # Sum all species
    habPatch <- calc(habPatch, sum)
    # Crop to focal stratum
    #habPatch <- habPatch * clippingMask
    
    # Get habitat patch maps for year and all other iterations
    for(i in 2:numIterations){
      # Read in habitat Patchability map for all iterations for all species 
      habPatchTemp <- datasheetRaster(myScenario, mapName, iteration = i)#, timestep=year)
      # Sum all species
      habPatchTemp <- calc(habPatchTemp, sum)
      # Crop to focal stratum
      #  habPatchTemp <- habPatchTemp * clippingMask
      
      habPatch <- habPatch + habPatchTemp
    }    
    
    # Divide by num iterations to get probability
    habPatch <- habPatch / (numIterations * length(timesteps))    
    
    # Set NA outside BTSL
    habPatch_btsl<-habPatch*btslMask
    
    # Write rasters
    writeRaster(habPatch_btsl, paste0(workingDir, resultTag[scn], "_allYears_habitatPatchALL_btsl.tif"), overwrite=T)
#  }
}

# Habitat patch all species per year ---------------------------------------------
for(scn in 1:numScenarios){
  print(paste("Working on scenario", resultTag[scn]))
  
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  for(year in timesteps){
    print(paste("Working on scenario", resultTag[scn], "year", year))
    #### Create maps of habitat suitability summed across species and iterations ####
    mapName <- "stconnect_HSOutputHabitatPatch"
    
    # Get habitat patch map for year, iteration 1 for all species
    habPatch <- datasheetRaster(myScenario, mapName, iteration = 1, timestep = year)
    # Sum all species
    habPatch <- calc(habPatch, sum)
    # Crop to focal stratum
    #habPatch <- habPatch * clippingMask
    
    # Get habitat patch maps for year and all other iterations
    for(i in 2:numIterations){
      # Read in habitat Patchability map for all iterations for all species 
      habPatchTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
      # Sum all species
      habPatchTemp <- calc(habPatchTemp, sum)
      # Crop to focal stratum
      #  habPatchTemp <- habPatchTemp * clippingMask
      
      habPatch <- habPatch + habPatchTemp
    }    
    
    # Divide by num iterations to get probability
    habPatch <- habPatch / (numIterations)    
    
    # Set NA outside BTSL
    habPatch_btsl<-habPatch*btslMask
    
    # Write rasters
    writeRaster(habPatch_btsl, paste0(workingDir, resultTag[scn], "_", year, "_habitatPatchALL_btsl.tif"), overwrite=T)
  }
}


# Tabular Summary habitat patch per species per year ---------------------------------------------
# Create empty output table
landscapeSummary <- tibble(
  layer = as.integer(),
  level = as.character(), 
  class = as.integer(),
  id = as.integer(), 
  metric = as.character(),        
  value = as.double(), 
  Scenario = as.character(), 
  SpeciesCode = as.character(), 
  Timestep = as.integer(), 
  Iteration = as.integer()
)

# Process habitat patch maps
mapName <- "stconnect_HSOutputHabitatPatch"

# Calculate the following landscape metrics for habitat:
# lsm_c_ca - total area
# lsm_c_np - number of patches 
# lsm_c_area_mn - mean patch area
# lsm_c_area_cv - cv patch area
# lsm_c_para_mn - mean perimeter to area ratio
# lsm_c_para_cv - cv perimeter to area ratio

# Start with initial timestep (2010) and just 1 iteration 
year <- 2010  
i <- 1
for(scn in 1:numScenarios){
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  # Get habitat patch map for year, iteration for all species
  habPatchStack <- datasheetRaster(myScenario, mapName, iteration = i, timestep = year)
  # Clip to BTSL
  habPatchStack <- habPatchStack * btslMask
  # Set area outside patches to NA so there is only 1 class in landscape (halves time for landscape metrics)
  habPatchStack[habPatchStack==0] <- NA
  
  for(speciesIndex in 1:numSpecies){
    print(paste("Working on scenario", resultTag[scn], "year", year, "iteration", i, "species", speciesCodes[speciesIndex]))
    # Get map for focal species
    habPatch <- habPatchStack[[speciesIndex]]
    # Check landscape for landscape metrics
    print(check_landscape(habPatch))
    # Calculate landscape metrics
    #start_time <- Sys.time()
    landscapeSummaryTemp <- calculate_lsm(habPatch,
                                          what = c("lsm_c_ca", "lsm_c_np", 
                                                   "lsm_c_area_mn", "lsm_c_area_cv", 
                                                   "lsm_c_para_mn", "lsm_c_para_cv")) %>%
      mutate(Scenario = resultTag[scn], 
             SpeciesCode = speciesCodes[speciesIndex], 
             Timestep = year,
             Iteration = i)
    
    #end_time <- Sys.time()
    #end_time - start_time
    landscapeSummary <- landscapeSummary %>%
      rbind(landscapeSummaryTemp)
  }
}
rm("year", "i")

# Repeat for all timesteps and numIterations 
for(scn in 1:numScenarios){
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  for(year in timesteps){
    # Get habitat patch maps for year and all other iterations
    for(i in 1:numIterations){
      # Get habitat patch map for year, iteration 1 for all species
      habPatchStack <- datasheetRaster(myScenario, mapName, iteration = i, timestep = year)
      # Clip to BTSL
      habPatchStack <- habPatchStack * btslMask
      # Set area outside patches to NA so there is only 1 class in landscape (halves time for landscape metrics)
      habPatchStack[habPatchStack==0] <- NA
      
      for(speciesIndex in 1:numSpecies){
        print(paste("Working on scenario", resultTag[scn], "year", year, "iteration", i, "species", speciesCodes[speciesIndex]))
        # Get map for focal species
        habPatch <- habPatchStack[[speciesIndex]]
        # Check landscape for landscape metrics
        print(check_landscape(habPatch))
        # Calculate landscape metrics
        #start_time <- Sys.time()
        landscapeSummaryTemp <- calculate_lsm(habPatch,
                        what = c("lsm_c_ca", "lsm_c_np", 
                                 "lsm_c_area_mn", "lsm_c_area_cv", 
                                 "lsm_c_para_mn", "lsm_c_para_cv")) %>%
          mutate(Scenario = resultTag[scn], 
                 SpeciesCode = speciesCodes[speciesIndex], 
                 Timestep = year,
                 Iteration = i)
    
        #end_time <- Sys.time()
        #end_time - start_time
        landscapeSummary <- landscapeSummary %>%
          rbind(landscapeSummaryTemp)
        
      }
      removeTmpFiles(h=2)
    }    
  }
}

write_csv(landscapeSummary, paste0(workingDir, "/test_landscapeMetrics.csv"))


# One plot per species
speciesMetricSummary <- landscapeSummary %>%
  #  mutate(Scenario = as.factor(Scenario)) %>%
  group_by(Scenario, SpeciesCode, Timestep, metric) %>%
  summarise(value_mn = mean(value), value_sd = sd(value)) %>%
  #  filter(SpeciesCode == "BLBR") %>%
  filter(metric %in% c("ca", "np", "area_mn", "area_cv", "para_mn", "para_cv"))


p <- ggplot(speciesMetricSummary, aes(Timestep, value_mn, group = Scenario, color = Scenario)) + 
  geom_errorbar(aes(ymin=value_mn-value_sd, ymax=value_mn+value_sd), width=.1) + 
  geom_line() +
  geom_point() +
  scale_color_brewer(palette="Paired") + 
  facet_wrap(~metric, scales = "free") +
  theme_minimal()

x11(); p



#Fixed/free scales
metricList <- c("Total Area", "Number of Patches", "Mean Patch Area", "Mean Perimeter-Area Ratio", "CV Perimeter-Area Ratio", "CV Patch Area")
for(m in metricList){
  #"NC_NC", "NC_45", "NC_85", "BAU_NC", "BAU_45", "BAU_85", "CON_NC", "CON_45","CON_85"
  speciesMetricSummary <- landscapeSummary %>%
    mutate(Scenario = factor(Scenario, levels = c("NC_NC", "NC_45", "NC_85", "BAU_NC", "BAU_45", "BAU_85", "CON_NC", "CON_45","CON_85")),
           metric = factor(metric, levels=c("ca", "np", "area_mn", "area_cv", "para_mn", "para_cv", "area_sd", "para_sd")),
           metric = fct_recode(metric, "Total Area" = "ca",
                               "Number of Patches" = "np",
                               "Mean Patch Area" = "area_mn",
                               "CV Patch Area" = "area_cv",
                               "Mean Perimeter-Area Ratio" = "para_mn",
                               "CV Perimeter-Area Ratio" = "para_cv",
                               "SD Patch Area" = "area_sd",
                               "SD Perimeter-area Ratio" = "para_sd")) %>%
    group_by(Scenario, SpeciesCode, Timestep, metric) %>%
    summarise(value_mn = mean(value), value_sd = sd(value)) %>%
    filter(metric == m)
  
  
  p <- ggplot(speciesMetricSummary, aes(Timestep, value_mn, group=Scenario, color=Scenario)) +
    geom_errorbar(aes(ymin=value_mn-value_sd, ymax=value_mn+value_sd), width=1) +
    geom_line(size=1.2) +
    geom_point() +
    ylab(m) +
    scale_color_manual(values = scnColours) +
    scale_x_continuous(breaks=c(2010, 2035, 2060, 2085, 2110)) +
    facet_wrap(~SpeciesCode, ncol = 5, scales="free") +
    theme_gdocs()+
    theme(axis.text.y = element_text( size=9),
          axis.text.x = element_text( size=8),
          strip.text.x = element_text(size=0))
  
  p<-ggdraw(p) + draw_image("D:/Apex/Projects/A224/species_icons/BLBR.png", x = 0.65, y = 1.45, hjust = 1, vjust = 1, scale=0.05) +
    draw_image("D:/Apex/Projects/A224/species_icons/MAAM.png", x = 0.81, y = 1.45, hjust = 1, vjust = 1, scale=0.05) +
    draw_image("D:/Apex/Projects/A224/species_icons/PLCI.png", x = 0.975, y = 1.45, hjust = 1, vjust = 1, scale=0.05) +
    draw_image("D:/Apex/Projects/A224/species_icons/RANA.png", x = 1.13, y = 1.45, hjust = 1, vjust = 1, scale=0.05) +
    draw_image("D:/Apex/Projects/A224/species_icons/URAM.png", x = 1.3, y = 1.45, hjust = 1, vjust = 1, scale=0.05)
  
  ggsave(paste0(workingDir, "LandscapeMetrics_,", m, "_yFree.png"), p, height=4, width=11)
  
}





# Habitat patch per species all years ---------------------------------------------
for(scn in 1:numScenarios){
  print(paste("Working on scenario", resultTag[scn]))
  
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  for(year in timesteps){
    print(paste("Working on scenario", resultTag[scn], "year", year))
    #### Create maps of habitat suitability summed across species and iterations ####
    mapName <- "stconnect_HSOutputHabitatPatch"
    
    for(sp in 1:numSpecies){
    # Get habitat patch map for year, iteration 1 for all species
    habPatch <- datasheetRaster(myScenario, mapName, iteration = 1, timestep = year)
    # Sum all species
    habPatch <- calc(habPatch, sum)
    # Crop to focal stratum
    #habPatch <- habPatch * clippingMask
    
    # Get habitat patch maps for year and all other iterations
    for(i in 2:numIterations){
      # Read in habitat Patchability map for all iterations for all species 
      habPatchTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
      # Sum all species
      habPatchTemp <- calc(habPatchTemp, sum)
      # Crop to focal stratum
      #  habPatchTemp <- habPatchTemp * clippingMask
      
      habPatch <- habPatch + habPatchTemp
    }    
    }
    # Divide by num iterations to get probability
    habPatch <- habPatch / (numIterations)    
    
    # Set NA outside BTSL
    habPatch_btsl<-habPatch*btslMask
    
    # Write rasters
    writeRaster(habPatch_btsl, paste0(workingDir, resultTag[scn], "_", year, "_habitatPatchALL_btsl.tif"), overwrite=T)
  }
}
