library(tidyverse)
library(rsyncrosim)
library(raster)
library(sf)
library(viridis)
library(scales)
library(landscapemetrics)
library(cowplot)
library(ggthemes)

options(tibble.width = Inf, tibble.print_min = Inf)

#### Inputs ####
# Input parameters
resultTag <- c("NC_NC", "NC_45", "NC_85", "BAU_NC", "BAU_45", "BAU_85", "CON_NC", "CON_45","CON_85")
resultTag <- c("NC_NC", "NC_45", "NC_85", "BAU_NC", "BAU_45", "BAU_85", "CON_NC", "CON_45","CON_85")
numScenarios <- 6#length(resultTag)
numIterations <- 1
timeInterval <- 10
timesteps <- seq(from=2000, to=2020, by=timeInterval)
numTimesteps = length(timesteps)
speciesCodes <- c("PEPE")
numSpecies <- length(speciesCodes)


# Input files and folders
workingDir <- "C:/Users/bronw/Documents/Apex/Projects/Active/A224_MDDELCC/stsim/ResultsSummary/"

# Primary stratum
spatialInitialConditionsDir <- "C:/Users/bronw/Documents/Apex/Projects/Active/A224_MDDELCC/stsim/SpatialInitialConditions/FullExtent/"
primaryStratumFileName <- "PrimaryStratum.tif"

#### Create clipping mask
primaryStratum <- raster(paste0(spatialInitialConditionsDir, primaryStratumFileName))
# BTSL map
btslMask <- Which(primaryStratum %in% c(3))
btslMask[btslMask == 0] <- NA

#Saint-Lawrence Lowlands
bt=st_read("C:/Users/bronw/Documents/Apex/Projects/Active/A224_MDDELCC/Data/btsl_90m_polygon.shp")
btp=as(bt, "Spatial")

#Raster with ecoregions
bt6 <- raster("C:/Users/bronw/Documents/Apex/Projects/Active/A224_MDDELCC/Data/CERO04_BTSL20201127.tif")

####MAPPING FUNCTIONS####

#Assemble all the pieces and map them together
#Define the map theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Arial", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      #panel.border = element_rect(colour = "grey", fill=NA, size=1),
      legend.position="bottom",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-10,-10,-10,-10),
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    )
}

# STSim
SyncroSimDir <- "C:/Users/bronw/Documents/Apex/SyncroSim/2-2-10/"
modelDir <- c("D:/Apex/Projects/A224/stsim/NC_NC_backup/",
              "D:/Apex/Projects/A224/stsim/NC_45_backup/",
              "D:/Apex/Projects/A224/stsim/NC_85_backup/",
              "D:/Apex/Projects/A224/stsim/BAU_NC_backup/",
              "D:/Apex/Projects/A224/stsim/BAU_45_backup/",
              "D:/Apex/Projects/A224/stsim/BAU_85_backup/",
              "D:/Apex/Projects/A224/stsim/CON_NC_backup/",
              "D:/Apex/Projects/A224/stsim/CON_45_backup/",
              "D:/Apex/Projects/A224/stsim/CON_85_backup/")
resultScenarioNumber <- c(164, 164, 164, 164, 164, 164, 166, 166, 166) 
modelFile <- "BTSL_stconnect.ssim"


# Habitat suitability all species ---------------------------------------------
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
    mapName <- "stconnect_HSOutputHabitatSuitability"
    
    # Get habitat suitability map for year, iteration 1 for all species
    habSuit <- datasheetRaster(myScenario, mapName, iteration = 1, timestep = year)
    # Sum all species
    habSuit <- calc(habSuit, sum)
    # Crop to focal stratum
    #habSuit <- habSuit * clippingMask
    
    # Get habitat suiability maps for year and all other iterations
    for(i in 2:numIterations){
      # Read in habitat suitability map for all iterations for all species 
      habSuitTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
      # Sum all species
      habSuitTemp <- calc(habSuitTemp, sum)
      # Crop to focal stratum
      #  habSuitTemp <- habSuitTemp * clippingMask
      
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
