



library(raster)
library(tidyverse)
library(rsyncrosim)

# Create a simple ST-Sim library which will calculate spatial initial conditions maps from non-spatial input
SyncroSimDir <- "C:/Users/bronw/Documents/Apex/SyncroSim/2-2-26"
mySession <- session(SyncroSimDir)

ssimDir = "models/"

myLibrary = ssimLibrary(name = paste0(ssimDir,"Spatial Model Conus.ssim"), session = mySession)
#myLibrary = ssimLibrary(name = paste0(ssimDir,"Initial Stocks Model.ssim"), session = mySession)
myProject = project(myLibrary, project="Spatial Model")

# BR added code
maxAge=800
enableAddon(myLibrary, "stsimsf")


########################
# Definitions (ST-Sim) #
########################

# ST-Sim Terminology
sheetName <- "stsim_Terminology"
mySheet <- datasheet(myProject, name=sheetName)
mySheet$AmountLabel[1] <- "Area"
mySheet$AmountUnits[1] <- "Hectares"
mySheet$StateLabelX[1] <- "LULC"
mySheet$StateLabelY[1] <- "Subclass"
mySheet$PrimaryStratumLabel[1] <- "Ecological Boundary"
mySheet$SecondaryStratumLabel[1] <- "Administrative Boundary"
mySheet$TimestepUnits[1] <- "Year"
saveDatasheet(myProject, mySheet, sheetName)

# ST-Sim Primary Strata 
sheetName <- "stsim_Stratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/ecoregions.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim Secondary Strata 
sheetName <- "stsim_SecondaryStratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/states.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim Tertiary Strata 
sheetName <- "stsim_TertiaryStratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/land-managers-strata.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim State Label x
sheetName <- "stsim_StateLabelX"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/state-label-xid.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim State Label y
sheetName <- "stsim_StateLabelY"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/state-label-yid.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim State Class
sheetName <- "stsim_StateClass"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/state-class-types.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim Transition Types
sheetName <- "stsim_TransitionType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/transition-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

# ST-Sim Transition Groups
sheetName <- "stsim_TransitionGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/transition-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### ST-Sim Transition Types by Group
sheetName <- "stsim_TransitionTypeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/transition-types-by-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim Transition Simulation Groups
sheetName <- "stsim_TransitionSimulationGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/transition-simulation-groups.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=F)

#ST-Sim Age Type
sheetName <- "stsim_AgeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1,"Frequency"] <- 1
mySheet[1,"MaximumAge"] <- 300
saveDatasheet(myProject, mySheet, name=sheetName)

#ST-Sim Age Group
sheetName <- "stsim_AgeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1:(maxAge/20),"MaximumAge"] <- c(seq(from=20, to=(maxAge-1), by=20), maxAge-1)
saveDatasheet(myProject, mySheet, name=sheetName)

#ST-Sim Attribute Groups
sheetName <- "stsim_AttributeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/state-attribute-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

#ST-Sim Attribute Types
sheetName <- "stsim_StateAttributeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/state-attribute-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim Distributions
sheetName <- "corestime_DistributionType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/distributions.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### ST-Sim External Variables
sheetName <- "corestime_ExternalVariableType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/external-variables.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

#SF Stock Types
sheetName <- "stsimsf_StockType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/stock-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

#SF Stock Groups
sheetName <- "stsimsf_StockGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/stock-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

#SF Flow Types
sheetName <- "stsimsf_FlowType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/flow-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

#SF Flow Groups
sheetName <- "stsimsf_FlowGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("data/definitions/flow-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)



####################################
# Sub-scenario datasheets (ST-Sim) #
####################################

# ST-Sim Run Control #####
maxTimestep <- 2020
maxIteration <- 1
minTimestep <- 2001
minIteration <- 1
myScenario <- scenario(myProject, scenario <- paste0("Run Control [Spatial; 2001-2020; 1 MC]"))
sheetName <- "stsim_RunControl"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"MinimumIteration"] <- minIteration
mySheet[1,"MaximumIteration"] <- maxIteration
mySheet[1,"MinimumTimestep"] <- minTimestep
mySheet[1,"MaximumTimestep"] <- maxTimestep
mySheet[1,"IsSpatial"] <- T
saveDatasheet(myScenario, mySheet, sheetName)

# Transition Pathways #####
# Urbanization
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Urbanization]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/transition-pathways/pathway-diagram-probabilistic-urbanization.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Expansion
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Ag Expansion]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/transition-pathways/pathway-diagram-probabilistic-agexpansion.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Contraction
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Ag Contraction]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/transition-pathways/pathway-diagram-probabilistic-agcontraction.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Urban Intensification
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Intensification]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/transition-pathways/pathway-diagram-probabilistic-intensification.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Fire
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Fire]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/transition-pathways/pathway-diagram-probabilistic-fire.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Insect
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Insect]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/transition-pathways/pathway-diagram-probabilistic-insect.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Harvest
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Harvest]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/transition-pathways/pathway-diagram-probabilistic-harvest.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Succession Post Fire
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Succession Post Fire]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/transition-pathways/pathway-diagram-probabilistic-succession.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Reforestation
myScenario <- scenario(myProject, scenario = "Pathway Diagram [Reforestation]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/transition-pathways/pathway-diagram-probabilistic-reforestation.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Merge Pathway Dependencies
myScenario <- scenario(myProject, scenario = "Pathway Diagram")
mergeDependencies(myScenario) = TRUE
dependency(myScenario, c("Pathway Diagram [Urbanization]", "Pathway Diagram [Ag Expansion]", "Pathway Diagram [Ag Contraction]", "Pathway Diagram [Intensification]",
                         "Pathway Diagram [Fire]", "Pathway Diagram [Insect]", "Pathway Diagram [Harvest]", "Pathway Diagram [Succession Post Fire]", "Pathway Diagram [Reforestation]"))



# Initial Conditions #####
# CONUS Model
myScenario <- scenario(myProject, scenario = "Initial Conditions [Spatial; Imputed Age]")
sheetName <- "stsim_InitialConditionsSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "StratumFileName"] <- file.path(getwd(), "data/initial-conditions/final/ic-ecoregion.tif")
mySheet[1, "SecondaryStratumFileName"] <- file.path(getwd(), "data/initial-conditions/final/ic-states.tif")
mySheet[1, "TertiaryStratumFileName"] <- file.path(getwd(), "data/initial-conditions/final/ic-land-managers.tif")
mySheet[1, "StateClassFileName"] <- file.path(getwd(), "data/initial-conditions/final/ic-state-class.tif")
mySheet[1, "AgeFileName"] <- file.path(getwd(), "data/initial-conditions/final/ic-imputed-age.tif")
saveDatasheet(myScenario, mySheet, sheetName)

# ST-Sim Output options #####
myScenario <- scenario(myProject, scenario = "Output Options [Spatial]")
sheetName <- "stsim_OutputOptions"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "SummaryOutputSC"] <- T
mySheet[1, "SummaryOutputSCTimesteps"] <- 1
mySheet[1, "SummaryOutputSCZeroValues"] <- F
mySheet[1, "SummaryOutputTR"] <- T
mySheet[1, "SummaryOutputTRTimesteps"] <- 1
mySheet[1, "SummaryOutputTRIntervalMean"] <- F
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Output Options [Spatial]")
sheetName <- "stsim_OutputOptionsSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "RasterOutputSC"] <- T
mySheet[1, "RasterOutputSCTimesteps"] <- 1
mySheet[1, "RasterOutputAge"] <- T
mySheet[1, "RasterOutputAgeTimesteps"] <- 1
mySheet[1, "RasterOutputTR"] <- T
mySheet[1, "RasterOutputTRTimesteps"] <- 1
saveDatasheet(myScenario, mySheet, sheetName)

# ST-Sim State Attribute Values #####
# Net Growth
myScenario <- scenario(myProject, scenario = "State Attributes Spin-up [Net Growth]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_StateAttributeValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/state-attributes/state-attributes-spin-up-net-growth.csv") 
saveDatasheet(myScenario, mySheet, sheetName)

# Adjacency
myScenario <- scenario(myProject, scenario = "State Attributes [Adjacency]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_StateAttributeValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/state-attributes/state-attribute-values-adjacency.csv") 
saveDatasheet(myScenario, mySheet, sheetName)

# Merge State Attribute Values
myScenario <- scenario(myProject, scenario = "State Attributes")
mergeDependencies(myScenario) = T
dependency(myScenario, c("State Attributes Spin-up [Net Growth]", "State Attributes [Adjacency]"))


# Distributions #####
myScenario <- scenario(myProject, scenario = "Distirbutions [Urbanization]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/distributions/distribution-urbanization-total.csv")  %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Intensification]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/distributions/distribution-intensification-total.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Ag Expansion]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/distributions/distribution-ag-expansion-total.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Ag Contraction]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/distributions/distribution-ag-contraction-total.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Fire]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/distributions/distribution-fire-total.csv") 
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Insect]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/distributions/distribution-insect.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Harvest]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/distributions/distribution-harvest.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)


myScenario <- scenario(myProject, scenario = "Distributions")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Distirbutions [Urbanization]", "Distirbutions [Intensification]", 
                         "Distirbutions [Ag Expansion]", "Distirbutions [Ag Contraction]", 
                         "Distirbutions [Fire]", "Distirbutions [Insect]", "Distirbutions [Harvest]"))


# External Variables #####
myScenario <- scenario(myProject, scenario = "External Variables")
sheetName <- "corestime_ExternalVariableValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/external-variables/external-variables-bau.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Transition Targets #####
myScenario <- scenario(myProject, scenario = "Transition Targets [Reference]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-targets/transition-targets-reference.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Transition Multipliers #####
myScenario <- scenario(myProject, scenario = "Transition Multipliers [Urbanization]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-multipliers/transition-multipliers-urbanization-type.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Transition Multipliers [Intensification]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-multipliers/transition-multipliers-intensification-type.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Transition Multipliers [Ag Expansion]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-multipliers/transition-multipliers-ag-expansion-type.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Transition Multipliers [Ag Contraction]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-multipliers/transition-multipliers-ag-contraction-type.csv") 
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Transition Multipliers [Forest Harvest Age]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-multipliers/transition-multipliers-harvest-age.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# myScenario <- scenario(myProject, scenario = "Transition Multipliers [Forest Harvest]")
# mergeDependencies(myScenario) = T
# sheetName <- "stsim_TransitionMultiplierValue"
# mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
# mySheet <- read.csv("data/transition-multipliers/transition-multipliers-harvest-type.csv")
# saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Transition Multipliers [Fire Severity]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-multipliers/transition-multipliers-fire-severity.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Transition Multipliers [Insect Severity]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-multipliers/transition-multipliers-insect-severity.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Transition Multipliers [Succession]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-multipliers/transition-multipliers-succession.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Transition Multipliers")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Transition Multipliers [Urbanization]", "Transition Multipliers [Intensification]",
                         "Transition Multipliers [Ag Expansion]", "Transition Multipliers [Ag Contraction]", "Transition Multipliers [Forest Harvest Age]",
                         "Transition Multipliers [Fire Severity]", "Transition Multipliers [Succession]"))

# Transition Size Multipliers #####
# Fire
myScenario <- scenario(myProject, scenario = "Transition Size Distribution [Fire]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSizeDistribution"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/transition-size-distribution/transition-size-distribution-fire.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_TransitionSizePrioritization"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet = read.csv("data/transition-size-distribution/transition-size-priority.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Transition Slope Multipliers #####
# Fire
myScenario <- scenario(myProject, scenario = "Transition Slope Multipliers [Fire]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DigitalElevationModel"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet[1,"DigitalElevationModelFileName"] = file.path(getwd(), "data/slope-multipliers/dem-1km.tif")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_TransitionSlopeMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("data/slope-multipliers/transition-slope-multipliers-fire.csv")
saveDatasheet(myScenario, mySheet, sheetName)


# Transition Spatial Multipliers #####
# Urbanization

# Fire
dir1 = file.path(getwd(), "data", "spatial-multipliers", "fire")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Fire"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Fire]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2020,1), TransitionGroupID = tg1, MultiplierFileName = file.path(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = 2021, TransitionGroupID = tg1, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-all-cells-1.tif")))
saveDatasheet(myScenario, mySheet, sheetName)

# Insect
dir1 = file.path(getwd(), "data", "spatial-multipliers", "insect-high-severity")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Insect: High Severity [Type]"

dir2 = file.path(getwd(), "data", "spatial-multipliers", "insect-medium-severity")
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Insect: Medium Severity [Type]"

dir3 = file.path(getwd(), "data", "spatial-multipliers", "insect-low-severity")
list3 = list.files(dir3, pattern = "*.tif$")
tg3 = "Insect: Low Severity [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Insect]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2015,1), TransitionGroupID = tg1, MultiplierFileName = file.path(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2015,1), TransitionGroupID = tg2, MultiplierFileName = file.path(dir2,list2)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2015,1), TransitionGroupID = tg3, MultiplierFileName = file.path(dir3,list3)))
mySheet = addRow(mySheet, data.frame(Timestep = 2016, TransitionGroupID = tg1, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-all-cells-1.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2016, TransitionGroupID = tg2, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-all-cells-1.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2016, TransitionGroupID = tg3, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-all-cells-1.tif")))
saveDatasheet(myScenario, mySheet, sheetName)

# Harvest
dir1 = file.path(getwd(), "data", "spatial-multipliers", "clearcut")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Forest Harvest: Forest Clearcut [Type]"

dir2 = file.path(getwd(), "data", "spatial-multipliers", "selection")
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Forest Harvest: Forest Selection [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Forest Harvest]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2016,1), TransitionGroupID = tg1, MultiplierFileName = file.path(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2016,1), TransitionGroupID = tg2, MultiplierFileName = file.path(dir2,list2)))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-harvest.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg2, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-harvest.tif")))
saveDatasheet(myScenario, mySheet, sheetName)

# Urbanization
# dir1 = "data/spatial-multipliers/urbanization-high/"
# list1 = list.files(dir1, pattern = "*.tif$")
# tg1 = "Urbanization: High [Type]"
# 
# dir2 = "data/spatial-multipliers/urbanization-medium/"
# list2 = list.files(dir2, pattern = "*.tif$")
# tg2 = "Urbanization: Medium [Type]"
# 
# dir3 = "data/spatial-multipliers/urbanization-low/"
# list3 = list.files(dir3, pattern = "*.tif$")
# tg3 = "Urbanization: Low [Type]"
# 
# dir4 = "data/spatial-multipliers/urbanization-open/"
# list4 = list.files(dir4, pattern = "*.tif$")
# tg4 = "Urbanization: Open [Type]"
# 
# myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Urbanization]")
# mergeDependencies(myScenario) = T
# sheetName <- "stsim_TransitionSpatialMultiplier"
# mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg3, MultiplierFileName = paste0(dir3,list3)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg4, MultiplierFileName = paste0(dir4,list4)))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = "data/spatial-multipliers/projection-urbanization.tif"))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg2, MultiplierFileName = "data/spatial-multipliers/projection-urbanization.tif"))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg3, MultiplierFileName = "data/spatial-multipliers/projection-urbanization.tif"))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg4, MultiplierFileName = "data/spatial-multipliers/projection-urbanization.tif"))
# saveDatasheet(myScenario, mySheet, sheetName)

# Urbanization
dir1 = file.path(getwd(), "data", "spatial-multipliers", "urbanization")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Urbanization"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Urbanization]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = file.path(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-urbanization.tif")))
saveDatasheet(myScenario, mySheet, sheetName)




# Intensification
# dir1 = "data/spatial-multipliers/intensification-open-low/"
# list1 = list.files(dir1, pattern = "*.tif$")
# tg1 = "Intensification: Open to Low [Type]"
# 
# dir2 = "data/spatial-multipliers/intensification-open-medium/"
# list2 = list.files(dir2, pattern = "*.tif$")
# tg2 = "Intensification: Open to Medium [Type]"
# 
# dir3 = "data/spatial-multipliers/intensification-open-high/"
# list3 = list.files(dir3, pattern = "*.tif$")
# tg3 = "Intensification: Open to High [Type]"
# 
# dir4 = "data/spatial-multipliers/intensification-low-medium/"
# list4 = list.files(dir4, pattern = "*.tif$")
# tg4 = "Intensification: Low to Medium [Type]"
# 
# dir5 = "data/spatial-multipliers/intensification-low-high/"
# list5 = list.files(dir5, pattern = "*.tif$")
# tg5 = "Intensification: Low to High [Type]"
# 
# dir6 = "data/spatial-multipliers/intensification-medium-high/"
# list6 = list.files(dir6, pattern = "*.tif$")
# tg6 = "Intensification: Medium to High [Type]"
# 
# myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Intensification]")
# mergeDependencies(myScenario) = T
# sheetName <- "stsim_TransitionSpatialMultiplier"
# mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg3, MultiplierFileName = paste0(dir3,list3)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg4, MultiplierFileName = paste0(dir4,list4)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg5, MultiplierFileName = paste0(dir5,list5)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg6, MultiplierFileName = paste0(dir6,list6)))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = "data/spatial-multipliers/projection-all-cells-1.tif"))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg2, MultiplierFileName = "data/spatial-multipliers/projection-all-cells-1.tif"))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg3, MultiplierFileName = "data/spatial-multipliers/projection-all-cells-1.tif"))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg4, MultiplierFileName = "data/spatial-multipliers/projection-all-cells-1.tif"))
# saveDatasheet(myScenario, mySheet, sheetName)

dir1 = file.path(getwd(),"data", "spatial-multipliers", "intensification")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Intensification"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Intensification]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = file.path(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-all-cells-1.tif")))
saveDatasheet(myScenario, mySheet, sheetName)


# Ag Contraction
# dir1 = "data/spatial-multipliers/ag-contraction-cropland/"
# list1 = list.files(dir1, pattern = "*.tif$")
# tg1 = "Ag Contraction: Cropland"
# 
# dir2 = "data/spatial-multipliers/ag-contraction-pasture/"
# list2 = list.files(dir2, pattern = "*.tif$")
# tg2 = "Ag Contraction: Pasture"
# 
# myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Ag Contraction]")
# mergeDependencies(myScenario) = T
# sheetName <- "stsim_TransitionSpatialMultiplier"
# mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = "data/spatial-multipliers/projection-all-cells-1.tif"))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg2, MultiplierFileName = "data/spatial-multipliers/projection-all-cells-1.tif"))
# saveDatasheet(myScenario, mySheet, sheetName)

dir1 = file.path(getwd(), "data", "spatial-multipliers", "ag-contraction")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Ag Contraction"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Ag Contraction]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = file.path(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-all-cells-1.tif")))
saveDatasheet(myScenario, mySheet, sheetName)




# Ag Expansion
# dir1 = "data/spatial-multipliers/ag-expansion-cropland/"
# list1 = list.files(dir1, pattern = "*.tif$")
# tg1 = "Ag Expansion: Cropland [Type]"
# 
# dir2 = "data/spatial-multipliers/ag-expansion-pasture/"
# list2 = list.files(dir2, pattern = "*.tif$")
# tg2 = "Ag Expansion: Pasture [Type]"
# 
# myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Ag Expansion]")
# mergeDependencies(myScenario) = T
# sheetName <- "stsim_TransitionSpatialMultiplier"
# mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
# mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = "data/spatial-multipliers/projection-ag-expansion.tif"))
# mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg2, MultiplierFileName = "data/spatial-multipliers/projection-ag-expansion.tif"))
# saveDatasheet(myScenario, mySheet, sheetName)

dir1 = file.path(getwd(), "data", "spatial-multipliers", "ag-expansion")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Ag Expansion"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Ag Expansion]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = file.path(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = file.path(getwd(), "data", "spatial-multipliers", "projection-ag-expansion.tif")))
saveDatasheet(myScenario, mySheet, sheetName)




# Merge Spatial Multipliers

myScenario <- scenario(myProject, scenario = "Spatial Multipliers")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Spatial Multipliers [Fire]",
                         "Spatial Multipliers [Insect]", 
                         "Spatial Multipliers [Forest Harvest]",
                         "Spatial Multipliers [Urbanization]",
                         "Spatial Multipliers [Intensification]",
                         "Spatial Multipliers [Ag Contraction]",
                         "Spatial Multipliers [Ag Expansion]"))







# Transition Adjacency Multipliers #####

myScenario = scenario(myProject, scenario = "Adjacency Settings")
sheetName = "stsim_TransitionAdjacencySetting"
mySheet = datasheet(myScenario, sheetName, optional = T, empty = T)
mySheet = read.csv("data/adjacency/transition-adjacency-setting.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName = "stsim_TransitionAdjacencyMultiplier"
mySheet = datasheet(myScenario, sheetName, optional = T, empty = T)
mySheet = read.csv("data/adjacency/transition-adjacency-multipliers.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# SF Flow Pathways Diagram #####
# Base Flows
myScenario <- scenario(myProject, scenario = "Flow Pathways [Base Flows]")
mergeDependencies(myScenario) = T
sheetName <- "stsimsf_FlowPathwayDiagram"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/stock-flow-model/flow-pathway-diagram.csv")
saveDatasheet(myScenario, mySheet, sheetName)
myScenario <- scenario(myProject, scenario = "Flow Pathways [Base Flows]")
sheetName <- "stsimsf_FlowPathway"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/stock-flow-model/flow-pathways-spinup-base-flows.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Transition Flows
myScenario <- scenario(myProject, scenario = "Flow Pathways [Event Flows]")
mergeDependencies(myScenario) = T
sheetName <- "stsimsf_FlowPathwayDiagram"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/stock-flow-model/flow-pathway-diagram.csv")
saveDatasheet(myScenario, mySheet, sheetName)
myScenario <- scenario(myProject, scenario = "Flow Pathways [Event Flows]")
sheetName <- "stsimsf_FlowPathway"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/stock-flow-model/flow-pathways-lulc-other-non-spinup.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Flow Pathways")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Flow Pathways [Base Flows]", "Flow Pathways [Event Flows]"))

# SF Stock Group Membership #####
myScenario <- scenario(myProject, scenario = "SF Stock Group Membership")
sheetName <- "stsimsf_StockTypeGroupMembership"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/stock-flow-model/stock-type-group-membership.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# SF Flow Group Membership #####
myScenario <- scenario(myProject, scenario = "SF Flow Group Membership")
sheetName <- "stsimsf_FlowTypeGroupMembership"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/stock-flow-model/flow-type-group-membership.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# SF Flow Flow Order #####
myScenario <- scenario(myProject, scenario = "SF Flow Order")
sheetName <- "stsimsf_FlowOrder"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/stock-flow-model/flow-order.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "SF Flow Order")
sheetName <- "stsimsf_FlowOrderOptions"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"ApplyEquallyRankedSimultaneously"] = T
saveDatasheet(myScenario, mySheet, sheetName)

# SF Output Options #####
myScenario <- scenario(myProject, scenario = "SF Output Options")
sheetName <- "stsimsf_OutputOptions"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"SummaryOutputST"] = T
mySheet[1,"SummaryOutputFL"] = T
mySheet[1,"SummaryOutputSTTimesteps"] = 1
mySheet[1,"SummaryOutputFLTimesteps"] = 1
mySheet[1,"SpatialOutputST"] = T
mySheet[1,"SpatialOutputFL"] = F
mySheet[1,"SpatialOutputSTTimesteps"] = 1
mySheet[1,"SpatialOutputFLTimesteps"] = 1
saveDatasheet(myScenario, mySheet, sheetName)

# SF Flow Multipliers #####
myScenario <- scenario(myProject, scenario = "Flow Multipliers [Base]")
mergeDependencies(myScenario) = T
sheetName <- "stsimsf_FlowMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/stock-flow-model/flow-pathways-spinup-base-multipliers.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Flow Multipliers [Non Forest]")
mergeDependencies(myScenario) = T
sheetName <- "stsimsf_FlowMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/stock-flow-model/flow-pathways-non-forest-multipliers.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Flow Multipliers [Scalar]")
mergeDependencies(myScenario) = T
sheetName <- "stsimsf_FlowMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = 2002, FlowGroupID = "Net Growth: Total", Value = 0.01))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, FlowGroupID = "Q10 Fast Flows", Value = 0.01))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, FlowGroupID = "Q10 Slow Flows", Value = 0.01))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario = scenario(myProject, scenario = "SF Flow Multipliers")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Flow Multipliers [Base]", "Flow Multipliers [Non Forest]", "Flow Multipliers [Scalar]"))

# SF Spatial Flow Multipliers #####

# Growth spatial multiplier
indir = file.path(getwd(), "data", "flow-spatial-multipliers", "growth")
gcm = "historical"
rcp = ""

myScenario <- scenario(myProject, scenario = "SF Flow Spatial Multipliers [Growth]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
list = list.files(path = "data/flow-spatial-multipliers/growth/historical/", pattern = "*.tif")
mySheet = data.frame(Timestep = seq(1979,2020), FlowGroupID = "Net Growth: Total", MultiplierFileName = file.path(indir,gcm, paste0(rcp,list)))
saveDatasheet(myScenario, mySheet, sheetName)

# Q10 Fast Multiplier
indir = file.path(getwd(), "data", "flow-spatial-multipliers", "q10Fast")
gcm = "historical"
rcp = ""

myScenario <- scenario(myProject, scenario = "SF Flow Spatial Multipliers [Q10 Fast]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
list = list.files(path = "data/flow-spatial-multipliers/q10Fast/historical/", pattern = "*.tif")
mySheet = data.frame(Timestep = seq(1979,2020), FlowGroupID = "Q10 Fast Flows", MultiplierFileName = file.path(indir,gcm,paste0(rcp,list)))
saveDatasheet(myScenario, mySheet, sheetName)

# Q10 Slow Multiplier
indir = file.path(getwd(), "data", "flow-spatial-multipliers", "q10Slow")
gcm = "historical"
rcp = ""

myScenario <- scenario(myProject, scenario = "SF Flow Spatial Multipliers [Q10 Slow]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
list = list.files(path = "data/flow-spatial-multipliers/q10Slow/historical/", pattern = "*.tif")
mySheet = data.frame(Timestep = seq(1979,2020), FlowGroupID = "Q10 Slow Flows", MultiplierFileName = file.path(indir,gcm, paste0(rcp,list)))
saveDatasheet(myScenario, mySheet, sheetName)

# Merge Flow Spatial Multipliers
myScenario = scenario(myProject, scenario = "SF Flow Spatial Multipliers")
mergeDependencies(myScenario) = TRUE
dependency(myScenario, c("SF Flow Spatial Multipliers [Growth]", "SF Flow Spatial Multipliers [Q10 Fast]", "SF Flow Spatial Multipliers [Q10 Slow]"))


# SF Initial Stocks #####

myScenario = scenario(myProject, scenario = "SF Initial Stocks [Spatial; Imputed Age; Fire]")
sheetName <- "stsimsf_InitialStockSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Fine Root", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "Biomass Fine Root [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Coarse Root", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "Biomass Coarse Root [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Foliage", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "Biomass Foliage [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Merchantable", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "Biomass Merchantable [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Other Wood", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "Biomass Other Wood [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Very Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "DOM Aboveground Very Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Very Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "DOM Belowground Very Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "DOM Aboveground Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "DOM Belowground Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Medium", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "DOM Aboveground Medium [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Slow", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "DOM Aboveground Slow [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Slow", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "DOM Belowground Slow [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Branch", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "DOM Snag Branch [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Stem", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "fire", "DOM Snag Stem [Type].tif")))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario = scenario(myProject, scenario = "SF Initial Stocks [Spatial; Imputed Age; Harvest]")
sheetName <- "stsimsf_InitialStockSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Fine Root", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "Biomass Fine Root [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Coarse Root", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "Biomass Coarse Root [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Foliage", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "Biomass Foliage [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Merchantable", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "Biomass Merchantable [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Other Wood", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "Biomass Other Wood [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Very Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "DOM Aboveground Very Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Very Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "DOM Belowground Very Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "DOM Aboveground Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "DOM Belowground Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Medium", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "DOM Aboveground Medium [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Slow", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "DOM Aboveground Slow [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Slow", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "DOM Belowground Slow [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Branch", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "DOM Snag Branch [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Stem", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "harvest", "DOM Snag Stem [Type].tif")))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario = scenario(myProject, scenario = "SF Initial Stocks [Spatial; Imputed Age; Merged]")
sheetName <- "stsimsf_InitialStockSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Fine Root", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "Biomass Fine Root [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Coarse Root", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "Biomass Coarse Root [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Foliage", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "Biomass Foliage [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Merchantable", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "Biomass Merchantable [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Other Wood", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "Biomass Other Wood [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Very Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "DOM Aboveground Very Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Very Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "DOM Belowground Very Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "DOM Aboveground Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Fast", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "DOM Belowground Fast [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Medium", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "DOM Aboveground Medium [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Slow", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "DOM Aboveground Slow [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Slow", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "DOM Belowground Slow [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Branch", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "DOM Snag Branch [Type].tif")))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Stem", RasterFileName = file.path(getwd(), "data", "initial-stocks", "imputed", "merged", "DOM Snag Stem [Type].tif")))
saveDatasheet(myScenario, mySheet, sheetName)

# Spatial Multiprocessing #####
myScenario = scenario(myProject, scenario = "Spatial Multiprocessing")
sheetName = "corestime_Multiprocessing"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"MaskFileName"] = file.path(getwd(), "data", "spatial-multiprocessing", "multi-processing-regions-16.tif")
saveDatasheet(myScenario, mySheet, sheetName)

datasheet(myScenario)




#############################
# Full scenarios (STSim SF) #
#############################

# STSM Model Only #####
myScenario = scenario(myProject, scenario = "Conus Historical [STSM Only, 2001-2020, 1 MC]")
mergeDependencies(myScenario) = T
dependency(myScenario, 
           c("Run Control [Spatial; 2001-2020; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Spatial; Imputed Age]",
             "Output Options [Spatial]",
             "State Attributes",
             "Distributions",
             "External Variables",
             "Transition Targets [Reference]",
             "Transition Multipliers",
             "Transition Size Distribution [Fire]",
             "Transition Slope Multipliers [Fire]",
             "Spatial Multipliers",
             "Adjacency Settings",
             "Spatial Multiprocessing"))

# Full Model with Fire as Last Disturbance #####
myScenario = scenario(myProject, scenario = "Conus Historical [Full Model; Fire Last Disturbance; Imputed Age; 2001-2020; 1 MC]")
mergeDependencies(myScenario) = T
dependency(myScenario, 
           c("Run Control [Spatial; 2001-2020; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Spatial; Imputed Age]",
             "Output Options [Spatial]",
             "State Attributes",
             "Distributions",
             "External Variables",
             "Transition Targets [Reference]",
             "Transition Multipliers",
             "Transition Size Distribution [Fire]",
             "Transition Slope Multipliers [Fire]",
             "Spatial Multipliers",
             "Adjacency Settings",
             "Flow Pathways",
             "SF Stock Group Membership",
             "SF Flow Group Membership",
             "SF Flow Order",
             "SF Output Options",
             "SF Flow Multipliers",
             "SF Flow Spatial Multipliers",
             "SF Initial Stocks [Spatial; Imputed Age; Fire]",
             "Spatial Multiprocessing"))

# Full Model with Harvest as Last Disturbance #####
myScenario = scenario(myProject, scenario = "Conus Historical [Full Model; Harvest Last Disturbance; Imputed Age; 2001-2020; 1 MC]")
mergeDependencies(myScenario) = T
dependency(myScenario, 
           c("Run Control [Spatial; 2001-2020; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Spatial; Imputed Age]",
             "Output Options [Spatial]",
             "State Attributes",
             "Distributions",
             "External Variables",
             "Transition Targets [Reference]",
             "Transition Multipliers",
             "Transition Size Distribution [Fire]",
             "Transition Slope Multipliers [Fire]",
             "Spatial Multipliers",
             "Adjacency Settings",
             "Flow Pathways",
             "SF Stock Group Membership",
             "SF Flow Group Membership",
             "SF Flow Order",
             "SF Output Options",
             "SF Flow Multipliers",
             "SF Flow Spatial Multipliers",
             "SF Initial Stocks [Spatial; Imputed Age; Harvest]",
             "Spatial Multiprocessing"))

# Full Model with Merged Initial Carbon Stocks #####
myScenario = scenario(myProject, scenario = "Conus Historical [Full Model; Merged Disturbance; Imputed Age; 2001-2020; 1 MC]")
mergeDependencies(myScenario) = T
dependency(myScenario, 
           c("Run Control [Spatial; 2001-2020; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Spatial; Imputed Age]",
             "Output Options [Spatial]",
             "State Attributes",
             "Distributions",
             "External Variables",
             "Transition Targets [Reference]",
             "Transition Multipliers",
             "Transition Size Distribution [Fire]",
             "Transition Slope Multipliers [Fire]",
             "Spatial Multipliers",
             "Adjacency Settings",
             "Flow Pathways",
             "SF Stock Group Membership",
             "SF Flow Group Membership",
             "SF Flow Order",
             "SF Output Options",
             "SF Flow Multipliers",
             "SF Flow Spatial Multipliers",
             "SF Initial Stocks [Spatial; Imputed Age; Merged]",
             "Spatial Multiprocessing"))

##### Full Model with Merged Initial Carbon Stocks - No LULC Change #####

myScenario <- scenario(myProject, scenario <- "Pathway Diagram [No LULC Change]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("data/transition-pathways/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario = scenario(myProject, scenario = "Conus Historical [No LULC; Merged Disturbance; Imputed Age; 2001-2020; 1 MC]")
mergeDependencies(myScenario) = T
dependency(myScenario, 
           c("Run Control [Spatial; 2001-2020; 1 MC]",
             "Pathway Diagram [No LULC Change]",
             "Initial Conditions [Spatial; Imputed Age]",
             "Output Options [Spatial]",
             "State Attributes",
             "Distributions",
             "External Variables",
             "Transition Targets [Reference]",
             "Transition Multipliers",
             "Transition Size Distribution [Fire]",
             "Transition Slope Multipliers [Fire]",
             "Spatial Multipliers",
             "Adjacency Settings",
             "Flow Pathways",
             "SF Stock Group Membership",
             "SF Flow Group Membership",
             "SF Flow Order",
             "SF Output Options",
             "SF Flow Multipliers",
             "SF Flow Spatial Multipliers",
             "SF Initial Stocks [Spatial; Imputed Age; Merged]",
             "Spatial Multiprocessing"))

##### Full Model with Merged Initial Carbon Stocks - No Climate Change #####

myScenario = scenario(myProject, scenario = "Conus Historical [No Climate; Merged Disturbance; Imputed Age; 2001-2020; 1 MC]")
mergeDependencies(myScenario) = T
dependency(myScenario, 
           c("Run Control [Spatial; 2001-2020; 1 MC]",
             "Pathway Diagram [No LULC Change]",
             "Initial Conditions [Spatial; Imputed Age]",
             "Output Options [Spatial]",
             "State Attributes",
             "Distributions",
             "External Variables",
             "Transition Targets [Reference]",
             "Transition Multipliers",
             "Transition Size Distribution [Fire]",
             "Transition Slope Multipliers [Fire]",
             "Spatial Multipliers",
             "Adjacency Settings",
             "Flow Pathways",
             "SF Stock Group Membership",
             "SF Flow Group Membership",
             "SF Flow Order",
             "SF Output Options",
             "SF Initial Stocks [Spatial; Imputed Age; Merged]",
             "Spatial Multiprocessing"))



datasheet(myScenario)
