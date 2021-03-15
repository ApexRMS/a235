library(rsyncrosim)
setwd("C:/")

RPath <- "C:/Program Files/R/R-3.6.3/bin/Rscript.exe"
RStudioPath <- "C:/Program Files/RStudio/bin/rstudio.exe"

# Syncrosim session
SyncroSimDir <- "C:/Users/bronw/Documents/Apex/SyncroSim/2-2-27"
mySession <- session(SyncroSimDir)

# stconnect library
myLibrary = ssimLibrary(name = file.path(ssimDir,"test.ssim"), session = mySession, package = "stconnect")

# Run model in developer mode (runs in RStudio)
# Default is to run in R
runInRStudio <- F

# This should work whether or not it is a new library or an existing library
sheetName <- "core_RConfig"
mySheet <- datasheet(myLibrary, name=sheetName)
if(runInRStudio == TRUE) mySheet$ExePath[1] <- RStudioPath else mySheet$ExePath[1] <- RPath
mySheet$RunInWindow[1] <- FALSE
saveDatasheet(myLibrary, mySheet, sheetName)


# Work around for now -  Add if statement to check if empty (nrow)
sheetName <- "core_RConfig"
mySheet <- datasheet(myLibrary, name=sheetName, empty=TRUE)
if(nrow(mySheet)>0){
  if(runInRStudio){ 
    mySheet$ExePath[1] <- RStudioPath
  } else{
    mySheet$ExePath[1] <- RPath
  }
  mySheet$RunInWindow[1] <- FALSE
} else if(nrow(mySheet)==0){
  if(runInRStudio) {
    mySheet <- addRow(mySheet, c(ExePath=RStudioPath, RunInWindow=FALSE))
  } else {
    mySheet <- addRow(mySheet, c(ExePath=RPath, RunInWindow=FALSE))
  }
}

saveDatasheet(myLibrary, mySheet, sheetName)