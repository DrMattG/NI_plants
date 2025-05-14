#library(NIplants)
library(NIcalc)
library(magrittr)
library(rgbif)
library(rgdal)
library(sp)
library(raster)
library(rio)
library(mgcv)


#devtools::install_github("DrMattG/NI_plants")

# Source all functions in "R" folder
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir('R')


# Run locally
#analysis_path <- "P:/41201612_naturindeks_2021_2023_database_og_innsynslosning/Karplanter_Dataflyt/NI2025_plants"
#data_path <- "P:/41201612_naturindeks_2021_2023_database_og_innsynslosning/Karplanter_Dataflyt/GBIF_Data" # Local

# Run from server
analysis_path <- getwd()
data_path <- "/data/P-Prosjekter/41201612_naturindeks_2021_2023_database_og_innsynslosning/Karplanter_Dataflyt/GBIF_Data"


if(!dir.exists(paste0(analysis_path, "/Data"))){
  dir.create(paste0(analysis_path, "/Data"))
}
if(!dir.exists(paste0(analysis_path, "/Results"))){
  dir.create(paste0(analysis_path, "/Results"))
}


indicators <- c("Alm","Fjellvalmue","Greplyng","Issoleie","Kusymre","Myrtelg","Olavsstake","Prestekrage",
                "Purpurlyng","Sennegras","Solblom","Sveltstarr","Engmarihand","Hvitmyrak","Brunmyrak","Smalsoldogg","Dikesoldogg")

species <- c("Ulmus glabra","Papaver radicatum","Kalmia procumbens","Ranunculus glacialis","Primula vulgaris",
             "Thelypteris palustris","Moneses uniflora","Leucanthemum vulgare","Erica cinerea","Carex vesicaria",
             "Arnica montana","Carex pauciflora","Dactylorhiza incarnata","Rhynchospora alba","Rhynchospora fusca",
             "Drosera anglica","Drosera intermedia")


# Step 1: Downloading old NI data
downloadOldNIData <- TRUE
#downloadOldNIData <- FALSE

# Step 2: Downloading raw occurrence data from GBIF
downloadGBIFData <- TRUE
#downloadGBIFData <- FALSE

# Step 3: Building training datasets for model fitting
buildTrainingData <- TRUE
#buildTrainingData <- FALSE

# Step 4: Fitting GAMs
fitGAM <- TRUE
#fitGAM <- FALSE

# Step 5: Making GAM predictions
predictGAM <- TRUE
#predictGAM <- FALSE

# Step 6: Calculating indicator values
calcIndicators <- TRUE
#calcIndicators <- FALSE

# Step 7: Formatting indicator values
formatIndicators <- TRUE
#formatIndicators <- FALSE

# Step 8: Comparing new indicator values to old and/or upload to NI database
processIndicators <- TRUE
#processIndicators <- FALSE


if(downloadOldNIData){

  # Execute function for downloading old indicator data
  oldIndicatorData <- downloadData_NIdb(species = species, indicators = indicators,
                                        save = TRUE, save_path = paste0(analysis_path, "/Data"))

}else{

  # Load old indicator data from local save
  oldIndicatorData <- readRDS(paste0(analysis_path, '/Data/oldIndicatorData.rds'))
}


# Execute function for setting up download key and requesting download
if(downloadGBIFData){
 download_key <- getKey_GBIF()
 message("Download key retrieved.")
 message("Wait until your GBIF download is ready (typically within 15 minutes, but can take up to 3 hours).")
 message("You should receive an email confirmation once the download is ready.")
}


if(downloadGBIFData){

  # Execute function to download, unzip, and import raw data (using generated key)
  sp <- downloadData_GBIF(key = download_key$key, path = data_path)

  # Move file from working directory into GBIF data folder
  fs::file_move("occurrence.txt", paste0(data_path, "/occurrence.txt"))

}

# Import raw data from previously downloaded occurrence file
GBIF_data <- import(paste0(data_path, "/occurrence.txt"), header = T, sep = "\t")


# Set start, end, and year interval for time period of interest
min.year <- 1820
max.year <- 2022
year.interval <- 5

# Execute function for building training data
if(buildTrainingData){
  build_TrainingData(GBIF_data, species, min.year, max.year, year.interval,
                     save_path = paste0(analysis_path, "/Data/"), save.unfiltered = TRUE)
}


# Execute function to run GAM analyses
if(fitGAM){
  gam.results <- run_GAM(species = species, data_path = paste0(analysis_path, "/Data"),
                         save = TRUE, save_path = paste0(analysis_path, "/Results"))
}


# Make a list of years for which to make predictions
yearList <- c(1900, 1950, 1960, 1970, 1980, 1990, 2000, 2005, 2010, 2014, 2019, 2020, 2021, 2022)

# Execute function to predict from GAMs and plot predictions (optional)
if(predictGAM){
  NIGAM_All.list <- predict_GAM(species = species, year = yearList, plot.pdf = TRUE,
                                save = TRUE, wd_path = analysis_path)
}


# Execute function to calculate indicator values
if(calcIndicators){
  newIndicatorData <- calculate_IndicatorValues(species = species, year = yearList,
                                                save = TRUE, wd_path = analysis_path)
}


# Execute function to create and format distribution object data
if(formatIndicators){
  updatedIndicatorData <- create_DistObjects(species = species, save = TRUE, wd_path = analysis_path)
}


# Execute function to compare updated indicator data to previous
if(processIndicators){
  uploadData_NIdb(species = species, speciesList = species, mode = "compare", newdata_path = paste0(analysis_path, "/Data"))
}


# Execute function to write updated indicator data to NI database
if(processIndicators){
  uploadData_NIdb(species = species, speciesList = species, mode = "overwrite", newdata_path = paste0(analysis_path, "/Data"))
}

