#' Calculate and assemble new NI indicator values from GAM predictions
#'
#' This function calculates scaled indicator values for each species- and year-
#' combination at the municipality level. 
#' The reference value used for scaling indicator values is extracted from
#' previously downloaded old indicator data (as stored in NI database) via the
#' label `Referanseverdi`.
#' 
#' @param species character string or vector of character strings containing
#' the latin name of the species for which to calculate indicator values
#' @param year vector of years for which to calculate indicator values
#' @param save logical, default = `FALSE`. If `TRUE`, new indicator data will be
#' saved in the working directory as `newIndicatorData.RData`.
#'@param wd_path Character string specifying the working directory containing 
#' the "Data" and "Results" subfolders. This ensures that all data is read from
#' and stored in the correct locations. 
#' 
#' @return A list containing scaled indicator values and associated information
#'  for each municipality per `species` and `year`.
#' @export
#'
#' @examples
calculate_IndicatorValues <- function(species, year, save = FALSE, wd_path){
  
  ## Load saved data if not present
  
  # Old indicator data
  if(!exists("oldIndicatorData")){
    oldIndicatorData <- readRDS(paste0(wd_path, "/Data/oldIndicatorData.rds"))
    message('Old indicator data loaded from file.')
  }
  
  # GAM predictions
  if(!exists("NIGAM_All.list")){
    NIGAM_All.list <- readRDS(paste0(wd_path, "/Results/NIGAM_All.list.rds"))
    message('GAM prediction data loaded from file.')
  }
  
  ## Set up structure for new indicator data from old
  newIndicatorData <- oldIndicatorData
  
  ## Calculate indicator values for each species and year
  message("Calculating indicator values for:")
  
  for(j in 1:length(species))
  {
    message(species[j])
    
    # Make data objects of old and new indicator sets for further manipulation
    old <- oldIndicatorData[[j]]$indicatorValues
    new <- newIndicatorData[[j]]$indicatorValues
    
    # Set reference value
    selected.year <- old$yearName=="Referanseverdi"
    oldref <- old[selected.year,]
    old$ref <- oldref$verdi[match(old$areaName,oldref$areaName)]
    
    # Set up polygon of new GAM predictions and match names to previous
    newref.poly <- NIGAM_All.list[[j]][[1]]$p
    newref.se.poly <- NIGAM_All.list[[j]][[1]]$p.se
    r <- match(newref.poly$NAVN, oldref$areaName)
    
    # Set up reference values
    new$verdi <- NA
    new$verdi[selected.year][r[!is.na(r)]] <- newref.poly$Norway[!is.na(r)]
    new$verdiSE <- NA
    new$verdiSE[selected.year][r[!is.na(r)]] <- newref.se.poly$Norway[!is.na(r)]
    newref <- new[new$yearName=="Referanseverdi",]
    new$ref <- newref$verdi[match(new$areaName,newref$areaName)]
    
    # Calculate scaled indicator value for each year
    for(i in 2:(length(year)-1)){
      selected.year <- old$yearName == as.character(year[i])
      oldval <- old[selected.year,]
      newval <- NIGAM_All.list[[j]][[i]]$p
      newval.se <- NIGAM_All.list[[j]][[i]]$p.se
      o <- match(newval$NAVN, oldval$areaName)
      new$verdi[selected.year][o[!is.na(o)]] <- newval$Norway[!is.na(o)]
      new$verdiSE[selected.year][o[!is.na(o)]] <- newval.se$Norway[!is.na(o)]
    }
    
    # Remove new predictions outside definition area (indicator value for definition area (1) or not (NA) )
    def <- old$ref/old$ref  
    new$ref <- new$ref*def
    new$verdi <- new$verdi*def
    new$verdiSE <- new$verdiSE*def
    
    # Fill new value into indicator data list
    newIndicatorData[[j]]$indicatorValues <- new
  }
  
  ## Save new indicator data (optional)
  if(save){
    saveRDS(newIndicatorData, file = paste0(wd_path, "/Data/newIndicatorData.rds")
  }
  
  ## Return new indicator data
  return(newIndicatorData)
}
