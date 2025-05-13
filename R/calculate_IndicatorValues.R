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
#' @param referenceYear integer. Year to use as reference.
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
calculate_IndicatorValues <- function(species, year, 
                                      referenceYear,
                                      save = FALSE, wd_path){
  
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
  
  ## Set reference year
  referenceYear <- 1900
  
  ## Calculate indicator values for each species and year
  message("Calculating indicator values for:")
  
  for(j in 1:length(species))
  {
    message(species[j])
    
    # Make data objects of old and new indicator sets for further manipulation
    old <- oldIndicatorData[[j]]$indicatorValues
    
    ind_areas <- old %>%
      dplyr::distinct(indicatorId, indicatorName, 
                      areaId, areaName)
    new_ref <- ind_areas %>%
      dplyr::mutate(yearId = 0,
                    yearName = "Referanseverdi")
    
    
    # Set up new data for reference values
    newref.poly <- NIGAM_All.list[[j]][[which(year == referenceYear)]]$p
    newref.se.poly <- NIGAM_All.list[[j]][[which(year == referenceYear)]]$p.se
    
    new_ref <- new_ref %>%
      dplyr::left_join(newref.poly@data[,c("NAVN", "Norway")], by = dplyr::join_by("areaName" == "NAVN")) %>%
      dplyr::rename("verdi" = "Norway") %>%
      dplyr::left_join(newref.se.poly@data[,c("NAVN", "Norway")], by = dplyr::join_by("areaName" == "NAVN")) %>%
      dplyr::rename("verdiSE" = "Norway") %>%
      dplyr::mutate(ref = verdi)
    
    # Set up new data for yearly data
    year_noRef <- year[which(year != referenceYear)]
    new_yrs <- data.frame()
    
    for(i in 1:length(year_noRef)){
      
      yr.poly <- NIGAM_All.list[[j]][[which(year == year_noRef[i])]]$p
      yr.se.poly <- NIGAM_All.list[[j]][[which(year == year_noRef[i])]]$p.se
      
      data_add <- ind_areas %>%
        dplyr::mutate(yearId = i,
                      yearName = year_noRef[i]) %>%
        dplyr::left_join(yr.poly@data[,c("NAVN", "Norway")], by = dplyr::join_by("areaName" == "NAVN")) %>%
        dplyr::rename("verdi" = "Norway") %>%
        dplyr::left_join(yr.se.poly@data[,c("NAVN", "Norway")], by = dplyr::join_by("areaName" == "NAVN")) %>%
        dplyr::rename("verdiSE" = "Norway") %>%
        dplyr::left_join(new_ref[,c("areaName", "ref")], by = "areaName")
        
      new_yrs <- rbind(new_yrs, data_add)
    }
    
    # Combine new reference and yearly data
    new <- rbind(new_ref, new_yrs) %>%
      dplyr::mutate(datatypeId = 3, datatypeName = "Beregnet fra modeller",
                    unitOfMeasurement = "Prosent av 10x10km-ruter med forekomst",
                    customDistributionUUID = NA,
                    distributionName = NA, distributionId = NA,
                    distParam1 = NA, distParam2 = NA)
    
    # Fill new value into indicator data list
    newIndicatorData[[j]]$indicatorValues <- new
  }
  
  ## Save new indicator data (optional)
  if(save){
    saveRDS(newIndicatorData, file = paste0(wd_path, "/Data/newIndicatorData.rds"))
  }
  
  ## Return new indicator data
  return(newIndicatorData)
}
