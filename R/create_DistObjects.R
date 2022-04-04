#' Create distribution objects and format data for import to NI base
#'
#' This function creates distribution objects that quantify uncertainty in 
#' indicator calculations and prepares data for upload to the NI database. 
#' 
#' @param species character string or vector of character strings containing
#' the latin name of the species for which to prepare data
#' @param save logical, default = `FALSE`. If `TRUE`, indicator data will be
#' saved in the working directory as `updatedIndicatorData.RData`.
#'#'@param wd_path Character string specifying the working directory containing 
#' the "Data" and "Results" subfolders. This ensures that all data is read from
#' and stored in the correct locations. 
#' 
#' @return A list containing scaled indicator values and associated information,
#' including specifications for custom distributions specifying uncertainty, for 
#' each municipality per `species` and `year`.
#' @export
#'
#' @examples
create_DistObjects <- function(species, save = FALSE, wd_path){
  
  ## Load saved data if not present
  
  # Old indicator data
  if(!exists("oldIndicatorData")){
    oldIndicatorData <- readRDS(paste0(wd_path, "/Data/oldIndicatorData.rds"))
    message('Old indicator data loaded from file.')
  }
  
  
  # New indicator data
  if(!exists("newIndicatorData")){
    newIndicatorData <- readRDS(paste0(wd_path, "/Data/oldIndicatorData.rds"))
    message('New indicator data loaded from file.')
  }
  
  ## Set up structure for updated indicator data
  updatedIndicatorData <- oldIndicatorData
  
  message("Assembling updated NI data for:")
  
  ## Create distribution objects for each species
  for(j in 1:length(species)){
    
    message(species[j])
    
    d <- newIndicatorData[[j]]$indicatorValues
    myData <- data.frame(estimatedStates = d$verdi,
                         standardErrors = d$verdiSE)
    logNormalParams <- NIcalc::normal2Lognormal(mean = myData$estimatedStates, 
                                                sd = myData$standardErrors)
    myData$muLogNormal <- logNormalParams$mean 
    myData$sigmaLogNormal <- logNormalParams$sd
    
    ddd <- NULL
    for (i in 1:dim(myData)[[1]])
    {
      if(any(is.na(myData[i,c("muLogNormal","sigmaLogNormal")]))) {ddd[i] <- NA; next}
      ddd[i] <- list(NIcalc::makeDistribution(
        input = "logNormal",
        distParams = list(mean = myData$muLogNormal[i],
                          sd = myData$sigmaLogNormal[i]))) 
    }
    myData$distrObjects <- ddd
    myData$areaIDs <- d$areaId
    myData$years <- d$yearName
    myData$Datatype <- d$datatypeId
    
    rowsWithNAs <- which(is.na(myData$estimatedStates))
    rowsWithoutNAs <- which(!is.na(myData$estimatedStates))
    if(any(rowsWithNAs)){
      
      for(i in rowsWithNAs){
        updatedIndicatorData[[j]] <- NIcalc::setIndicatorValues(updatedIndicatorData[[j]],
                                                                areaId = myData$areaIDs[i],
                                                                years = myData$years[i],
                                                                est = myData$estimatedStates[i],
                                                                lower = myData$estimatedStates[i]-myData$standardErrors[i],
                                                                upper = myData$estimatedStates[i]+myData$standardErrors[i])
      }
    }
    for(i in rowsWithoutNAs){

      updatedIndicatorData[[j]] <- NIcalc::setIndicatorValues(updatedIndicatorData[[j]], 
                                                              areaId = myData$areaIDs[i], 
                                                              years = myData$years[i], 
                                                              distribution = myData$distrObjects[[i]],
                                                              datatype = myData$Datatype[i])
    } 
  }
  
  ## Save updated indicator data (optional)
  if(save){
    saveRDS(updatedIndicatorData, file = paste0(wd_path, "/Data/updatedIndicatorData.rds"))
  }
  
  ## Return updated indicator data
  return(updatedIndicatorData)
}
