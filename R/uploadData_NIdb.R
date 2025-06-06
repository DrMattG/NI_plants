#' Write updated indicator data to NI database
#'
#' This function operates differently depending on the specified `mode` (see
#' Arguments).
#' 
#' In "compare" mode, the function downloads the current indicator data from the NI 
#' database and compares the newly assembled indicator with it. The comparison 
#' is displayed as proportional change in the new data relative to the data in 
#' the data base. 
#' In "overwrite" mode, the function uploads the newly assembled indicator data
#' to the NI database. Provided the user has writing access, this will overwrite
#' the data that is currently in the database and should ONLY be done when the 
#' database needs to be updated, and after the newly assembled indicator data
#' has been compared and quality-checked. 
#' 
#' When attempting to write to the NI database, the used will be prompted to 
#' confirm the action prior to its execution. 
#' 
#' Irrespective of the `mode`, executing this function requires accessing the NI
#' database through a token generated by `getToken`, and users are prompted to 
#' enter their NI database log-in credentials for that purpose. NI database 
#' credentials consist of a registered email address and a password. 
#' 
#' 
#' NOTE: As per now, unitOfMeasurement needs to be updated manually 
#' (new units = "Funnsannsynlighet i 1x1km-ruter")
#'
#' @param species character string or vector of character strings containing
#' the latin name of the species for which to upload/compare data
#' @param speciesList vector of character string containing latin names of all 
#' species in the set of indicators
#' @param mode switch for running different versions of the function. If 
#' `mode = "compare"` updated indicator data is compared to indicator data 
#' currently stored in the NI database. If `mode = "overwrite"`, updated 
#' indicator data is uploaded to the NI database. 
#' @param newdata_path Character string specifying the directory in which the 
#' newly calculated indicator data (output of `NI_plants::create_DistObjects()`)
#' is stored. 
#' 
#' @return
#' @export
#'
#' @examples
uploadData_NIdb <- function(species, speciesList, mode, newdata_path){
  
  if(!(mode%in%c("overwrite", "compare"))){
    stop("Incorrect mode specification. See documentation for supported modes.")
  }
  
  ## Load saved data if not present
  
  # Updated indicator data
  if(!exists("updatedIndicatorData")){
    updatedIndicatorData <- readRDS(paste0(newdata_path, "/updatedIndicatorData.rds"))
    message('Updated indicator data loaded from file.')
  }
  
  ## Provide user credentials for NI database and request token
  myUserName_NIdb <- rstudioapi::askForPassword("NI database username") # = NINA email address
  myPassword_NIdb <- rstudioapi::askForPassword("NI database password")
  
  NIcalc::getToken(username = myUserName_NIdb,  
                   password = myPassword_NIdb,
                   url = "https://www8.nina.no/NaturindeksNiCalc"
                   )
  
  ## Write updated indicator data into NI database
  if(mode == "overwrite"){
    
    # Ask the user for confirmation to write to database
    command <- askYesNo("Do you want to write to the database?", default = FALSE)
    
    # Write to database if confirmed (halt execution otherwise)
    if(!is.na(command) & command){
      
      message("Uploading new indicator data to NI database:")
      
      for(j in 1:length(species)){
        message(species[j])
        #NIcalc::writeIndicatorValues(updatedIndicatorData[[j]])
        writeIndicatorValues(updatedIndicatorData[[j]])
        
      }  
    }else{
      message("Function halted.")
    }
  }
  
  ## Compare updated indicator data to data currently stored in NI database
  if(mode == "compare"){
    
    # Specify indicators for which to compare data
    myIndicators <- NIcalc::getIndicators() %>%
      dplyr::right_join(data.frame(name = indicators, species = speciesList), by = 'name')
    
    # Compare values for each species
    message("Comparing updated to previous indicator data.")
    
    for(j in 1:length(species)){
      message("")
      message(paste0("Change in indicator values for ", species[j], ":"))
      
      d1 <- updatedIndicatorData[[j]]$indicatorValues %>%
        dplyr::select(areaName, yearName, verdi) %>%
        dplyr::rename(verdi_new = verdi)
      
      indicatorData <- NIcalc::getIndicatorValues(indicatorID = myIndicators$id[myIndicators$species==species[j]]) 
      d2 <- indicatorData$indicatorValues %>%
        dplyr::select(areaName, yearName, verdi) %>%
        dplyr::rename(verdi_old = verdi)
      
      check_all <- dplyr::inner_join(d1, d2, by = c("areaName", "yearName")) %>%
        dplyr::filter(!is.na(verdi_old) & !is.na(verdi_new)) %>%
        dplyr::mutate(propDiff = verdi_old/verdi_new)

      message("Proportional difference in values:")
      print(summary(check_all$propDiff))
      message(paste0("Correlation coefficient: ", round(cor(check_all$verdi_new, check_all$verdi_old), digits = 3)))
      
    }
  }
}
