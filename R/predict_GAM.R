#' Make spatio-temporal occurrence predictions from GAM fits
#'
#' This function uses the GAM fits to make occurrence predictions per 
#' municipality for each species-year combination. 
#' 
#' @param species character string or vector of character strings containing
#' the latin name of the species for which to make predictions
#' @param year vector of years for which to make predictions
#' @param plot.pdf logical, default = `TRUE`. Whether or not to generate and
#' save pdf plots of predictions
#' @param save logical, default = `FALSE`. If `TRUE`, model fits for all 
#' species are saved into the directory specified via `save_path` as 
#' `NIGAM_All.list.rds`.
#' @param wd_path Character string specifying the working directory containing 
#' the "Data" and "Results" subfolders. This ensures that raster data are loaded
#' from the "Data" folder, and that GAM fits are loaded from, and predictions 
#' and plots saved into, the "Results" folder. 
#'
#' @return A list containing for each `species` and `year` the mean and standard
#' deviation of predicted occurrence at the municipality level (stored as class
#'  `SpatialPolygonsDataFrame`).
#' @export
#'
#' @examples
#' 
predict_GAM <- function(species, year, plot.pdf = TRUE, save = FALSE, wd_path){
  
  ## Load polygon for Norwegian counties
  kommune.poly <- readOGR(dsn = paste0(wd_path, "/Data/Shapefiles"), layer = "Norway municipalities", use_iconv = TRUE, encoding = "UTF-8")
  
  ## Load and set up raster for Norway
  norway <- raster(paste0(wd_path, "/Data/Raster/Norway.tif")) 
  xy <- coordinates(norway)
  x <- y <- norway
  values(x) <- xy[,1]
  values(y) <- xy[,2]
  
  ## Load GAM results if not already in workspace
  if(!exists("gam.results")){
    gam.results <- readRDS(wd_path, "/Results/gam.results.all.gamma3.rds")
  }
  
  ## Set up empty list to store predictions
  NIGAM_All.list <- list()
  
  ## Make - and optionally plot - spatio-temporal GAM predictions
  for(j in 1:length(species)){
    message("")
    message(paste0("Making predictions for ", species[j], ":"))
    NIGAM_All.list[[j]] <- list()
    
    if(plot.pdf){plotList <- list()}
        
    for(i in 1:length(year)){
      message(paste0("Year = ", year[i]))
      pred.ras <- stack(year[i]*norway, x, y, log(norway))
      names(pred.ras)[1:4] <- c("year", "x", "y", "logS")
      pred.dat <- as.data.frame(values(pred.ras))
      p <- norway
      pred <- predict(gam.results[[j]], pred.dat, se.fit = TRUE, type = "response") # The ordinary raster prediction does not always work: p <- predict(pred.ras, gam.results[[j]], type = "response")
      values(p) <- as.vector(pred$fit)
      p.poly <- raster::extract(p, kommune.poly, sp = TRUE, fun = mean, na.rm = T, weights = TRUE, normalizeWeights = TRUE)
      b <- seq(0, 0.1, by = 0.001)
      n <- length(b)
      
      #if(plot.pdf){
      #  plot(p.poly, col = rev(terrain.colors(n-1))[raster::cut(p.poly$Norway, breaks = b)], main = paste(species[j], year[i]))
      #  plotList[[i]] <- recordPlot()
      #  graphics.off()
      #}
      
      values(p) <- as.vector(pred$se.fit)
      p.poly.se <- raster::extract(p, kommune.poly, sp = TRUE, fun = mean, na.rm = T, weights = TRUE, normalizeWeights = TRUE)
      NIGAM_All.list[[j]][[i]] <- list(p = p.poly, p.se = p.poly.se)
    }
    
    if(plot.pdf){
      
      maxPred <- rep(NA, length(year))
      
      for(i in 1:length(year)){
        maxPred[i] <- max(NIGAM_All.list[[j]][[i]]$p$Norway)
      }
      
      plotLim <- ifelse(round(max(maxPred), digits = 1) < max(maxPred), 
                        round(max(maxPred), digits = 1) + 0.1, round(max(maxPred), digits = 1))
      
      message(paste0("Assembling pdf plot for ", species[j], "."))
      pdf(paste0(wd_path, "/Results/GAMplotsMunic_", species[j], ".pdf"))
      
      for(i in 1:length(year)){
        p.poly <- NIGAM_All.list[[j]][[i]]$p
        
        print(
          tmap::tm_shape(p.poly) + 
            tmap::tm_polygons("Norway", 
                              title = paste0("Prediction (year ", year[i], ")"),
                              n = 10,
                              breaks = seq(0, plotLim, length.out = 10),
                              #style = "cont",
                              palette = rev("BuGn"), 
                              colorNA = "grey80") +
            tm_layout(legend.outside = TRUE)
        )
      }
      dev.off()
    }
  }
  
  ## Rename list objects
  names(NIGAM_All.list) <- species
  
  ## Save predictions (optional)
  if(save){
    saveRDS(NIGAM_All.list, file = paste0(wd_path, "/Results/NIGAM_All.list.rds"))
  }
  
  ## Return results
  return(NIGAM_All.list)
}

