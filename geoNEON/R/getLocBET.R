##############################################################################################
#' @title Calculate more precise geolocations for ground beetles (DP1.10020.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON beetle data.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2025-02-13)
##############################################################################################
getLocBET <- function(
    data,
    token=NA_character_
){
  
  data$rowid <- 1:nrow(data)
  
  # concatenate the named location with the trapID
  traps <- paste(data$namedLocation, data$trapID, sep=".")
  data <- cbind(data, traps)
  
  # Use the getLocByName function to pull the subplot geolocations from the API
  locCol <- "traps"
  trap.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T, token=token)
  
  # Use relevant columns
  trap.all <- trap.all[,c("namedLocation","utmZone",
                          "northing","easting","namedLocationCoordUncertainty",
                          "decimalLatitude","decimalLongitude",
                          "elevation","namedLocationElevUncertainty",
                          "current","locationStartDate","locationEndDate")]
  names(trap.all) <- c(locCol,"utmZone",
                       "adjNorthing","adjEasting","adjCoordinateUncertainty",
                       "adjDecimalLatitude","adjDecimalLongitude",
                       "adjElevation","adjElevationUncertainty",
                       "locationCurrent","locationStartDate","locationEndDate")
  
  # merge location data with original data
  trap.loc <- merge(data, trap.all, by="traps", all.x=T)
  
  # keep location data that matches date of collection
  if(any(trap.loc$locationCurrent=="FALSE", na.rm=TRUE)) {
    trap.loc <- findDateMatch(trap.loc, locCol="traps", 
                                 recDate="collectDate")
  }
  
  # increase coordinate uncertainty: traps may be moved up to 2 meters to avoid obstacles
  trap.loc$adjCoordinateUncertainty <- as.numeric(trap.loc$adjCoordinateUncertainty) + 2
  trap.loc$adjElevationUncertainty <- as.numeric(trap.loc$adjElevationUncertainty) + 1
  
  # sort to original order
  trap.return <- trap.loc[order(trap.loc$rowid),]
  trap.return <- trap.return[,-which(colnames(trap.return)=='rowid')]
  
  return(trap.return)
 
}
