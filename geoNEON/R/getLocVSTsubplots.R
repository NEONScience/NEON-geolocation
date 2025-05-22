##############################################################################################
#' @title Calculate more precise geolocations for vegetation structure subplots (used for measurements made on unmapped plants) (DP1.10098.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON vegetation structure subplots.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.
#' 
#' @keywords internal

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2025-02-13)
##############################################################################################
getLocVSTsubplots <- function(
    data,
    token=NA_character_
){
  
  # Concatenate the named location (the plot) and subplot IDs to get the 
  #      subplot named locations
  subplots <- paste(data$namedLocation, data$subplotID, sep=".")
  data <- cbind(data, subplots)
  
  # Use the getLocByName function to pull the subplot geolocations from the API
  locCol <- "subplots"
  subplot.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T, token=token)
  
  # increase coordinate uncertainty by Value for Subplot size / 2
  subplot.all$namedLocationCoordUncertainty <- 
    as.numeric(subplot.all$namedLocationCoordUncertainty) + 
    as.numeric(subplot.all$Value.for.Subplot.size)/2
  
  subplot.all$northing <- as.numeric(subplot.all$northing)
  subplot.all$easting <- as.numeric(subplot.all$easting)
  subplot.all$decimalLatitude <- as.numeric(subplot.all$decimalLatitude)
  subplot.all$decimalLongitude <- as.numeric(subplot.all$decimalLongitude)
  subplot.all$elevation <- as.numeric(subplot.all$elevation)
  subplot.all$namedLocationElevUncertainty <- as.numeric(subplot.all$namedLocationElevUncertainty)
  
  # Use relevant columns
  subplot.all <- subplot.all[,c("namedLocation","utmZone",
                                "northing","easting","namedLocationCoordUncertainty",
                                "decimalLatitude","decimalLongitude",
                                "elevation","namedLocationElevUncertainty",
                                "current","locationStartDate","locationEndDate")]
  names(subplot.all) <- c(locCol,"utmZone",
                          "adjNorthing","adjEasting","adjCoordinateUncertainty",
                          "adjDecimalLatitude","adjDecimalLongitude",
                          "adjElevation","adjElevationUncertainty",
                          "locationCurrent","locationStartDate","locationEndDate")
  
  # merge location data with original data
  subplot.loc <- merge(data, subplot.all, by="subplots", all.x=T)
  
  # keep location data that matches date of collection
  if(any(subplot.loc$locationCurrent=="FALSE" | isFALSE(subplot.loc$locationCurrent), 
         na.rm=TRUE)) {
    subplot.loc <- findDateMatch(subplot.loc, locCol="subplots", 
                               recDate="date")
  }
  
  return(subplot.loc)
 
}
