##############################################################################################
#' @title Calculate more precise geolocations for soil collection (DP1.10086.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON soil data.
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
getLocSLS <- function(
    data,
    token=NA_character_
){
  
  data$rowid <- 1:nrow(data)
  
  # Use the getLocByName function to pull the plot geolocations from the API
  locCol <- "namedLocation"
  plot.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=TRUE, 
                                    history=TRUE, token=token)
  
  # Use relevant columns
  plot.merg <- plot.all[,c("namedLocation","utmZone",
                           "northing","easting","namedLocationCoordUncertainty",
                           "decimalLatitude","decimalLongitude",
                           "elevation","namedLocationElevUncertainty",
                           "current","locationStartDate","locationEndDate")]
  colnames(plot.merg) <- c(locCol, 'utmZone',"adjNorthing","adjEasting",
                           "adjCoordinateUncertainty","adjDecimalLatitude",
                           "adjDecimalLongitude","adjElevation",
                           "adjElevationUncertainty",
                           "locationCurrent","locationStartDate","locationEndDate")
  plot.loc <- base::merge(data, plot.merg, by=locCol, all.x=T)
  
  # keep location data that matches date of collection
  if(any(plot.loc$locationCurrent=="FALSE")) {
    plot.loc <- findDateMatch(plot.loc, locCol="namedLocation", 
                                 recDate="collectDate")
  }
  plot.loc <- plot.loc[order(plot.loc$rowid),]
  
  # Subtract 20 meters from the easting and northing values to get the 
  # location of the southwest corner
  plot.loc$adjEasting <- as.numeric(plot.loc$adjEasting) - 20
  plot.loc$adjNorthing <- as.numeric(plot.loc$adjNorthing) - 20
  
  # Add coreCoordinateX to the easting value and coreCoordinateY to the northing value
  plot.loc$adjEasting <- plot.loc$adjEasting + data$coreCoordinateX
  plot.loc$adjNorthing <- plot.loc$adjNorthing + data$coreCoordinateY
  
  # Set the coordinate uncertainty to 0.5 meter
  plot.loc$adjCoordinateUncertainty <- 0.5
  
  # calculate latitude and longitude from the corrected northing and easting
  adjLatLong <- geoNEON::calcLatLong(easting=plot.loc$adjEasting, 
                                     northing=plot.loc$adjNorthing,
                                     utmZone=plot.loc$utmZone)
  plot.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
  plot.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
  
  # reorder to original order
  all.return <- plot.loc[order(plot.loc$rowid),]
  all.return <- all.return[,!names(all.return) %in% c('rowid')]

  return(all.return)
 
}
