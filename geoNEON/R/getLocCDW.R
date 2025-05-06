##############################################################################################
#' @title Calculate more precise geolocations for coarse downed wood tally (DP1.10010.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON coarse downed wood data.
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
getLocCDW <- function(
    data,
    token=NA_character_
){
  
  # Pull out data with no distance or azimuth
  data$rowid <- 1:nrow(data)
  dataN <- data[which(is.na(data$logDistance) | is.na(data$lidsAzimuth)),]
  data <- data[which(!is.na(data$logDistance) & !is.na(data$lidsAzimuth)),]
  
  # Use the getLocByName function to pull the subplot geolocations from the API
  locCol <- "namedLocation"
  plot.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=TRUE, 
                                    history=TRUE, token=token)
  
  # Use relevant columns
  plot.all <- plot.all[,c("namedLocation","utmZone",
                          "northing","easting","namedLocationCoordUncertainty",
                          "decimalLatitude","decimalLongitude",
                          "elevation","namedLocationElevUncertainty",
                          "current","locationStartDate","locationEndDate")]
  names(plot.all) <- c(locCol,"utmZone",
                       "adjNorthing","adjEasting","adjCoordinateUncertainty",
                       "adjDecimalLatitude","adjDecimalLongitude",
                       "adjElevation","adjElevationUncertainty",
                       "locationCurrent","locationStartDate","locationEndDate")
  
  # merge location data with original data
  plot.loc <- merge(data, plot.all, by="namedLocation", all.x=T)
  
  # keep location data that matches date of collection
  if(any(plot.loc$locationCurrent=="FALSE", na.rm=TRUE)) {
    plot.loc <- findDateMatch(plot.loc, locCol="namedLocation", 
                              recDate="date")
  }
  
  # Calculate easting and northing from distance and azimuth,
  # adding 3 to distance because transects start at a 3 meter radius from plot centroid
  plot.loc$adjEasting <- as.numeric(plot.loc$adjEasting) + (plot.loc$logDistance + 3) * 
    sin((plot.loc$lidsAzimuth * pi) / 180)
  plot.loc$adjNorthing <- as.numeric(plot.loc$adjNorthing) + (plot.loc$logDistance + 3) * 
    cos((plot.loc$lidsAzimuth * pi) / 180)
  
  # Increase coordinate uncertainties by reasonable estimate for navigation error
  plot.loc$adjCoordinateUncertainty <- 
    as.numeric(plot.loc$adjCoordinateUncertainty) + 1
  plot.loc$adjElevationUncertainty <- 
    as.numeric(plot.loc$adjElevationUncertainty) + 1
  
  # calculate latitude and longitude from the corrected northing and easting
  adjLatLong <- geoNEON::calcLatLong(easting=plot.loc$adjEasting, 
                                     northing=plot.loc$adjNorthing,
                                     utmZone=plot.loc$utmZone)
  plot.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
  plot.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
  
  # add back in individuals that weren't mapped
  all.return <- plyr::rbind.fill(plot.loc, dataN)
  all.return <- all.return[order(all.return$rowid),]
  all.return <- all.return[,-which(colnames(all.return)=='rowid')]
  
  return(all.return)
 
}
