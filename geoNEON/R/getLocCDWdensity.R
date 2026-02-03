##############################################################################################
#' @title Calculate more precise geolocations for coarse downed wood bulk density (DP1.10014.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON coarse downed wood bulk density data.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param token User specific API token (generated within data.neonscience.org user accounts). Optional.
#' 
#' @keywords internal

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2026-01-29)
##############################################################################################
getLocCDWdensity <- function(
    data,
    token=NA_character_
){
  
  # Pull out data with no distance or azimuth
  data$rowid <- 1:nrow(data)
  dataN <- data[which(is.na(data$logDistance) | is.na(data$logAzimuth)),]
  data <- data[which(!is.na(data$logDistance) & !is.na(data$logAzimuth)),]
  
  # Use the first two digits of the subplotID to get the pointID
  # Concatenate the named location (the plot) and the pointID to get the 
  #      point named locations
  pointIDs <- substring(data$pointID, 1, 2)
  data$points <- paste(data$namedLocation, pointIDs, sep=".")

  # Use the getLocByName function to pull the point geolocations from the API
  locCol <- "points"
  point.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=TRUE, 
                                    history=TRUE, token=token)
  
  # Use relevant columns
  point.all <- point.all[,c("namedLocation","utmZone",
                          "northing","easting","namedLocationCoordUncertainty",
                          "decimalLatitude","decimalLongitude",
                          "elevation","namedLocationElevUncertainty",
                          "current","locationStartDate","locationEndDate")]
  names(point.all) <- c(locCol,"utmZone",
                       "adjNorthing","adjEasting","adjCoordinateUncertainty",
                       "adjDecimalLatitude","adjDecimalLongitude",
                       "adjElevation","adjElevationUncertainty",
                       "locationCurrent","locationStartDate","locationEndDate")
  
  # merge location data with original data
  point.loc <- merge(data, point.all, by="points", all.x=T)
  
  # correct azimuth to be azimuth from point to log
  correctedAzimuth <- data$logAzimuth + 180
  
  # Calculate easting and northing for individuals
  point.loc$adjEasting <- as.numeric(point.loc$adjEasting) + as.numeric(point.loc$logDistance) * 
    sin((as.numeric(point.loc$correctedAzimuth) * pi) / 180)
  point.loc$adjNorthing <- as.numeric(point.loc$adjNorthing) + as.numeric(point.loc$logDistance) * 
    cos((as.numeric(point.loc$correctedAzimuth) * pi) / 180)
  
  # keep location data that matches date of collection
  if(any(point.loc$locationCurrent=="FALSE" | isFALSE(point.loc$locationCurrent), 
         na.rm=TRUE)) {
    point.loc <- findDateMatch(point.loc, locCol="namedLocation", 
                              recDate="date")
  }
  
  # Calculate easting and northing from distance and azimuth,
  # adding 3 to distance because transects start at a 3 meter radius from plot centroid
  point.loc$adjEasting <- as.numeric(point.loc$adjEasting) + (point.loc$logDistance + 3) * 
    sin((point.loc$lidsAzimuth * pi) / 180)
  point.loc$adjNorthing <- as.numeric(point.loc$adjNorthing) + (point.loc$logDistance + 3) * 
    cos((point.loc$lidsAzimuth * pi) / 180)
  
  # Increase coordinate uncertainties by reasonable estimate for navigation error
  point.loc$adjCoordinateUncertainty <- 
    as.numeric(point.loc$adjCoordinateUncertainty) + 0.6
  point.loc$adjElevationUncertainty <- 
    as.numeric(point.loc$adjElevationUncertainty) + 0.6
  
  # calculate latitude and longitude from the corrected northing and easting
  adjLatLong <- geoNEON::calcLatLong(easting=point.loc$adjEasting, 
                                     northing=point.loc$adjNorthing,
                                     utmZone=point.loc$utmZone)
  point.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
  point.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
  
  # add back in individuals that weren't mapped
  all.return <- plyr::rbind.fill(point.loc, dataN)
  all.return <- all.return[order(all.return$rowid),]
  all.return <- all.return[,-which(colnames(all.return)=='rowid')]
  
  return(all.return)
 
}
