##############################################################################################
#' @title Calculate more precise geolocations for vegetation structure mapped individuals (DP1.10098.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON vegetation structure individuals.
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
getLocVSTmapped <- function(
    data,
    token=NA_character_
){
  
  # Pull out data with no pointID
  data$rowid <- 1:nrow(data)
  dataN <- data[which(is.na(data$pointID)),]
  data <- data[which(!is.na(data$pointID)),]
  
  if(nrow(data)==0) {
    stop("There are no mapped individuals in the input data table.")
  }
  
  # Concatenate the named location (the plot) and point IDs to get the 
  #      point named locations
  points <- paste(data$namedLocation, data$pointID, sep=".")
  data <- cbind(data, points)
  
  # Use the getLocByName function to pull the subplot geolocations from the API
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
  
  # keep location data that matches date of collection
  if(any(point.loc$locationCurrent=="FALSE" | isFALSE(point.loc$locationCurrent), 
         na.rm=TRUE)) {
    point.loc <- findDateMatch(point.loc, locCol="points", 
                              recDate="date")
  }

  # Calculate easting and northing for individuals
  point.loc$adjEasting <- as.numeric(point.loc$adjEasting) + as.numeric(point.loc$stemDistance) * 
    sin((as.numeric(point.loc$stemAzimuth) * pi) / 180)
  point.loc$adjNorthing <- as.numeric(point.loc$adjNorthing) + as.numeric(point.loc$stemDistance) * 
    cos((as.numeric(point.loc$stemAzimuth) * pi) / 180)
  
  # Increase coordinate uncertainties
  point.loc$adjCoordinateUncertainty <- 
    as.numeric(point.loc$adjCoordinateUncertainty) + 0.6
  point.loc$adjElevationUncertainty <- 
    as.numeric(point.loc$adjElevationUncertainty) + 1
  
  # calculate latitude and longitude from the corrected northing and easting
  adjLatLong <- geoNEON::calcLatLong(easting=point.loc$adjEasting, 
                                     northing=point.loc$adjNorthing,
                                     utmZone=point.loc$utmZone)
  point.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
  point.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
  
  # add back in individuals that weren't mapped
  all.return <- plyr::rbind.fill(point.loc, dataN)
  all.return <- all.return[order(all.return$rowid),]
  all.return <- all.return[,!names(all.return) %in% c('rowid','points')]
  
  return(all.return)
 
}
