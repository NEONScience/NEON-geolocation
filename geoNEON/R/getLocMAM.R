##############################################################################################
#' @title Calculate more precise geolocations for small mammal trapping (DP1.10072.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON small mammal data.
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
getLocMAM <- function(
    data,
    token=NA_character_
){
  
  # Concatenate the named location (the plot) and trapCoordinate to get the 
  #      point named locations
  points <- paste(data$namedLocation, data$trapCoordinate, sep=".")
  data <- cbind(data, points)
  data$row.index <- 1:nrow(data)
  
  #remove any X columns
  
  # Use the getLocByName function to pull the subplot geolocations from the API
  # Don't bother looking up any of the 'X' traps - those have uncertain geolocations
  dataX <- data[grepl('X', data$points),]
  data <- data[!grepl('X', data$points),]
  
  locCol <- "points"
  point.loc <- getLocByName(data, locCol=locCol, 
                            history=TRUE, locOnly=TRUE, token=token)
  names(point.loc)[names(point.loc)=='namedLocation']<-locCol
  point.loc$adjCoordinateUncertainty<-as.numeric(point.loc$namedLocationCoordUncertainty)
  # named location tables don't include plotID, need to fill it in
  point.loc$plotID <- substring(point.loc$points, 1, 8)
  
  #coordinate uncertainty is only provided for select locations per grid that
  # are monumented; assume since the rest of the grid is set up relative
  # to those monumented points, that the max per monumented point applies
  # to the rest of the points
  
  maxUncPerGrid <- data.frame(tapply(point.loc$adjCoordinateUncertainty, INDEX=point.loc$plotID, 
                                   FUN=function(x) {
                                     if(all(is.na(x))){
                                       NA
                                       } else {
                                         max(x, na.rm=T)
                                         }
                                   }, simplify=TRUE))
  maxUncPerGrid$plotID <- row.names(maxUncPerGrid)
  names(maxUncPerGrid)[1] <- 'maxUncertainty'
  
  point.loc <- merge(point.loc, maxUncPerGrid)
  
  #add additional coordinateUncertainty 3m for nonmonumented, 1m otherwise
  
  #monumented corners, no additional uncertainty, GPS readings from here
  tot.unc <- function(adjCoordinateUncertainty,maxUncertainty){
    if (is.na(adjCoordinateUncertainty)){unc=maxUncertainty+3}else{unc=adjCoordinateUncertainty+1}
    return(unc)
  }
  point.loc$tot.unc <- mapply(tot.unc, point.loc$adjCoordinateUncertainty, point.loc$maxUncertainty)
  
  # calculate latitude and longitude from the corrected northing and easting
  adjLatLong <- calcLatLong(easting=point.loc$easting, 
                            northing=point.loc$northing,
                            utmZone=point.loc$utmZone)
  point.loc$decimalLatitude <- adjLatLong$decimalLatitude
  point.loc$decimalLongitude <- adjLatLong$decimalLongitude
  
  # Return relevant columns
  point.return <- point.loc[,c(locCol,"utmZone",
                               "northing","easting","tot.unc",
                               "decimalLatitude","decimalLongitude",
                               "elevation","namedLocationElevUncertainty",
                               "current","locationStartDate",
                               "locationEndDate")]
  colnames(point.return) <- c("points", "utmZone", "adjNorthing", "adjEasting", 
                              "adjCoordinateUncertainty","adjDecimalLatitude",
                              "adjDecimalLongitude","adjElevation",
                              "adjElevationUncertainty","locationCurrent",
                              "locationStartDate","locationEndDate")
  all.return <- merge(data, point.return, by=locCol, all.x=T)
  
  # get the correct location from the history
  if(any(all.return$locationCurrent=="FALSE" | isFALSE(all.return$locationCurrent))) {
    all.return <- findDateMatch(all.return, locCol="points", recDate="collectDate")
  }
  
  all.return <- data.frame(data.table::rbindlist(list(all.return, dataX), fill=TRUE))
  
  all.return <- all.return[order(all.return$row.index),]
  all.return <- all.return[,!names(all.return) %in% c("row.index","locationCurrent",
                                                    "locationStartDate","locationEndDate")]

  return(all.return)
 
}
