##############################################################################################
#' @title Calculate more precise geolocations for plant presence and percent cover (DP1.10058.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON plant presence data.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2025-02-13)
##############################################################################################
getLocDIV <- function(
    data,
    token=NA_character_
){
  
  # Concatenate the named location (the plot) and trapCoordinate to get the 
  #      point named locations
  subplots <- paste(data$namedLocation, data$subplotID, sep=".")
  data <- cbind(data, subplots)
  
  # Use the getLocByName function to pull the subplot geolocations from the API
  locCol="subplots"
  subplot.loc <- getLocByName(data, locCol=locCol, locOnly=TRUE, 
                              history=TRUE, token=token)
  names(subplot.loc)[names(subplot.loc)=='namedLocation']<-locCol
  subplot.loc$adjCoordinateUncertainty<-as.numeric(subplot.loc$namedLocationCoordUncertainty)
  
  #Increase coordinateUncertainty by an appropriate amount to account for error
  # introduced by navigating within plots. Additional error may be introduced due to
  # to tape stretching to navigate to locations within plots
  # and is estimated as:
  # 0.25m for 1m2 subplot centroids
  # 1.0m for 10m2 subplot centroids
  # 2.0m for 100m2 subplot centroids
  
  subplot.loc$adjCoordinateUncertainty[grepl('\\.1$', subplot.loc$subplotID)]<-
    subplot.loc$adjCoordinateUncertainty[grepl('\\.1$', subplot.loc$subplotID)]+0.25
  
  subplot.loc$adjCoordinateUncertainty[grepl('\\.10$', subplot.loc$subplotID)]<-
    subplot.loc$adjCoordinateUncertainty[grepl('\\.10$', subplot.loc$subplotID)]+0.1
  
  subplot.loc$adjCoordinateUncertainty[!grepl('\\.10$|\\.1$', subplot.loc$subplotID)]<-
    subplot.loc$adjCoordinateUncertainty[!grepl('\\.10$|\\.1$', subplot.loc$subplotID)]+2
  
  subplot.loc$adjDecimalLatitude <- subplot.loc$decimalLatitude
  subplot.loc$adjDecimalLongitude <- subplot.loc$decimalLongitude
  subplot.loc$adjElevation <- subplot.loc$elevation
  subplot.loc$adjElevationUncertainty <- subplot.loc$namedLocationElevUncertainty
  subplot.loc$adjNorthing <- subplot.loc$northing
  subplot.loc$adjEasting <- subplot.loc$easting
  subplot.loc$locationCurrent <- subplot.loc$current
  
  # Return relevant columns
  subplot.return <- subplot.loc[,c(locCol,"utmZone",
                                   "adjNorthing","adjEasting","adjCoordinateUncertainty",
                                   "adjDecimalLatitude","adjDecimalLongitude",
                                   "adjElevation","adjElevationUncertainty",
                                   "locationCurrent","locationStartDate","locationEndDate")]
  data$row.index<-1:nrow(data)
  all.return <- merge(data, subplot.return, by=locCol, all.x=T)
  
  # keep location data that matches date of collection
  if(any(all.return$locationCurrent=="FALSE")) {
    all.return <- findDateMatch(all.return, locCol="subplots", 
                              recDate="endDate")
  }

  all.return<-all.return[order(all.return$row.index),]
  all.return<-all.return[,!names(all.return)%in%'row.index']
  
  return(all.return)
 
}
