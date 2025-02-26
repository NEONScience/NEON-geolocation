##############################################################################################
#' @title Calculate more precise geolocations for bird point counts (DP1.10003.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON bird observations.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param dataProd The table name of the NEON data product table to find locations for. 
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2025-02-13)
##############################################################################################
getLocBRD <- function(
    data,
    dataProd,
    token=NA_character_
){
  
  #check to make sure pointID is in the column names
  if (!'pointID'%in%names(data)){stop('Input table is missing pointID field.')}
  
  # Concatenate the named location (the plot) and point IDs to get the 
  #      point named locations
  points <- paste(data$namedLocation, data$pointID, sep=".")
  data <- cbind(data, points)
  data$points<-as.character(data$points)
  
  #if it's an 88 bird, the only resolution is SITE
  
  if (dataProd=="brd_countdata"){
    data$points[data$pointCountMinute==88]<-substr(data$namedLocation[data$pointCountMinute==88], 1,4)
  }
  
  # Use the getLocByName function to pull the subplot geolocations from the API
  locCol <- "points"
  point.loc <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T, token=token)
  names(point.loc)[names(point.loc)=='namedLocation']<-locCol
  
  #add additional coordinateUncertainty
  point.loc$additionalUncertainty<-NA
  #monumented corners, no additional uncertainty, GPS readings from here
  point.loc$additionalUncertainty[grepl('\\.21$', point.loc[[locCol]])]<-0
  #monumented grid centers, no additional uncertainty, GPS readings from here
  point.loc$additionalUncertainty[grepl('\\.B2$', point.loc[[locCol]])]<-0
  
  #sum uncertainties
  point.loc$adjCoordinateUncertainty <- as.numeric(point.loc$namedLocationCoordUncertainty) + point.loc$additionalUncertainty
  #rest navigated to with recreational GPS, uncertainty of ~15m, not provided in spatial data
  point.loc$adjCoordinateUncertainty[!grepl('\\.21$|\\.B2$',point.loc[[locCol]])] <- 15
  
  #88 points uncertainty unknown, all that's being provided is the site (aka tower) location
  point.loc$adjCoordinateUncertainty[nchar(point.loc$points)==4]<-NA
  point.loc$namedLocationElevUncertainty[nchar(point.loc$points)==4]<-NA
  
  # Return relevant columns
  point.return <- point.loc[,c(locCol, 
                               "utmZone","northing","easting",
                               "adjCoordinateUncertainty",
                               "decimalLatitude","decimalLongitude",
                               "elevation","namedLocationElevUncertainty")]
  
  col.name.list <- names(point.return)
  col.name.list <- gsub('northing','adjNorthing', col.name.list)
  col.name.list <- gsub('easting','adjEasting', col.name.list)
  col.name.list <- gsub('decimalLatitude','adjDecimalLatitude', col.name.list)
  col.name.list <- gsub('decimalLongitude','adjDecimalLongitude', col.name.list)
  col.name.list <- gsub('elevation','adjElevation', col.name.list)
  col.name.list <- gsub('namedLocationElevUncertainty','adjElevationUncertainty', col.name.list)
  colnames(point.return) <- col.name.list
  
  data$row.index <- 1:nrow(data)
  all.return <- merge(data, point.return, by=locCol)
  all.return <- all.return[order(all.return$row.index),]
  all.return <- all.return[,!names(all.return) %in% 'row.index']
  
  return(all.return)
 
}
