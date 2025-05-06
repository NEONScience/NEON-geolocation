##############################################################################################
#' @title Calculate more precise geolocations for soil initial characterization (DP1.10047.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON soil characterization data.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2025-02-13)
##############################################################################################
getLocSPC <- function(
    data,
    token=NA_character_
){
  
  data$rowid <- 1:nrow(data)
  
  # get easting/northing
  locCol <- 'namedLocation'
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
                              recDate="collectDate")
  }
  
  # adjust northing and easting using point offsets
  plot.loc$adjEasting <- as.numeric(plot.loc$adjEasting)
  plot.loc$adjNorthing <- as.numeric(plot.loc$adjNorthing)
  
  # get easting and northing of reference corner
  refCornerEasting <- plot.loc$adjEasting
  refCornerNorthing <- plot.loc$adjNorthing
  refCornerEasting[which(plot.loc$referenceCorner=="SW20")] <- 
    plot.loc$adjEasting[which(plot.loc$referenceCorner=="SW20")] - 10
  refCornerNorthing[which(plot.loc$referenceCorner=="SW20")] <- 
    plot.loc$adjNorthing[which(plot.loc$referenceCorner=="SW20")] - 10
  refCornerEasting[which(plot.loc$referenceCorner=="SW40")] <- 
    plot.loc$adjEasting[which(plot.loc$referenceCorner=="SW40")] - 20
  refCornerNorthing[which(plot.loc$referenceCorner=="SW40")] <- 
    plot.loc$adjNorthing[which(plot.loc$referenceCorner=="SW40")] - 20
  refCornerEasting[which(plot.loc$referenceCorner=="SE20")] <- 
    plot.loc$adjEasting[which(plot.loc$referenceCorner=="SE20")] + 10
  refCornerNorthing[which(plot.loc$referenceCorner=="SE20")] <- 
    plot.loc$adjNorthing[which(plot.loc$referenceCorner=="SE20")] - 10
  refCornerEasting[which(plot.loc$referenceCorner=="SE40")] <- 
    plot.loc$adjEasting[which(plot.loc$referenceCorner=="SE40")] + 20
  refCornerNorthing[which(plot.loc$referenceCorner=="SE40")] <- 
    plot.loc$adjNorthing[which(plot.loc$referenceCorner=="SE40")] - 20
  refCornerEasting[which(plot.loc$referenceCorner=="NE20")] <- 
    plot.loc$adjEasting[which(plot.loc$referenceCorner=="NE20")] + 10
  refCornerNorthing[which(plot.loc$referenceCorner=="NE20")] <- 
    plot.loc$adjNorthing[which(plot.loc$referenceCorner=="NE20")] + 10
  refCornerEasting[which(plot.loc$referenceCorner=="NE40")] <- 
    plot.loc$adjEasting[which(plot.loc$referenceCorner=="NE40")] + 20
  refCornerNorthing[which(plot.loc$referenceCorner=="NE40")] <- 
    plot.loc$adjNorthing[which(plot.loc$referenceCorner=="NE40")] + 20
  refCornerEasting[which(plot.loc$referenceCorner=="NW20")] <- 
    plot.loc$adjEasting[which(plot.loc$referenceCorner=="NW20")] - 10
  refCornerNorthing[which(plot.loc$referenceCorner=="NW20")] <- 
    plot.loc$adjNorthing[which(plot.loc$referenceCorner=="NW20")] + 10
  refCornerEasting[which(plot.loc$referenceCorner=="NW40")] <- 
    plot.loc$adjEasting[which(plot.loc$referenceCorner=="NW40")] - 20
  refCornerNorthing[which(plot.loc$referenceCorner=="NW40")] <- 
    plot.loc$adjNorthing[which(plot.loc$referenceCorner=="NW40")] + 20
  
  # adjust northing and easting for distance and bearing from reference
  plot.loc$adjEasting <- refCornerEasting + 
    plot.loc$sampleDistance * sin(plot.loc$sampleBearing*pi/180)
  plot.loc$adjNorthing <- refCornerNorthing + 
    plot.loc$sampleDistance * cos(plot.loc$sampleBearing*pi/180)
  
  # calculate latitude and longitude from the corrected northing and easting
  adjLatLong <- geoNEON::calcLatLong(easting=plot.loc$adjEasting, 
                                     northing=plot.loc$adjNorthing,
                                     utmZone=plot.loc$utmZone)
  plot.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
  plot.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
  
  # increase coordinate uncertainty: navigation within plot
  plot.loc$adjCoordinateUncertainty <- as.numeric(plot.loc$adjCoordinateUncertainty) + 1
  plot.loc$adjElevationUncertainty <- as.numeric(plot.loc$adjElevationUncertainty) + 1
  
  # sort to original order
  plot.return <- plot.loc[order(plot.loc$rowid),]
  plot.return <- plot.return[,-which(colnames(plot.return)=='rowid')]
  
  return(plot.return)
 
}
