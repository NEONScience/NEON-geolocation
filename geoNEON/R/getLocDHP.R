##############################################################################################
#' @title Calculate more precise geolocations for digital hemispherical photos (DP1.10017.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON DHP photos.
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
getLocDHP <- function(
    data,
    token=NA_character_
){
  
  data$rowid <- 1:nrow(data)
  
  # plot spatial data are in the dhp_perplot table, so need to download
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
  if(any(plot.loc$locationCurrent=="FALSE" | isFALSE(plot.loc$locationCurrent), 
         na.rm=TRUE)) {
    plot.loc <- findDateMatch(plot.loc, locCol="namedLocation", 
                              recDate="endDate")
  }
  
  # adjust northing and easting using point offsets
  plot.loc$adjEasting <- as.numeric(plot.loc$adjEasting)
  plot.loc$adjNorthing <- as.numeric(plot.loc$adjNorthing)
  for(i in unique(plot.loc$pointID)) {
    eastOff.i <- dhpOffsets$eastOff[which(dhpOffsets$pointID==i)]
    northOff.i <- dhpOffsets$northOff[which(dhpOffsets$pointID==i)]
    
    # if point in data doesn't match any point in DHP offsets, skip and delete location data
    if(length(eastOff.i)==0 | length(northOff.i)==0) {
      message(paste('Point ', i, ' not found in DHP points. Locations not calculated.', sep=''))
      plot.loc$easting[which(plot.loc$pointID==i)] <- NA
      plot.loc$northing[which(plot.loc$pointID==i)] <- NA
      next
    }
    
    # apply offsets
    plot.loc$adjEasting[which(plot.loc$pointID==i)] <- plot.loc$adjEasting[which(plot.loc$pointID==i)] + eastOff.i
    plot.loc$adjNorthing[which(plot.loc$pointID==i)] <- plot.loc$adjNorthing[which(plot.loc$pointID==i)] + northOff.i
  }
  
  # calculate latitude and longitude from the corrected northing and easting
  adjLatLong <- geoNEON::calcLatLong(easting=plot.loc$adjEasting, 
                                     northing=plot.loc$adjNorthing,
                                     utmZone=plot.loc$utmZone)
  plot.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
  plot.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
  
  # increase coordinate uncertainty: flexibility in camera placement
  plot.loc$adjCoordinateUncertainty <- as.numeric(plot.loc$adjCoordinateUncertainty) + 2
  plot.loc$adjElevationUncertainty <- as.numeric(plot.loc$adjElevationUncertainty) + 2
  
  # sort to original order
  plot.return <- plot.loc[order(plot.loc$rowid),]
  plot.return <- plot.return[,-which(colnames(plot.return)=='rowid')]
  
  return(plot.return)
 
}
