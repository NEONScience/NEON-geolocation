##############################################################################################
#' @title Calculate more precise geolocations for site management data (DP1.10111.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON site management data.
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
getLocSIM <- function(
    data,
    token=NA_character_
){
  
  # get individual locations from the location column
  loclist <- lapply(data$locationID, FUN=function(x) {
    a <- unlist(strsplit(unlist(x), "[,][ ]{0,}"))
    a <- data.frame(namedLocation=a)
    return(a)
  })
  locset <- unique(unlist(loclist))
  
  # get spatial data for each location
  locdata <- getLocByName(data.frame(namedLocation=locset), 
                          locOnly=TRUE, history=TRUE, token=token)
  locd <- locdata[,grep("Value.for", colnames(locdata), 
                        invert=TRUE, fixed=TRUE)]
  
  # get coordinates for IS locations that don't have them
  if(any(is.na(locd$easting))) {
    locis <- locd[which(is.na(locd$easting)),]
    locd <- locd[which(!is.na(locd$easting)),]
    
    # if lat and long are populated but easting and northing are not
    if(any(!is.na(locis$decimalLatitude))) {
      locislatlong <- locis[which(!is.na(locis$decimalLatitude)),]
      locisnolatlong <- locis[which(is.na(locis$decimalLatitude)),]
      for(j in unique(locislatlong$siteID)) {
        utmZj <- getLocBySite(j, type="site", history=FALSE, token=token)$utmZone
        locislatlong$utmZone[which(locislatlong$siteID==j)] <- utmZj
      }
      locislatlongen <- calcEastNorth(locislatlong$decimalLatitude,
                                      locislatlong$decimalLongitude,
                                      locislatlong$utmZone)
      locislatlong$easting <- locislatlongen$easting
      locislatlong$northing <- locislatlongen$northing
      
      locis <- data.table::rbindlist(list(locisnolatlong, locislatlong), fill=TRUE)
    }
    
    # for soil plots
    # use the un-numbered coordinates, and set uncertainty to include the whole plot
    if("locationPolygon.coordinates.latitude" %in% colnames(locis)) {
      if(any(!is.na(locis$locationPolygon.coordinates.latitude))) {
        locsp <- locis[which(!is.na(locis$locationPolygon.coordinates.latitude)),]
        locnosp <- locis[which(is.na(locis$locationPolygon.coordinates.latitude)),]
        
        locsp$decimalLatitude <- locsp$locationPolygon.coordinates.latitude
        locsp$decimalLongitude <- locsp$locationPolygon.coordinates.longitude
        locsp$easting <- locsp$locationPolygon.coordinates.utmEasting
        locsp$northing <- locsp$locationPolygon.coordinates.utmNorthing
        locsp$elevation <- locsp$locationPolygon.coordinates.elevation
        locsp$namedLocationCoordUncertainty <- 5
        
        locis <- data.table::rbindlist(list(locnosp, locsp), fill=TRUE)
      }
    }
    
    # get coordinates from reference locations
    # if(any(is.na(locis$decimalLatitude))) {
    #   locisnrf <- locis[which(is.na(locis$decimalLatitude)),]
    #   locisll <- locis[which(!is.na(locis$decimalLatitude)),]
    #   
    #   if(any(!is.na(locisnrf$xOffset))) {
    #     locisnrf$easting <- locisnrf$referenceLocation.locationUtmEasting + 
    #       locisnrf$xOffset
    #     locisnrf$northing <- locisnrf$referenceLocation.locationUtmNorthing + 
    #       locisnrf$yOffset
    #     locisnrf$elevation <- locisnrf$referenceLocation.locationElevation + 
    #       locisnrf$zOffset
    #     
    #     locisnrfll <- calcLatLong(locisnrf$easting,
    #                               locisnrf$northing,
    #                               locisnrf$utmZone)
    #     
    #     locisnrf$decimalLatitude <- locisnrfll$decimalLatitude
    #     locisnrf$decimalLongitude <- locisnrfll$decimalLongitude
    #   }
    #   
    #   locis <- data.table::rbindlist(list(locisnrf, locisll), fill=TRUE)
    # }
  }
  
  # merge spatial data back into data frame
  for(i in 1:length(loclist)) {
    data$locationID[i] <- list(base::merge(loclist[[i]], 
                                           locd, by="namedLocation",
                                           all.x=T))
  }
  return(data)
 
}
