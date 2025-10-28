##############################################################################################
#' @title Convert decimalLatitude and decimalLongitude to utms, one zone at a time.

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Conversion Function. Given vectors of decimalLatitude and decimalLongitude., and a UTM zone, return corresponding easting and northing.
#' 
#' @param latitude Decimal latitude [numeric] 
#' @param longitude Decimal longitude [numeric]
#' @param utmZone A single utmZone in form "17N" [character]

#' @return A data frame with 2 columns: easting, northing

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples
#' calcEastNorthByZone(latitude=45.8, longitude=-101, '17N')

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2025-10-27)
#     adapted from calcLatLongByZone()
##############################################################################################


calcEastNorthByZone <- function(latitude, longitude, utmZone) {

  if(length(latitude)!=length(longitude)) {
    stop('Variables must be the same length.')
  }
  if (grepl('^-|S$', utmZone)){stop('This function only defined for Northern Hemisphere locations')}
  
  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)
  
  df <- cbind(longitude, latitude)
  df <- data.frame(df)
  
  # convert coordinates
  colnames(df) <- c("longitude","latitude")
  
  df <- terra::vect(df, geom=c("longitude","latitude"), 
                    crs=paste("+proj=longlat +zone=", gsub("[^0-9]", "", utmZone), sep=""))
  
  dfconv <- terra::project(df, y=paste("+proj=utm +zone=", gsub("[^0-9]", "", utmZone), sep=""))
  eastnorth <- terra::geom(dfconv, df=TRUE)
  
  names(eastnorth)[which(names(eastnorth)=="x")] <- "easting"
  names(eastnorth)[which(names(eastnorth)=="y")] <- "northing"
  
  return(eastnorth[,c("easting","northing")])
  
}

