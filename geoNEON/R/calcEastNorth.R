##############################################################################################
#' @title Convert decimalLatitude and decimalLongitude to utms. Multiple UTM zones are allowed.

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Conversion Function. Given vectors of decimalLatitude, decimalLongitude, and UTM zone, return corresponding easting and northing.
#' 
#' @param latitude Decimal latitude [numeric] 
#' @param longitude Decimal longitude [numeric]
#' @param utmZone utmZones corresponding to the coordinates [character]

#' @return A data frame with 2 columns: easting, northing

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples

#' @export

# changelog and author contributions / copyrights
#     Claire Lunch (2025-10-27)
#     adapted from calcLatLong()
##############################################################################################


calcEastNorth <- function(latitude, longitude, utmZone) {
  
  if(length(latitude)!=length(longitude) | length(longitude)!=length(utmZone)) {
    stop('Variables must be the same length.')
  }
  
  out <- matrix(data=NA, nrow=length(longitude), ncol=2)
  out <- data.frame(out)
  names(out) <- c('easting','northing')
  
  for(i in unique(utmZone)) {
    if(is.na(i)) {
      next
    } else {
      eastnorth <- calcEastNorthByZone(latitude=latitude[which(utmZone==i)], 
                                       longitude=longitude[which(utmZone==i)], i)
      out$easting[which(utmZone==i)] <- eastnorth$easting
      out$northing[which(utmZone==i)] <- eastnorth$northing
    }
  }
  return(out)
}
