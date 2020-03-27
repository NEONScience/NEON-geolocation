##############################################################################################
#' @title Convert utms to decimalLatitude and decimalLongitude. Multiple UTM zones are allowed.

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Conversion Function. Given vectors of easting, northing, and UTM zone, return corresponding decimalLatitude and decimalLongitude. This conversion function wraps calcLatLongByZone to apply over multiple utmZones.
#' 
#' @param easting Easting UTM coordinates [numeric] 
#' @param northing Northing UTM coordinates [numeric]
#' @param utmZone UTM zones corresponding to the coordinates [character]

#' @return A data frame with 2 columns: decimalLatitude, decimalLongitude

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples
#' calcLatLong(easting=c(747725.21, 735000), 
#'      northing=c(4308017.75, 4307000), 
#'      utmZone=c('17N','17N'))
#' calcLatLong(easting=c(747725.21, 581417.95, NA), 
#'      northing=c(4308017.75, 5074637.09, NA), 
#'      utmZone=c('17N','10N','10N'))

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2020-03-26)
#     adapted from
#   Sarah Elmendorf (2017-05-05)
##############################################################################################


calcLatLong <- function(easting, northing, utmZone) {
  
  if(length(easting)!=length(northing) | length(northing)!=length(utmZone)) {
    stop('Variables must be the same length.')
  }
  
  out <- matrix(data=NA, nrow=length(easting), ncol=2)
  out <- data.frame(out)
  names(out) <- c('decimalLatitude','decimalLongitude')
  
  for(i in unique(utmZone)) {
    if(is.na(i)) {
      next
    } else {
      latlong <- calcLatLongByZone(easting[which(utmZone==i)], northing[which(utmZone==i)], i)
      out$decimalLatitude[which(utmZone==i)] <- latlong$decimalLatitude
      out$decimalLongitude[which(utmZone==i)] <- latlong$decimalLongitude
    }
  }
  return(out)
}
