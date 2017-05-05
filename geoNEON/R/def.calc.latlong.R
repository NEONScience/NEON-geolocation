##############################################################################################
#' @title Convert utms to decimalLatitude and decimalLongitude, one zone at a time.

#' @author 
#' Sarah Elmendorf \email{selmendorf@battelleecology.org}

#' @description 
#' Conversion Function. Given a data frame with geolocations in UTMs, return corresponding decimalLatitude and decimalLongitude. Original decimalLatitude and decimalLongitude will be removed. This conversion function wraps def.calc.utm.tolatlong.by.zone to apply over variable utmZones.
#' 
#' @param \code{df} A data frame columns labeled easting, northing.

#' @return The original data frame with decimalLatitude and decimalLongitude calculated.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Sarah Elmendorf (2017-05-05)
#     original creation
##############################################################################################


def.calc.latlong<-function(df){ #requires columns easting, northing, utmZone
    res<-plyr::dlply(df, c('utmZone'), function(x) def.calc.latlong.by.zone (x, unique (x$utmZone)))
    res<-plyr::rbind.fill(res)
    res<-dplyr::select(res, -optional)
    if (any(is.na(df$easting)|any(is.na(df$northing)|any(is.na(df$utmZone))))){
      warning('one or more rows had missing inputs for easting, northing, or UTM zone and were not converted')
    }
  return(res)
}
