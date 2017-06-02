##############################################################################################
#' @title Convert utms to decimalLatitude and decimalLongitude, one zone at a time.

#' @author 
#' Sarah Elmendorf \email{selmendorf@battelleecology.org}

#' @description 
#' Conversion Function. Given a data frame with geolocations in UTMs, return corresponding decimalLatitude and decimalLongitude. Original decimalLatitude and decimalLongitude will be removed. This conversion function wraps def.calc.utm.tolatlong.by.zone to apply over variable utmZones.
#' 
#' @param df A data frame columns labeled easting, northing.

#' @return The original data frame with decimalLatitude and decimalLongitude calculated.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples
#' d <- data.frame(northing=c(4308017.75), easting=c(747725.21), utmZone=c('17N'))
#' def.calc.latlong(d)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Sarah Elmendorf (2017-05-05)
#     original creation
##############################################################################################


def.calc.latlong<-function(df){ #requires columns easting, northing, utmZone
    df<-df[,!names(df) %in% c("decimalLatitude","decimalLongitude")]
    res<-plyr::dlply(df, c('utmZone'), function(x) def.calc.latlong.by.zone (x, unique (x$utmZone)))
    res<-plyr::rbind.fill(res)
    if (any(is.na(df$easting)|any(is.na(df$northing)|any(is.na(df$utmZone))))){
      warning('one or more rows had missing inputs for easting, northing, or UTM zone and were not converted')
    }
  return(res)
}
