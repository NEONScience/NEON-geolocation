##############################################################################################
#' @title Convert utms to decimalLatitude and decimalLongitude, one zone at a time.

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Conversion Function. Given vectors of easting, northing, and UTM zone, return corresponding decimalLatitude and decimalLongitude.
#' 
#' @param easting Easting UTM coordinates [numeric] 
#' @param northing Northing UTM coordinates [numeric]
#' @param utmZone utmZone of each coordinate pair in form "17N" [character]

#' @return A data frame with 3 columns: decimalLatitude, decimalLongitude, and utmZone.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples
#' d <- data.frame(northing=c(4308017.75), easting=c(747725.21))
#' def.calc.latlong.by.zone(d, '17N')

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2020-03-26)
#     adapted from
#   Sarah Elmendorf (2017-05-05)
##############################################################################################


calcLatLongByZone <- function(easting, northing, utmZone) {

  if(length(easting)!=length(northing) | length(easting)!=length(utmZone)) {
    stop('Variables must be the same length.')
  }
  if (grepl('^-|S$', utmZone)){stop('This function only defined for Northern Hemisphere locations')}
  
    easting <- as.numeric(easting)
    northing <- as.numeric(northing)
    
    df <- cbind(easting, northing, utmZone)
    sp::coordinates(df) <- c("easting", "northing")
    sp::proj4string(df) <- sp::CRS(paste('+proj=utm +zone=', 
                                     gsub("[^0-9]", "", utmZone), " ellps=WGS84",
                                     sep=''))
    transf <- sp::spTransform(df, sp::CRS('+proj=longlat'))
    latLong<-data.frame(sp::coordinates(transf))
    names (latLong)<-c('decimalLongitude', 'decimalLatitude')
    res <- cbind(data.frame(df), latLong)
    if (nrow (df1)>0){
      res<-suppressWarnings(data.table::rbindlist(list(res, df1), fill=T)) #add back in untransformed rows
    }
  }
  return(as.data.frame(res))
}

