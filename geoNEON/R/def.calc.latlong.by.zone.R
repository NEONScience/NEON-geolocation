##############################################################################################
#' @title Convert utms to decimalLatitude and decimalLongitude, one zone at a time.

#' @author 
#' Sarah Elmendorf \email{selmendorf@battelleecology.org}

#' @description 
#' Conversion Function. Given a data frame with geolocations in UTMs, return corresponding decimalLatitude and decimalLongitude. Original decimalLatitude and decimalLongitude will be removed. This conversion function works only with a datafram with a single (unique) utmZone. Use def.utm.tolatlong to apply over variable utmZones.
#' 
#' @param \code{df} A data frame columns labeled easting, northing.
#' @param \code{utmZone} The utmZone of all rows of the dataframe to be converted in form "17N"

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


def.calc.latlong.by.zone<-function(df, utmZone){
  #df requires columns easting, northing
  if (is.na(utmZone)){
    res<-df
  }else{
    #remove any old names
    if ('decimalLatitude'%in%names(df)){
      df<-dplyr::select(data.frame(df), -decimalLatitude)
    }
    if ('decimalLongitude'%in%names(df)){
      df<-dplyr::select(data.frame(df), -decimalLongitude)
    }
    df$easting<-as.numeric(df$easting)
    df$northing<-as.numeric(df$northing)
    #subset out any rows which do not contain easting and northing
    #so they don't error out
    df1<-dplyr::filter(df, is.na(easting)|is.na(northing))
    df<-dplyr::filter(df, !is.na(easting)&!is.na(northing))
    sp::coordinates(df) <- c("easting", "northing")
    #fail on s. hemisphere zones, outside of neon area
    if (grepl('^-|S$', utmZone)){stop('This function only defined for Northern Hemisphere locations')}
    sp::proj4string(df) <- CRS(paste('+proj=utm +zone=', 
                                     gsub("[^0-9]", "", utmZone), " ellps=WGS84",
                                     sep=''))
    transf <- sp::spTransform(df, CRS('+proj=longlat'))
    latLong<-data.frame(coordinates(transf))
    names (latLong)<-c('decimalLongitude', 'decimalLatitude')
    res <- cbind(data.frame(df), latLong)
    if (nrow (df1)>0){
      res<-suppressWarnings(gtools::smartbind(res, df1)) #add back in untransformed rows
    }
  }
  return(as.data.frame(res))
}

