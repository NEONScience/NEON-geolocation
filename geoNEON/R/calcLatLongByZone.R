##############################################################################################
#' @title Convert utms to decimalLatitude and decimalLongitude, one zone at a time.

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Conversion Function. Given vectors of easting and northing, and a UTM zone, return corresponding decimalLatitude and decimalLongitude.
#' 
#' @param easting Easting UTM coordinates [numeric] 
#' @param northing Northing UTM coordinates [numeric]
#' @param utmZone A single utmZone in form "17N" [character]

#' @return A data frame with 2 columns: decimalLatitude, decimalLongitude

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples
#' calcLatLongByZone(easting=747725.21, northing=4308017.75, '17N')

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2020-03-26)
#     adapted from
#   Sarah Elmendorf (2017-05-05)
##############################################################################################


calcLatLongByZone <- function(easting, northing, utmZone) {

  if(length(easting)!=length(northing)) {
    stop('Variables must be the same length.')
  }
  if (grepl('^-|S$', utmZone)){stop('This function only defined for Northern Hemisphere locations')}
  
    easting <- as.numeric(easting)
    northing <- as.numeric(northing)
    rowid <- 1:length(easting)
    
    df <- cbind(easting, northing, rowid)
    df <- data.frame(df)
    
    # remove missing values, sp can't handle
    dfN <- df[which(is.na(df$easting) | is.na(df$northing)),]
    df <- df[which(!is.na(df$easting) & !is.na(df$northing)),]
    
    # convert coordinates
    sp::coordinates(df) <- c("easting", "northing")
    
    # proj4string() is deprecated. Attempting back-compatibility.
    if(utils::packageVersion('sp')<'1.4.2') {
      sp::proj4string(df) <- sp::CRS(paste('+proj=utm +zone=', 
                                           gsub("[^0-9]", "", utmZone), " ellps=WGS84",
                                           sep=''))
    } else {
      epsg.z <- relevantEPSGs$code[grep(paste('+proj=utm +zone=', 
                                              gsub('[^0-9]', '', utmZone), sep=''), 
                                        relevantEPSGs$prj4, fixed=T)]
      raster::crs(df) <- sp::CRS(paste('+init=epsg:', epsg.z, sep=''))
    }
    transf <- sp::spTransform(df, sp::CRS('+proj=longlat'))
    latLong <- data.frame(cbind(sp::coordinates(transf), transf$rowid))
    names(latLong) <- c('decimalLongitude', 'decimalLatitude', 'rowid')
    
    # merge missing values back in
    names(dfN)[which(names(dfN) %in% c('easting','northing'))] <- c('decimalLatitude','decimalLongitude')
    dfN$decimalLatitude[which(is.na(dfN$decimalLongitude))] <- NA
    dfN$decimalLongitude[which(is.na(dfN$decimalLatitude))] <- NA
    
    all.return <- suppressWarnings(data.table::rbindlist(list(latLong, dfN), fill=T))
    all.return <- all.return[order(all.return$rowid),]
    
    return(all.return[,c('decimalLatitude','decimalLongitude')])
    
  }

