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
#   Claire Lunch (2023-08-28): Modified to use terra package for conversion
##############################################################################################


calcLatLongByZone <- function(easting, northing, utmZone) {

  if(length(easting)!=length(northing)) {
    stop('Variables must be the same length.')
  }
  if (grepl('^-|S$', utmZone)){stop('This function only defined for Northern Hemisphere locations')}
  
    easting <- as.numeric(easting)
    northing <- as.numeric(northing)

    df <- cbind(easting, northing)
    df <- data.frame(df)
    
    # convert coordinates
    colnames(df) <- c("easting","northing")
    
    df <- terra::vect(df, geom=c("easting","northing"), 
                      crs=paste("+proj=utm +zone=", gsub("[^0-9]", "", utmZone), sep=""))
    
    dfconv <- terra::project(df, y="+proj=longlat")
    latLong <- terra::geom(dfconv, df=TRUE)
    
    names(latLong)[which(names(latLong)=="x")] <- "decimalLongitude"
    names(latLong)[which(names(latLong)=="y")] <- "decimalLatitude"
    
    return(latLong[,c("decimalLatitude","decimalLongitude")])
    
  }

