##############################################################################################
#' @title Extract geolocation data from NEON API

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Definition Function. Extract geolocation data from the NEON API, for a given set of named location values
#' 
#' @param data A data frame in which one column contains the named locations
#' @param locCol The column name of the column containing the named locations. Defaults to namedLocation
#' @param locOnly Boolean whether to return the full input data frame or just the extracted geolocations

#' @return A data frame of the geolocation data for the input named locations

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples 
#' d <- data.frame(namedLocation=c("GUAN_044.basePlot.ltr","GRSM_003.birdGrid.brd"), otherData=c(1,2))
#' def.extr.geo.os(d, "namedLocation")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   deprecated 2019-09-03 - use getLocByName()
#   inherits from getLocByName() for back compatibility
#   Claire Lunch (2017-02-03)
#     adapted from code written by
#   Sarah Elmendorf (2016)
##############################################################################################
def.extr.geo.os <- function(
  data,
  locCol = "namedLocation",
  locOnly=F
){
  
  getLocByName(data=data, locCol=locCol, locOnly=locOnly)
  
}

