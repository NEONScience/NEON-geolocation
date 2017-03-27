##############################################################################################
#' @title Calculate more precise geolocations for specific NEON data products

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON data products, based on product-specific rules and spatial designs.
#' 
#' @param \code{data} A data frame containing either NEON named locations or geolocations. Field names of locations must match standard NEON location field names.
#' @param \code{dataProd} The data product ID of the NEON data product to find locations for. Must be one of: NEON.DP1.10033.001 (list will continue to expand over time)

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2017-02-08)
#     original creation
##############################################################################################
def.calc.geo.os <- function(
  data,
  dataProd
){
  
  # Litter trap location calculations:
  if(dataProd=="NEON.DP1.10033.001") {
    
    # Concatenate the named location (the plot) and subplot IDs to get the 
    #      subplot named locations
    subplots <- paste(data$namedLocation, data$subplotID, sep=".")
    data <- cbind(data, subplots)
    
    # Use the def.extr.geo.os function to pull the subplot geolocations from the API
    subplot.loc <- geoNEON::def.extr.geo.os(data, locCol="subplots")
    
    # Strip the final 3 digits of trapID to get the clip cell numbers
    cellNum <- as.numeric(substr(data$trapID, 10, 12))
    eastOff <- numeric(length(cellNum))
    northOff <- numeric(length(cellNum))
    subplot.loc <- cbind(subplot.loc, cellNum, eastOff, northOff)
    
    # Look up the clip cell numbers in the clip cell table (included in this package),
    #      check that the clip cell-subplot combination is valid, and find the 
    #      easting and northing offsets
    for(i in 1:nrow(subplot.loc)) {
      clipInd <- which(clipCell$clipCellNumber==subplot.loc$cellNum[i] & 
                         clipCell$pointID==data$subplotID[i])
      if(length(clipInd)==0) {
        print(paste("Subplot ", data$subplotID[i], "and clip cell ", 
                     subplot.loc$cellNum[i], "is not a valid location", sep=""))
        subplot.loc$eastOff[i] <- NA
        subplot.loc$northOff[i] <- NA
      } else {
        subplot.loc$eastOff[i] <- clipCell$offsetEasting[clipInd]
        subplot.loc$northOff[i] <- clipCell$offsetNorthing[clipInd]
      }
    }
    
    # Adjust the easting and northing values by the offset amounts found
    options(digits=15)
    subplot.loc$easting <- as.numeric(subplot.loc$easting) + subplot.loc$eastOff
    subplot.loc$northing <- as.numeric(subplot.loc$northing) + subplot.loc$northOff
    subplot.loc$coordinateUncertainty <- as.numeric(subplot.loc$coordinateUncertainty) + 1
    
    # Return relevant columns
    subplot.return <- subplot.loc[,c("domainID","siteID","data.locationName","utmZone",
                                     "northing","easting","coordinateUncertainty",
                                     "elevation","elevationUncertainty")]
    return(subplot.return)
  }
  
  else {
    print(paste("This function has not been configured for data product ", 
                 dataProd, sep=""))
  }

}
