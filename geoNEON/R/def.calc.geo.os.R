##############################################################################################
#' @title Calculate more precise geolocations for specific NEON data products

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON data products, based on product-specific rules and spatial designs.
#' 
#' @param data A data frame containing either NEON named locations or geolocations. Field names of locations must match standard NEON location field names.
#' @param dataProd The table name of the NEON data product table to find locations for. Must be one of: ltr_pertrap, hbp_perbout, sls_soilCoreCollection, brd_perpoint (list will continue to expand over time)

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples 
#' d <- data.frame(namedLocation="GUAN_044.basePlot.ltr", subplotID=23, trapID="GUAN_044_385")
#' def.calc.geo.os(d, "ltr_pertrap")

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
  if(dataProd=="ltr_pertrap" | dataProd=="hbp_pertrap") {
    
    # Concatenate the named location (the plot) and subplot IDs to get the 
    #      subplot named locations
    subplots <- paste(data$namedLocation, data$subplotID, sep=".")
    data <- cbind(data, subplots)
    
    # Use the def.extr.geo.os function to pull the subplot geolocations from the API
    subplot.loc <- geoNEON::def.extr.geo.os(data, locCol="subplots")
    
    # Strip the final 3 digits of trapID to get the clip cell numbers
    if(dataProd=="ltr_pertrap") {
      cellID <- data$trapID
    } else {
      if(dataProd=="hbp_pertrap") {
        cellID <- data$clipID
      }
    }
    cellNum <- as.numeric(substr(cellID, 10, 12))
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
    
    # calculate latitude and longitude from the corrected northing and easting
    subplot.loc <- def.calc.latlong(subplot.loc)
    
    # Return relevant columns
    subplot.return <- subplot.loc[,c("data.locationName","utmZone",
                                     "northing","easting","coordinateUncertainty",
                                     "decimalLatitude","decimalLongitude",
                                     "elevation","elevationUncertainty")]
    colnames(subplot.return)[5:9] <- c("adjCoordinateUncertainty","adjDecimalLatitude",
                                       "adjDecimalLongitude","adjElevation",
                                       "adjElevationUncertainty")
    
    all.return <- cbind(data,subplot.return)
    return(all.return)
  }
  
  # Soil core location calculations:
  if(dataProd=="sls_soilCoreCollection") {
    
    # Use the def.extr.geo.os function to pull the plot geolocations from the API
    plot.loc <- geoNEON::def.extr.geo.os(data, locCol="namedLocation")
    
    # Subtract 20 meters from the easting and northing values to get the 
    # location of the southwest corner
    options(digits=15)
    plot.loc$easting <- as.numeric(plot.loc$easting) - 20
    plot.loc$northing <- as.numeric(plot.loc$northing) - 20
    
    # Add coreCoordinateX to the easting value and coreCoordinateY to the northing value
    plot.loc$easting <- plot.loc$easting + data$coreCoordinateX
    plot.loc$northing <- plot.loc$northing + data$coreCoordinateY
    
    # Set the coordinate uncertainty to 0.5 meter
    plot.loc$coordinateUncertainty <- 0.5
    
    # calculate latitude and longitude from the corrected northing and easting
    plot.loc <- def.calc.latlong(plot.loc)
    
    # Return relevant columns
    plot.return <- plot.loc[,c("data.locationName","utmZone",
                                     "northing","easting","coordinateUncertainty",
                               "decimalLatitude","decimalLongitude",
                                     "elevation","elevationUncertainty")]
    colnames(plot.return)[5:9] <- c("adjCoordinateUncertainty","adjDecimalLatitude",
                                       "adjDecimalLongitude","adjElevation",
                                       "adjElevationUncertainty")
    
    all.return <- cbind(data,plot.return)
    return(all.return)
  }
  # Bird point calculations:
  if(dataProd=="brd_perpoint") {
    #check to make sure pointID is in the name of the file
    if (!'pointID'%in%names(data)){stop('pointID is a required input to this function')}
    
    # Concatenate the named location (the plot) and point IDs to get the 
    #      point named locations
    points <- paste(data$namedLocation, data$pointID, sep=".")
    data <- cbind(data, points)
    data$points<-as.character(data$points)
    
    #if it's an 88 bird, the only resolution is SITE
    data$points[data$pointID==88]<-substr(data$namedLocation[data$pointID==88], 1,4)
    
    # Use the def.extr.geo.os function to pull the subplot geolocations from the API
    point.loc <- geoNEON::def.extr.geo.os(data, locCol="points")

    #add additional coordinateUncertainty
    point.loc$additionalUncertainty<-NA
    #monumented corners, no additional uncertainty, GPS readings from here
    point.loc$additionalUncertainty[grepl('\\.21$', point.loc$data.locationName)]<-0
    #monumented grid centers, no additional uncertainty, GPS readings from here
    point.loc$additionalUncertainty[grepl('\\.B2$', point.loc$data.locationName)]<-0
    
    #sum uncertainties
    point.loc$coordinateUncertainty <- as.numeric(point.loc$coordinateUncertainty) + point.loc$additionalUncertainty
    #rest navigated to with recreational GPS, uncertainty of ~15m, not provided in spatial data
    point.loc$coordinateUncertainty[!grepl('\\.21$|\\.B2$',point.loc$data.locationName)] <- 15
    
    #88 points uncertainty unknown, all that's being provided is the site (aka tower) location
    point.loc$coordinateUncertainty[is.na(point.loc$Value.for.Point.ID)]<-NA
    point.loc$elevationUncertainty[is.na(point.loc$Value.for.Point.ID)]<-NA
    
    # calculate latitude and longitude from the corrected northing and easting
    point.loc <- def.calc.latlong(point.loc)
    
    # Return relevant columns
    point.return <- point.loc[,c("data.locationName","utmZone",
                                 "northing","easting","coordinateUncertainty",
                                 "decimalLatitude","decimalLongitude",
                                 "elevation","elevationUncertainty")]
    colnames(point.return)[5:9] <- c("adjCoordinateUncertainty","adjDecimalLatitude",
                                       "adjDecimalLongitude","adjElevation",
                                       "adjElevationUncertainty")
    
    all.return <- cbind(data,point.return)
    return(all.return)
  }
  
  else {
    print(paste("This function has not been configured for data product table ", 
                 dataProd, sep=""))
  }
}



