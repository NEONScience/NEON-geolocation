##############################################################################################
#' @title Calculate more precise geolocations for NEON data sampled from clip strips

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON litter traps, herbaceous clip, root sampling, and herbaceous foliar sampling.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param dataProd The table name of the NEON data product table to find locations for. 
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2025-02-13)
##############################################################################################
getLocClip <- function(
    data,
    dataProd,
    token=NA_character_
){
  
  # Use the first two digits of the subplotID to get the pointID
  # Concatenate the named location (the plot) and the pointID to get the 
  #      point named locations
  pointIDs <- substring(data$subplotID, 1, 2)
  data$points <- paste(data$namedLocation, pointIDs, sep=".")
  data$rowid <- 1:nrow(data)
  
  if(dataProd=="cfc_fieldData") {
    dataN <- data[which(data$clipID=="" | is.na(data$clipID)),]
    data <- data[which(data$clipID!="" & !is.na(data$clipID)),]
  }
  
  # Use the getLocByName function to pull the subplot geolocations from the API
  locCol <- "points"
  
  # samplingImpractical records have subplotID = NA
  dataS <- data[which(is.na(data$subplotID)),]
  data <- data[which(!is.na(data$subplotID)),]
  
  subplot.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=TRUE, 
                                       history=TRUE, token=token)
  data <- plyr::rbind.fill(data, dataS)
  data <- data[order(data$rowid),]
  
  # Use relevant columns
  subplot.merg <- subplot.all[,c("namedLocation","utmZone",
                                 "northing","easting","namedLocationCoordUncertainty",
                                 "decimalLatitude","decimalLongitude",
                                 "elevation","namedLocationElevUncertainty",
                                 "current","locationStartDate","locationEndDate")]
  colnames(subplot.merg) <- c(locCol, "utmZone","adjNorthing","adjEasting",
                              "adjCoordinateUncertainty","adjDecimalLatitude",
                              "adjDecimalLongitude","adjElevation",
                              "adjElevationUncertainty",
                              "locationCurrent","locationStartDate","locationEndDate")
  if(!is.null(data$utmZone)) { 
    subplot.merg <- subplot.merg[,which(colnames(subplot.merg)!="utmZone")]
  }
  subplot.loc <- base::merge(data, subplot.merg, by=locCol, all.x=T)
  
  # keep location data that matches date of collection
  if(any(subplot.loc$locationCurrent=="FALSE")) {
    if(dataProd=="ltr_pertrap") {
      subplot.loc <- findDateMatch(subplot.loc, locCol="points", recDate="date")
    } else {
      subplot.loc <- findDateMatch(subplot.loc, locCol="points", recDate="collectDate")
    }
  }
  subplot.loc <- subplot.loc[order(subplot.loc$rowid),]
  
  # Strip the final 3 digits of trapID or clipID to get the clip cell numbers
  if(dataProd=="ltr_pertrap") {
    cellID <- data$trapID
    data$cellID <- cellID
  } else {
    if(dataProd=="hbp_perbout" | dataProd=="cfc_fieldData" | dataProd=="bbc_percore") {
      cellID <- data$clipID
      data$cellID <- cellID
    }
  }
  # clip cell is final 3 digits of cellID, which can be different lengths
  cellNum <- as.numeric(sapply(data$cellID, function(x) { substr(x, nchar(x)-2, nchar(x)) }))
  eastOff <- numeric(length(cellNum))
  northOff <- numeric(length(cellNum))
  subplot.loc <- cbind(subplot.loc, cellID, cellNum, eastOff, northOff)
  
  # Look up the clip cell numbers in the clip cell table (included in this package),
  #      check that the clip cell-subplot combination is valid, and find the 
  #      easting and northing offsets
  for(i in 1:nrow(subplot.loc)) {
    if(is.na(subplot.loc$cellNum[i])) {
      next
    }
    if(any(grepl(pattern="_400", x=data$subplotID[i], fixed=TRUE))) {
      clipInd <- which(clipCell$clipCellNumber==subplot.loc$cellNum[i] & 
                         clipCell$subplotID==data$subplotID[i])
    } else {
      clipInd <- which(clipCell$clipCellNumber==subplot.loc$cellNum[i] & 
                         clipCell$pointID==data$subplotID[i])
    }
    
    if(length(clipInd)==0) {
      print(paste("Subplot ", data$subplotID[i], " and clip cell ", 
                  subplot.loc$cellNum[i], " is not a valid location", sep=""))
      subplot.loc$eastOff[i] <- NA
      subplot.loc$northOff[i] <- NA
    } else {
      subplot.loc$eastOff[i] <- clipCell$offsetEasting[clipInd]
      subplot.loc$northOff[i] <- clipCell$offsetNorthing[clipInd]
    }
  }
  
  # Adjust the easting and northing values by the offset amounts found
  subplot.loc$adjEasting <- as.numeric(subplot.loc$adjEasting) + subplot.loc$eastOff
  subplot.loc$adjNorthing <- as.numeric(subplot.loc$adjNorthing) + subplot.loc$northOff
  subplot.loc$adjCoordinateUncertainty <- 
    as.numeric(subplot.loc$adjCoordinateUncertainty) + 1
  
  # calculate latitude and longitude from the corrected northing and easting
  adjLatLong <- geoNEON::calcLatLong(easting=subplot.loc$adjEasting, 
                                     northing=subplot.loc$adjNorthing,
                                     utmZone=subplot.loc$utmZone)
  subplot.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
  subplot.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
  
  if(dataProd=="cfc_fieldData") {
    all.return <- plyr::rbind.fill(subplot.loc, dataN)
    message("Please note locations have been calculated only for herbaceous clip samples. Woody vegetation sample locations can be calculated based on the vst_mappingandtagging table.")
  } else {
    all.return <- subplot.loc
  }
  all.return <- all.return[order(all.return$rowid),]
  all.return <- all.return[,!names(all.return) %in% c('rowid','cellID')]
  return(all.return)
 
}
