##############################################################################################
#' @title Calculate more precise geolocations for specific NEON data products

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON data products, based on product-specific rules and spatial designs.
#' 
#' @param data A data frame containing NEON named locations and other sampling information. For reliable results, use data tables as downloaded from the NEON data portal or API.
#' @param dataProd The table name of the NEON data product table to find locations for. Must be one of: ltr_pertrap, hbp_perbout, sls_soilCoreCollection, brd_perpoint or brd_countdata, mam_pertrapnight, div_1m2Data or div_10m2Data100m2Data, vst_mappingandtagging.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples 
#' d <- data.frame(namedLocation="GUAN_044.basePlot.ltr", subplotID=23, trapID="GUAN_044_385")
#' getLocTOS(d, "ltr_pertrap")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-09-03)
#     adapted from and replaces def.calc.geo.os()
##############################################################################################
getLocTOS <- function(
  data,
  dataProd
){
  
  # convert format for safety
  data <- data.frame(data)
  
    # Litter trap, herb clip, cfc herb clip, and root sampling location calculations
    # These are all protocols using clip strips
    if(dataProd=="ltr_pertrap" | dataProd=="hbp_perbout" | dataProd=="cfc_fieldData" | 
       dataProd=="bbc_percore"){
    
    # Concatenate the named location (the plot) and subplot IDs to get the 
    #      subplot named locations
    subplots <- paste(data$namedLocation, data$subplotID, sep=".")
    data <- cbind(data, subplots)
    data$rowid <- 1:nrow(data)
    
    if(dataProd=="cfc_fieldData") {
      dataN <- data[which(data$clipID=="" | is.na(data$clipID)),]
      data <- data[which(data$clipID!="" & !is.na(data$clipID)),]
    }
    
    # Use the getLocByName function to pull the subplot geolocations from the API
    locCol <- "subplots"
    subplot.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T)
    
    # Use relevant columns
    subplot.merg <- subplot.all[,c("namedLocation","utmZone",
                                     "northing","easting","namedLocationCoordUncertainty",
                                     "decimalLatitude","decimalLongitude",
                                     "elevation","namedLocationElevUncertainty")]
    colnames(subplot.merg) <- c(locCol, 'utmZone',"adjNorthing","adjEasting",
                                        "adjCoordinateUncertainty","adjDecimalLatitude",
                                        "adjDecimalLongitude","adjElevation",
                                        "adjElevationUncertainty")
    subplot.loc <- base::merge(data, subplot.merg, by=locCol, all.x=T)
    subplot.loc <- subplot.loc[order(subplot.loc$rowid),]
    
    # Strip the final 3 digits of trapID to get the clip cell numbers
    if(dataProd=="ltr_pertrap") {
      cellID <- data$trapID
      data$cellID <- cellID
    } else {
      if(dataProd=="hbp_perbout" | dataProd=="cfc_fieldData" | dataProd=="bbc_percore") {
        cellID <- data$clipID
        data$cellID <- cellID
      }
    }
    cellNum <- as.numeric(substr(data$cellID, 10, 12))
    eastOff <- numeric(length(cellNum))
    northOff <- numeric(length(cellNum))
    subplot.loc <- cbind(subplot.loc, cellID, cellNum, eastOff, northOff)
    
    # Look up the clip cell numbers in the clip cell table (included in this package),
    #      check that the clip cell-subplot combination is valid, and find the 
    #      easting and northing offsets
    for(i in 1:nrow(subplot.loc)) {
      clipInd <- which(clipCell$clipCellNumber==subplot.loc$cellNum[i] & 
                         clipCell$pointID==data$subplotID[i])

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
      print("Please note locations have been calculated only for herbaceous clip samples. Woody vegetation sample locations can be calculated using the woody vegetation structure data product.")
    } else {
      all.return <- subplot.loc
    }
    all.return <- all.return[order(all.return$rowid),]
    all.return <- all.return[,!names(all.return) %in% c('rowid','cellID')]
    return(all.return)
  }
  
  # Soil core location calculations:
  if(dataProd=="sls_soilCoreCollection") {
    
    data$rowid <- 1:nrow(data)
    
    # Use the getLocByName function to pull the plot geolocations from the API
    locCol <- "namedLocation"
    plot.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T)
    
    # Use relevant columns
    plot.merg <- plot.all[,c("namedLocation","utmZone",
                                   "northing","easting","namedLocationCoordUncertainty",
                                   "decimalLatitude","decimalLongitude",
                                   "elevation","namedLocationElevUncertainty")]
    colnames(plot.merg) <- c(locCol, 'utmZone',"adjNorthing","adjEasting",
                                "adjCoordinateUncertainty","adjDecimalLatitude",
                                "adjDecimalLongitude","adjElevation",
                                "adjElevationUncertainty")
    plot.loc <- base::merge(data, subplot.merg, by=locCol, all.x=T)
    plot.loc <- subplot.loc[order(subplot.loc$rowid),]
    
    # Subtract 20 meters from the easting and northing values to get the 
    # location of the southwest corner
    plot.loc$adjEasting <- as.numeric(plot.loc$adjEasting) - 20
    plot.loc$adjNorthing <- as.numeric(plot.loc$adjNorthing) - 20
    
    # Add coreCoordinateX to the easting value and coreCoordinateY to the northing value
    plot.loc$adjEasting <- plot.loc$adjEasting + data$coreCoordinateX
    plot.loc$adjNorthing <- plot.loc$adjNorthing + data$coreCoordinateY
    
    # Set the coordinate uncertainty to 0.5 meter
    plot.loc$adjCoordinateUncertainty <- 0.5
    
    # calculate latitude and longitude from the corrected northing and easting
    adjLatLong <- geoNEON::calcLatLong(easting=plot.loc$adjEasting, 
                                       northing=plot.loc$adjNorthing,
                                       utmZone=plot.loc$utmZone)
    plot.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
    plot.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
    
    # reorder to original order
    all.return <- plot.loc[order(plot.loc$rowid),]
    all.return <- all.return[,!names(all.return) %in% c('rowid')]
    
    return(all.return)
  }
  
  # Bird point calculations:
  if(dataProd=="brd_perpoint" | dataProd=="brd_countdata") {
    #check to make sure pointID is in the name of the file
    if (!'pointID'%in%names(data)){stop('pointID is a required input to this function')}
    
    # Concatenate the named location (the plot) and point IDs to get the 
    #      point named locations
    points <- paste(data$namedLocation, data$pointID, sep=".")
    data <- cbind(data, points)
    data$points<-as.character(data$points)
    
    #if it's an 88 bird, the only resolution is SITE
    
    if (dataProd=="brd_countdata"){
      data$points[data$pointCountMinute==88]<-substr(data$namedLocation[data$pointCountMinute==88], 1,4)
    }
    
    # Use the getLocByName function to pull the subplot geolocations from the API
    locCol <- "points"
    point.loc <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T)
    names(point.loc)[names(point.loc)=='namedLocation']<-locCol

    #add additional coordinateUncertainty
    point.loc$additionalUncertainty<-NA
    #monumented corners, no additional uncertainty, GPS readings from here
    point.loc$additionalUncertainty[grepl('\\.21$', point.loc[[locCol]])]<-0
    #monumented grid centers, no additional uncertainty, GPS readings from here
    point.loc$additionalUncertainty[grepl('\\.B2$', point.loc[[locCol]])]<-0
    
    #sum uncertainties
    point.loc$adjCoordinateUncertainty <- as.numeric(point.loc$namedLocationCoordUncertainty) + point.loc$additionalUncertainty
    #rest navigated to with recreational GPS, uncertainty of ~15m, not provided in spatial data
    point.loc$adjCoordinateUncertainty[!grepl('\\.21$|\\.B2$',point.loc[[locCol]])] <- 15
    
    #88 points uncertainty unknown, all that's being provided is the site (aka tower) location
    point.loc$adjCoordinateUncertainty[is.na(point.loc$Value.for.Point.ID)]<-NA
    point.loc$namedLocationElevUncertainty[is.na(point.loc$Value.for.Point.ID)]<-NA
    
    # Return relevant columns
    point.return <- point.loc[,c(locCol, 
                               "utmZone","northing","easting",
                               "adjCoordinateUncertainty",
                               "decimalLatitude","decimalLongitude",
                               "elevation","namedLocationElevUncertainty")]
    
    col.name.list <- names(point.return)
    col.name.list <- gsub('northing','adjNorthing', col.name.list)
    col.name.list <- gsub('easting','adjEasting', col.name.list)
    col.name.list <- gsub('decimalLatitude','adjDecimalLatitude', col.name.list)
    col.name.list <- gsub('decimalLongitude','adjDecimalLongitude', col.name.list)
    col.name.list <- gsub('elevation','adjElevation', col.name.list)
    col.name.list <- gsub('namedLocationElevUncertainty','adjElevationUncertainty', col.name.list)
    colnames(point.return) <- col.name.list
    
    data$row.index <- 1:nrow(data)
    all.return <- merge(data, point.return, by=locCol)
    all.return <- all.return[order(all.return$row.index),]
    all.return <- all.return[,!names(all.return) %in% 'row.index']
    return(all.return)
  }
  
  #Plant phenology individual location calculations:
  if(dataProd=="phe_perindividual") {
    data$row.index <- 1:nrow(data)

    #for phenocamRows, the sampleLat, long, datum, etc are correct
    phenocamRows <- data[data$subtypeSpecification=='phenocam',]
    phenocamRows$geodeticDatum <- phenocamRows$sampleGeodeticDatum
    phenocamRows$adjDecimalLatitude <- phenocamRows$sampleLatitude
    phenocamRows$adjDecimalLongitude <- phenocamRows$sampleLongitude
    phenocamRows$adjCoordinateUncertainty <- phenocamRows$sampleCoordinateUncertainty
    phenocamRows$adjElevation <- phenocamRows$sampleElevation
    phenocamRows$adjElevationUncertainty <- phenocamRows$sampleElevationUncertainty

    data$tempLat <- data$decimalLatitude
    data$tempLong <- data$decimalLongitude
    data <- data[!data$subtypeSpecification=='phenocam',]
    corners <- data.frame(namedLocation=paste(unique(data$namedLocation), 
                                              c('N', 'E', 'S', 'W', 'NE', 'SE', 'SW', 'NW'), sep="."), vals=NA)

    # Use the getLocByName function to pull the subplot geolocations from the API
    locCol="namedLocation"
    pointSpatialData <- geoNEON::getLocByName(corners, locCol=locCol, locOnly=T)

    #exception handling for missing inputs
    nogeo <- data[which(is.na(data$transectMeter) | is.na(data$directionFromTransect)),]

    if (nrow(nogeo)>0){
      data <- data[-which(is.na(data$transectMeter) | is.na(data$directionFromTransect)),]
    }

    data$referencePoint_tempA<-NA
    data$referencePoint_tempB<-NA

    #exceptions in R -> this just will pass the NA if transectMeter is NA
    data$referencePoint_tempA[data$transectMeter>0&data$transectMeter<=100]<-'SW'
    data$referencePoint_tempB[data$transectMeter>0&data$transectMeter<=100]<-'W'
    data$referencePoint_tempA[data$transectMeter>100&data$transectMeter<=200]<-'W'
    data$referencePoint_tempB[data$transectMeter>100&data$transectMeter<=200]<-'NW'
    data$referencePoint_tempA[data$transectMeter>200&data$transectMeter<=300]<-'NW'
    data$referencePoint_tempB[data$transectMeter>200&data$transectMeter<=300]<-'N'
    data$referencePoint_tempA[data$transectMeter>300&data$transectMeter<=400]<-'N'
    data$referencePoint_tempB[data$transectMeter>300&data$transectMeter<=400]<-'NE'
    data$referencePoint_tempA[data$transectMeter>400&data$transectMeter<=500]<-'NE'
    data$referencePoint_tempB[data$transectMeter>400&data$transectMeter<=500]<-'E'
    data$referencePoint_tempA[data$transectMeter>=500&data$transectMeter<=600]<-'E'
    data$referencePoint_tempB[data$transectMeter>=500&data$transectMeter<=600]<-'SE'
    data$referencePoint_tempA[data$transectMeter>600&data$transectMeter<=700]<-'SE'
    data$referencePoint_tempB[data$transectMeter>600&data$transectMeter<=700]<-'S'
    data$referencePoint_tempA[data$transectMeter>700&data$transectMeter<=800]<-'S'
    data$referencePoint_tempB[data$transectMeter>700&data$transectMeter<=800]<-'SW'

    #determine which way the offsets are going
    data$offset_sign<-NA
    data$offset_sign[data$transectMeter<=200&
                       data$directionFromTransect=='Left']<-'W'
    data$offset_sign[data$transectMeter>200&
                       data$transectMeter<=400&data$directionFromTransect=='Left']<-'N'
    data$offset_sign[data$transectMeter>400&data$transectMeter<=600&data$directionFromTransect=='Left']<-'E'
    data$offset_sign[data$transectMeter>600&
                       data$transectMeter<=800&data$directionFromTransect=='Left']<-'S'

    data$offset_sign[data$transectMeter<=200&
                       data$directionFromTransect=='Right']<-'E'

    data$offset_sign[data$transectMeter>200&

                       data$transectMeter<=400&data$directionFromTransect=='Right']<-'S'
    data$offset_sign[data$transectMeter>400&data$transectMeter<=600&data$directionFromTransect=='Right']<-'W'
    data$offset_sign[data$transectMeter>600&
                       data$transectMeter<=800&data$directionFromTransect=='Right']<-'N'

    #determine distance from last point by subtraction
    calcDistanceFromLast<-function(distance){
      while(distance>100){
        distance<-distance-100
      }
      return (distance)
    }

    data$distFromLastPoint<-NA

    for (i in 1:nrow(data)){
      if (!is.na(data$transectMeter[i])){
        data$distFromLastPoint[i]<-calcDistanceFromLast(data$transectMeter[i])
      }
    }

    data$northing<-NA
    data$easting<-NA
    data$utmZone<-NA
    data$namedLocation<-as.character(data$namedLocation)
    pointSpatialData$namedLocation<-as.character(pointSpatialData$namedLocation)

    for (i in 1:nrow(data)){
      if (!is.na(data$referencePoint_tempA[i])&
          !is.na(data$referencePoint_tempB[i])){
        northingA<-as.numeric(pointSpatialData$northing[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
                                                          pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]])
        northingB<-as.numeric(pointSpatialData$northing[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
                                                          pointSpatialData$Value.for.Point.ID==data$referencePoint_tempB[i]])
        eastingA<-as.numeric(pointSpatialData$easting[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
                                                        pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]])
        eastingB<-as.numeric(pointSpatialData$easting[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
                                                        pointSpatialData$Value.for.Point.ID==data$referencePoint_tempB[i]])

        elevationA<-as.numeric(pointSpatialData$elevation[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
                                                            pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]])
        elevationB<-as.numeric(pointSpatialData$elevation[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
                                                            pointSpatialData$Value.for.Point.ID==data$referencePoint_tempB[i]])
        wt<-c(100-data$distFromLastPoint[i],data$distFromLastPoint[i])
        if (length(c(northingA, northingB, eastingA, eastingB))==4){#make sure all elements known
          northing <- stats::weighted.mean(c(northingA,northingB), wt)
          easting <- stats::weighted.mean(c(eastingA,eastingB), wt)
          data$adjCoordinateUncertainty[i]<-max (as.numeric(pointSpatialData$namedLocationCoordUncertainty[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i]&
                                                                                                     pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]]),
                                                 as.numeric(pointSpatialData$namedLocationCoordUncertainty[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i]&
                                                                                                     pointSpatialData$Value.for.Point.ID==data$referencePoint_tempB[i]]))+2
        }else{
          northing<-NA
          easting<-NA
          data$adjCoordinateUncertainty[i]<-NA
        }
        if (length(c(elevationA, elevationB))==2){#make sure all elements known
          data$adjElevation[i]<- stats::weighted.mean(c(elevationA,elevationB), wt)
        }else{
          data$adjElevation[i]<- NA
        }


        #then add the distance from transect
        if (data$offset_sign[i]=='N'){
          northing<-northing+data$ninetyDegreeDistance[i]
        }
        if (data$offset_sign[i]=='E'){
          easting<-easting+data$ninetyDegreeDistance[i]
        }
        if (data$offset_sign[i]=='W'){
          easting<-easting-data$ninetyDegreeDistance[i]
        }
        if (data$offset_sign[i]=='S'){
          northing<-northing-data$ninetyDegreeDistance[i]
        }
        data$easting[i]<-easting
        data$northing[i]<-northing
        if (!is.na(northing)&&!is.na(easting)){
          data$utmZone[i]<-pointSpatialData$utmZone[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i]&
                                                      pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]]
        }
      }
    }

    #don't transform the missing points
    if(length(which(is.na(data$easting)|is.na(data$northing)))>0) {
      nogeo <- data.table::rbindlist(list(nogeo, data[is.na(data$easting)|is.na(data$northing),]), 
                                     fill=T)
      data <- data[-which(is.na(data$easting)|is.na(data$northing)),]
    }

    #elevation uncertainty is really unknowable without knowing the microterrain
    data$adjElevationUncertainty<-NA

    # calculate latitude and longitude from the corrected northing and easting
    data <- def.calc.latlong(data)
    
    names(data)[names(data)=='easting'] <- 'adjEasting'
    names(data)[names(data)=='northing'] <- 'adjNorthing'
    names(data)[names(data)=='decimalLatitude'] <- 'adjDecimalLatitude'
    names(data)[names(data)=='decimalLongitude'] <- 'adjDecimalLongitude'
    names(data)[names(data)=='tempLat'] <- 'decimalLatitude'
    names(data)[names(data)=='tempLong'] <- 'decimalLongitude'
    
    # merge with other data
    if (nrow(nogeo)>0){
      data <- data.table::rbindlist(list(data, nogeo), fill=T)
    }

    if (nrow(phenocamRows)>0){
      data <- data.table::rbindlist(list(data, phenocamRows), fill=T)
    }

    #cleanup
    data<-data[order(data$row.index),]
    data<-data[,!names(data) %in% c('row.index','referencePoint_tempA', 'referencePoint_tempB',
                       'offset_sign', 'distFromLastPoint', 'tempLat', 'tempLong')]
    return(data)
  }

  
  #Small mammal trap locations:
  if(dataProd=="mam_pertrapnight"){
    
    # Concatenate the named location (the plot) and trapCoordinate to get the 
    #      point named locations
    points <- paste(data$namedLocation, data$trapCoordinate, sep=".")
    data <- cbind(data, points)
    
    #remove any X columns
    
    # Use the getLocByName function to pull the subplot geolocations from the API
    # Don't bother looking up any of the 'X' traps - those have uncertain geolocations
    locCol <- "points"
    point.loc <- getLocByName(data[!grepl('X', data$points),], locCol=locCol, locOnly=T)
    names(point.loc)[names(point.loc)=='namedLocation']<-locCol
    point.loc$adjCoordinateUncertainty<-as.numeric(point.loc$namedLocationCoordUncertainty)
    
    #coordinate uncertainty is only provided for select locations per grid that
    # are monumented; assume since the rest of the grid is set up relative
    # to those monumented points, that the max per monumented point applies
    # to the rest of the points
    
    maxUncPerGrid<-data.frame(tapply(point.loc$adjCoordinateUncertainty, point.loc$points, FUN=function(x) if(all(is.na(x))){NA} else {max(x, na.rm=T)}))
    maxUncPerGrid$points<-row.names(maxUncPerGrid)
    names(maxUncPerGrid)[1]<-'maxUncertainty'
    
    point.loc<-merge(point.loc, maxUncPerGrid)
    
    #add additional coordinateUncertainty 3m for nonmonumented, 1m otherwise
    
    #monumented corners, no additional uncertainty, GPS readings from here
    tot.unc <- function(adjCoordinateUncertainty,maxUncertainty){
      if (is.na(adjCoordinateUncertainty)){unc=maxUncertainty+3}else{unc=adjCoordinateUncertainty+1}
      return(unc)
    }
    point.loc$tot.unc <- mapply(tot.unc, point.loc$adjCoordinateUncertainty, point.loc$maxUncertainty)
    
    # calculate latitude and longitude from the corrected northing and easting
    point.loc <- def.calc.latlong(point.loc)
    
    # Return relevant columns
    point.return <- point.loc[,c(locCol,"utmZone",
                                 "northing","easting","tot.unc",
                                 "decimalLatitude","decimalLongitude",
                                 "elevation","namedLocationElevUncertainty")]
    colnames(point.return) <- c("points", "utmZone", "adjNorthing", "adjEasting", 
                                "adjCoordinateUncertainty","adjDecimalLatitude",
                                "adjDecimalLongitude","adjElevation",
                                "adjElevationUncertainty")
    data$row.index<-1:nrow(data)
    all.return <- merge(data,point.return, by=locCol, all.x=T)
    all.return<-all.return[order(all.return$row.index),]
    all.return<-all.return[,!names(all.return)%in%'row.index']
    return(all.return)
  }
  
  #Plant present and percent cover subplot centroids:
  if(dataProd=="div_1m2Data"|dataProd=="div_10m2Data100m2Data"){
    
    # Concatenate the named location (the plot) and trapCoordinate to get the 
    #      point named locations
    subplots <- paste(data$namedLocation, data$subplotID, sep=".")
    data <- cbind(data, subplots)
    
    # Use the getLocByName function to pull the subplot geolocations from the API
    locCol="subplots"
    subplot.loc <- getLocByName(data, locCol=locCol, locOnly=T)
    names(subplot.loc)[names(subplot.loc)=='namedLocation']<-locCol
    subplot.loc$adjCoordinateUncertainty<-as.numeric(subplot.loc$namedLocationCoordUncertainty)
    
    #Increase coordinateUncertainty by an appropriate amount to account for error
    # introduced by navigating within plots. Additional error may be introduced due to
    # to tape stretching to navigate to locations within plots
    # and is estimated as:
    # 0.25m for 1m2 subplot centroids
    # 1.0m for 10m2 subplot centroids
    # 2.0m for 100m2 subplot centroids
    
    subplot.loc$adjCoordinateUncertainty[grepl('\\.1$', subplot.loc$subplotID)]<-
      subplot.loc$adjCoordinateUncertainty[grepl('\\.1$', subplot.loc$subplotID)]+0.25
    
    subplot.loc$adjCoordinateUncertainty[grepl('\\.10$', subplot.loc$subplotID)]<-
      subplot.loc$adjCoordinateUncertainty[grepl('\\.10$', subplot.loc$subplotID)]+0.1
    
    subplot.loc$adjCoordinateUncertainty[!grepl('\\.10$|\\.1$', subplot.loc$subplotID)]<-
      subplot.loc$adjCoordinateUncertainty[!grepl('\\.10$|\\.1$', subplot.loc$subplotID)]+2
    
    subplot.loc$adjDecimalLatitude <- subplot.loc$decimalLatitude
    subplot.loc$adjDecimalLongitude <- subplot.loc$decimalLongitude
    subplot.loc$adjElevation <- subplot.loc$elevation
    subplot.loc$adjElevationUncertainty <- subplot.loc$namedLocationElevUncertainty
    subplot.loc$adjNorthing <- subplot.loc$northing
    subplot.loc$adjEasting <- subplot.loc$easting
    
    # Return relevant columns
    subplot.return <- subplot.loc[,c(locCol,"utmZone",
                                     "adjNorthing","adjEasting","adjCoordinateUncertainty",
                                     "adjDecimalLatitude","adjDecimalLongitude",
                                     "adjElevation","adjElevationUncertainty")]
    data$row.index<-1:nrow(data)
    all.return <- merge(data, subplot.return, by=locCol, all.x=T)
    all.return<-all.return[order(all.return$row.index),]
    all.return<-all.return[,!names(all.return)%in%'row.index']
    return(all.return)
  }
  
  # woody vegetation structure locations of individuals
  if(dataProd=='vst_mappingandtagging'){
    
    # Pull out data with no pointID
    data$rowid <- 1:nrow(data)
    dataN <- data[which(is.na(data$pointID)),]
    data <- data[which(!is.na(data$pointID)),]
    
    # Concatenate the named location (the plot) and point IDs to get the 
    #      point named locations
    points <- paste(data$namedLocation, data$pointID, sep=".")
    data <- cbind(data, points)
    
    # Use the getLocByName function to pull the subplot geolocations from the API
    locCol <- "points"
    point.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T)
    
    # Use relevant columns
    point.all <- point.all[,c("namedLocation","utmZone",
                                 "northing","easting","namedLocationCoordUncertainty",
                                 "decimalLatitude","decimalLongitude",
                                 "elevation","namedLocationElevUncertainty")]
    names(point.all) <- c(locCol,"utmZone",
                             "adjNorthing","adjEasting","adjCoordinateUncertainty",
                             "adjDecimalLatitude","adjDecimalLongitude",
                             "adjElevation","adjElevationUncertainty")
    
    # merge location data with original data
    point.loc <- merge(data, point.all, by="points", all.x=T)
    
    # Calculate easting and northing for individuals
    point.loc$adjEasting <- as.numeric(point.loc$adjEasting) + point.loc$stemDistance * 
      sin((point.loc$stemAzimuth * pi) / 180)
    point.loc$adjNorthing <- as.numeric(point.loc$adjNorthing) + point.loc$stemDistance * 
      cos((point.loc$stemAzimuth * pi) / 180)
    
    # Increase coordinate uncertainties
    point.loc$adjCoordinateUncertainty <- 
      as.numeric(point.loc$adjCoordinateUncertainty) + 0.6
    point.loc$adjElevationUncertainty <- 
      as.numeric(point.loc$adjElevationUncertainty) + 1
    
    # calculate latitude and longitude from the corrected northing and easting
    adjLatLong <- geoNEON::calcLatLong(easting=point.loc$adjEasting, 
                                       northing=point.loc$adjNorthing,
                                       utmZone=point.loc$utmZone)
    point.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
    point.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
    
    # add back in individuals that weren't mapped
    all.return <- plyr::rbind.fill(point.loc, dataN)
    all.return <- all.return[order(all.return$rowid),]
    all.return <- all.return[,!names(all.return) %in% c('rowid','points'))]
    
    return(all.return)
    
  }
  
  if(dataProd=='cdw_fieldtally') {
    
    # Pull out data with no distance or azimuth
    data$rowid <- 1:nrow(data)
    dataN <- data[which(is.na(data$logDistance) | is.na(data$lidsAzimuth)),]
    data <- data[which(!is.na(data$logDistance) & !is.na(data$lidsAzimuth)),]
    
    # Use the getLocByName function to pull the subplot geolocations from the API
    locCol <- "namedLocation"
    plot.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T)
    
    # Use relevant columns
    plot.all <- plot.all[,c("namedLocation","utmZone",
                              "northing","easting","namedLocationCoordUncertainty",
                              "decimalLatitude","decimalLongitude",
                              "elevation","namedLocationElevUncertainty")]
    names(plot.all) <- c(locCol,"utmZone",
                          "adjNorthing","adjEasting","adjCoordinateUncertainty",
                          "adjDecimalLatitude","adjDecimalLongitude",
                          "adjElevation","adjElevationUncertainty")
    
    # merge location data with original data
    plot.loc <- merge(data, plot.all, by="namedLocation", all.x=T)
    
    # Calculate easting and northing from distance and azimuth,
    # adding 3 to distance because transects start at a 3 meter radius from plot centroid
    plot.loc$adjEasting <- as.numeric(plot.loc$adjEasting) + (plot.loc$logDistance + 3) * 
      sin((plot.loc$lidsAzimuth * pi) / 180)
    plot.loc$adjNorthing <- as.numeric(plot.loc$adjNorthing) + (plot.loc$logDistance + 3) * 
      cos((plot.loc$lidsAzimuth * pi) / 180)
    
    # Increase coordinate uncertainties by reasonable estimate for navigation error
    plot.loc$adjCoordinateUncertainty <- 
      as.numeric(plot.loc$adjCoordinateUncertainty) + 1
    plot.loc$adjElevationUncertainty <- 
      as.numeric(plot.loc$adjElevationUncertainty) + 2
    
    # calculate latitude and longitude from the corrected northing and easting
    adjLatLong <- geoNEON::calcLatLong(easting=plot.loc$adjEasting, 
                                       northing=plot.loc$adjNorthing,
                                       utmZone=plot.loc$utmZone)
    plot.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
    plot.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
    
    # add back in individuals that weren't mapped
    all.return <- plyr::rbind.fill(plot.loc, dataN)
    all.return <- all.return[order(all.return$rowid),]
    all.return <- all.return[,-which(colnames(all.return)=='rowid')]
    
    return(all.return)
    
  }

  # if(dataProd=='cdw_densitylog') {
  #   
  #   # Pull out data with no distance or azimuth or pointID
  #   # Remaining data should be 'Relative' mappingMethod points
  #   data$rowid <- 1:nrow(data)
  #   dataN <- data[which(is.na(data$logDistance) | is.na(data$logAzimuth) | is.na(data$pointID)),]
  #   data <- data[which(!is.na(data$logDistance) & !is.na(data$logAzimuth) & !is.na(data$pointID)),]
  #   
  #   # Concatenate the named location (the plot) and point IDs to get the 
  #   #      point named locations
  #   points <- paste(data$namedLocation, data$pointID, sep=".")
  #   data <- cbind(data, points)
  #   
  #   # Use the getLocByName function to pull the subplot geolocations from the API
  #   locCol <- "points"
  #   point.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T)
  #   
  #   # Use relevant columns
  #   point.all <- point.all[,c("namedLocation","utmZone",
  #                           "northing","easting","namedLocationCoordUncertainty",
  #                           "decimalLatitude","decimalLongitude",
  #                           "elevation","namedLocationElevUncertainty")]
  #   names(point.all) <- c(locCol,"utmZone",
  #                        "adjNorthing","adjEasting","adjCoordinateUncertainty",
  #                        "adjDecimalLatitude","adjDecimalLongitude",
  #                        "adjElevation","adjElevationUncertainty")
  #   
  #   # merge location data with original data
  #   point.loc <- merge(data, point.all, by="points", all.x=T)
  #   
  #   # Calculate easting and northing from distance and azimuth
  #   point.loc$adjEasting <- as.numeric(point.loc$adjEasting) + point.loc$logDistance * 
  #     sin((point.loc$logAzimuth * pi) / 180)
  #   point.loc$adjNorthing <- as.numeric(point.loc$adjNorthing) + point.loc$logDistance * 
  #     cos((point.loc$logAzimuth * pi) / 180)
  #   
  #   # Increase coordinate uncertainties by reasonable estimate for navigation error
  #   point.loc$adjCoordinateUncertainty <- 
  #     as.numeric(point.loc$adjCoordinateUncertainty) + 1
  #   point.loc$adjElevationUncertainty <- 
  #     as.numeric(point.loc$adjElevationUncertainty) + 2
  #   
  #   # calculate latitude and longitude from the corrected northing and easting
  #   adjLatLong <- geoNEON::calcLatLong(easting=point.loc$adjEasting, 
  #                                      northing=point.loc$adjNorthing,
  #                                      utmZone=point.loc$utmZone)
  #   point.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
  #   point.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
  #   
  #   # Get utmZone for unmapped locations
  #   unm.all <- geoNEON::getLocByName(dataN, locCol='namedLocation', locOnly=T)
  #   unm.all <- unm.all[,c("namedLocation","utmZone")]
  #   dataN <- merge(dataN, unm.all, by='namedLocation', all.x=T)
  # 
  #   # Subset data without distance and azimuth to calculate GPS points
  #   dataG <- dataN[which(!is.na(dataN$sampleEasting) & !is.na(dataN$sampleNorthing)),]
  #   dataN <- dataN[which(is.na(dataN$sampleEasting) | is.na(dataN$sampleNorthing)),]
  #   
  #   # calculate latitude and longitude from GPS easting and northing
  #   adjLatLongGPS <- geoNEON::calcLatLong(easting=dataG$sampleEasting, 
  #                                         northing=dataG$sampleNorthing,
  #                                         utmZone=dataG$utmZone)
  #   dataG$adjDecimalLatitude <- adjLatLongGPS$decimalLatitude
  #   dataG$adjDecimalLongitude <- adjLatLongGPS$decimalLongitude
  #   dataG$adjCoordinateUncertainty <- 10
  #   
  #   # add back in individuals that weren't mapped
  #   all.return <- plyr::rbind.fill(point.loc, dataG)
  #   all.return <- plyr::rbind.fill(all.return, dataN)
  #   all.return <- all.return[order(all.return$rowid),]
  #   all.return <- all.return[,-which(colnames(all.return)=='rowid')]
  #   
  #   return(all.return)
  #   
  # }
  
    
  if(dataProd=="bet_fielddata") {
    
    data$rowid <- 1:nrow(data)
    
    # concatenate the named location with the trapID
    traps <- paste(data$namedLocation, data$trapID, sep=".")
    data <- cbind(data, traps)
    
    # Use the getLocByName function to pull the subplot geolocations from the API
    locCol <- "traps"
    trap.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T)
    
    # Use relevant columns
    trap.all <- trap.all[,c("namedLocation","utmZone",
                              "northing","easting","namedLocationCoordUncertainty",
                              "decimalLatitude","decimalLongitude",
                              "elevation","namedLocationElevUncertainty")]
    names(trap.all) <- c(locCol,"utmZone",
                          "adjNorthing","adjEasting","adjCoordinateUncertainty",
                          "adjDecimalLatitude","adjDecimalLongitude",
                          "adjElevation","adjElevationUncertainty")
    
    # merge location data with original data
    trap.loc <- merge(data, trap.all, by="traps", all.x=T)
    
    # increase coordinate uncertainty: traps may be moved up to 2 meters to avoid obstacles
    trap.loc$adjCoordinateUncertainty <- as.numeric(trap.loc$adjCoordinateUncertainty) + 2
    trap.loc$adjElevationUncertainty <- as.numeric(trap.loc$adjElevationUncertainty) + 1
    
    # sort to original order
    trap.return <- trap.loc[order(trap.loc$rowid),]
    trap.return <- trap.return[,-which(colnames(trap.return)=='rowid')]
    
    return(trap.return)
    
  }
  
  if(dataProd=="mos_trapping") {
    cat('Mosquito trapping location is flexible within the plot; locations provided in downloaded data are accurate.')
  }

  if(dataProd=="tck_fielddata") {
    cat('Ticks are sampled around the entire perimeter of the plot; locations provided in downloaded data are accurate.')
  }
  
  if(dataProd=='dhp_perimagefile') {
    
    data$rowid <- 1:nrow(data)
    
    # plot spatial data are in the dhp_perplot table, so need to download
    locCol <- 'namedLocation'
    plot.all <- geoNEON::getLocByName(data, locCol=locCol, locOnly=T)
    
    # Use relevant columns
    plot.all <- plot.all[,c("namedLocation","utmZone",
                            "northing","easting","namedLocationCoordUncertainty",
                            "decimalLatitude","decimalLongitude",
                            "elevation","namedLocationElevUncertainty")]
    names(plot.all) <- c(locCol,"utmZone",
                         "adjNorthing","adjEasting","adjCoordinateUncertainty",
                         "adjDecimalLatitude","adjDecimalLongitude",
                         "adjElevation","adjElevationUncertainty")
    
    # merge location data with original data
    plot.loc <- merge(data, plot.all, by="namedLocation", all.x=T)
    
    # adjust northing and easting using point offsets
    plot.loc$adjEasting <- as.numeric(plot.loc$adjEasting)
    plot.loc$adjNorthing <- as.numeric(plot.loc$adjNorthing)
    for(i in unique(plot.loc$pointID)) {
      eastOff.i <- dhpOffsets$eastOff[which(dhpOffsets$pointID==i)]
      northOff.i <- dhpOffsets$northOff[which(dhpOffsets$pointID==i)]
      
      # if point in data doesn't match any point in DHP offsets, skip and delete location data
      if(length(eastOff.i)==0 | length(northOff.i)==0) {
        cat(paste('Point ', i, ' not found in DHP points. Locations not calculated.', sep=''))
        plot.loc$easting[which(plot.loc$pointID==i)] <- NA
        plot.loc$northing[which(plot.loc$pointID==i)] <- NA
        next
      }
      
      # apply offsets
      plot.loc$adjEasting[which(plot.loc$pointID==i)] <- plot.loc$adjEasting[which(plot.loc$pointID==i)] + eastOff.i
      plot.loc$adjNorthing[which(plot.loc$pointID==i)] <- plot.loc$adjNorthing[which(plot.loc$pointID==i)] + northOff.i
    }
    
    # calculate latitude and longitude from the corrected northing and easting
    adjLatLong <- geoNEON::calcLatLong(easting=plot.loc$adjEasting, 
                                       northing=plot.loc$adjNorthing,
                                       utmZone=plot.loc$utmZone)
    plot.loc$adjDecimalLatitude <- adjLatLong$decimalLatitude
    plot.loc$adjDecimalLongitude <- adjLatLong$decimalLongitude
    
    # increase coordinate uncertainty: flexibility in camera placement
    plot.loc$adjCoordinateUncertainty <- as.numeric(plot.loc$adjCoordinateUncertainty) + 2
    plot.loc$adjElevationUncertainty <- as.numeric(plot.loc$adjElevationUncertainty) + 2
    
    # sort to original order
    plot.return <- plot.loc[order(plot.loc$rowid),]
    plot.return <- plot.return[,-which(colnames(plot.return)=='rowid')]
    
    return(plot.return)
  }
  
  else {
    print(paste("This function has not been configured for data product table ", 
                 dataProd, sep=""))
  }
}

