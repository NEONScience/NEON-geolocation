##############################################################################################
#' @title Calculate more precise geolocations for specific NEON data products

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON data products, based on product-specific rules and spatial designs.
#' 
#' @param data A data frame containing either NEON named locations or geolocations. Field names of locations must match standard NEON location field names.
#' @param dataProd The table name of the NEON data product table to find locations for. Must be one of: ltr_pertrap, hbp_perbout, sls_soilCoreCollection,
#'  brd_perpoint, phe_perindividual (list will continue to expand over time)

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
  
    #Litter trap and herb clip location calculations:
    if(dataProd=="ltr_pertrap" | dataProd=="hbp_perbout" | dataProd=="cfc_fieldData"){
    
    # Concatenate the named location (the plot) and subplot IDs to get the 
    #      subplot named locations
    subplots <- paste(data$namedLocation, data$subplotID, sep=".")
    data <- cbind(data, subplots)
    
    if(dataProd=="cfc_fieldData") {
      data$row.index <- 1:nrow(data)
      dataN <- data[which(data$clipID=="" | is.na(data$clipID)),]
      data <- data[which(data$clipID!="" & !is.na(data$clipID)),]
    }
    
    # Use the def.extr.geo.os function to pull the subplot geolocations from the API
    locCol <- "subplots"
    subplot.loc <- geoNEON::def.extr.geo.os(data, locCol=locCol)
    
    # Strip the final 3 digits of trapID to get the clip cell numbers
    if(dataProd=="ltr_pertrap") {
      data$cellID <- data$trapID
    } else {
      if(dataProd=="hbp_perbout" | dataProd=="cfc_fieldData") {
        data$cellID <- data$clipID
      }
    }
    cellNum <- as.numeric(substr(data$cellID, 10, 12))
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
    options(digits=15)
    subplot.loc$easting <- as.numeric(subplot.loc$api.easting) + subplot.loc$eastOff
    subplot.loc$northing <- as.numeric(subplot.loc$api.northing) + subplot.loc$northOff
    subplot.loc$api.coordinateUncertainty <- 
      as.numeric(subplot.loc$api.coordinateUncertainty) + 1
    
    # calculate latitude and longitude from the corrected northing and easting
    names(subplot.loc)[names(subplot.loc)=='api.utmZone'] <- 'utmZone'
    subplot.loc <- def.calc.latlong(subplot.loc)
    names(subplot.loc)[names(subplot.loc)=='utmZone'] <- 'api.utmZone'
    
    # Return relevant columns
    subplot.return <- subplot.loc[,c(locCol,cellID,"api.utmZone",
                                     "northing","easting","api.coordinateUncertainty",
                                     "decimalLatitude","decimalLongitude",
                                     "api.elevation","api.elevationUncertainty")]
    colnames(subplot.return)[6:10] <- c("adjCoordinateUncertainty","adjDecimalLatitude",
                                       "adjDecimalLongitude","adjElevation",
                                       "adjElevationUncertainty")
    
    if(dataProd=="cfc_fieldData") {
      sub.return <- base::merge(data, subplot.return, by=c(locCol, cellID))
      all.return <- plyr::rbind.fill(sub.return, dataN)
      print("Please note locations have been calculated only for herbaceous clip samples. Woody vegetation sample locations can be calculated using the woody vegetation structure data product.")
    } else {
      data$row.index <- 1:nrow(data)
      all.return <- base::merge(data, subplot.return, by=c(locCol, cellID))
    }
    all.return <- all.return[order(all.return$row.index),]
    all.return <- all.return[,!names(all.return) %in% c('row.index','cellID')]
    return(all.return)
  }
  
  # Soil core location calculations:
  if(dataProd=="sls_soilCoreCollection") {
    
    # Use the def.extr.geo.os function to pull the plot geolocations from the API
    locCol="namedLocation"
    plot.loc <- geoNEON::def.extr.geo.os(data, locCol=locCol)
    
    # Subtract 20 meters from the easting and northing values to get the 
    # location of the southwest corner
    options(digits=15)
    plot.loc$easting <- as.numeric(plot.loc$api.easting) - 20
    plot.loc$northing <- as.numeric(plot.loc$api.northing) - 20
    
    # Add coreCoordinateX to the easting value and coreCoordinateY to the northing value
    plot.loc$easting <- plot.loc$easting + data$coreCoordinateX
    plot.loc$northing <- plot.loc$northing + data$coreCoordinateY
    
    # Set the coordinate uncertainty to 0.5 meter
    plot.loc$coordinateUncertainty <- 0.5
    
    # calculate latitude and longitude from the corrected northing and easting
    plot.loc$utmZone <- plot.loc$api.utmZone
    plot.loc <- geoNEON::def.calc.latlong(plot.loc)
    
    # Return relevant columns
    plot.return <- plot.loc[,c('uid',locCol,"utmZone",
                               "northing","easting","coordinateUncertainty",
                               "decimalLatitude","decimalLongitude",
                               "elevation","elevationUncertainty")]
    col.name.list <- names(plot.return)
    col.name.list <- gsub('coordinateUncertainty','adjCoordinateUncertainty', col.name.list)
    col.name.list <- gsub('decimalLatitude','adjDecimalLatitude', col.name.list)
    col.name.list <- gsub('decimalLongitude','adjDecimalLongitude', col.name.list)
    col.name.list <- gsub('elevation','adjElevation', col.name.list)
    col.name.list <- gsub('elevationUncertainty','adjElevationUncertainty', col.name.list)
    
    colnames(plot.return) <- col.name.list
    
    cols.keepers <- names(plot.return)[which(!names(plot.return) %in% names(data))]
    
    all.return <- cbind(data, plot.return[cols.keepers])
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
    
    # Use the def.extr.geo.os function to pull the subplot geolocations from the API
    locCol="points"
    point.loc <- geoNEON::def.extr.geo.os(data, locCol=locCol, locOnly=T)
    names(point.loc)[names(point.loc)=='data.locationName']<-locCol

    #add additional coordinateUncertainty
    point.loc$additionalUncertainty<-NA
    #monumented corners, no additional uncertainty, GPS readings from here
    point.loc$additionalUncertainty[grepl('\\.21$', point.loc[[locCol]])]<-0
    #monumented grid centers, no additional uncertainty, GPS readings from here
    point.loc$additionalUncertainty[grepl('\\.B2$', point.loc[[locCol]])]<-0
    
    #sum uncertainties
    point.loc$coordinateUncertainty <- as.numeric(point.loc$api.coordinateUncertainty) + point.loc$additionalUncertainty
    #rest navigated to with recreational GPS, uncertainty of ~15m, not provided in spatial data
    point.loc$coordinateUncertainty[!grepl('\\.21$|\\.B2$',point.loc[[locCol]])] <- 15
    
    #88 points uncertainty unknown, all that's being provided is the site (aka tower) location
    point.loc$coordinateUncertainty[is.na(point.loc$Value.for.Point.ID)]<-NA
    point.loc$api.elevationUncertainty[is.na(point.loc$Value.for.Point.ID)]<-NA
    
    # Return relevant columns
    point.return <- point.loc[,c(locCol,"api.utmZone",
                                 "api.northing","api.easting","coordinateUncertainty",
                                 "api.decimalLatitude","api.decimalLongitude",
                                 "api.elevation","api.elevationUncertainty")]
    colnames(point.return)[5:9] <- c("adjCoordinateUncertainty","adjDecimalLatitude",
                                       "adjDecimalLongitude","adjElevation",
                                       "adjElevationUncertainty")

    all.return <- cbind(data,point.return)
    return(all.return)
  }
  
  # Plant phenology individual location calculations:
  # if(dataProd=="phe_perindividual") {
  #   data$row.index<-1:nrow(data)
  #   
  #   #for phenocamRows, the sampleLat, long, datum, etc are correct
  #   phenocamRows<-data[data$subtypeSpecification=='phenocam',]
  #   phenocamRows$geodeticDatum<-phenocamRows$sampleGeodeticDatum
  #   phenocamRows$decimalLatitude<-phenocamRows$sampleLatitude
  #   phenocamRows$decimalLongitude<-phenocamRows$sampleLongitude
  #   phenocamRows$adjCoordinateUncertainty-phenocamRows$sampleCoordinateUncertainty
  #   phenocamRows$adjElevation<-phenocamRows$sampleElevation
  #   phenocamRows$adjElevationUncertainty<-phenocamRows$sampleElevationUncertainty
  #   
  #   data<-data[!data$subtypeSpecification=='phenocam',]
  #   corners <- data.frame(namedLocation=paste(unique(data$namedLocation), c('N', 'E', 'S', 'W', 'NE', 'SE', 'SW', 'NW'), sep="."), vals=NA)
  #   
  #   # Use the def.extr.geo.os function to pull the subplot geolocations from the API
  #   locCol="namedLocation"
  #   pointSpatialData <- geoNEON::def.extr.geo.os(corners, locCol=locCol, locOnly=T)
  #   
  #   #exception handling for missing inputs
  #   nogeo<-data[which(is.na(data$transectMeter)|is.na(data$directionFromTransect)),]
  #   
  #   if (nrow(nogeo)>0){
  #     data<-data[-which(is.na(data$transectMeter)|is.na(data$directionFromTransect)),]
  #   }
  #   
  #   data$referencePoint_tempA<-NA
  #   data$referencePoint_tempB<-NA
  #   
  #   #exceptions in R -> this just will pass the NA if transectMeter is NA
  #   data$referencePoint_tempA[data$transectMeter>0&data$transectMeter<=100]<-'SW'
  #   data$referencePoint_tempB[data$transectMeter>0&data$transectMeter<=100]<-'W'
  #   data$referencePoint_tempA[data$transectMeter>100&data$transectMeter<=200]<-'W'
  #   data$referencePoint_tempB[data$transectMeter>100&data$transectMeter<=200]<-'NW'
  #   data$referencePoint_tempA[data$transectMeter>200&data$transectMeter<=300]<-'NW'
  #   data$referencePoint_tempB[data$transectMeter>200&data$transectMeter<=300]<-'N'
  #   data$referencePoint_tempA[data$transectMeter>300&data$transectMeter<=400]<-'N'
  #   data$referencePoint_tempB[data$transectMeter>300&data$transectMeter<=400]<-'NE'
  #   data$referencePoint_tempA[data$transectMeter>400&data$transectMeter<=500]<-'NE'
  #   data$referencePoint_tempB[data$transectMeter>400&data$transectMeter<=500]<-'E'
  #   data$referencePoint_tempA[data$transectMeter>=500&data$transectMeter<=600]<-'E'
  #   data$referencePoint_tempB[data$transectMeter>=500&data$transectMeter<=600]<-'SE'
  #   data$referencePoint_tempA[data$transectMeter>600&data$transectMeter<=700]<-'SE'
  #   data$referencePoint_tempB[data$transectMeter>600&data$transectMeter<=700]<-'S'
  #   data$referencePoint_tempA[data$transectMeter>700&data$transectMeter<=800]<-'S'
  #   data$referencePoint_tempB[data$transectMeter>700&data$transectMeter<=800]<-'SW'
  #   
  #   #determine which way the offsets are going
  #   data$offset_sign<-NA
  #   data$offset_sign[data$transectMeter<=200&
  #                      data$directionFromTransect=='Left']<-'W'
  #   data$offset_sign[data$transectMeter>200&
  #                      data$transectMeter<=400&data$directionFromTransect=='Left']<-'N'
  #   data$offset_sign[data$transectMeter>400&data$transectMeter<=600&data$directionFromTransect=='Left']<-'E'
  #   data$offset_sign[data$transectMeter>600&
  #                      data$transectMeter<=800&data$directionFromTransect=='Left']<-'S'
  #   
  #   data$offset_sign[data$transectMeter<=200&
  #                      data$directionFromTransect=='Right']<-'E'
  #   
  #   data$offset_sign[data$transectMeter>200&
  #                      
  #                      data$transectMeter<=400&data$directionFromTransect=='Right']<-'S'
  #   data$offset_sign[data$transectMeter>400&data$transectMeter<=600&data$directionFromTransect=='Right']<-'W'
  #   data$offset_sign[data$transectMeter>600&
  #                      data$transectMeter<=800&data$directionFromTransect=='Right']<-'N'
  #   
  #   #determine distance from last point by subtraction
  #   calcDistanceFromLast<-function(distance){
  #     while(distance>100){
  #       distance<-distance-100
  #     }
  #     return (distance)
  #   }
  #   
  #   data$distFromLastPoint<-NA
  #   
  #   for (i in 1:nrow(data)){
  #     if (!is.na(data$transectMeter[i])){
  #       data$distFromLastPoint[i]<-calcDistanceFromLast(data$transectMeter[i])
  #     }  
  #   }
  #   
  #   data$northing<-NA
  #   data$easting<-NA
  #   data$utmZone<-NA
  #   data$coordinateUncertainty<-NA
  #   data$elevation<-NA
  #   data$namedLocation<-as.character(data$namedLocation)
  #   pointSpatialData$namedLocation<-as.character(pointSpatialData$namedLocation)
  #   
  #   for (i in 1:nrow(data)){
  #     if (!is.na(data$referencePoint_tempA[i])&
  #         !is.na(data$referencePoint_tempB[i])){
  #       northingA<-as.numeric(pointSpatialData$northing[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
  #                                                         pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]])
  #       northingB<-as.numeric(pointSpatialData$northing[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
  #                                                         pointSpatialData$Value.for.Point.ID==data$referencePoint_tempB[i]])
  #       eastingA<-as.numeric(pointSpatialData$easting[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
  #                                                       pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]])
  #       eastingB<-as.numeric(pointSpatialData$easting[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
  #                                                       pointSpatialData$Value.for.Point.ID==data$referencePoint_tempB[i]])
  #       
  #       elevationA<-as.numeric(pointSpatialData$elevation[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
  #                                                           pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]])
  #       elevationB<-as.numeric(pointSpatialData$elevation[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i] &
  #                                                           pointSpatialData$Value.for.Point.ID==data$referencePoint_tempB[i]])
  #       wt<-c(100-data$distFromLastPoint[i],data$distFromLastPoint[i])
  #       if (length(c(northingA, northingB, eastingA, eastingB))==4){#make sure all elements known
  #         northing <- stats::weighted.mean(c(northingA,northingB), wt)
  #         easting <- stats::weighted.mean(c(eastingA,eastingB), wt)
  #         data$adjCoordinateUncertainty[i]<-max (as.numeric(pointSpatialData$coordinateUncertainty[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i]&
  #                                                                                                    pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]]),
  #                                                as.numeric(pointSpatialData$coordinateUncertainty[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i]&
  #                                                                                                    pointSpatialData$Value.for.Point.ID==data$referencePoint_tempB[i]]))+2
  #       }else{
  #         northing<-NA
  #         easting<-NA
  #         data$adjCoordinateUncertainty[i]<-NA
  #       }
  #       if (length(c(elevationA, elevationB))==2){#make sure all elements known
  #         data$adjElevation[i]<- stats::weighted.mean(c(elevationA,elevationB), wt)
  #       }else{
  #         data$elevation[i]<- NA
  #       }
  #       
  #       
  #       #then add the distance from transect
  #       if (data$offset_sign[i]=='N'){
  #         northing<-northing+data$ninetyDegreeDistance[i]
  #       }
  #       if (data$offset_sign[i]=='E'){
  #         easting<-easting+data$ninetyDegreeDistance[i]
  #       }
  #       if (data$offset_sign[i]=='W'){
  #         easting<-easting-data$ninetyDegreeDistance[i]
  #       }
  #       if (data$offset_sign[i]=='S'){
  #         northing<-northing-data$ninetyDegreeDistance[i]
  #       }
  #       data$easting[i]<-easting
  #       data$northing[i]<-northing
  #       if (!is.na(northing)&&!is.na(easting)){
  #         data$utmZone[i]<-pointSpatialData$utmZone[substr(pointSpatialData$namedLocation,1,22)==data$namedLocation[i]&
  #                                                     pointSpatialData$Value.for.Point.ID==data$referencePoint_tempA[i]]
  #       }
  #     }
  #   }
  #   
  #   #don't transform the missing points
  #   if (length(which(is.na(data$easting)|is.na(data$northing))>0)){
  #     nogeo<-rbind(nogeo, data[is.na(data$easting)|is.na(data$northing),])
  #     data<-data[-which(is.na(data$easting)|is.na(data$northing)),]
  #   }
  #   
  #   #elevation uncertainty is really unknowable without knowing the microterrain
  #   data$adjElevationUncertainty<-NA
  #   
  #   # calculate latitude and longitude from the corrected northing and easting
  #   data <- def.calc.latlong(data)
  #   if (nrow(nogeo)>0){
  #     data<-gtools::smartbind(data, nogeo)
  #   }
  #   
  #   if (nrow(phenocamRows)>0){
  #     data<-gtools::smartbind(data, phenocamRows)
  #   }
  #   
  #   #cleanup
  #   data<-data[order(data$row.index),]
  #   data<-data[,!names(data)%in%c('row.index','referencePoint_tempA', 'referencePoint_tempB',
  #                      'offset_sign', 'distFromLastPoint', 'sampleGeodeticDatum',
  #                       'sampleLatitude', 'sampleLongitude', 'sampleCoordinateUncertainty',
  #                       'sampleElevation', 'sampleElevationUncertainty',
  #                       'elevation', 'elevationUncertainty', 'coordinateUncertainty')]
  #   names(data)[names(data)=='decimalLatitude']<-'adjDecimalLatitude'
  #   names(data)[names(data)=='decimalLongitude']<-'adjDecimalLongitude'
  #   return(data)
  # }
  #Small mammal trap locations:
  if(dataProd=="mam_pertrapnight"){
    
    # Concatenate the named location (the plot) and trapCoordinate to get the 
    #      point named locations
    points <- paste(data$namedLocation, data$trapCoordinate, sep=".")
    data <- cbind(data, points)
    
    #remove any X columns
    
    # Use the def.extr.geo.os function to pull the subplot geolocations from the API
    # Don't bother looking up any of the 'X' traps - those have uncertain geolocations
    locCol="points"
    point.loc <- def.extr.geo.os(data[!grepl('X', data$points),], locCol=locCol, locOnly=T)
    names(point.loc)[names(point.loc)=='data.locationName']<-locCol
    point.loc$api.coordinateUncertainty<-as.numeric(point.loc$api.coordinateUncertainty)
    
    #coordinate uncertainty is only provided for select locations per grid that
    # are monumented; assume since the rest of the grid is set up relative
    # to those monumented points, that the max per monumented point applies
    # to the rest of the points
    
    maxUncPerGrid<-data.frame(tapply(point.loc$api.coordinateUncertainty, point.loc$points, FUN=function(x) if(all(is.na(x))){NA} else {max(x, na.rm=T)}))
    maxUncPerGrid$points<-row.names(maxUncPerGrid)
    names(maxUncPerGrid)[1]<-'maxUncertainty'
    
    point.loc<-merge(point.loc, maxUncPerGrid)
    
    #add additional coordinateUncertainty 3m for nonmonumented, 1m otherwise
    
    #monumented corners, no additional uncertainty, GPS readings from here
    tot.unc <- function(api.coordinateUncertainty,maxUncertainty){
      if (is.na(api.coordinateUncertainty)){unc=maxUncertainty+3}else{unc=api.coordinateUncertainty+1}
      return(unc)
    }
    point.loc$tot.unc <- mapply(tot.unc, point.loc$api.coordinateUncertainty, point.loc$maxUncertainty)
    
    # calculate latitude and longitude from the corrected northing and easting
    #point.loc <- def.calc.latlong(point.loc)
    
    # Return relevant columns
    point.return <- point.loc[,c(locCol,"api.utmZone",
                                 "api.northing","api.easting","tot.unc",
                                 "api.decimalLatitude","api.decimalLongitude",
                                 "api.elevation","api.elevationUncertainty")]
    colnames(point.return) <- c("points", "utmZone", "adjNorthing", "adjEasting", "adjCoordinateUncertainty","adjDecimalLatitude",
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
    
    #remove any X columns
    
    # Use the def.extr.geo.os function to pull the subplot geolocations from the API
    # Don't bother looking up any of the 'X' traps - those have uncertain geolocations
    locCol="subplots"
    subplot.loc <- def.extr.geo.os(data, locCol=locCol, locOnly=T)
    names (subplot.loc)[names(subplot.loc)=='data.locationName']<-locCol
    subplot.loc$api.coordinateUncertainty<-as.numeric(subplot.loc$api.coordinateUncertainty)
    
    #Increase coordinateUncertainty by an appropriate amount to account for error
    # introduced by navigating within plots. Addi􀆟onal error may be introduced due 
    # to tape stretching to navigate to locations within plots
    # and is es􀆟mated as:
    # 0.25m for 1m2 subplot centroids
    # 1.0m for 10m2 subplot centroids
    # 2.0m for 100m2 subplot centroids
    
    subplot.loc$api.coordinateUncertainty[grepl('\\.1$', subplot.loc$subplotID)]<-
      subplot.loc$api.coordinateUncertainty[grepl('\\.1$', subplot.loc$subplotID)]+0.25
    
    subplot.loc$api.coordinateUncertainty[grepl('\\.10$', subplot.loc$subplotID)]<-
      subplot.loc$api.coordinateUncertainty[grepl('\\.10$', subplot.loc$subplotID)]+0.1
    
    subplot.loc$api.coordinateUncertainty[!grepl('\\.10$|\\.1$', subplot.loc$subplotID)]<-
      subplot.loc$api.coordinateUncertainty[!grepl('\\.10$|\\.1$', subplot.loc$subplotID)]+2
    
    # Return relevant columns
    subplot.return <- subplot.loc[,c(locCol,"api.utmZone",
                                     "api.northing","api.easting","api.coordinateUncertainty",
                                     "api.decimalLatitude","api.decimalLongitude",
                                     "api.elevation","api.elevationUncertainty")]
    colnames(subplot.return) <- c(locCol, "utmZone", "adjNorthing", "adjEasting", "adjCoordinateUncertainty","adjDecimalLatitude",
                                  "adjDecimalLongitude","adjElevation",
                                  "adjElevationUncertainty")
    data$row.index<-1:nrow(data)
    all.return <- merge(data, subplot.return, by=locCol, all.x=T)
    all.return<-all.return[order(all.return$row.index),]
    all.return<-all.return[,!names(all.return)%in%'row.index']
    return(all.return)
  }
  
  if(dataProd=="vst_mappingandtagging"){
    
    # Concatenate the named location (the plot) and point IDs to get the 
    #      point named locations
    points <- paste(data$namedLocation, data$pointID, sep=".")
    data <- cbind(data, points)
    
    # Use the def.extr.geo.os function to pull the subplot geolocations from the API
    locCol="points"
    point.loc <- geoNEON::def.extr.geo.os(data, locCol=locCol, locOnly=F)
    
    # Calculate easting and northing for individuals
    options(digits=15)
    point.loc$easting <- as.numeric(point.loc$api.easting) + point.loc$stemDistance * 
      sin((point.loc$stemAzimuth * pi) / 180)
    point.loc$northing <- as.numeric(point.loc$api.northing) + point.loc$stemDistance * 
      cos((point.loc$stemAzimuth * pi) / 180)
    
    # Increase coordinate uncertainties
    point.loc$adjCoordinateUncertainty <- 
      as.numeric(point.loc$api.coordinateUncertainty) + 0.6
    point.loc$adjElevationUncertainty <- 
      as.numeric(point.loc$api.elevationUncertainty) + 1
    
    # calculate latitude and longitude from the corrected northing and easting
    names(point.loc)[names(point.loc)=='api.utmZone'] <- 'utmZone'
    point.loc <- def.calc.latlong(point.loc)
    names(point.loc)[names(point.loc)=='utmZone'] <- 'api.utmZone'
    
    # Return relevant columns
    point.return <- point.loc[,c(locCol,"individualID","api.utmZone",
                                 "northing","easting","adjCoordinateUncertainty",
                                 "api.decimalLatitude","api.decimalLongitude",
                                 "api.elevation","adjElevationUncertainty")]
    colnames(point.return)[4:8] <- c("adjNorthing","adjEasting",
                                     "adjCoordinateUncertainty",
                                     "adjDecimalLatitude","adjDecimalLongitude")
    
    data$row.index <- 1:nrow(data)
    all.return <- merge(data, point.return, by=c("points","individualID"))
    all.return <- all.return[order(all.return$row.index),]
    all.return <- all.return[,!names(all.return) %in% 'row.index']
    return(all.return)
    
  }
  
  else {
    print(paste("This function has not been configured for data product table ", 
                 dataProd, sep=""))
  }
}

