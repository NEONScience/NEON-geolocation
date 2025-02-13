##############################################################################################
#' @title Calculate more precise geolocations for plant phenology (DP1.10055.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON plant phenology data.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2025-02-13)
##############################################################################################
getLocPHE <- function(
    data,
    token=NA_character_
){
  
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
  pointSpatialData <- geoNEON::getLocByName(corners, locCol=locCol, locOnly=T, token=token)
  
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
  adjLatLong <- calcLatLong(easting=data$easting, 
                            northing=data$northing,
                            utmZone=data$utmZone)
  data$adjDecimalLatitude <- adjLatLong$decimalLatitude
  data$adjDecimalLongitude <- adjLatLong$decimalLongitude
  
  names(data)[names(data)=='easting'] <- 'adjEasting'
  names(data)[names(data)=='northing'] <- 'adjNorthing'
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
