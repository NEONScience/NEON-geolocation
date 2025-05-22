##############################################################################################
#' @title Calculate more precise geolocations for plant phenology (DP1.10055.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON plant phenology data.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.
#' 
#' @keywords internal

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
  
  data <- data[!data$subtypeSpecification=='phenocam',]
  corners <- data.frame(namedLocation=paste(unique(data$namedLocation), 
                                            c('N', 'E', 'S', 'W', 'NE', 'SE', 'SW', 'NW'), sep="."), vals=NA)
  
  # Use the getLocByName function to pull the subplot geolocations from the API
  locCol="namedLocation"
  pointSpatialData <- geoNEON::getLocByName(corners, locCol=locCol, 
                                            locOnly=TRUE, history=TRUE,
                                            token=token)
  
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
  
  data$distFromLastPoint <- rep(NA, nrow(data))
  
  for (i in 1:nrow(data)){
    if (!is.na(data$transectMeter[i])){
      data$distFromLastPoint[i]<-calcDistanceFromLast(data$transectMeter[i])
    }
  }
  
  data$northing <- rep(NA, nrow(data))
  data$easting <- rep(NA, nrow(data))
  data$utmZone <- rep(NA, nrow(data))
  data$namedLocation <- as.character(data$namedLocation)
  pointSpatialData$namedLocation <- as.character(pointSpatialData$namedLocation)
  
  for (i in 1:nrow(data)){
    if (!is.na(data$referencePoint_tempA[i])&
        !is.na(data$referencePoint_tempB[i])){
      
      # check for location histories and get spatial data matching date of collection
      if(any(pointSpatialData$current=="FALSE" | isFALSE(pointSpatialData$current),
             na.rm=TRUE)) {
        startind <- which(pointSpatialData$locationStartDate <= data$date[i])
        endind <- union(which(pointSpatialData$locationEndDate > data$date[i]), 
                        which(is.na(pointSpatialData$locationEndDate)))
        indj <- intersect(startind, endind)
        if(length(indj)==0) {
          message(paste(data$date[i], " is outside the valid date range for associated locations. Spatial data returned match most recent valid date.", sep=""))
          pointSpatialSub <- pointSpatialData[which(pointSpatialData$current=="TRUE" | isTRUE(pointSpatialData$current)),]
        } else {
          pointSpatialSub <- pointSpatialData[indj,]
        }
      } else {
        pointSpatialSub <- pointSpatialData
      }
      
      northingA<-as.numeric(pointSpatialSub$northing[substr(pointSpatialSub$namedLocation,1,22)==data$namedLocation[i] &
                                                        pointSpatialSub$locationPointID==data$referencePoint_tempA[i]])
      northingB<-as.numeric(pointSpatialSub$northing[substr(pointSpatialSub$namedLocation,1,22)==data$namedLocation[i] &
                                                        pointSpatialSub$locationPointID==data$referencePoint_tempB[i]])
      eastingA<-as.numeric(pointSpatialSub$easting[substr(pointSpatialSub$namedLocation,1,22)==data$namedLocation[i] &
                                                      pointSpatialSub$locationPointID==data$referencePoint_tempA[i]])
      eastingB<-as.numeric(pointSpatialSub$easting[substr(pointSpatialSub$namedLocation,1,22)==data$namedLocation[i] &
                                                      pointSpatialSub$locationPointID==data$referencePoint_tempB[i]])
      
      elevationA<-as.numeric(pointSpatialSub$elevation[substr(pointSpatialSub$namedLocation,1,22)==data$namedLocation[i] &
                                                          pointSpatialSub$locationPointID==data$referencePoint_tempA[i]])
      elevationB<-as.numeric(pointSpatialSub$elevation[substr(pointSpatialSub$namedLocation,1,22)==data$namedLocation[i] &
                                                          pointSpatialSub$locationPointID==data$referencePoint_tempB[i]])
      wt<-c(100-data$distFromLastPoint[i],data$distFromLastPoint[i])
      if (length(c(northingA, northingB, eastingA, eastingB))==4){#make sure all elements known
        northing <- stats::weighted.mean(c(northingA,northingB), wt)
        easting <- stats::weighted.mean(c(eastingA,eastingB), wt)
        data$adjCoordinateUncertainty[i]<-max (as.numeric(pointSpatialSub$namedLocationCoordUncertainty[substr(pointSpatialSub$namedLocation,1,22)==data$namedLocation[i]&
                                                                                                           pointSpatialSub$locationPointID==data$referencePoint_tempA[i]]),
                                               as.numeric(pointSpatialSub$namedLocationCoordUncertainty[substr(pointSpatialSub$namedLocation,1,22)==data$namedLocation[i]&
                                                                                                           pointSpatialSub$locationPointID==data$referencePoint_tempB[i]]))+2
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
        data$utmZone[i]<-pointSpatialSub$utmZone[substr(pointSpatialSub$namedLocation,1,22)==data$namedLocation[i]&
                                                    pointSpatialSub$locationPointID==data$referencePoint_tempA[i]]
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
  data$adjElevationUncertainty <- rep(NA, nrow(data))
  
  # calculate latitude and longitude from the corrected northing and easting
  adjLatLong <- calcLatLong(easting=data$easting, 
                            northing=data$northing,
                            utmZone=data$utmZone)
  data$adjDecimalLatitude <- adjLatLong$decimalLatitude
  data$adjDecimalLongitude <- adjLatLong$decimalLongitude
  
  names(data)[names(data)=='easting'] <- 'adjEasting'
  names(data)[names(data)=='northing'] <- 'adjNorthing'

  # merge with other data
  if (nrow(nogeo)>0){
    data <- data.table::rbindlist(list(data, nogeo), fill=T)
  }
  
  if (nrow(phenocamRows)>0){
    data <- data.table::rbindlist(list(data, phenocamRows), fill=T)
  }
  
  #cleanup
  data <- data[order(data$row.index),]
  data <- data[,which(!names(data) %in% c('row.index','referencePoint_tempA', 'referencePoint_tempB',
                                  'offset_sign', 'distFromLastPoint'))]

  return(data)
 
}
