##############################################################################################
#' @title Convert easting and northing coordinates to align BLAN and LEWI locations with AOP flights

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Converts easting and northing coordinates from 18N to 17N to align BLAN and LEWI locations with AOP flights.
#'
#' @param data A data frame including columns of easting and northing coordinates [data frame]
#' @param easting Column name containing easting values [character]
#' @param northing Column name containing northing values [character]

#' @return The input data frame, with all easting and northing coordinates in 17N

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# Changelog and author contributions / copyrights
#   Claire Lunch (2025-07-30): adapted from code in byTileAOP()

##############################################################################################

convertBLAN <- function(data, 
                        easting="easting",
                        northing="northing") {
  
  # check input data for coordinate columns
  if(!all(c(easting, northing) %in% names(data))) {
    stop("Input data frame must include easting and northing columns.")
  }
  
  data[,easting] <- as.numeric(data[,easting])
  data[,northing] <- as.numeric(data[,northing])
  
  # check for siteID column and warn if not found
  if(!"siteID" %in% names(data)) {
    warning("siteID column not found in input data. All easting and northing values will be converted; if data include sites other than BLAN and LEWI, results will be erroneous.")
    dataSub <- data
    dataN <- data[0,]
  } else {
    if(length(which(data$siteID %in% c("BLAN","LEWI")))==0) {
      stop("siteID column does not include BLAN or LEWI. This function is only used for those two sites.")
    } else {
      data$rowid <- 1:nrow(data)
      dataSub <- data[which(data$siteID %in% c("BLAN","LEWI")),]
      dataN <- data[which(!data$siteID %in% c("BLAN","LEWI")),]
    }
  }
  
  # check for locations needing conversion
  if(length(which(dataSub[,easting]<=250000))==0) {
    message("No coordinates found in 18N. No conversions made.")
    return(data)
  }
  
  data17 <- dataSub[which(dataSub[,easting]>250000),]
  data18 <- dataSub[which(dataSub[,easting]<=250000),]
  
  epsg.z <- relevantEPSGs$code[grep("+proj=utm +zone=17", 
                                    relevantEPSGs$prj4, fixed=T)]
  
  # convert
  df18 <- terra::vect(data18, crs="+proj=utm +zone=18", 
                      geom=c(easting, northing))
  df18conv <- terra::project(df18, y=paste("EPSG:", epsg.z, sep=""))
  df18coords <- data.frame(terra::crds(df18conv))
  
  data18[,easting] <- df18coords$x
  data18[,northing] <- df18coords$y
  
  if("utmZone" %in% names(data18)) {
    data18$utmZone <- gsub(pattern="18", replacement="17", x=data18$utmZone)
  }
  
  # stitch back together and align to original row order
  dataConv <- rbind(data17, data18)
  dataOut <- rbind(dataConv, dataN)
  dataOut <- dataOut[order(dataOut$rowid),]
  dataOut <- dataOut[,!names(dataOut) %in% "rowid"]
  
  return(dataOut)

}
