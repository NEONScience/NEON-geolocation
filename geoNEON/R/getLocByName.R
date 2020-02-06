##############################################################################################
#' @title Extract geolocation data from NEON API

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Get geolocation data from the NEON API, for a given set of named location values
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
#' getLocByName(d, "namedLocation")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-09-03)
#     adapted from and replaces def.extr.geo.os()
##############################################################################################
getLocByName <- function(
  data,
  locCol = "namedLocation",
  locOnly=F
){
  
  # Define simple function for JSON extraction
  getIndexval <- function(x, indexVal){
    v <- unlist(x)
    v[indexVal]
  }
  
  # Initiate list of outputs
  outList <- list()
  pb <- utils::txtProgressBar(min = 0, max = length(unique(data[,locCol])), style = 3)
  i<-0 # i iterates for progress bar
  # Iterate over input locations
  for (j in unique(data[,locCol])) {
    utils::setTxtProgressBar(pb, i)
    i<-i+1
    k<-1 #k iterates for 100 attempts at curl
    while(k<100){
      # Pull data from API
      tmp<-try(req <- httr::GET(paste("http://data.neonscience.org/api/v0/locations/", j, sep='')))
      k<-k+1 
      if(!class(tmp) == 'try-error'){
        k<-k+100
      }
    }
    req.content <- httr::content(req, as="parsed")
    
    # Give warnings for missing values & API errors
    if (!is.null(req.content$doc)){
      warning("The NEON publication server is down, please try again later. 
              Check the NEON data portal for additional service messages.")
    }
    if (!is.null(req.content$error$status)){
      warning(paste("WARNING: the following namedLocation was not found:",
                    j, sep=" "))
    }
    
    # Extract location properties from JSON
    properties <- req.content$data$locationProperties
    props <- lapply(properties, function(x) getIndexval(x, 2))
    propTitles <- lapply(properties, function(x) getIndexval(x, 1))
    props <- data.frame(t(props))
    names(props) <- propTitles
    
    # Reorganize data
    vals <- rapply(req.content, f=`[[`, ...=1, how="unlist")
    vals <- data.frame(t(vals))
    
    # Don't include the children or properties
    vals <- vals[!grepl('locationProperties', names (vals))]
    vals <- vals[!grepl('locationChildren', names (vals))]
    vals <- cbind(vals, props)
    
    # Write out the list of location properties
    outList[[length(outList)+1]]<-data.frame(lapply(vals, as.character), stringsAsFactors=FALSE)
  }
  
  # Make data frame of locations to return
  plotInfo <- plyr::rbind.fill(outList)
  
  # Simplify names from the database names
  names (plotInfo)[names(plotInfo)=='data.siteCode'] <- 'siteID'
  names (plotInfo)[names(plotInfo)=='data.domainCode'] <- 'domainID'
  names (plotInfo)[names(plotInfo)=='data.locationUtmEasting'] <- 'easting'
  names (plotInfo)[names(plotInfo)=='data.locationUtmNorthing'] <- 'northing'
  names (plotInfo)[names(plotInfo)=='Value.for.UTM.Zone'] <- 'utmZone'
  names (plotInfo)[names(plotInfo)=='data.locationElevation'] <- 'elevation'
  names (plotInfo)[names(plotInfo)=='data.locationDecimalLatitude'] <- 'decimalLatitude'
  names (plotInfo)[names(plotInfo)=='data.locationDecimalLongitude'] <- 'decimalLongitude'
  names (plotInfo)[names(plotInfo)=='Value.for.Coordinate.uncertainty'] <- 'coordinateUncertainty'
  names (plotInfo)[names(plotInfo)=='Value.for.Elevation.uncertainty'] <- 'elevationUncertainty'
  names (plotInfo)[names(plotInfo)=='Value.for.National.Land.Cover.Database..2001.'] <- 'nlcdClass'
  names (plotInfo)[names(plotInfo)=='Value.for.Plot.dimensions'] <- 'plotDimensions'
  names (plotInfo)[names(plotInfo)=='Value.for.Soil.type.order'] <- 'soilTypeOrder'
  names (plotInfo)[names(plotInfo)=='Value.for.Subtype.Specification'] <- 'subtypeSpecification'
  names (plotInfo)[names(plotInfo)=='Value.for.Plot.type'] <- 'plotType'
  names (plotInfo)[names(plotInfo)=='Value.for.Reference.Point.Position'] <- 'referencePointPosition'
  names (plotInfo)[names(plotInfo)=='Value.for.Plot.subtype'] <- 'subtype'
  names (plotInfo)[names(plotInfo)=='Value.for.Plot.size'] <- 'plotSize'
  names (plotInfo)[names(plotInfo)=='Value.for.Maximum.elevation'] <- 'maximumElevation'
  names (plotInfo)[names(plotInfo)=='Value.for.Slope.aspect'] <- 'slopeAspect'
  names (plotInfo)[names(plotInfo)=='Value.for.Horizontal.dilution.of.precision'] <- 'plotHdop'
  names (plotInfo)[names(plotInfo)=='Value.for.Positional.dilution.of.precision'] <- 'plotPdop'
  names (plotInfo)[names(plotInfo)=='Value.for.Slope.gradient'] <- 'slopeGradient'
  names (plotInfo)[names(plotInfo)=='Value.for.Minimum.elevation'] <- 'minimumElevation'
  names (plotInfo)[names(plotInfo)=='Value.for.Coordinate.source'] <- 'coordinateSource'
  names (plotInfo)[names(plotInfo)=='Value.for.Filtered.positions'] <- 'filteredPositions'
  names (plotInfo)[names(plotInfo)=='data.locationDescription'] <- 'locationDescription'
  names (plotInfo)[names(plotInfo)=='data.locationType'] <- 'locationType'
  names (plotInfo)[names(plotInfo)=='Value.for.Geodetic.datum'] <- 'geodeticDatum'
  names (plotInfo)[names(plotInfo)=='Value.for.State.province'] <- 'stateProvince'
  names (plotInfo)[names(plotInfo)=='Value.for.County'] <- 'county'
  names (plotInfo)[names(plotInfo)=='Value.for.Country'] <- 'country'
  names (plotInfo)[names(plotInfo)=='Value.for.Plot.ID'] <- 'plotID'
  names (plotInfo)[names(plotInfo)=='data.xOffset'] <- 'xOffset'
  names (plotInfo)[names(plotInfo)=='data.yOffset'] <- 'yOffset'
  names (plotInfo)[names(plotInfo)=='data.zOffset'] <- 'zOffset'
  names (plotInfo)[names(plotInfo)=='data.alphaOrientation'] <- 'alphaOrientation'
  names (plotInfo)[names(plotInfo)=='data.betaOrientation'] <- 'betaOrientation'
  names (plotInfo)[names(plotInfo)=='data.gammaOrientation'] <- 'gammaOrientation'
  names (plotInfo)[names(plotInfo)=='data.locationUtmHemisphere'] <- 'utmHemisphere'
  names (plotInfo)[names(plotInfo)=='data.locationUtmZone'] <- 'utmZoneNumber'
  names (plotInfo)[names(plotInfo)=='data.locationParent'] <- 'locationParent'
  names (plotInfo)[names(plotInfo)=='data.locationParentUrl'] <- 'locationParentUrl'
  
  
  allTerms <- c('domainID', 'type', 'description', 'filteredPositions', 'coordinateSource','minimumElevation',
              'slopeGradient', 'plotPdop', 'plotHdop', 'slopeAspect', 'maximumElevation', 'plotSize',
              'subtype', 'referencePointPosition', 'plotType', 'siteID', 'easting','northing' ,'utmZone',
              'elevation','decimalLatitude', 'decimalLongitude','coordinateUncertainty', 'elevationUncertainty',
              'nlcdClass','plotDimensions','soilTypeOrder', 'subtypeSpecification', 'county', 'stateProvince', 'country')
  
  # Fill unused fields with NA
  plotInfo[,allTerms[!allTerms %in% (names(plotInfo))]] <- NA
  
  # add blank column if all values are invalid
  # ADD: need something like this, but not this column name. What happens if all locations invalid?
  if (!'data.locationName'%in%names(plotInfo)){
    plotInfo$data.locationName<-NA
  }
  
  utils::setTxtProgressBar(pb, length(unique(data[,locCol])))
  close(pb)

  # Return the original data with location data added
  # Only add columns that weren't already in the data
  # ADD: check that values match in the columns that were already in the data
  if (!locOnly){
    data$row.index <- 1:nrow(data)
    plotInfo <- plotInfo[,!names(plotInfo) %in% names(data)]
    allInfo <- merge(data, plotInfo, by.x=locCol, by.y='data.locationName', all.x=T)
    allInfo <- allInfo[order(allInfo$row.index),]
    allInfo <- allInfo[,!names(allInfo)%in%'row.index']
  } else { 
    allInfo <- plotInfo
  }
  return(allInfo)
}
