##############################################################################################
#' @title Extract geolocation data from NEON API

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Definition Function. Extract geolocation data from the NEON API, for a given set of named location values
#' 
#' @param data A data frame in which one column contains the named locations
#' @param locCol The column name of the column containing the named locations. Defaults to namedLocation

#' @return A data frame of the geolocation data for the input named locations

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples 
#' d <- data.frame(namedLocation=c("GUAN_044.basePlot.ltr","GRSM_003.birdGrid.brd"))
#' def.extr.geo.os(d, "namedLocation")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2017-02-03)
#     adapted from code written by
#   Sarah Elmendorf (2016)
##############################################################################################
def.extr.geo.os <- function(
  data,
  locCol = "namedLocation"
){
  
  # Define simple function for JSON extraction
  getIndexval <- function(x, indexVal){
    v <- unlist(x)
    v[indexVal]
  }
  
  # Initiate list of outputs
  outList <- list()
  
  # Iterate over input locations
  for (j in data[,locCol]) {
    
    # Pull data from API
    req <- httr::GET(paste("http://data.neonscience.org/api/v0/locations/", j, sep=''))
    req.content <- httr::content(req, as="parsed")
    
    # Give warnings for missing values
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
  names (plotInfo)[names(plotInfo)=='data.locationDescription'] <- 'description'
  names (plotInfo)[names(plotInfo)=='data.locationType'] <- 'type'
  
  allTerms <- c('domainID', 'type', 'description', 'filteredPositions', 'coordinateSource','minimumElevation',
              'slopeGradient', 'plotPdop', 'plotHdop', 'slopeAspect', 'maximumElevation', 'plotSize',
              'subtype', 'referencePointPosition', 'plotType', 'siteID', 'domainID', 'easting','northing' ,'utmZone',
              'elevation','decimalLatitude', 'decimalLongitude','coordinateUncertainty', 'elevationUncertainty',
              'nlcdClass','plotDimensions','soilTypeOrder', 'subtypeSpecification')
  
  # Fill unused fields with NA
  plotInfo[, paste(allTerms[!allTerms %in% (names(plotInfo))])] <- NA

  # Return a data frame of the named locations and geolocations
  return(plotInfo)
  
}
