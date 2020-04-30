##############################################################################################
#' @title Extract location values from the JSON returned by the NEON API

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Extract location data and properties from JSON returned by the NEON API; designed to be used iteratively for many locations. Called within getLocBySite(), not exported for independent use.
#'
#' @param locJSON A JSON object returned by the locations endpoint of the NEON API.
#' @param history Does locJSON include the location history? T or F, defaults to F.

#' @return A data frame of location data.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2020-04-07): original creation

##############################################################################################

getLocValues <- function(locJSON, history=F) {
  
  if(!history) {
    loc.values <- locJSON$data[intersect(intersect(grep('ParentUrl', names(locJSON$data), invert=T), 
                                                   grep('Child', names(locJSON$data), invert=T)),
                                         grep('Properties', names(locJSON$data), invert=T))]
    loc.props.mat <- locJSON$data$locationProperties
    loc.props <- loc.props.mat$locationPropertyValue
    names(loc.props) <- loc.props.mat$locationPropertyName
    
    loc.all <- unlist(c(loc.values, loc.props), use.names=T)
    loc.all <- data.frame(t(loc.all))
  }
  
  if(history) {
    loc.values <- locJSON$data$locationHistory[,-which(names(locJSON$data$locationHistory) %in% 'locationProperties')]
    locids <- cbind(rep(locJSON$data$locationDescription, nrow(loc.values)), 
                    rep(locJSON$data$locationName, nrow(loc.values)), 
                    rep(locJSON$data$domainCode, nrow(loc.values)), 
                    rep(locJSON$data$siteCode, nrow(loc.values)))
    names(locids) <- c('locationDescription', 'locationName', 'domainCode', 'siteCode')
    loc.values <- cbind(locids, loc.values)

    if(length(locJSON$data$locationHistory$locationProperties[[1]])!=0) {
      loc.props.list <- lapply(locJSON$data$locationHistory$locationProperties, tLocProps)
      loc.props <- data.table::rbindlist(loc.props.list, fill=T)
      loc.all <- cbind(loc.values, loc.props)
    } else {
      loc.all <- loc.values
    }
    
  }
  
  names(loc.all)[names(loc.all)=='locationName'] <- 'namedLocation'
  names(loc.all)[names(loc.all)=='siteCode'] <- 'siteID'
  names(loc.all)[names(loc.all)=='domainCode'] <- 'domainID'
  names(loc.all)[names(loc.all)=='locationUtmEasting'] <- 'easting'
  names(loc.all)[names(loc.all)=='locationUtmNorthing'] <- 'northing'
  names(loc.all)[names(loc.all)=='Value.for.UTM.Zone'] <- 'utmZone'
  names(loc.all)[names(loc.all)=='locationElevation'] <- 'elevation'
  names(loc.all)[names(loc.all)=='locationDecimalLatitude'] <- 'decimalLatitude'
  names(loc.all)[names(loc.all)=='locationDecimalLongitude'] <- 'decimalLongitude'
  names(loc.all)[names(loc.all)=='Value.for.Coordinate.uncertainty'] <- 'namedLocationCoordUncertainty'
  names(loc.all)[names(loc.all)=='Value.for.Elevation.uncertainty'] <- 'namedLocationElevUncertainty'
  names(loc.all)[names(loc.all)=='Value.for.National.Land.Cover.Database..2001.'] <- 'nlcdClass'
  names(loc.all)[names(loc.all)=='Value.for.HABITAT'] <- 'siteType'
  names(loc.all)[names(loc.all)=='Value.for.Plot.dimensions'] <- 'plotDimensions'
  names(loc.all)[names(loc.all)=='Value.for.Soil.type.order'] <- 'soilTypeOrder'
  names(loc.all)[names(loc.all)=='Value.for.Subtype.Specification'] <- 'subtypeSpecification'
  names(loc.all)[names(loc.all)=='Value.for.Plot.type'] <- 'plotType'
  names(loc.all)[names(loc.all)=='Value.for.Reference.Point.Position'] <- 'referencePointPosition'
  names(loc.all)[names(loc.all)=='Value.for.Plot.subtype'] <- 'subtype'
  names(loc.all)[names(loc.all)=='Value.for.Plot.size'] <- 'plotSize'
  names(loc.all)[names(loc.all)=='Value.for.Maximum.elevation'] <- 'maximumElevation'
  names(loc.all)[names(loc.all)=='Value.for.Slope.aspect'] <- 'slopeAspect'
  names(loc.all)[names(loc.all)=='Value.for.Horizontal.dilution.of.precision'] <- 'plotHdop'
  names(loc.all)[names(loc.all)=='Value.for.Positional.dilution.of.precision'] <- 'plotPdop'
  names(loc.all)[names(loc.all)=='Value.for.Slope.gradient'] <- 'slopeGradient'
  names(loc.all)[names(loc.all)=='Value.for.Minimum.elevation'] <- 'minimumElevation'
  names(loc.all)[names(loc.all)=='Value.for.Coordinate.source'] <- 'coordinateSource'
  names(loc.all)[names(loc.all)=='Value.for.Filtered.positions'] <- 'filteredPositions'
  names(loc.all)[names(loc.all)=='Value.for.Geodetic.datum'] <- 'geodeticDatum'
  names(loc.all)[names(loc.all)=='Value.for.State.province'] <- 'stateProvince'
  names(loc.all)[names(loc.all)=='Value.for.State.Abbreviation'] <- 'stateAbbreviation'
  names(loc.all)[names(loc.all)=='Value.for.County'] <- 'county'
  names(loc.all)[names(loc.all)=='Value.for.Country'] <- 'country'
  names(loc.all)[names(loc.all)=='Value.for.Site.Timezone'] <- 'siteTimezone'
  names(loc.all)[names(loc.all)=='Value.for.Plot.ID'] <- 'plotID'
  names(loc.all)[names(loc.all)=='Value.for.Coordinate.source'] <- 'coordinateSource'
  names(loc.all)[names(loc.all)=='locationUtmHemisphere'] <- 'utmHemisphere'
  names(loc.all)[names(loc.all)=='locationUtmZone'] <- 'utmZoneNumber'

  if(length(which(names(loc.all) %in% c('Value.for.DURATION',
                                        'Value.for.Has.Location.Started.Receiving.Data',
                                        'Value.for.IS_ACTIVE',
                                        'Value.for.NEONSCI_FIELD_SITE_URL_PATH',
                                        'Value.for.Required.Asset.Management.Location.Code',
                                        'Value.for.Required.Asset.Management.Location.ID')))>0) {
    loc.all <- loc.all[,-which(names(loc.all) %in% c('Value.for.DURATION',
                                                     'Value.for.Has.Location.Started.Receiving.Data',
                                                     'Value.for.IS_ACTIVE',
                                                     'Value.for.NEONSCI_FIELD_SITE_URL_PATH',
                                                     'Value.for.Required.Asset.Management.Location.Code',
                                                     'Value.for.Required.Asset.Management.Location.ID'))]
  }

  return(loc.all)
  
}
