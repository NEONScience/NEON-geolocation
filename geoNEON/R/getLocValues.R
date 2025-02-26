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

getLocValues <- function(locJSON, history=FALSE) {
  
  if(!history) {
    loc.all <- getLocProperties(locList=locJSON, history=FALSE)
  }
  
  if(history) {
    locids <- cbind(rep(locJSON$data$locationDescription, length(locJSON$data$locationHistory)), 
                    rep(locJSON$data$locationName, length(locJSON$data$locationHistory)), 
                    rep(locJSON$data$domainCode, length(locJSON$data$locationHistory)), 
                    rep(locJSON$data$siteCode, length(locJSON$data$locationHistory)))
    loc.values <- data.frame(locids)
    names(loc.values) <- c('locationDescription', 'locationName', 'domainCode', 'siteCode')
    
    loc.props.mat <- locJSON$data$locationProperties
    loc.props <- unlist(lapply(loc.props.mat, function(x) {
      x$locationPropertyValue
    }))
    names(loc.props) <- unlist(lapply(loc.props.mat, function(x) { 
      x$locationPropertyName
    }))
    
    if(length(loc.props)>0) {
      loc.p <- data.frame(t(unlist(loc.props, use.names=T)))
    } else {
      loc.p <- NA
    }
    
    loc.each <- lapply(locJSON$data$locationHistory, FUN=getLocProperties, history=TRUE)
    loc.temp <- data.frame(data.table::rbindlist(loc.each, fill=TRUE))
    loc.temp <- cbind(loc.values, loc.temp)
    if(!all(is.na(loc.p))) {
      loc.all <- try(base::merge(loc.temp, loc.p, all=TRUE), silent=TRUE)
      if(inherits(loc.all, "try-error")) {
        loc.all <- loc.temp
        message("Location properties differ in location history.")
      }
    } else {
      loc.all <- loc.temp
    }
  }
  
  names(loc.all)[names(loc.all)=='locationName'] <- 'namedLocation'
  names(loc.all)[names(loc.all)=='siteCode'] <- 'siteID'
  names(loc.all)[names(loc.all)=='domainCode'] <- 'domainID'
  names(loc.all)[names(loc.all)=='locationUtmEasting'] <- 'easting'
  names(loc.all)[names(loc.all)=='locationUtmNorthing'] <- 'northing'
  names(loc.all)[names(loc.all) %in% c('Value.for.UTM.Zone',
                                       'Value for UTM Zone')] <- 'utmZone'
  names(loc.all)[names(loc.all)=='locationElevation'] <- 'elevation'
  names(loc.all)[names(loc.all)=='locationDecimalLatitude'] <- 'decimalLatitude'
  names(loc.all)[names(loc.all)=='locationDecimalLongitude'] <- 'decimalLongitude'
  names(loc.all)[names(loc.all) %in% c('Value.for.Coordinate.uncertainty',
                                       'Value for Coordinate uncertainty')] <- 'namedLocationCoordUncertainty'
  names(loc.all)[names(loc.all) %in% c('Value.for.Elevation.uncertainty',
                                       'Value for Elevation uncertainty')] <- 'namedLocationElevUncertainty'
  names(loc.all)[names(loc.all) %in% c('Value.for.National.Land.Cover.Database..2001.',
                                       'Value for National Land Cover Database (2001)')] <- 'nlcdClass'
  names(loc.all)[names(loc.all) %in% c('Value.for.HABITAT',
                                       'Value for HABITAT')] <- 'siteType'
  names(loc.all)[names(loc.all) %in% c('Value.for.Plot.dimensions',
                                       'Value for Plot dimensions')] <- 'plotDimensions'
  names(loc.all)[names(loc.all) %in% c('Value.for.Soil.type.order',
                                       'Value for Soil type order')] <- 'soilTypeOrder'
  names(loc.all)[names(loc.all) %in% c('Value.for.Subtype.Specification',
                                       'Value for Subtype Specification')] <- 'subtypeSpecification'
  names(loc.all)[names(loc.all) %in% c('Value.for.Plot.type',
                                       'Value for Plot type')] <- 'plotType'
  names(loc.all)[names(loc.all) %in% c('Value.for.Reference.Point.Position',
                                       'Value for Reference Point Position')] <- 'referencePointPosition'
  names(loc.all)[names(loc.all) %in% c('Value.for.Plot.subtype',
                                       'Value for Plot subtype')] <- 'subtype'
  names(loc.all)[names(loc.all) %in% c('Value.for.Plot.size',
                                       'Value for Plot size')] <- 'plotSize'
  names(loc.all)[names(loc.all) %in% c('Value.for.Maximum.elevation',
                                       'Value for Maximum elevation')] <- 'maximumElevation'
  names(loc.all)[names(loc.all) %in% c('Value.for.Slope.aspect',
                                       'Value for Slope aspect')] <- 'slopeAspect'
  names(loc.all)[names(loc.all) %in% c('Value.for.Horizontal.dilution.of.precision',
                                       'Value for Horizontal dilution of precision')] <- 'plotHdop'
  names(loc.all)[names(loc.all) %in% c('Value.for.Positional.dilution.of.precision',
                                       'Value for Positional dilution of precision')] <- 'plotPdop'
  names(loc.all)[names(loc.all) %in% c('Value.for.Slope.gradient',
                                       'Value for Slope gradient')] <- 'slopeGradient'
  names(loc.all)[names(loc.all) %in% c('Value.for.Minimum.elevation',
                                       'Value for Minimum elevation')] <- 'minimumElevation'
  names(loc.all)[names(loc.all) %in% c('Value.for.Coordinate.source',
                                       'Value for Coordinate source')] <- 'coordinateSource'
  names(loc.all)[names(loc.all) %in% c('Value.for.Filtered.positions',
                                       'Value for Filtered positions')] <- 'filteredPositions'
  names(loc.all)[names(loc.all) %in% c('Value.for.Geodetic.datum',
                                       'Value for Geodetic datum')] <- 'geodeticDatum'
  names(loc.all)[names(loc.all) %in% c('Value.for.State.province',
                                       'Value for State province')] <- 'stateProvince'
  names(loc.all)[names(loc.all) %in% c('Value.for.State.Abbreviation',
                                       'Value for State Abbreviation')] <- 'stateAbbreviation'
  names(loc.all)[names(loc.all) %in% c('Value.for.County',
                                       'Value for County')] <- 'county'
  names(loc.all)[names(loc.all) %in% c('Value.for.Country',
                                       'Value for Country')] <- 'country'
  names(loc.all)[names(loc.all) %in% c('Value.for.Site.Timezone',
                                       'Value for Site Timezone')] <- 'siteTimezone'
  names(loc.all)[names(loc.all) %in% c('Value.for.Plot.ID',
                                       'Value for Plot ID')] <- 'plotID'
  names(loc.all)[names(loc.all) %in% c('Value.for.Point.ID',
                                       'Value for Point ID')] <- 'locationPointID'
  names(loc.all)[names(loc.all) %in% c('Value.for.Coordinate.source',
                                       'Value for Coordinate source')] <- 'coordinateSource'
  names(loc.all)[names(loc.all)=='locationUtmHemisphere'] <- 'utmHemisphere'
  names(loc.all)[names(loc.all)=='locationUtmZone'] <- 'utmZoneNumber'

  if(length(which(names(loc.all) %in% c('Value.for.DURATION',
                                        'Value.for.Has.Location.Started.Receiving.Data',
                                        'Value.for.IS_ACTIVE',
                                        'Value.for.NEONSCI_FIELD_SITE_URL_PATH',
                                        'Value.for.Required.Asset.Management.Location.Code',
                                        'Value.for.Required.Asset.Management.Location.ID',
                                        'Value for DURATION',
                                        'Value for Has Location Started Receiving Data',
                                        'Value for IS_ACTIVE',
                                        'Value for NEONSCI_FIELD_SITE_URL_PATH',
                                        'Value for Required.Asset.Management.Location.Code',
                                        'Value for Required.Asset.Management.Location.ID')))>0) {
    loc.all <- loc.all[,-which(names(loc.all) %in% c('Value.for.DURATION',
                                                     'Value.for.Has.Location.Started.Receiving.Data',
                                                     'Value.for.IS_ACTIVE',
                                                     'Value.for.NEONSCI_FIELD_SITE_URL_PATH',
                                                     'Value.for.Required.Asset.Management.Location.Code',
                                                     'Value.for.Required.Asset.Management.Location.ID',
                                                     'Value for DURATION',
                                                     'Value for Has Location Started Receiving Data',
                                                     'Value for IS_ACTIVE',
                                                     'Value for NEONSCI_FIELD_SITE_URL_PATH',
                                                     'Value for Required Asset Management Location Code',
                                                     'Value for Required Asset Management Location ID'))]
  }

  # re-order to get essential columns first
  coreNames <- c("namedLocation", "locationDescription", "domainID", "siteID",
                 "current","locationStartDate","locationEndDate","decimalLatitude",
                 "decimalLongitude","elevation","easting","northing","utmZone",
                 "namedLocationCoordUncertainty","namedLocationElevUncertainty")
  coreNamesUsed <- coreNames[which(coreNames %in% names(loc.all))]
  loc.all <- loc.all[,c(coreNamesUsed, names(loc.all)[which(!names(loc.all) %in% coreNamesUsed)])]
  return(loc.all)
  
}
