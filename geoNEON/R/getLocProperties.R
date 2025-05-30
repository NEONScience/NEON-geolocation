##############################################################################################
#' @title Extract location property values from the list extracted from the JSON returned by the NEON API

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Extract location properties from JSON returned by the NEON API. Called within getLocValues(), not exported for independent use.
#'
#' @param locList A list derived from the JSON object returned by the locations endpoint of the NEON API.
#' @param history Does locJSON include the location history? T or F, defaults to F.
#' 
#' @keywords internal
#' 
#' @return A data frame of location data.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2025-02-20): original creation

##############################################################################################

getLocProperties <- function(locList, history=FALSE) {
  
  if(!history) {
    loc.values <- locList$data[intersect(intersect(grep('ParentUrl', names(locList$data), invert=T), 
                                                   grep('Child', names(locList$data), invert=T)),
                                         grep('Properties', names(locList$data), invert=T))]
    
    loc.props.mat <- locList$data$locationProperties
    loc.props <- unlist(lapply(loc.props.mat, function(x) {
      x$locationPropertyValue
    }))
    names(loc.props) <- unlist(lapply(loc.props.mat, function(x) { 
      x$locationPropertyName
    }))
    
    loc.all <- unlist(c(loc.values, loc.props), use.names=T)
    loc.all <- data.frame(t(loc.all))
  }
  
  if(history) {
    loc.values <- locList[grep('Properties', names(locList), invert=T)]

    loc.props.mat <- locList$locationProperties
    loc.props <- unlist(lapply(loc.props.mat, function(x) {
      x$locationPropertyValue
    }))
    names(loc.props) <- unlist(lapply(loc.props.mat, function(x) { 
      x$locationPropertyName
    }))
    
    loc.all <- unlist(c(loc.values, loc.props), use.names=T)
    loc.all <- data.frame(t(loc.all))
  }
  
  return(loc.all)
  
}
