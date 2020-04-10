##############################################################################################
#' @title Extract location values from the JSON returned by the NEON API

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Extract location data and properties from JSON returned by the NEON API; designed to be used iteratively for many locations. Called within getLocBySite(), not exported for independent use.
#'
#' @param locJSON A JSON object returned by the locations endpoint of the NEON API.

#' @return A data frame of location data.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2020-04-07): original creation

##############################################################################################

getLocValues <- function(locJSON) {
  
  loc.values <- locJSON$data[intersect(grep('Parent', names(locJSON$data), invert=T), 
                                   grep('Child', names(locJSON$data), invert=T))]
  
  
}
