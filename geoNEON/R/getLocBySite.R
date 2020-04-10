##############################################################################################
#' @title Download all location data for a given NEON site

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Query the API for location data by site. Return all available locations; options for subset to IS or OS locations.
#'
#' @param site The four-letter code of a single NEON site, e.g. 'CLBJ'.
#' @param type One of 'IS', 'OS', or 'all', indicating instrumented (IS), observational (OS), or all locations. Defaults to IS.

#' @return A data frame of location data.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# Changelog and author contributions / copyrights
#   Claire Lunch (2020-04-07): original creation

##############################################################################################

getLocBySite <- function(site, type='IS') {
  
  req <- httr::GET(paste('http://data.neonscience.org/api/v0/locations/', site, sep=''))
  req.content <- httr::content(req, as='parsed')

  if(!is.null(req.content$doc)) {
    stop("The NEON server is down, or your internet connection is down. 
              Check the NEON data portal for additional service messages.")
  }
  if(!is.null(req.content$error$status)) {
    stop(paste(site, ' is not a valid NEON site code.', sep=''))
  }
  
  loc <- jsonlite::fromJSON(httr::content(req, as='text', encoding='UTF-8'))
  loc.values <- getLocValues(loc)
  
  if(types=='IS') {
    loc <- loc$data$locationChildrenUrls[which(substring(loc$data$locationChildren, 1, 4)!=site)]
  }
  
}

