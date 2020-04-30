##############################################################################################
#' @title Download all location data for a given NEON site

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Query the API for location data by site. Return all available locations; options for subset to IS or OS locations.
#'
#' @param site The four-letter code of a single NEON site, e.g. 'CLBJ'.
#' @param type One of 'site', 'TIS', 'TOS', 'AQU', or 'all', indicating terrestrial instrument locations (TIS), terrestrial observational locations (TOS), aquatic locations (AQU), site-level only, or all available locations. Defaults to site.
#' @param history Should the location history be included in the query? T or F, defaults to F.

#' @return A data frame of location data.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# Changelog and author contributions / copyrights
#   Claire Lunch (2020-04-07): original creation

##############################################################################################

getLocBySite <- function(site, type='site', history=F) {
  
  if(!history) {
    req <- httr::GET(paste('http://data.neonscience.org/api/v0/locations/', site, sep=''))
  }
  if(history) {
    req <- httr::GET(paste('http://data.neonscience.org/api/v0/locations/', site, 
                           '?history=true', sep=''))
  }
  
  req.content <- httr::content(req, as='parsed')

  if(!is.null(req.content$doc)) {
    stop("The NEON server is down, or your internet connection is down. 
              Check the NEON data portal for additional service messages.")
  }
  
  if(!is.null(req.content$error$status)) {
    stop(paste(site, ' is not a valid NEON site code.', sep=''))
  }
  
  if(!type %in% c('TIS','TOS','AQU','all','site')) {
    stop('Type must be one of: TIS, TOS, AQU, all, or site.')
  }
  
  loc <- jsonlite::fromJSON(httr::content(req, as='text', encoding='UTF-8'))
  
  if(type=='site') {
    loc.des <- getLocValues(loc, history)
  }
  
  if(type=='all' | type=='AQU') {
    if(loc$data$locationProperties$locationPropertyValue
       [which(loc$data$locationProperties$locationPropertyName=='Value for HABITAT')]
       =='Terrestrial') {
      cat('Warning: using getLocBySite() to access OS locations at terrestrial sites is very slow.\nMost terrestrial sites have 5000+ OS locations. A more targeted approach is recommended, such as using getLocTOS().')
    }
    loc.des <- getLocChildren(site, history)
  }
  
  if(type=='TOS') {
    if(loc$data$locationProperties$locationPropertyValue
       [which(loc$data$locationProperties$locationPropertyName=='Value for HABITAT')]
       =='Terrestrial') {
      cat('Warning: using getLocBySite() to access OS locations at terrestrial sites is very slow.\nMost terrestrial sites have 5000+ OS locations. A more targeted approach is recommended, such as using getLocTOS().')
    }
    loc.site <- getLocValues(loc, history)
    loc <- loc$data$locationChildren[which(substring(loc$data$locationChildren, 1, 4)==site)]
    loc.des <- data.table::rbindlist(lapply(loc, getLocChildren, history=history), fill=T)
    loc.des <- data.table::rbindlist(list(loc.site, loc.des), fill=T)
  }
  
  if(type=='TIS') {
    loc.site <- getLocValues(loc, history)
    loc <- loc$data$locationChildren[which(substring(loc$data$locationChildren, 1, 4)!=site)]
    loc.des <- data.table::rbindlist(lapply(loc, getLocChildren, history=history), fill=T)
    loc.des <- data.table::rbindlist(list(loc.site, loc.des), fill=T)
  }
  
  return(loc.des)
  
}
