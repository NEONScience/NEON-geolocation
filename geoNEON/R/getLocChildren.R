##############################################################################################
#' @title Find names of child (and further descendent) named locations from the NEON API.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Find all descendent named locations of a given named location; return all names.
#'
#' @param namedLocation A NEON named location name.

#' @return A vector of named location names of all descendents of the input named location.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2020-04-13): original creation

##############################################################################################

getLocChildren <- function(namedLocation) {
  
  req <- httr::GET(paste('http://data.neonscience.org/api/v0/locations/', namedLocation, sep=''))
  loc <- jsonlite::fromJSON(httr::content(req, as='text', encoding='UTF-8'))

  loc.children <- loc$data[base::grep('Child', base::names(loc$data))]$locationChildren
  
  cat('Finding children of', namedLocation, rep('', 50), '\r')
  flush.console()
  
  if(length(loc.children)==0) {
    return()
  } else {
    loc.next <- unlist(lapply(loc.children, getLocChildren))
    loc.all <- c(loc.children, loc.next)
    
    return(loc.all)
  }
  
}
