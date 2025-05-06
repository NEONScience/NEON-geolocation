##############################################################################################
#' @title Find names of child (and further descendent) named locations from the NEON API.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Find all descendent named locations of a given named location; return all names.
#'
#' @param namedLocation A NEON named location name.
#' @param history Should the location history be included in the query? T or F, defaults to F.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.
#' 
#' @keywords internal

#' @return A vector of named location names of all descendents of the input named location.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2020-04-13): original creation

##############################################################################################

getLocChildren <- function(namedLocation, history=F, token=NA_character_) {
  
  if(!history) {
    req <- getAPI(paste('https://data.neonscience.org/api/v0/locations/', namedLocation, sep=''),
                     token=token)
  }
  if(history) {
    req <- getAPI(paste('https://data.neonscience.org/api/v0/locations/', namedLocation, 
                           '?history=true', sep=''), token=token)
  }
  
  if(!inherits(req, 'response')) {
    return()
  } else {
    req.content <- httr::content(req, as='parsed')
    if(!is.null(req.content$error$status)) {
      return()
    } else {
      
      #loc <- jsonlite::fromJSON(httr::content(req, as='text', encoding='UTF-8'))
      loc <- req.content
      
      loc.children <- loc$data$locationChildren
      loc.values <- getLocValues(loc, history)
      
      message('Finding spatial data for', namedLocation, rep('', 50), '\r')
      #utils::flush.console()
      
      if(length(loc.children)==0) {
        loc.all <- getLocValues(loc, history)
        return(loc.all)
      } else {
        loc.all <- data.table::rbindlist(list(loc.values, 
                                    data.table::rbindlist(lapply(loc.children, getLocChildren, history), 
                                                          fill=T)), fill=T)
        loc.all <- data.frame(loc.all)
        return(loc.all)
      }
    }
  }
  
}
