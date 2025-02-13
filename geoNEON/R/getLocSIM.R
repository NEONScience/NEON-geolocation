##############################################################################################
#' @title Calculate more precise geolocations for site management data (DP1.10111.001)

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON site management data.
#' 
#' @param data A data frame containing NEON named locations and other sampling information.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2025-02-13)
##############################################################################################
getLocSIM <- function(
    data,
    token=NA_character_
){
  
  # get individual locations from the location column
  loclist <- lapply(data$locationID, FUN=function(x) {
    a <- unlist(strsplit(unlist(x), "[,][ ]{0,}"))
    a <- data.frame(namedLocation=a)
    return(a)
  })
  locset <- unique(unlist(loclist))
  
  # get spatial data for each location
  locdata <- getLocByName(data.frame(namedLocation=locset), 
                          locOnly = T, token=Sys.getenv('NEON_TOKEN'))
  locd <- locdata[,1:46]
  
  # merge spatial data back into data frame
  for(i in 1:length(loclist)) {
    data$locationID[i] <- list(base::merge(loclist[[i]], 
                                           locd, by="namedLocation",
                                           all.x=T))
  }
  return(data)
 
}
