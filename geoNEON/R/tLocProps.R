##############################################################################################
#' @title Transpose dataframe of value names and values into single-row dataframe of values with names

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Transpose location properties format. Helper function for getLocValues().
#'
#' @param locProps A data frame of location properties values as returned by the NEON API.
#' 
#' @keywords internal

#' @return A data frame of location properties with columns named by the value names.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2020-04-23): original creation

##############################################################################################

tLocProps <- function(locProps) {
  
  vals <- locProps$locationPropertyValue
  if(is.null(vals)) {
    return()
  } else {
    names(vals) <- locProps$locationPropertyName
    vals <- t(vals)
    vals <- data.frame(vals)
    return(vals)
  }
    
}

