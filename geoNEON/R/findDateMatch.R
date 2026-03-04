##############################################################################################
#' @title Find the location data matching the sampling date

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' For a NEON location with a history, find the location matching the sampling date for a given sampling event. Used internally in TOS location calculations.
#' 
#' @param data A data frame including named locations and sampling dates, with duplicated records for locations with a history. This function identifies the record to keep.
#' @param locCol The column name of the column containing the named locations
#' @param recDate The column name of the date field to be used for matching

#' @return The original data frame with the correct location data added

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords none

#' @seealso Currently none
#' 


# changelog and author contributions / copyrights
#   Claire Lunch (2025-02-26)
##############################################################################################
findDateMatch <- function(
  data,
  locCol,
  recDate
){
  
  # check for uid column
  if(!"uid" %in% colnames(data)) {
    stop("Data file does not include uid column. Cannot determine location history.")
  }
  
  # find locations with history
  histloc <- unique(data[,locCol][which(data$locationCurrent=="FALSE" | isFALSE(data$locationCurrent))])
  histind <- which(data[,locCol] %in% histloc)
  histsub <- data[histind,]
  
  # remove the rows with history and add back below
  data <- data[-histind,]
  
  # uids should be duplicated for locations with history. for each uid, figure out which one to keep based on date.
  for(j in unique(histsub$uid)) {
    subj <- histsub[which(histsub$uid==j),]
    
    # if contents are identical keep the first one - this will probably never happen
    if(all(duplicated(subj)[2:nrow(subj)])) {
      data <- rbind(data, subj[1,])
    } else {
      datefield <- recDate
      dates <- unique(subj[,datefield])
      if(length(dates)>1 | length(dates)==0) {
        message("Valid dates could not be identified for location ", 
                subj$namedLocation[1], 
                ". Spatial data returned match most recent valid date.", sep="")
        data <- rbind(data, 
                         subj[which(subj$locationStartDate==max(subj$locationStartDate, na.rm=T)),])
      }
      startind <- which(subj$locationStartDate <= dates)
      endind <- union(which(subj$locationEndDate > dates), which(is.na(subj$locationEndDate)))
      indj <- intersect(startind, endind)
      if(length(indj)==0) {
        message(paste(unique(dates), " is outside the valid date range for location ", 
                      subj$namedLocation[1], 
                      ". Spatial data returned match most recent valid date.", sep=""))
        data <- rbind(data, 
                         subj[which(subj$locationStartDate==max(subj$locationStartDate, na.rm=T)),])
      } else {
        data <- rbind(data, subj[indj,])
      }
    }
  }

  return(data)
}
