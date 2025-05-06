##############################################################################################
#' @title Extract geolocation data from NEON API

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Get geolocation data from the NEON API, for a given set of named location values
#' 
#' @param data A data frame in which one column contains the named locations
#' @param locCol The column name of the column containing the named locations. Defaults to namedLocation
#' @param locOnly Boolean whether to return the full input data frame or just the extracted geolocations
#' @param history Boolean whether to retrieve the current location (FALSE) or the full location history (TRUE). If locOnly=FALSE and history=TRUE (the default), returns the location data that were active at the data collection date for each row.
#' @param token User specific API token (generated within data.neonscience.org user accounts). Optional.

#' @return A data frame of the geolocation data for the input named locations

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples 
#' d <- data.frame(namedLocation=c("GUAN_044.basePlot.ltr","GRSM_003.birdGrid.brd"), otherData=c(1,2))
#' getLocByName(d, "namedLocation")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-09-03)
#     adapted from and replaces def.extr.geo.os()
##############################################################################################
getLocByName <- function(
  data,
  locCol = "namedLocation",
  locOnly=FALSE,
  history=TRUE,
  token=NA_character_
){
  
  # ensure data are in data frame
  data <- as.data.frame(data, stringsAsFactors=F)
  
  # Initiate list of outputs
  outList <- list()
  pb <- utils::txtProgressBar(min = 0, max = length(unique(data[,locCol])), style = 3)
  i<-0 # i iterates for progress bar
  # Iterate over input locations
  for (j in unique(data[,locCol])) {
    utils::setTxtProgressBar(pb, i)
    i<-i+1
    
    # Pull data from API
    if(history) {
      req <- getAPI(paste("https://data.neonscience.org/api/v0/locations/", j, 
                          "?history=true", sep=""), 
                    token=token)
    } else {
      req <- getAPI(paste("https://data.neonscience.org/api/v0/locations/", j, sep=""), 
                    token=token)
    }

    req.content <- httr::content(req, as="parsed")
    
    # Give warnings for missing values & API errors
    if (!is.null(req.content$doc)){
      message("The NEON server is down, or your internet connection is down. 
              Check the NEON data portal for additional service messages.")
      next
    }
    if (!is.null(req.content$error$status)){
      message(paste("The following namedLocation was not found:",
                    j, sep=" "))
      next
    }
    
    # Extract location properties from JSON
    vals <- getLocValues(req.content, history=history)
    
    # Write out the list of location properties
    outList[[length(outList)+1]]<-data.frame(lapply(vals, as.character), stringsAsFactors=FALSE)
  }
  
  if(length(outList)==0) {
    stop('\nNone of the input named locations were found.')
  }
  
  # Make data frame of locations to return
  plotInfo <- plyr::rbind.fill(outList)
  
  if(history) {
    allTerms <- c('domainID', 'type', 'description', 'filteredPositions', 'coordinateSource',
                  'minimumElevation','slopeGradient', 'plotPdop', 'plotHdop', 'slopeAspect', 
                  'maximumElevation', 'plotSize','subtype', 'referencePointPosition', 
                  'plotType', 'siteID', 'easting','northing' ,'utmZone','elevation',
                  'decimalLatitude', 'decimalLongitude','namedLocationCoordUncertainty', 
                  'namedLocationElevUncertainty','nlcdClass','plotDimensions','soilTypeOrder', 
                  'subtypeSpecification', 'county', 'stateProvince', 'country','plotID','locationPointID',
                  'locationDescription','locationType','utmHemisphere','utmZoneNumber',
                  'alphaOrientation','betaOrientation','gammaOrientation','xOffset',
                  'yOffset','zOffset','locationParent','locationParentUrl','geodeticDatum',
                  'current','locationStartDate','locationEndDate')
  } else {
    allTerms <- c('domainID', 'type', 'description', 'filteredPositions', 'coordinateSource',
                  'minimumElevation','slopeGradient', 'plotPdop', 'plotHdop', 'slopeAspect', 
                  'maximumElevation', 'plotSize','subtype', 'referencePointPosition', 
                  'plotType', 'siteID', 'easting','northing' ,'utmZone','elevation',
                  'decimalLatitude', 'decimalLongitude','namedLocationCoordUncertainty', 
                  'namedLocationElevUncertainty','nlcdClass','plotDimensions','soilTypeOrder', 
                  'subtypeSpecification', 'county', 'stateProvince', 'country','plotID','locationPointID',
                  'locationDescription','locationType','utmHemisphere','utmZoneNumber',
                  'alphaOrientation','betaOrientation','gammaOrientation','xOffset',
                  'yOffset','zOffset','locationParent','locationParentUrl','geodeticDatum')
  }
  
  # Fill unused fields with NA
  plotInfo[,allTerms[!allTerms %in% (names(plotInfo))]] <- NA
  
  # add blank column if all values are invalid
  if (!'namedLocation' %in% names(plotInfo)){
    plotInfo$namedLocation<-NA
  }
  
  utils::setTxtProgressBar(pb, length(unique(data[,locCol])))
  close(pb)

  # Return the original data with location data added, unless locOnly=T
  # Only add columns that weren't already in the data
  messages <- NA
  if(!locOnly) {
    data$row.index <- 1:nrow(data)
    # check for locations with history. if there are none, use non-history workflow
    if(!any(duplicated(plotInfo$namedLocation))) {
      history <- FALSE
    }
    if(!history) {
      dataRep <- data[data[,locCol] %in% plotInfo$namedLocation,
                      names(data) %in% names(plotInfo)]
      
      # if no names are shared, merge and done
      if(length(dataRep)==0 | is.null(dim(dataRep))) {
        allInfo <- base::merge(data, plotInfo, by.x=locCol, by.y='namedLocation', all.x=T)
        allInfo <- allInfo[order(allInfo$row.index),]
        allInfo <- allInfo[,!names(allInfo) %in% c('row.index')]
      } else {
        
        # make sure to include location column
        if(!locCol %in% names(dataRep)) {
          dataRep <- cbind(dataRep, 
                           d=data[data[,locCol] %in% plotInfo$namedLocation,locCol])
          names(dataRep)[which(names(dataRep)=='d')] <- locCol
        }
        
        # iterate over shared names
        for(i in names(dataRep)) {
          if(i=='namedLocation' | i==locCol) {
            next
          } else {
            
            # check whether values in data match values in plotInfo (from API) for matching named locations
            # have to handle character and numeric separately - all.equal behaves strangely, so better to split
            dataRepUniq <- unique(dataRep[order(dataRep[,locCol]), c(locCol, i)])
            plotInfoUniq <- plotInfo[order(plotInfo$namedLocation), c('namedLocation',i)]
            locMatch <- TRUE
            eqVec <- !logical(length(dataRepUniq[,i]))
            if(inherits(dataRep[,i],'character')) {
              eqVec <- dataRepUniq[,i]==plotInfoUniq[,i]
              if(!all(eqVec, na.rm=T)) {locMatch <- FALSE}
            } else {
              if(inherits(dataRep[,i],'numeric')) {
                eqVec <- abs(dataRepUniq[,i] - as.numeric(plotInfoUniq[,i]))
                eqVec <- eqVec <= 0.5
                if(!all(eqVec, na.rm=T)) {locMatch <- FALSE}
              } else {
                eqVec <- eqVec
              }
            }
            # if mismatches are found, make a list of the named locations where values don't match
            # and drop the variable from the data table - will be replaced by database version in the merge
            if(!locMatch) {
              locMis <- plotInfo$namedLocation[order(plotInfo$namedLocation)][which(!eqVec)]
              messages <- rbind(messages, cbind(rep(i, length(locMis)), locMis))
              data <- data[,names(data)!=i]
            }
          }
        }
        # drop variables from plotInfo that were already in data, and matched
        plotInfo <- plotInfo[,!names(plotInfo) %in% names(data)[names(data)!='namedLocation']]
        # merge data and plotInfo - no columns besides namedLocation should be in both at this point
        allInfo <- base::merge(data, plotInfo, by.x=locCol, by.y='namedLocation', all.x=T)
        allInfo <- allInfo[order(allInfo$row.index),]
        allInfo <- allInfo[,!names(allInfo) %in% c('row.index')]
      }
      # report locations and variables with value mismatches
      if(!all(is.na(messages))) {
        colnames(messages) <- c('variable', 'namedLocation')
        message('\nMismatch between input data and location database for the following variables and locations:\n')
        print.table(messages[-1,])
        message('\nUsually this indicates database has been updated since data were processed. Output data are database values.')
      }
    } else {
      # when history=TRUE
      # in this case, use database values without checking
      # match dates to decide which values from the history to keep
      dataRep <- intersect(names(data), names(plotInfo))
      if(locCol %in% dataRep) {
        dataRep <- dataRep[-which(dataRep==locCol)]
      }
      if("namedLocation" %in% dataRep) {
        dataRep <- dataRep[-which(dataRep=="namedLocation")]
      }
      
      # drop columns from data that are available in location data
      data <- data[,-which(names(data) %in% dataRep)]
      
      # merge data and plotInfo - no columns besides namedLocation should be in both at this point
      allInfo <- base::merge(data, plotInfo, by.x=locCol, by.y="namedLocation", all.x=T)
      
      # find locations with history
      histloc <- unique(allInfo$namedLocation[which(allInfo$current==FALSE)])
      histind <- which(allInfo$namedLocation %in% histloc)
      histsub <- allInfo[histind,]
      
      # remove the rows with history and add back below
      allInfo <- allInfo[-histind,]
      
      # uids should be duplicated for locations with history. for each uid, figure out which one to keep based on date.
      for(j in unique(histsub$uid)) {
        subj <- histsub[which(histsub$uid==j),]
        
        # if contents are identical keep the first one - this will probably never happen
        if(all(duplicated(subj)[2:nrow(subj)])) {
          allInfo <- base::rbind(allInfo, subj[1,])
        } else {
          # get date field from data to compare to location dates
          datefields <- c("collectDate", "endDate", "date", "startDate")
          datefield <- intersect(datefields, names(subj))
          if(length(datefield)>1) {
            datefield <- datefield[1]
          }
          if(length(datefield)==0) {
            message("Valid dates could not be identified for location ", 
                          subj$namedLocation[1], 
                          ". Spatial data returned match most recent valid date.", sep="")
            allInfo <- base::rbind(allInfo, 
                             subj[which(subj$locationStartDate==max(subj$locationStartDate, na.rm=T)),])
          }
          dates <- unique(subj[,datefield])
          if(length(dates)>1) {
            message("Valid dates could not be identified for location ", 
                    subj$namedLocation[1], 
                    ". Spatial data returned match most recent valid date.", sep="")
            allInfo <- base::rbind(allInfo, 
                             subj[which(subj$locationStartDate==max(subj$locationStartDate, na.rm=T)),])
          }
          startind <- which(subj$locationStartDate <= dates)
          endind <- union(which(subj$locationEndDate > dates), which(is.na(subj$locationEndDate)))
          indj <- intersect(startind, endind)
          if(length(indj)==0) {
            message(paste(unique(dates), " is outside the valid date range for location ", 
                          subj$namedLocation[1], 
                          ". Spatial data returned match most recent valid date.", sep=""))
            allInfo <- base::rbind(allInfo, 
                             subj[which(subj$locationStartDate==max(subj$locationStartDate, na.rm=T)),])
          } else {
            allInfo <- base::rbind(allInfo, subj[indj,])
          }
        }
      }
      allInfo <- allInfo[order(allInfo$row.index),]
      allInfo <- allInfo[,!names(allInfo) %in% c('row.index')]
    }

  } else { 
    # when locOnly=TRUE (either value of history)
    allInfo <- plotInfo
  }
  return(allInfo)
}
