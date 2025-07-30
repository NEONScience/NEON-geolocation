##############################################################################################
#' @title Calculate more precise geolocations for specific NEON data products

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON data products, based on product-specific rules and spatial designs.
#' 
#' @param data A data frame containing NEON named locations and other sampling information. For reliable results, use data tables as downloaded from the NEON data portal or API.
#' @param dataProd The table name of the NEON data product table to find locations for. Refer to package readme for list of possible input data tables.
#' @param convertBLAN T or F: Convert locations at BLAN and LEWI from 18N to 17N to match remote sensing data? Defaults to FALSE.
#' @param token User specific API token (generated within data.neonscience.org user accounts). Optional.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples 
#' d <- data.frame(namedLocation="GUAN_044.basePlot.ltr", subplotID=23, trapID="GUAN_044_385")
#' getLocTOS(d, "ltr_pertrap")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-09-03)
#     adapted from and replaces def.calc.geo.os()
#   2025-02-13 Updated to separate each module function into its own file for easier maintenance
##############################################################################################
getLocTOS <- function(
  data,
  dataProd,
  convertBLAN=FALSE,
  token=NA_character_
){
  
  # convert format for safety
  data <- data.frame(data)
  
  dat.return <- NULL
  
  # Litter trap, herb clip, cfc herb clip, and root sampling location calculations
  # These are all protocols using clip strips
  if(dataProd=="ltr_pertrap" | dataProd=="hbp_perbout" | dataProd=="cfc_fieldData" | 
     dataProd=="bbc_percore"){
    
    dat.return <- getLocClip(data=data, dataProd=dataProd, token=token)
  }
  
  # Soil core location calculations:
  if(dataProd=="sls_soilCoreCollection") {
    
    dat.return <- getLocSLS(data=data, token=token)
  }
  
  # Bird point calculations:
  if(dataProd=="brd_perpoint" | dataProd=="brd_countdata") {
    
    dat.return <- getLocBRD(data=data, dataProd=dataProd, token=token)
  }
  
  #Plant phenology individual location calculations:
  if(dataProd=="phe_perindividual") {
    
    dat.return <- getLocPHE(data=data, token=token)
  }
  
  
  #Small mammal trap locations:
  if(dataProd=="mam_pertrapnight"){
    
    dat.return <- getLocMAM(data=data, token=token)
  }
  
  #Plant present and percent cover subplot centroids:
  if(dataProd=="div_1m2Data"|dataProd=="div_10m2Data100m2Data"){
    
    dat.return <- getLocDIV(data=data, token=token)
  }
  
  # woody vegetation structure locations of individuals
  if(dataProd=='vst_mappingandtagging'){
    
    message("Please note locations will be calculated only for mapped woody individuals. To find subplot locations for unmapped individuals, use this function with the vst_apparentindividual, vst_non-woody, and/or vst_shrubgroup tables.")
    
    dat.return <- getLocVSTmapped(data=data, token=token)
    
  }
  
  # vegetation structure locations of subplots for unmapped individuals
  if(dataProd=="vst_apparentindividual" | 
     dataProd=="vst_non-woody" | 
     dataProd=="vst_shrubgroup") {
    
    message("Please note locations will be calculated for all subplots. For mapped individuals, it is possible to calculate more precise locations by using this function with the vst_mappingandtagging table.")
    
    dat.return <- getLocVSTsubplots(data=data, token=token)
    
  }
  
  if(dataProd=='cdw_fieldtally') {
    
    dat.return <- getLocCDW(data=data, token=token)
    
  }
  
  if(dataProd=="bet_fielddata") {
    
    dat.return <- getLocBET(data=data, token=token)
    
  }
  
  if(dataProd=="mos_trapping") {
    message('Mosquito trapping location is flexible within the plot; plot-level location and uncertainty provided in downloaded data are accurate.')
  }
  
  if(dataProd=="tck_fielddata") {
    message('Ticks are sampled around the entire perimeter of the plot; plot-level location and uncertainty provided in downloaded data are accurate.')
  }
  
  if(dataProd=='dhp_perimagefile') {
    
    dat.return <- getLocDHP(data=data, token=token)
  }
  
  if(dataProd=='spc_perplot') {
    
    dat.return <- getLocSPC(data=data, token=token)
  }
  
  if(dataProd=='sim_eventData') {
    
    dat.return <- getLocSIM(data=data, token=token)
    
  }
  
  if(is.null(dat.return)) {
    message(paste("This function has not been configured for data product table ", 
                  dataProd, sep=""))
    return(invisible())
  }
  
  if(all(c("siteID","adjEasting","adjNorthing") %in% names(dat.return))) {
    if(any(c("BLAN","LEWI") %in% dat.return$siteID) &
       isTRUE(convertBLAN)) {
      dat.conv <- try(convertBLAN(data=dat.return, 
                                  easting="adjEasting",
                                  northing="adjNorthing"), silent=TRUE)
      if(inherits(dat.conv, "try-error")) {
        message("BLAN/LEWI location conversion failed, locations are in original UTM zones.")
      } else {
        dat.return <- dat.conv
        message("BLAN/LEWI locations in 18N have been converted to 17N to match remote sensing data.")
      }
    }
  }
  
  return(dat.return)
  
}

