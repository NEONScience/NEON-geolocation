##############################################################################################
#' @title Calculate more precise geolocations for specific NEON data products

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON data products, based on product-specific rules and spatial designs.
#' 
#' @param data A data frame containing NEON named locations and other sampling information. For reliable results, use data tables as downloaded from the NEON data portal or API.
#' @param dataProd The table name of the NEON data product table to find locations for. Refer to package readme for list of possible input data tables.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

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
  token=NA_character_
){
  
  # convert format for safety
  data <- data.frame(data)
  
  # Litter trap, herb clip, cfc herb clip, and root sampling location calculations
  # These are all protocols using clip strips
  if(dataProd=="ltr_pertrap" | dataProd=="hbp_perbout" | dataProd=="cfc_fieldData" | 
     dataProd=="bbc_percore"){
    
    all.return <- getLocClip(data=data, dataProd=dataProd, token=token)
    return(all.return)
  }
  
  # Soil core location calculations:
  if(dataProd=="sls_soilCoreCollection") {
    
    all.return <- getLocSLS(data=data, token=token)
    return(all.return)
  }
  
  # Bird point calculations:
  if(dataProd=="brd_perpoint" | dataProd=="brd_countdata") {

    all.return <- getLocBRD(data=data, dataProd=dataProd, token=token)
    return(all.return)
  }
  
  #Plant phenology individual location calculations:
  if(dataProd=="phe_perindividual") {

    all.return <- getLocPHE(data=data, token=token)
    return(all.return)
  }

  
  #Small mammal trap locations:
  if(dataProd=="mam_pertrapnight"){
    
    all.return <- getLocMAM(data=data, token=token)
    return(all.return)
  }
  
  #Plant present and percent cover subplot centroids:
  if(dataProd=="div_1m2Data"|dataProd=="div_10m2Data100m2Data"){
    
    all.return <- getLocDIV(data=data, token=token)
    return(all.return)
  }
  
  # woody vegetation structure locations of individuals
  if(dataProd=='vst_mappingandtagging'){
    
    print("Please note locations will be calculated only for mapped woody individuals. To find subplot locations for unmapped individuals, use this function with the vst_apparentindividual, vst_non-woody, and/or vst_shrubgroup tables.")
    
    ind.return <- getLocVSTmapped(data=data, token=token)
    return(ind.return)
    
  }
  
  # vegetation structure locations of subplots for unmapped individuals
  if(dataProd=="vst_apparentindividual" | 
     dataProd=="vst_non-woody" | 
     dataProd=="vst_shrubgroup") {
    
    print("Please note locations will be calculated for all subplots. For mapped individuals, it is possible to calculate more precise locations by using this function with the vst_mappingandtagging table.")
    
    subplot.return <- getLocVSTsubplots(data=data, token=token)
    return(subplot.return)
    
  }
    
  if(dataProd=='cdw_fieldtally') {
    
    all.return <- getLocCDW(data=data, token=token)
    return(all.return)
    
  }
    
  if(dataProd=="bet_fielddata") {
    
    trap.return <- getLocBET(data=data, token=token)
    return(trap.return)
    
  }
  
  if(dataProd=="mos_trapping") {
    cat('Mosquito trapping location is flexible within the plot; plot-level location and uncertainty provided in downloaded data are accurate.')
  }

  if(dataProd=="tck_fielddata") {
    cat('Ticks are sampled around the entire perimeter of the plot; plot-level location and uncertainty provided in downloaded data are accurate.')
  }
  
  if(dataProd=='dhp_perimagefile') {
    
    plot.return <- getLocDHP(data=data, token=token)
    return(plot.return)
  }
  
  if(dataProd=='spc_perplot') {
    
    plot.return <- getLocSPC(data=data, token=token)
    return(plot.return)
  }
  
  if(dataProd=='sim_eventData') {
    
    plot.return <- getLocSIM(data=data, token=token)
    return(plot.return)
    
  }
  
  else {
    print(paste("This function has not been configured for data product table ", 
                 dataProd, sep=""))
  }
}

