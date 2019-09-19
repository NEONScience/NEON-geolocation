##############################################################################################
#' @title Calculate more precise geolocations for specific NEON data products

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description 
#' Calculation Function. Refine the geolocation data associated with NEON data products, based on product-specific rules and spatial designs.
#' 
#' @param data A data frame containing NEON named locations and other sampling information. For reliable results, use data tables as downloaded from the NEON data portal or API.
#' @param dataProd The table name of the NEON data product table to find locations for. Must be one of: ltr_pertrap, hbp_perbout, sls_soilCoreCollection, brd_perpoint or brd_countdata, mam_pertrapnight, div_1m2Data or div_10m2Data100m2Data, vst_mappingandtagging.

#' @return A data frame of geolocations for the input product and data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples 
#' d <- data.frame(namedLocation="GUAN_044.basePlot.ltr", subplotID=23, trapID="GUAN_044_385")
#' def.calc.geo.os(d, "ltr_pertrap")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   deprecated 2019-09-03 - use getLocTOS()
#   inherits from getLocTOS() for back compatibility
#   Claire Lunch (2017-02-08)
#     original creation
##############################################################################################
def.calc.geo.os <- function(
  data,
  dataProd
){
  
  getLocTOS(data=data, dataProd=dataProd)
  
}

