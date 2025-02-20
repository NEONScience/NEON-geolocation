# context("Test getLocBySite")
# library(geoNEON)
# 
# test_that("SYCA location values are correct", {
#   out <- getLocBySite("SYCA", type="all", history=T)
#   expect_equal(out$locationDescription, c('Plot \"GUAN_044\" at site \"GUAN\"', 'Plot \"GUAN_045\" at site \"GUAN\"'))
#   expect_equal(as.numeric(out$northing), c(1987798.71019852, 1988068.47644692), tolerance=0.5)
# })
# 
# 
# test_that("Single nonsense input correctly returns warning message", {
#   df<-data.frame(namedLocation=c("nonsense", "GUAN_044.basePlot.ltr"), otherData=c(1,2))
#   expect_message(getLocByName(df),'The following namedLocation was not found: nonsense')
# })
# 
# test_that("All nonsense inputs correctly return error", {
#   df<-data.frame(namedLocation=c("nonsense", "nonsense"), otherData=c(1,2))
#   expect_error(suppressWarnings(getLocByName(df)),'None of the input named locations were found.')
# })




