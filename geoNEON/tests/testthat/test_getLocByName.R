context("Test getLocByName")
library(geoNEON)

test_that("GUAN_044.basePlot.ltr location values are correct", {
  df<-data.frame(namedLocation=c("GUAN_044.basePlot.ltr", "GUAN_045.basePlot.ltr"), otherData=c(1,2))
  out<-getLocByName(df)
  out<-out[order(out$namedLocation),]
  expect_equal(out$locationDescription, c('Plot \"GUAN_044\" at site \"GUAN\"', 'Plot \"GUAN_045\" at site \"GUAN\"'))
  expect_equal(as.numeric(out$northing), c(1987798.71019852, 1988068.47644692))
})


test_that("Single nonsense input correctly returns warning message", {
  df<-data.frame(namedLocation=c("nonsense", "GUAN_044.basePlot.ltr"), otherData=c(1,2))
  expect_warning(getLocByName(df),'WARNING: the following namedLocation was not found: nonsense')
})

test_that("All nonsense inputs correctly return error", {
  df<-data.frame(namedLocation=c("nonsense", "nonsense"), otherData=c(1,2))
  expect_error(suppressWarnings(getLocByName(df)),'None of the input named locations were found.')
})




