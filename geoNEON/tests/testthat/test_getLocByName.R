context("Test getLocByName")
library(geoNEON)

test_that("GUAN_044.basePlot.ltr location values are correct", {
  df<-data.frame(uid=c("uid1","uid2"), 
                 namedLocation=c("GUAN_044.basePlot.ltr", "GUAN_045.basePlot.ltr"), 
                 otherData=c(1,2))
  out<-getLocByName(df)
  out<-out[order(out$namedLocation),]
  expect_equal(out$locationDescription, c('Plot \"GUAN_044\" at site \"GUAN\"', 'Plot \"GUAN_045\" at site \"GUAN\"'))
  expect_equal(as.numeric(out$northing), c(1987798.71019852, 1988068.47644692), tolerance=0.5)
})


test_that("Single nonsense input correctly returns warning message", {
  df<-data.frame(uid=c("uid1","uid2"), 
                 namedLocation=c("nonsense", "GUAN_044.basePlot.ltr"), 
                 otherData=c(1,2))
  expect_message(getLocByName(df),'The following namedLocation was not found: nonsense')
})

test_that("All nonsense inputs correctly return error", {
  df<-data.frame(uid=c("uid1","uid2"), namedLocation=c("nonsense", "nonsense"), 
                 otherData=c(1,2))
  expect_error(suppressWarnings(getLocByName(df)),'None of the input named locations were found.')
})

test_that("Locations with history resolve correctly", {
  df<-data.frame(uid=c('uid1','uid2'),
                 namedLocation=c('OSBS_010.basePlot.dhp','OSBS_010.basePlot.dhp'),
                 pointID=c('E10','E10'),
                 endDate=c(as.POSIXct('2018-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                           as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocByName(df)
  out<-out[order(out$namedLocation),]
  expect_equal(as.numeric(out$northing), c(3285002.0839, 3285001.74674), tolerance=0.1)
})

test_that("Locations with history return correctly when locOnly=T", {
  df<-data.frame(uid=c("uid1","uid2"), 
                 namedLocation=c('JERC_006.basePlot.bgc', 'JERC_006.basePlot.bgc'), 
                 collectDate=c(as.POSIXct('2015-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2023-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocByName(df, locOnly = T, history=T)
  out<-out[order(out$namedLocation),]
  expect_equal(as.numeric(out$namedLocationElevUncertainty), c(0.14, 0.27), tolerance=0.02)
})


