context("def.extr.geo tests")

test_that("GUAN_044.basePlot.ltr location values are correct", {
  df<-data.frame(namedLocation=c("GUAN_044.basePlot.ltr", "GUAN_045.basePlot.ltr"), otherData=c(1,2))
  out<-def.extr.geo.os(df)
  out<-out[order(out$namedLocation),]
  expect_equal(out$description, c('Plot \"GUAN_044\" at site \"GUAN\"', 'Plot \"GUAN_045\" at site \"GUAN\"'))
  expect_equal(as.numeric(out$northing), c(1987798.71019852, 1988068.47644692))
})


test_that("error messages are correct", {
  df<-data.frame(namedLocation=c("nonsense", "nonsense"), otherData=c(1,2))
  expect_warning(def.extr.geo.os(df),'WARNING: the following namedLocation was not found: nonsense')
})


