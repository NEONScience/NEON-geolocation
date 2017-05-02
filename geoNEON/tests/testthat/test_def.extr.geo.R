context("def.extr.geo tests")

test_that("GUAN_044.basePlot.ltr location values are correct", {
  df<-data.frame(namedLocation="GUAN_044.basePlot.ltr")
  out<-def.extr.geo.os(df)
  expect_equal(out$description, ('Plot \"GUAN_044\" at site \"GUAN\"'))
  expect_equal(as.numeric(out$northing), 1987798.71019852)
})


test_that("error messages are correct", {
  df<-data.frame(namedLocation="nonsense")
  expect_warning(def.extr.geo.os(df),'WARNING: the following namedLocation was not found: nonsense')
})


