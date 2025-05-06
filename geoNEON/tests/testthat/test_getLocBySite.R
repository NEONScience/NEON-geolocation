context("Test getLocBySite")
library(geoNEON)

test_that("Select SYCA location values and history are correct", {
  out <- getLocBySite("SYCA", type="all", history=T)
  expect_true(all(out$current[which(is.na(out$locationEndDate))]==TRUE))
  expect_identical(min(out$locationStartDate[which(out$namedLocation=="CFGLOC114125")]), 
               "2021-09-30T00:00:00Z")
  expect_equal(as.numeric(out$easting[which(out$namedLocation=="S1LOC111007" & out$locationStartDate=="2022-01-25T00:00:00Z")]),
               453099.49, tolerance=0.1)
})

test_that("Select ABBY locations without history are correct", {
  out <- getLocBySite("ABBY", type="TIS", history=F)
  expect_equal(as.numeric(out$easting[which(out$locationType=="HUT")]), 552091.11, tolerance=0.1)
})





