context("calcLatLong tests")

test_that("sample data from SCBI and BART converts to lat/long", {
  out<-calcLatLong(northing=c(4308017.75, 4880714.90, 4880464.93),
                   easting=c(747725.21, 318472.04, 318722.01),
                   utmZone=c('17N', '19N', '19N'))
  #check one real conversion
  expect_equal(out$decimalLatitude[1], 38.88613, tolerance = .002)
})

