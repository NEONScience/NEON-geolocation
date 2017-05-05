context("def.calc.latlong tests")

test_that("sample data from SCBI and BART converts to lat/long", {
  df<-data.frame(northing=c(4308017.75, 4880714.90, 4880464.93, 4880464.93, NA),
                 easting=c(747725.21, 318472.04, 318722.01, 318722.01, 318722.01),
                 utmZone=c('17N', '19N', '19N', NA, '19N'))
  out<-def.calc.latlong(df)
  #check one real conversion
  expect_equal(out$decimalLatitude[1], 38.88613, tolerance = .002)
  #check vals that should not convert
  expect_equal(sum(is.na(out$decimalLatitude)),2)
})


