context("def.calc.geo.os")

test_that("Select bird named locations are correct", {
  df<-data.frame(namedLocation=c('SCBI_023.basePlot.brd', 'BART_003.birdGrid.brd', 
                                'BART_003.birdGrid.brd'), pointID=c(21,'A1', 'B2'))
  out<-def.calc.geo.os(df, 'NEON.DP1.10003.001')
  expect_equal(as.numeric(out$northing), c(4308018.7592, 4880715.9042, 4880465.9323), tolerance=1)
  expect_equal(as.numeric(out$coordinateUncertainty), c(0.24, 15, 0.32), tolerance=0.01)
})


