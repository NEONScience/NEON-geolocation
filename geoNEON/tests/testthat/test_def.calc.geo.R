context("def.calc.geo.os")

test_that("Select bird named locations are correct", {
  df<-data.frame(namedLocation=c('SCBI_023.basePlot.brd', 'BART_003.birdGrid.brd', 
                                'BART_003.birdGrid.brd', 'BART_003.birdGrid.brd'), pointID=c(21,'A1', 'B2', 'B2'),
                 pointCountMinute=c(1,2,3,88))
  out<-def.calc.geo.os(df, 'brd_countdata')
  #out<-out[order(row.names(out),]
  expect_equal(as.numeric(out$northing), c(4308018.7592, 4880715.9042, 4880465.9323, 4881510.4750), tolerance=1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(0.24, 15, 0.32, NA), tolerance=0.01)
})


