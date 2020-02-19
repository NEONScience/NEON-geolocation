context("Test getLocTOS for each data product type")
library(geoNEON)

test_that("Select bird named locations are correct", {
  df<-data.frame(namedLocation=c('SCBI_023.basePlot.brd', 'BART_003.birdGrid.brd', 
                                'BART_003.birdGrid.brd', 'BART_003.birdGrid.brd'), pointID=c(21,'A1', 'B2', 'B2'),
                 pointCountMinute=c(1,2,3,88))
  out<-getLocTOS(df, 'brd_countdata')
  expect_equal(as.numeric(out$adjNorthing), c(4308017.759165, 4880714.9041744, 4880464.93234, 4881510.4750), tolerance=0.1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(0.24, 15, 0.32, NA), tolerance=0.01)
})

test_that("Select smammal named locations are correct", {
  df<-data.frame(namedLocation=c('BART_001.mammalGrid.mam', 'BART_001.mammalGrid.mam',
                                 'BART_084.mammalGrid.mam'), trapCoordinate=c('J9','X1', 'E5'))
  out<-getLocTOS(df, 'mam_pertrapnight')
  expect_equal(as.numeric(out$adjNorthing), c(4879827.13675762, NA, 4879827.1367), tolerance=1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(NA, NA, 1.18), tolerance=0.01)
})

test_that("Select plant pres named locations are correct", {
  df<-data.frame(namedLocation=c('BART_036.basePlot.div', 'BART_071.basePlot.div',
                                 'BART_019.basePlot.div'), subplotID=c('32.4.1','32.4.10', '32'))
  out<-getLocTOS(df, 'div_10m2Data100m2Data')
  expect_equal(as.numeric(out$adjNorthing), c(4881309.50371794, 4881968.47614891, 4882112.72882359), tolerance=1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(NA, NA, 2.29), tolerance=0.01)
})


