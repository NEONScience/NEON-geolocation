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

test_that("Select litter trap named locations are correct", {
  df<-data.frame(namedLocation=c('WREF_088.basePlot.ltr', 'WREF_088.basePlot.ltr',
                                 'WREF_075.basePlot.ltr'), subplotID=c('39','41', '23'),
                 trapID=c('WREF_088_524', 'WREF_088_823', 'WREF_075_276'))
  out<-getLocTOS(df, 'ltr_pertrap')
  expect_equal(as.numeric(out$adjNorthing), c(5075552.04572389, 5075557.51949259, 5075325.03289727), tolerance=1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(1.22, 1.19, 1.14), tolerance=0.1)
})

test_that("Select phenology named locations are correct", {
  df<-data.frame(namedLocation=c('CPER_091.phenology.phe', 'CPER_077.phenology.phe',
                                 'CPER_077.phenology.phe'), 
                 subtypeSpecification=c('phenocam','primary', 'primary'),
                 sampleGeodeticDatum=c('WGS84','WGS84','WGS84'),
                 sampleLatitude=c(40.815923,NA,NA),
                 sampleLongitude=c(-104.745499,NA,NA),
                 sampleCoordinateUncertainty=c(0.3,NA,NA),
                 sampleElevation=c(1653.1,NA,NA),
                 sampleElevationUncertainty=c(NA,NA,NA),
                 decimalLatitude=c(40.820608,40.817814,40.817814),
                 decimalLongitude=c(-104.75468,-104.745302,-104.745302),
                 transectMeter=c(NA,62.9,468.1),
                 directionFromTransect=c(NA,'Left','Left'),
                 ninetyDegreeDistance=c(NA,1.9,1.9))
  out<-getLocTOS(df, 'phe_perindividual')
  expect_equal(as.numeric(out$adjDecimalLatitude), c(40.81592300, 40.81747037, 40.81808392), tolerance=0.1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(0.3, 2.1, 2.1), tolerance=0.1)
})

test_that("Select herb clip named locations are correct", {
  df<-data.frame(namedLocation=c('KONZ_045.basePlot.hbp', 'KONZ_069.basePlot.hbp',
                                 'KONZ_052.basePlot.hbp'), subplotID=c('31','31', '31'),
                 clipID=c('KONZ_045_0193', 'KONZ_069_0043', 'KONZ_052_055'))
  out<-getLocTOS(df, 'hbp_perbout')
  expect_equal(as.numeric(out$adjEasting), c(710831.7982755, 710882.8424974, 710980.5469347), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(394.88, 394.88, 407.95), tolerance=0.1)
})

test_that("Select woody veg named locations are correct", {
  df<-data.frame(namedLocation=c('NIWO_061.basePlot.vst', 'NIWO_064.basePlot.vst',
                                 'NIWO_004.basePlot.vst'), pointID=c('33', NA, '49'),
                 stemDistance=c(7.7, NA, 7.8),
                 stemAzimuth=c(298.3, NA, 90.0),
                 individualID=c('NEON.PLA.D13.NIWO.00602', 'NEON.PLA.D13.NIWO.00711',
                                'NEON.PLA.D13.NIWO.01138'))
  out<-getLocTOS(df, 'vst_mappingandtagging')
  expect_equal(as.numeric(out$adjNorthing), c(4433200.50143, NA, 4432708.48699), tolerance=0.1)
})

