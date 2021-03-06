context("Test getLocTOS for each data product type")
library(geoNEON)

test_that("Select bird named locations are correct", {
  df<-data.frame(namedLocation=c('SCBI_023.basePlot.brd', 'BART_003.birdGrid.brd', 
                                'BART_003.birdGrid.brd', 'BART_003.birdGrid.brd'), pointID=c(21,'A1', 'B2', 'B2'),
                 pointCountMinute=c(1,2,3,88))
  out<-getLocTOS(df, 'brd_countdata')
  expect_equal(as.numeric(out$adjNorthing), c(4308017.759165, 4880714.9041744, 4880464.93234, 4881510.4750), tolerance=0.1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(0.24, 15, 0.32, NA), tolerance=0.1)
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
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(NA, NA, 2.29), tolerance=0.5)
})

test_that("Select soil locations are correct", {
  df<-data.frame(namedLocation=c('UNDE_017.basePlot.bgc', 'UNDE_013.basePlot.bgc'), 
                 coreCoordinateX=c(15,18), coreCoordinateY=c(33,37))
  out<-getLocTOS(df, 'sls_soilCoreCollection')
  expect_equal(as.numeric(out$adjNorthing), c(5123529, 5122515), tolerance=1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(0.5,0.5), tolerance=0.1)
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

test_that("Select herbaceous cfc named locations are correct", {
  df<-data.frame(namedLocation=c('WOOD_044.basePlot.cfc', 'SJER_012.basePlot.cfc',
                                 'SOAP_002.basePlot.cfc'), subplotID=c('31','31', NA),
                 clipID=c('WOOD_044_067', 'SJER_012_069', NA))
  out<-getLocTOS(df, 'cfc_fieldData')
  expect_equal(as.numeric(out$adjEasting), c(481536.1, 258495.2, NA), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(586.53, 320.98, NA), tolerance=0.1)
})

test_that("Select root sampling locations are correct", {
  df<-data.frame(namedLocation=c('TALL_051.basePlot.bbc', 'TALL_051.basePlot.bbc',
                                 'TALL_060.basePlot.bbc'), subplotID=c('39','21', '41'),
                 clipID=c('TALL_051_703', 'TALL_051_041', 'TALL_060_882'))
  out<-getLocTOS(df, 'bbc_percore')
  expect_equal(as.numeric(out$adjEasting), c(462857.9, 462848.6, 462729.9), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(120.63, 117.93, 128.37), tolerance=0.1)
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

test_that('Select beetle locations are correct', {
  df <- data.frame(namedLocation=c('TALL_007.basePlot.bet','TALL_013.basePlot.bet'),
                   trapID=c('N','S'))
  out <- getLocTOS(df, 'bet_fielddata')
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(4.3,4.13), tolerance=0.5)
})

test_that('Mosquitoes correctly returns only a message', {
  df <- NA
  expect_output(getLocTOS(df, 'mos_trapping'), 'Mosquito trapping location is flexible within the plot; locations provided in downloaded data are accurate.')
})

test_that('Select DHP locations are correct', {
  df <- data.frame(namedLocation=c('SJER_055.basePlot.dhp','SJER_057.basePlot.dhp'),
                   pointID=c('W10','S2'))
  out <- getLocTOS(df, 'dhp_perimagefile')
  expect_equal(as.numeric(out$adjNorthing), c(4111210,4110846), tolerance=1)
})

test_that('Select CDW tally locations are correct', {
  df <- data.frame(namedLocation=c('MLBS_018.basePlot.cdw','GUAN_042.basePlot.cdw',
                                   'SCBI_010.basePlot.cdw'),
                   lidsAzimuth=c(110,130,350), logDistance=c(NA,1.3,10.1))
  out <- getLocTOS(df, 'cdw_fieldtally')
  expect_equal(as.numeric(out$adjNorthing), c(NA,1988245,4307931), tolerance=1)
})

