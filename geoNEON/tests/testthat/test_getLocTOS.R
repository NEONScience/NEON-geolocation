context("Test getLocTOS for each data product type")
library(geoNEON)

test_that("Select bird named locations are correct", {
  df<-data.frame(namedLocation=c('SCBI_023.basePlot.brd', 'BART_003.birdGrid.brd', 
                                'BART_003.birdGrid.brd', 'BART_003.birdGrid.brd'), pointID=c(21,'A1', 'B2', 'B2'),
                 pointCountMinute=c(1,2,3,88))
  out<-getLocTOS(df, 'brd_countdata')
  expect_equal(as.numeric(out$adjNorthing), c(4308017.64, 4880714.904, 4880464.93234, 4881512.696), tolerance=0.1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(0.24, 15, 0.32, NA), tolerance=0.1)
})

test_that("Select bird named locations with history are correct", {
  df<-data.frame(uid=c('uid1','uid2'),
                 namedLocation=c('SCBI_022.basePlot.brd', 'SCBI_022.basePlot.brd'), 
                 pointID=c(21,21),
                 startDate=c(as.POSIXct('2018-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                             as.POSIXct('2022-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocTOS(df, 'brd_perpoint')
  expect_equal(as.numeric(out$adjNorthing), c(4307659.96, 4307658.31), tolerance=0.1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(0.19, 0.18), tolerance=0.1)
})

test_that("Select smammal named locations are correct", {
  df<-data.frame(namedLocation=c('BART_001.mammalGrid.mam', 'BART_001.mammalGrid.mam',
                                 'BART_001.mammalGrid.mam', 'BART_084.mammalGrid.mam'), 
                 trapCoordinate=c('J9','X1','E5','E5'))
  out<-getLocTOS(df, 'mam_pertrapnight')
  expect_equal(as.numeric(out$adjNorthing), c(4879827.13675762, NA, 4879662.76178, 4879827.1367), 
               tolerance=1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(3.19, NA, 1.19, 1.18), tolerance=0.01)
})

test_that("Select smammal named locations with a history are correct", {
  df<-data.frame(uid=c('uid1','uid2','uid3','uid4'),
                 namedLocation=c('HEAL_032.mammalGrid.mam', 'HEAL_032.mammalGrid.mam',
                                 'HEAL_032.mammalGrid.mam', 'HEAL_032.mammalGrid.mam'), 
                 trapCoordinate=c('D3','D3','E8','E8'),
                 collectDate=c(as.POSIXct('2015-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2023-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2015-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2023-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocTOS(df, 'mam_pertrapnight')
  expect_equal(as.numeric(out$adjEasting), c(388636.76997, 388559.32848, 388646.74103, 388609.35656), 
               tolerance=1)
  expect_equal(as.numeric(out$adjElevation), c(669.95, 664.24, 669.68, 666.72), tolerance=0.1)
})

test_that("Select plant pres named locations are correct", {
  df<-data.frame(namedLocation=c('BART_036.basePlot.div', 'BART_071.basePlot.div',
                                 'BART_019.basePlot.div'), subplotID=c('32.4.1','32.4.10', '32'))
  out<-getLocTOS(df, 'div_10m2Data100m2Data')
  expect_equal(as.numeric(out$adjNorthing), c(4881309.50371794, 4881968.47614891, 4882112.72882359), tolerance=1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(NA, NA, 2.29), tolerance=0.5)
})

test_that("Select plant pres named locations with history are correct", {
  df<-data.frame(uid=c('uid1','uid2','uid3','uid4'),
                 namedLocation=c('OSBS_002.basePlot.div', 'OSBS_002.basePlot.div',
                                 'OSBS_013.basePlot.div', 'OSBS_013.basePlot.div'), 
                 subplotID=c('32_1_4','32_1_4', '31_1_1', '31_1_1'),
                 endDate=c(as.POSIXct('2019-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2019-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocTOS(df, 'div_1m2Data')
  expect_equal(as.numeric(out$adjEasting), c(407451.24, 407451.279, 
                                             406953.467, 406953.507), tolerance=0.1)
  expect_equal(as.numeric(out$adjDecimalLatitude), c(29.703858, 29.70385769, 
                                                     29.682072, 29.682071932), tolerance=0.01)
})

test_that("Select soil locations are correct", {
  df<-data.frame(namedLocation=c('UNDE_017.basePlot.bgc', 'UNDE_013.basePlot.bgc'), 
                 coreCoordinateX=c(15,18), coreCoordinateY=c(33,37))
  out<-getLocTOS(df, 'sls_soilCoreCollection')
  expect_equal(as.numeric(out$adjNorthing), c(5123529, 5122515), tolerance=1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(0.5,0.5), tolerance=0.1)
})

test_that("Select soil locations with history are correct", {
  df<-data.frame(uid=c('uid1','uid2'),
                 namedLocation=c('JERC_006.basePlot.bgc', 'JERC_006.basePlot.bgc'), 
                 coreCoordinateX=c(15,21), coreCoordinateY=c(33,39),
                 collectDate=c(as.POSIXct('2015-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2023-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocTOS(df, 'sls_soilCoreCollection')
  expect_equal(as.numeric(out$adjNorthing), c(3453202, 3453209), tolerance=1)
  expect_equal(as.numeric(out$adjElevationUncertainty), c(0.27,0.14), tolerance=0.1)
})

test_that("Select litter trap named locations are correct", {
  df<-data.frame(namedLocation=c('WREF_088.basePlot.ltr', 'WREF_088.basePlot.ltr',
                                 'WREF_075.basePlot.ltr'), subplotID=c('39_400','41_400', '23_400'),
                 trapID=c('WREF_088_524', 'WREF_088_823', 'WREF_075_276'))
  out<-getLocTOS(df, 'ltr_pertrap')
  expect_equal(as.numeric(out$adjNorthing), c(5075552.04572389, 5075557.51949259, 5075325.03289727), tolerance=1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(1.22, 1.19, 1.14), tolerance=0.1)
})

test_that("Select litter trap named locations with history are correct", {
  df<-data.frame(uid=c('uid1','uid2'),
                 namedLocation=c('GRSM_051.basePlot.ltr', 'GRSM_051.basePlot.ltr'), 
                 subplotID=c('23_400', '23_400'),
                 trapID=c('GRSM_051_310', 'GRSM_051_324'),
                 date=c(as.POSIXct('2017-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                        as.POSIXct('2022-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocTOS(df, 'ltr_pertrap')
  expect_equal(as.numeric(out$adjEasting), c(273127.4, 273117.0), tolerance=0.1)
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(1.15, 1.12), tolerance=0.1)
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

# test for phenology locations with history currently omitted, because there are no 
# instances of moved phenology plots, and the sampling design de facto prevents 
# moving of plots

test_that("Select herb clip named locations are correct", {
  df<-data.frame(namedLocation=c('KONZ_045.basePlot.hbp', 'KONZ_069.basePlot.hbp',
                                 'KONZ_052.basePlot.hbp'), subplotID=c('31_400','31_400', '31_400'),
                 clipID=c('KONZ_045_0193', 'KONZ_069_0043', 'KONZ_052_055'))
  out<-getLocTOS(df, 'hbp_perbout')
  expect_equal(as.numeric(out$adjEasting), c(710831.7982755, 710882.8424974, 710980.5469347), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(394.88, 394.88, 407.95), tolerance=0.1)
})

test_that("Select herb clip named locations with a history are correct", {
  df<-data.frame(uid=c('uid1','uid2','uid3','uid4'),
                 namedLocation=c('BART_046.basePlot.hbp', 'BART_046.basePlot.hbp',
                                 'GRSM_051.basePlot.hbp', 'GRSM_051.basePlot.hbp'), 
                 subplotID=c('39_400','39_400', '23_400','23_400'),
                 collectDate=c(as.POSIXct('2015-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2023-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2017-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2023-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')),
                 clipID=c('BART_046_552', 'BART_046_663', 'GRSM_051_0324', 'GRSM_051_258'))
  out<-getLocTOS(df, 'hbp_perbout')
  expect_equal(as.numeric(out$adjEasting), c(316939.7, 316940.5, 273116.4, 273120.0), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(298.46, 299.79, 654.24, 654.87), tolerance=0.1)
})

test_that("Select herbaceous cfc named locations are correct", {
  df<-data.frame(namedLocation=c('WOOD_044.basePlot.cfc', 'SJER_012.basePlot.cfc',
                                 'SOAP_002.basePlot.cfc'), subplotID=c('31_400','31_400', NA),
                 clipID=c('WOOD_044_067', 'SJER_012_069', NA))
  out<-getLocTOS(df, 'cfc_fieldData')
  expect_equal(as.numeric(out$adjEasting), c(481536.1, 258495.2, NA), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(586.53, 320.98, NA), tolerance=0.1)
})

test_that("Select herbaceous cfc locations with history are correct", {
  df<-data.frame(uid=c('uid1','uid2'),
                 namedLocation=c('DSNY_044.basePlot.cfc', 'DSNY_043.basePlot.cfc'), 
                 subplotID=c('31_400','31_400'),
                 clipID=c('DSNY_044_136', 'DSNY_043_006'),
                 collectDate=c(as.POSIXct('2013-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2023-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocTOS(df, 'cfc_fieldData')
  expect_equal(as.numeric(out$adjEasting), c(457416.1, 457284.3), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(20.3, 18.31), tolerance=0.1)
})

test_that("Select root sampling locations are correct", {
  df<-data.frame(namedLocation=c('TALL_051.basePlot.bbc', 'TALL_051.basePlot.bbc',
                                 'TALL_060.basePlot.bbc'), subplotID=c('39_400','21_400', '41_400'),
                 clipID=c('TALL_051_703', 'TALL_051_041', 'TALL_060_882'))
  out<-getLocTOS(df, 'bbc_percore')
  expect_equal(as.numeric(out$adjEasting), c(462857.9, 462848.6, 462729.9), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(120.63, 117.93, 128.37), tolerance=0.1)
})

test_that("Select root sampling locations with history are correct", {
  df<-data.frame(uid=c('uid1','uid2'),
                 namedLocation=c('JERC_049.basePlot.bbc', 'JERC_049.basePlot.bbc'), 
                 subplotID=c('39_400','21_400'),
                 clipID=c('JERC_049_643', 'JERC_049_178'),
                 collectDate=c(as.POSIXct('2018-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2023-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocTOS(df, 'bbc_percore')
  expect_equal(as.numeric(out$adjEasting), c(741628.4, 741627.8), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(46.89, 44.79), tolerance=0.1)
})

test_that("Select woody veg mapped individuals are correct", {
  df<-data.frame(namedLocation=c('NIWO_061.basePlot.vst', 'NIWO_064.basePlot.vst',
                                 'NIWO_004.basePlot.vst'), pointID=c('33', NA, '49'),
                 stemDistance=c(7.7, NA, 7.8),
                 stemAzimuth=c(298.3, NA, 90.0),
                 individualID=c('NEON.PLA.D13.NIWO.00602', 'NEON.PLA.D13.NIWO.00711',
                                'NEON.PLA.D13.NIWO.01138'))
  out<-getLocTOS(df, 'vst_mappingandtagging')
  expect_equal(as.numeric(out$adjNorthing), c(4433200.50143, NA, 4432708.48699), tolerance=0.1)
})

test_that("Select woody veg mapped individuals with history are correct", {
  df<-data.frame(uid=c('uid1','uid2','uid3','uid4'),
                 namedLocation=c('JERC_005.basePlot.vst', 'JERC_005.basePlot.vst',
                                 'JERC_006.basePlot.vst', 'JERC_006.basePlot.vst'), 
                 pointID=c('41', '41', '41', '41'),
                 stemDistance=c(6.1, 6.1, 8.1, 8.1),
                 stemAzimuth=c(190, 190, 357.4, 357.4),
                 individualID=c('NEON.PLA.D03.JERC.00005', 'NEON.PLA.D03.JERC.00005',
                                'NEON.PLA.D03.JERC.01390', 'NEON.PLA.D03.JERC.01390'),
                 date=c(as.POSIXct('2017-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2017-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                               as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocTOS(df, 'vst_mappingandtagging')
  expect_equal(as.numeric(out$adjEasting), c(742858.5, 742858.3, 742439.9, 742440.8), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(46.75, 46.31, 42.18, 42.27), tolerance=0.1)
})

# most location data match before and after change. confirmed in API, differences are tiny
test_that("Select veg structure subplots with history are correct", {
  df<-data.frame(uid=c('uid1','uid2','uid3','uid4'),
                 namedLocation=c('DSNY_017.basePlot.vst', 'DSNY_017.basePlot.vst',
                                 'DSNY_017.basePlot.vst', 'DSNY_017.basePlot.vst'),
                 subplotID=c('31_100', '31_100', '40_100', '40_100'),
                 date=c(as.POSIXct('2017-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'),
                        as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'),
                        as.POSIXct('2017-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'),
                        as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out<-getLocTOS(df, 'vst_apparentindividual')
  expect_equal(as.numeric(out$adjEasting), c(460404.4, 460404.5, 460404.3, 460404.3), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(20.28, 20.28, 19.88, 19.88), tolerance=0.1)
})

test_that('Select beetle locations are correct', {
  df <- data.frame(namedLocation=c('TALL_007.basePlot.bet','TALL_013.basePlot.bet'),
                   trapID=c('N','S'))
  out <- getLocTOS(df, 'bet_fielddata')
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(4.3,4.13), tolerance=0.5)
})

test_that('Select beetle locations with history are correct', {
  df <- data.frame(uid=c('uid1','uid2'),
                   namedLocation=c('JERC_005.basePlot.bet','JERC_005.basePlot.bet'),
                   trapID=c('W','S'),
                   collectDate=c(as.POSIXct('2018-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                                 as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out <- getLocTOS(df, 'bet_fielddata')
  expect_equal(as.numeric(out$adjCoordinateUncertainty), c(4.29,4.12), tolerance=0.1)
})

test_that('Mosquitoes correctly returns only a message', {
  df <- NA
  expect_message(getLocTOS(df, 'mos_trapping'), 'Mosquito trapping location is flexible within the plot; plot-level location and uncertainty provided in downloaded data are accurate.')
})

test_that('Select DHP locations are correct', {
  df <- data.frame(namedLocation=c('SJER_055.basePlot.dhp','SJER_057.basePlot.dhp'),
                   pointID=c('W10','S2'))
  out <- getLocTOS(df, 'dhp_perimagefile')
  expect_equal(as.numeric(out$adjNorthing), c(4111210,4110846), tolerance=1)
})

test_that('Select DHP locations with history are correct', {
  df <- data.frame(uid=c('uid1','uid2'),
                   namedLocation=c('OSBS_010.basePlot.dhp','OSBS_010.basePlot.dhp'),
                   pointID=c('E10','E10'),
                   endDate=c(as.POSIXct('2018-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'), 
                                 as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out <- getLocTOS(df, 'dhp_perimagefile')
  expect_equal(as.numeric(out$adjElevation), c(38.98,40.9), tolerance=0.2)
})

test_that('Select CDW tally locations are correct', {
  df <- data.frame(namedLocation=c('MLBS_018.basePlot.cdw','GUAN_042.basePlot.cdw',
                                   'SCBI_010.basePlot.cdw'),
                   lidsAzimuth=c(110,130,350), logDistance=c(NA,1.3,10.1))
  out <- getLocTOS(df, 'cdw_fieldtally')
  expect_equal(as.numeric(out$adjNorthing), c(NA,1988245,4307931), tolerance=1)
})

test_that('Select CDW tally locations with history are correct', {
  df <- data.frame(uid=c('uid1','uid2','uid3','uid4'),
                   namedLocation=c('SOAP_043.basePlot.cdw','SOAP_043.basePlot.cdw',
                                   'UNDE_058.basePlot.cdw','UNDE_058.basePlot.cdw'),
                   lidsAzimuth=c(210,210,190,190), logDistance=c(44.5,44.5,16,16),
                   date=c(as.POSIXct('2017-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'),
                          as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'),
                          as.POSIXct('2017-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'),
                          as.POSIXct('2024-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out <- getLocTOS(df, 'cdw_fieldtally')
  expect_equal(as.numeric(out$adjEasting), c(298190.8,298191.3,304230.6,304231.3), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(1247.62,1249.07,520.62,524.4), tolerance=0.1)
})

# since SPC locations are used only once, the plot may move but the SPC sampling location shouldn't
test_that('Select SPC locations with and without history are correct', {
  df <- data.frame(uid=c('uid1','uid2','uid3'),
                   namedLocation=c('JERC_021.basePlot.all','JERC_005.basePlot.all',
                                   'LAJA_020.basePlot.all'),
                   referenceCorner=c('SW20','SW20','SW40'), 
                   sampleDistance=c(0.2,0.2,0.2), sampleBearing=c(45,45,45),
                   collectDate=c(as.POSIXct('2015-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'),
                          as.POSIXct('2015-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT'),
                          as.POSIXct('2017-06-01T18:00:00', format='%Y-%m-%dT%H:%M:%S', tz='GMT')))
  out <- getLocTOS(df, 'spc_perplot')
  expect_equal(as.numeric(out$adjEasting), c(741201.1,742849.7,703664.2), tolerance=0.1)
  expect_equal(as.numeric(out$adjElevation), c(37.3,46.75,27.06), tolerance=0.1)
})

