### Description

NEON data files include differing levels of information about where data were collected. Aside from remote sensing data, there are 4 categories for delivery of spatial data:

Terrestrial Observational Sampling (TOS): These are data collected under the TOS Spatial Design, available here: http://data.neonscience.org/api/v0/documents/NEON.DOC.000913vA Downloads of these data products include latitude, longitude, elevation, and associated uncertainties, at the level of a TOS plot. More location information can be pulled using the geoNEON package, including easting, northing, and UTM zone. For select TOS data products, more precise sampling locations within a plot can be derived as well. See below for instructions on using the geoNEON package to do this.

Terrestrial and Aquatic Instrumentation (TIS & AIS): Data collected by streaming sensors does not include any location information in the data download. Some AIS locations and a very small number of TIS locations are currently available via the NEON API; location data will be expanded in the near future and the geoNEON package will be updated to access these data.

Aquatic Observational Sampling (AOS): Observational data collected in aquatic systems include latitude, longitude, elevation, and associated uncertainties. Most AOS data products also report an additional uncertainty that should be added to the reported uncertainty; the user guides for each product describe the spatial data in more detail.

Special cases: A limited subset of terrestrial data are collected outside the TOS Spatial Design, including soil and root megapit data, dust mass, and wet deposition. Downloads of these data include latitude, longitude, elevation, and associated uncertainties.


### Instructions

The geoNEON package currently contains two functions, `def.extr.geo.os()` and `def.calc.geo.os()`. To load the package, install from [GitHub](https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON)

```
library(devtools)
install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)
```

To append geolocation data from the NEON API to an existing data table from an OS data product, use `def.extr.geo.os()`. Here, `data` is an OS data table, and `'namedLocation'` is the name of the column in the table where NEON named locations can be found (it's `namedLocation` for nearly all OS data products).

```
data.plusSpatial <- def.extr.geo.os(data, 'namedLocation')
```

To get geolocation data from the NEON API for each of the locations in a data table, without merging the geolocation data into the original table, use the `locOnly=T` option. If your data include many repetitions of the same locations, this will be faster.

```
spatialOnly <- def.extr.geo.os(data, 'namedLocation', locOnly=T)
```

To calculate precise geolocations for one of the OS data product tables covered by `def.calc.geo.os()`, input the data table and specify the table name, shown here for the litterfall data product:

```
def.calc.geo.os(data, 'ltr_pertrap')
```

Not just any table from a covered data product can be used, because typically the spatial data details are provided in only one table for a given product. Currently, the data product tables included in `def.calc.geo.os()` are:

+ Litterfall: ltr_pertrap
+ Herbaceous clip harvest: hbp_perbout
+ Soil physical properties (Distributed periodic): sls_soilCoreCollection
+ Breeding bird point counts: brd_perpoint or brd_countdata
+ Small mammal box trapping: mam_pertrapnight
+ Plant presence and percent cover: div_1m2Data or div_10m2Data100m2Data


