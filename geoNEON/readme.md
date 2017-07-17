### Description

NEON data files include differing levels of information about where data were collected. Aside from remote sensing data, there are 4 categories for delivery of spatial data:

Terrestrial Observational Sampling (TOS): These are data collected under the TOS Spatial Design, available here:  Downloads of these data products include latitude, longitude, elevation, and associated uncertainties, at the level of a TOS plot. More location information can be pulled using the geoNEON package, including easting, northing, and UTM zone. For select TOS data products, more precise sampling locations within a plot can be derived as well. See below for instructions on using the geoNEON package to do this.

Terrestrial and Aquatic Instrumentation (TIS & AIS): Data collected by streaming sensors does not include any location information in the data download. Some AIS locations and a very small number of TIS locations are currently available via the NEON API; location data will be expanded in the near future and the geoNEON package will be updated to access these data.

Aquatic Observational Sampling (AOS): Observational data collected in aquatic systems include latitude, longitude, elevation, and associated uncertainties. Most AOS data products also report an additional uncertainty that should be added to the reported uncertainty; the user guides for each product describe the spatial data in more detail.

Special cases: A limited subset of terrestrial data are collected outside the TOS Spatial Design, including soil and root megapit data, dust mass, and wet deposition. Downloads of these data include latitude, longitude, elevation, and associated uncertainties.


### Instructions

The geoNEON package currently contains two functions, `def.extr.geo.os()` and `def.calc.geo.os()`

```
library(devtools)
install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)
```

To pull geolocation data from the NEON API, where 'data' is a data frame with NEON named locations in the column 'namedLocation' (this will work on nearly all OS data tables):

```
def.extr.geo.os(data, 'namedLocation')
```

To calculate precise geolocations for one of the data product tables covered by `def.calc.geo.os()` (see function documentation):

```
def.calc.geo.os(data, 'ltr_pertrap')
```

