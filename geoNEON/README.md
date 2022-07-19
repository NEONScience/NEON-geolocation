### Description

NEON data files include differing levels of information about where data were collected. Aside from remote sensing data, there are 4 categories for delivery of spatial data:

Terrestrial Observational Sampling (TOS): These are data collected under the TOS Spatial Design, available here: http://data.neonscience.org/api/v0/documents/NEON.DOC.000913vA Downloads of these data products include latitude, longitude, elevation, and associated uncertainties, at the level of a TOS plot. More location information can be pulled using the NEON API, including easting, northing, and UTM zone. For select TOS data products, more precise sampling locations within a plot can be derived as well. See below for instructions on using the geoNEON package to do this.

Terrestrial and Aquatic Instrumentation (TIS & AIS): Downloaded data are accompanied by a file called sensor_positions that contains coordinates of a reference location and the offsets to the location of each sensor relevant to the data. These locations are also accessible via the NEON API and the geoNEON package.

Aquatic Observational Sampling (AOS): Observational data collected in aquatic systems include latitude, longitude, elevation, and associated uncertainties. Most AOS data products also report an additional uncertainty that should be added to the reported uncertainty; the user guides for each product describe the spatial data in more detail. The NEON API and geoNEON package can be used to access location data that are not included in the download, such as easting and northing.

Special cases: A limited subset of terrestrial data are collected outside the TOS Spatial Design, including soil and root megapit data, dust mass, and wet deposition. Downloads of these data include latitude, longitude, elevation, and associated uncertainties.


### Instructions

The geoNEON package currently contains three functions, `getLocBySite()`, `getLocByName()` and `getLocTOS()`. To load the package, install from [GitHub](https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON)

```
library(devtools)
install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)
```

To find all defined locations at a site, use `getLocBySite()`. By default, it will return the location data for the site itself, but not the many child locations within the site. The `type` parameter is used to specify the child locations returned, and can be "TIS", "TOS", "AQU", "all", or "site" (the default). Use caution in requesting "TOS" or "all"; terrestrial sites typically contain thousands of defined observational sampling locations. Example to get all sensor locations at CPER:

```
CPER.loc <- getLocBySite("CPER", type="TIS")
```

Occasionally sensors are moved, for example relocated within the tower infrastructure. By default `getLocBySite()` returns the current location. To get the full location history of each sensor:

```
CPER.loc <- getLocBySite("CPER", type="TIS", history=T)
```

To append geolocation data from the NEON API to an existing data table from an OS data product, use `getLocByName()`. Here, `data` is an OS data table, and `'namedLocation'` is the name of the column in the table where NEON named locations can be found (it's `namedLocation` for nearly all OS data products).

If location data have been updated in the database more recently than the data were published, the function will notify you that there were mismatches between the data file and the API, and the location data returned by the function will match the most recent values in the database.

```
data.plusSpatial <- getLocByName(data, 'namedLocation')
```

To get geolocation data from the NEON API for each of the locations in a data table, without merging the geolocation data into the original table, use the `locOnly=T` option. If your data include many repetitions of the same locations, this will be faster. It will not carry out the check described above, so it will not warn you if locations have been updated.

```
spatialOnly <- getLocByName(data, 'namedLocation', locOnly=T)
```

To calculate precise geolocations for one of the OS data product tables covered by `getLocTOS()`, input the data table and specify the table name, shown here for the litterfall data product:

```
getLocTOS(data, 'ltr_pertrap')
```

Not just any table from a covered data product can be used, because typically the spatial data details are provided in only one table for a given product. The data product tables covered by `getLocTOS()` are:

+ Litterfall and fine woody debris production and chemistry: ltr_pertrap
+ Herbaceous clip harvest: hbp_perbout
+ Soil physical and chemical properties, periodic: sls_soilCoreCollection
+ Breeding bird point counts: brd_perpoint or brd_countdata
+ Small mammal box trapping: mam_pertrapnight
+ Plant presence and percent cover: div_1m2Data or div_10m2Data100m2Data
+ Plant phenology observation: phe_perindividual
+ Woody vegetation structure: vst_mappingandtagging
+ Plant foliar traits: cfc_fieldData for herbaceous samples, vst_mappingandtagging for woody plant samples
+ Root biomass and chemistry, periodic: bbc_percore
+ Ground beetles: bet_fielddata
+ Digital hemispherical photos: dhp_perimagefile
+ Coarse downed wood log survey: cdw_fieldtally



### Known issues

`getLocTOS()` and `getLocByName()` currently do not support location histories; both functions return the current location values, rather than values matching the time of data collection. This affects a small amount of data, since few NEON sampling locations have moved while retaining the same location name, but the functions will be updated in the future to handle this sitation correctly. `getLocBySite()` does support location histories.
