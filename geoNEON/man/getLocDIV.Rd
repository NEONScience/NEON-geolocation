% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getLocDIV.R
\name{getLocDIV}
\alias{getLocDIV}
\title{Calculate more precise geolocations for plant presence and percent cover (DP1.10058.001)}
\usage{
getLocDIV(data, token = NA_character_)
}
\arguments{
\item{data}{A data frame containing NEON named locations and other sampling information.}

\item{token}{User specific API token (generated within neon.datascience user accounts). Optional.}
}
\value{
A data frame of geolocations for the input product and data
}
\description{
Calculation Function. Refine the geolocation data associated with NEON plant presence data.
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
}
\author{
Claire Lunch \email{clunch@battelleecology.org}
}
\keyword{internal}
