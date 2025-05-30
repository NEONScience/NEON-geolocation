% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getLocVSTsubplots.R
\name{getLocVSTsubplots}
\alias{getLocVSTsubplots}
\title{Calculate more precise geolocations for vegetation structure subplots (used for measurements made on unmapped plants) (DP1.10098.001)}
\usage{
getLocVSTsubplots(data, token = NA_character_)
}
\arguments{
\item{data}{A data frame containing NEON named locations and other sampling information.}

\item{token}{User specific API token (generated within neon.datascience user accounts). Optional.}
}
\value{
A data frame of geolocations for the input product and data
}
\description{
Calculation Function. Refine the geolocation data associated with NEON vegetation structure subplots.
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
}
\author{
Claire Lunch \email{clunch@battelleecology.org}
}
\keyword{internal}
