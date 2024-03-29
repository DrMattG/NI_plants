% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_DistObjects.R
\name{create_DistObjects}
\alias{create_DistObjects}
\title{Create distribution objects and format data for import to NI base}
\usage{
create_DistObjects(species, save = FALSE)
}
\arguments{
\item{species}{character string or vector of character strings containing
the latin name of the species for which to prepare data}

\item{save}{logical, default = `FALSE`. If `TRUE`, indicator data will be
saved in the working directory as `updatedIndicatorData.RData`.}
}
\value{
A list containing scaled indicator values and associated information,
including specifications for custom distributions specifying uncertainty, for 
each municipality per `species` and `year`.
}
\description{
This function creates distribution objects that quantify uncertainty in 
indicator calculations and prepares data for upload to the NI database.
}
