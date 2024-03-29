% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stoploc.R
\name{stoploc}
\alias{stoploc}
\title{A function to create spatial files for stop locations}
\usage{
stoploc(gtfszip, agency)
}
\arguments{
\item{gtfszip}{The location of the gtfs zip file}

\item{agency}{Name of agency as identified in output data frame}
}
\description{
This function allows you to create stop location from a gtfs file.
It creates a spatial file that indicates what agency the stops are associated with.
}
\examples{
stoploc("2019-10_Arlington.zip", "ART")

}
