% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Read in one or more years of FARS data.}
\usage{
fars_read_years(years, path = "inst/extdata")
}
\arguments{
\item{years}{A year or vector of four-digit years for which you want to read
in FARS data.}

\item{path}{The path to the FARS data file to read. The default is the path
to the three supplied raw data files.}
}
\value{
A table or list of tables of FARS data.
}
\description{
Reads in Fatality Analysis Reporting System data for one or more
years.
}
\examples{
\dontrun{fars_read_years(years = 2014)}   # Returns FARS data for the year 2014.
years <- c(2013, 2014, 2015)
\dontrun{fars_read_years(years = years)} # Returns a list of three tables of FARS data.
\dontrun{fars_read_years(years = 14)}    # Results in an invalid year error.

}
