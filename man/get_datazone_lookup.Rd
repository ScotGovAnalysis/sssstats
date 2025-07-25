% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_datazone_lookup.R
\name{get_datazone_lookup}
\alias{get_datazone_lookup}
\title{get_datazone_lookup}
\usage{
get_datazone_lookup(census_year)
}
\arguments{
\item{census_year}{A specific Census year ("2011" or "2022")in which the Scottish data zones are derived from.}
}
\value{
A data frame.
}
\description{
Function using SPARQLendpoint at https://statistics.gov.scot/sparql
to get the correct datazone file. The function first gets the filename from
sparql then reads it in using read_csv.
}
