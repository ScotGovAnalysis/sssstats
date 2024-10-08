% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_types.R
\name{make_supp_negative}
\alias{make_supp_negative}
\title{Converts Suppressed Value to -1}
\usage{
make_supp_negative(value, convert = "\\\\[c\\\\]")
}
\arguments{
\item{value}{Value to be converted}

\item{convert}{String indicating suppression, to be converted}
}
\description{
This function take a value and if it is suppressed converts it to the string
"-1".
Defaults to converting the string "[c]".
This is useful for preparing suppressed tables for excel output and
manipulating numeric columns with suppression.
}
