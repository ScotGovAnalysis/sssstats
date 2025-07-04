% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_geography.R
\name{add_geography}
\alias{add_geography}
\title{add_geography Adds geography fields from the Scottish Statistics Postcode Lookup}
\usage{
add_geography(input_data, postcode_column, sspl_path = NULL)
}
\arguments{
\item{input_data}{dataset containing the postcode column}

\item{postcode_column}{column name containing postcode}

\item{sspl_path}{(optional) path to sspl file (see description)}
}
\value{
data frame with geography fields added
}
\description{
Selects relevant geography fields from the Scottish Statistics Postcode
Lookup for all Social Security Scotland official statistics publications.
As this lookup does not contain the local authority name, we need to join
the local authority names from the data zone 2022 lookup. To add Scottish
Index of Multiple Deprivation columns the 2011 datazones are needed.

Then, after cleaning the postcodes, those geography fields are added to the
target data frame by joining via the postcode column.

There are some postcodes that cannot be matched to any version of the
Scottish Statistics Postcode lookup, and thus those postcodes cannot be
assigned to the relevant geography area (e.g. a local authority area etc.)
Therefore, postcodes would be assigned to:
\itemize{
\item 'Unknown - Scottish postcode' if its postcode area is identified as
Scottish postcode
\item 'Unknown - Non-Scottish postcode' if it meets the UK standard postcode,
but its postcode area is not identified as Scottish postcode
\item 'Unknown - Others' if it does not meet the UK standard postcode.
}

For further explanation on the structure of a typical UK postcode:
https://www.ons.gov.uk/methodology/geography/ukgeographies/postalgeography.

For further explanation on the structure of a typical Scottish postcode:
https://www.nrscotland.gov.uk/publications/geography-postcode-information-note/.

Function defaults to using a config file for the path to the sspl file
(uses config$sspl) but if a path is given as as the third argument in the
function call then the function will use that instead. This means a config
file is not essential for use.
}
