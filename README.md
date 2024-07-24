# Description

This package is designed to help with common tasks in the Social
Security Scotland statistics team.

# Getting started

To install the package directly from GitHub, open RStudio and run:

    devtools::install_github("ScotGovAnalysis/sssstats",
                   upgrade = "never",
                   build_vignettes = TRUE)

For details of the functions available, run `??sssstats` and click
`Index` at the bottom of the help page to access the package
documentation. 

You can also run `vignettes("VIGNETTE-NAME")`view the following vignettes for a brief
outline of the functions in action:
* "date_tools" - describes tools for manipulating and working with dates and financial years

# Aims - What does this package do?

This package is a collection of functions that crop up often in producing tables
for Social Security Scotland statistical publications. These tools fall broadly 
into the following categories:

* wrangling data - changing the structure of data, e.g. transposing data frames
* augmenting data - creating new useful columns, e.g. bucketing by age band or local authority
* summarising data - automating common summary processes, e.g. adding total by financial years
* tidying tables - getting tables publications ready, e.g. rounding and suppressing data 

# Future development areas - What doesn't this package do? (yet!)

Currently there are no tools in this package to help with the following jobs. Please help by 
suggesting or adding these! Also feel free to suggest tools that aren't in the areas listed 
below - all ideas welcome!

* data import
* excel output and formatting - there is currently another package in development 
to do this. Get in touch if you are interested or see [here](https://github.com/izaak-jephson/excel_output)


# Contributing to the package

Contributions to the package are very welcome. If you would like to
contribute, please fork the directory and open a pull request.

Details of how to do so are available
[here](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request).
