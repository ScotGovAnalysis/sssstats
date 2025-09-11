#' get_sspl_lookup Read in the Scottish Statistics Postcode Lookup
#' @description
#' Currently, the Scottish Statistics Postcode Lookup is available on this
#'  webpage: https://www.nrscotland.gov.uk/publications/scottish-statistics-postcode-lookup/
#'  -- future plans may include making this lookup available on https://statistics.gov.scot/home.
#'
#' The Scottish Statistics Postcode Lookup should be used for all official
#' statistical production to ensure geographic consistency across all official
#' statistics and to support the implementation of the Government Statistical
#' Service Geography Policy.
#' By default the function will return the 8 columns needed for official publications.
#' If the optional parameter keep_all is set to TRUE then all the columns in the sspl file
#' will be returned.
#' @param file_path A string specifying the path to the CSV file containing the SSPL data.
#' @param keep_all Logical; if `FALSE` (default), only a subset of columns is retained.
#' If `TRUE`, all columns from the input file are returned.
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @importFrom dplyr select all_of
#' @export

get_sspl_lookup <- function(file_path, keep_all = FALSE) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  sspl <- readr::read_csv(file_path, show_col_types = FALSE) |>
    janitor::clean_names(case = "snake")

  # identifies which columns to keep
  sspl_keep <- c(
    "postcode",
    "postcode_district",
    "postcode_type",
    "data_zone2022code",
    "intermediate_zone2022code",
    "data_zone2011code",
    "intermediate_zone2011code",
    "council_area2019code",
    "island_code"
  )

  if (keep_all == FALSE) {
    sspl <- sspl |>
      dplyr::select(dplyr::all_of(sspl_keep))
  }

  return(sspl)
}
