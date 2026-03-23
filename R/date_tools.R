#' Convert Date String from One Format to Another
#'
#' This function takes a string as input, parses it as a date if possible
#' and outputs a date in a new format, defaulting to the standard Social
#' Scotland date format, i.e. "%B %Y"
#' @param date Date to be converted to new format
#' @param input_format Input format of date. Defaults to "%Y-%m"
#' @param output_format Output format of date. Defaults to "%B %Y"
#' @export
#' @examples
#' convert_date("24-11")
#' convert_date("13/11/24", "%d/%m/%Y")
#' convert_date("13 November 2024", "%d %B %Y", "%Y-%m")
convert_date <- function(date, input_format = "%Y-%m", output_format = "%B %Y") {
  if (!is.na(suppressWarnings(readr::parse_date(date, input_format)))) {
    format(readr::parse_date(date, input_format), format = output_format)
  } else {
    date
  }
}

#' Convert All Date Strings in a Vector from One Format to Another
#'
#' This function applies [convert_date()] over a vector, defaulting to the standard Social
#' Scotland date format, i.e. "%B %Y"
#' @param data Vector of dates to be converted to new format
#' @param input_format Input format of date. Defaults to "%Y-%m"
#' @param output_format Output format of date. Defaults to "%B %Y"
#' @export

convert_col_date <- function(data, input_format = "%Y-%m", output_format = "%B %Y") {
  data %>% purrr::modify(convert_date, input_format = input_format, output_format = output_format)
}


#' Convert Date into Financial Year
#'
#' This function takes a date object as input, and outputs string corresponding to
#' financial year starting at 1 April.
#' @param date Date to be converted to new format
#' @export

financial_year <- function(date) {
  dplyr::if_else(
    lubridate::month(date) <= 3,
    paste0(lubridate::year(date) - 1, "-", lubridate::year(date)),
    paste0(lubridate::year(date), "-", lubridate::year(date) + 1)
  )
}


#' Calculate age in years using two dates
#'
#' This function calculates the age someone would be between a date of birth and a given date,
#' either single date or column of dates
#' @param date_column Column for Date of Birth
#' @param reference_date Date or column of dates to calculate age on
#' @export

age_on_date <- function(date_column, reference_date) {
  # Convert dates to Date objects
  date_column <- as.Date(date_column)
  reference_date <- as.Date(reference_date)

  # Floor age to nearest integer
  age_years <- floor(lubridate::time_length(lubridate::interval(date_column, reference_date), "years"))

  return(age_years)
}
