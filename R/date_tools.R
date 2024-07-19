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

convert_date <- function(date, input_format = "%Y-%m",output_format = "%B %Y") {
  if(!is.na(suppressWarnings(readr::parse_date(date,input_format)))) {
      format(readr::parse_date(date,input_format), format = output_format)}
     else date
}

#' Convert All Date Strings in a Vector from One Format to Another
#'
#' This function applies [convert_date()] over a vector, defaulting to the standard Social
#' Scotland date format, i.e. "%B %Y"
#' @param data Vector of dates to be converted to new format
#' @param input_format Input format of date. Defaults to "%Y-%m"
#' @param output_format Output format of date. Defaults to "%B %Y"
#' @export

convert_col_date <- function(data, input_format = "%Y-%m",output_format = "%B %Y") {
  data %>% purrr::modify(convert_date)
}


#' Convert Date into Financial Year
#'
#' This function takes a date object as input, and outputs string corresponding to
#' financial year. Defaults to counting financial year start as 1 April.
#' @param date Date to be converted to new format
#' @param fin_year_start_day First day of financial year. Defaults to 1.
#' @param fin_year_start_month First month of financial year. Defaults to 4 (i.e. April).
#' @export


financial_year <- function(date, fin_year_start_day = 1, fin_year_start_month = 4){
  dplyr::case_when(lubridate::month(date) + lubridate::day(date) <
      fin_year_start_month + 0.01 * fin_year_start_day) ~
      paste0(lubridate::year(date) - 1, "-", lubridate::year(date))
  .default = (paste0(lubridate::year(date), "-", lubridate::year(date) + 1))
}
