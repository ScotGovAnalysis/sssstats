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
      fin_year_start_month + 0.01 * fin_year_start_day ~
      paste0(lubridate::year(date) - 1, "-", lubridate::year(date)),
  .default = paste0(lubridate::year(date), "-", lubridate::year(date) + 1))
}

#' Create standard calendar with Scottish bank holidays
#'
#' This function generates a calendar using Scottish bank holidays for use with
#' `bizdays`.
#' @param date_from Calendar start date
#' @param date_to Calendar end date
#' @export

create_sss_calendar <- function(date_from = "2018-01-01", date_to = "2070-01-01"){
  weekend <-  c("saturday", "sunday")
  bank_holidays_scot <- lubridate::ymd(
    c(
      "2025-01-02",
      "2025-01-01",
      "2024-12-26",
      "2024-12-25",
      "2024-11-29",
      "2024-05-27",
      "2024-05-06",
      "2024-04-01",
      "2024-03-29",
      "2024-01-02",
      "2024-01-01",
      "2023-12-26",
      "2023-12-25",
      "2023-12-01",
      "2023-05-29",
      "2023-05-08",
      "2023-05-01",
      "2023-04-10",
      "2023-04-07",
      "2023-01-03",
      "2023-01-02",
      "2022-12-27",
      "2022-12-26",
      "2022-11-25",
      "2022-06-03",
      "2022-06-02",
      "2022-05-02",
      "2022-04-18",
      "2022-04-15",
      "2022-01-04",
      "2022-01-03",
      "2021-12-28",
      "2021-12-27",
      "2021-11-26",
      "2021-05-31",
      "2021-05-03",
      "2021-04-05",
      "2021-04-02",
      "2021-01-04",
      "2021-01-01",
      "2020-12-28",
      "2020-12-25",
      "2020-11-27",
      "2020-05-25",
      "2020-05-08",
      "2020-04-13",
      "2020-04-10",
      "2020-01-02",
      "2020-01-01",
      "2019-12-26",
      "2019-12-25",
      "2019-11-29",
      "2019-05-27",
      "2019-05-06",
      "2019-04-22",
      "2019-04-19",
      "2019-01-02",
      "2019-01-01",
      "2018-12-26",
      "2018-12-25",
      "2018-11-30",
      "2018-05-28",
      "2018-05-07",
      "2018-04-02",
      "2018-03-30",
      "2018-01-02",
      "2018-01-01"
    )
  )

  sss_calendar <- bizdays::create.calendar(
    'sss_calendar',
    holidays = bank_holidays_scot,
    weekdays = weekend,
    start.date = date_from,
    end.date = date_to
  )

  sss_calendar
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
        age_years <- floor(time_length(interval(date_column, reference_date), "years"))

        return(age_years)
      }

