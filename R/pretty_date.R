#' Convert Date to Pretty Output Format
#'
#' This function takes a string as input, parses it as a date if possible
#' and outputs a date in the standard Social Security Scotland publication format
#' @param date Date to be converted to pretty format
#' @keywords date
#' @export
#' @examples
#' pretty_date()

pretty_date <- function(date) {
  dplyr::case_when(!is.na(suppressWarnings(lubridate::ym(date))) ~ format(suppressWarnings(lubridate::ym(date)), format = "%B %Y"),
            .default = date)
}
