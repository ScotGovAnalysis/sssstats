#' create_sss_calendar Creates the Social Security Scotland calendar
#'
#' @param date_from A character string or `Date` representing the start date
#'   for the calendar. Defaults to `"2018-01-01"`.
#'
#' @param date_to A character string or `Date` representing the end date
#'   for the calendar. Defaults to `Sys.Date()`.
#'
#' @details
#' Constructs a `bizdays` calendar object used for Social Security Scotland
#' processing time calculations. The calendar defines business days as all days
#' except Glasgow and Dundee joint bank holidays (hard‑coded within the function)
#' and weekends (Saturday and Sunday).
#' The calendar can be created for any date range, and is typically
#' used when calculating processing times.
#' The function uses a hard‑coded list of Scottish bank holidays for the years
#' 2018–2027. If `date_to` is after the last available holiday in the
#' list, the function emits a warning to prompt the user to update the holiday vector.
#'
#' This function wraps \code{bizdays::create.calendar()} and returns the created
#' calendar object.
#'
#' @return
#' A `Calendar` object (S3 class) from the **bizdays** package, containing the
#' specified holidays, weekend structure, and date bounds.
#'
#' @examples
#' \dontrun{
#' # Create calendar for default range
#' calendar <- create_sss_calendar("2018-01-01", "2026-12-31")
#' }
#'
#' @export

create_sss_calendar <- function(date_from = "2018-01-01", date_to = Sys.Date()) {
  weekend <- c("saturday", "sunday")
  bank_holidays_scot <- lubridate::ymd(
    c(
      "2027-01-04",
      "2027-01-01",
      "2026-12-28",
      "2026-12-25",
      "2026-11-30",
      "2026-06-15",
      "2026-05-25",
      "2026-05-04",
      "2026-04-06",
      "2026-04-03",
      "2026-01-02",
      "2026-01-01",
      "2025-12-26",
      "2025-12-25",
      "2025-11-28",
      "2025-05-26",
      "2025-05-05",
      "2025-04-21",
      "2025-04-18",
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
      "2022-09-19",
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

  # warning if the holiday list is outdated
  if (date_to > max(bank_holidays_scot)) {
    warning("Please check the holiday dates in function `sssstats::create_sss_calendar` are up to date.
         The hard-coded dates are outdated for your date_to value.")
  }
  sss_calendar <- bizdays::create.calendar(
    "sss_calendar",
    holidays = bank_holidays_scot,
    weekdays = weekend,
    start.date = date_from,
    end.date = date_to
  )

  sss_calendar
}
