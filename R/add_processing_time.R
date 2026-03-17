#' add_processing_time Adds a column to df containing number of working days between two dates.
#'
#' @param df dataframe containing data to which the new column will be added
#' @param date_col_from column in date format to count from (unquoted)
#' @param date_col_to column in date format to count to (unquoted)
#' @param output_col_name column to put the processing time into (unquoted)
#' @param adjust number of days to add to total default is NULL which will not add any days.
#' @description Adds a column processing_time which counts the number of working days
#'              between col_date_from and date_col_to.
#' The function:
#' \itemize{
#'   \item checks that `df` is a data frame;
#'   \item verifies that the specified columns exist;
#'   \item requires that the date columns are of type `Date` or `POSIXct`;
#'   \item constructs SSS calendar using \code{sssstats::create_sss_calendar};
#'   \item computes business days using \code{bizdays::bizdays()}.
#'   \item if adjust is not zero adds a number of days.
#' }
#' Errors are raised if required packages are missing, if date columns are
#' invalid, or if the calendar making or business‑day calculation fails.
#'
#' @return dataframe identical to `df` but with an additional column (named using
#' `output_col_name`) containing the calculated processing times.
#'
#' @examples
#' # Example data
#' df_example <- data.frame(
#'   received_date = as.Date(c("2024-01-02", "2024-01-10", "2024-02-01")),
#'   decision_date = as.Date(c("2024-01-05", "2024-01-15", "2024-02-10"))
#' )
#'
#' # Add processing time without adjustment
#' add_processing_time(
#'   df = df_example,
#'   date_col_from = received_date,
#'   date_col_to = decision_date,
#'   output_col_name = processing_time
#' )
#'
#' # Add processing time with a 1 day adjustment
#' add_processing_time(
#'   df = df_example,
#'   date_col_from = received_date,
#'   date_col_to = decision_date,
#'   output_col_name = processing_time,
#'   adjust = 1
#' )
#'
#' @export

add_processing_time <- function(df,
                                date_col_from,
                                date_col_to,
                                output_col_name,
                                adjust = NULL) {
  # checks packages are installed
  if (!requireNamespace("sssstats", quietly = TRUE)) {
    stop("The `sssstats` package is required but not installed.")
  }
  if (!requireNamespace("bizdays", quietly = TRUE)) {
    stop("The `bizdays` package is required but not installed.")
  }

  # validates df
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  # captures columns
  output_col_name <- rlang::enquo(output_col_name)
  date_col_from <- rlang::enquo(date_col_from)
  date_col_to <- rlang::enquo(date_col_to)

  # extracts names
  from_name <- rlang::as_name(date_col_from)
  to_name <- rlang::as_name(date_col_to)
  out_name <- rlang::as_name(output_col_name)

  # checks that columns exist
  missing_cols <- setdiff(c(from_name, to_name), names(df))
  if (length(missing_cols) > 0) {
    stop(
      "This column does not exist in `df`: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # checks columns are dates
  if (!inherits(df[[from_name]], c("Date", "POSIXct", "POSIXt"))) {
    stop(paste0("Column `", from_name, "` must be of type Date or POSIX."))
  }
  if (!inherits(df[[to_name]], c("Date", "POSIXct", "POSIXt"))) {
    stop(paste0("Column `", to_name, "` must be of type Date or POSIX."))
  }

  # uses sssstats to create calendar
  sss_cal <- tryCatch(
    {
      sssstats::create_sss_calendar(
        date_from = "2018-01-01",
        date_to = as.character(Sys.Date())
      )
    },
    error = function(e) {
      stop("Creating sss calendar failed: ", e$message)
    }
  )

  # uses bizdays package to count working days
  df <- tryCatch(
    {
      result <- bizdays::bizdays(
        df[[from_name]],
        df[[to_name]],
        sss_cal
      )

      if (!is.null(adjust)) result <- result + adjust

      df[[out_name]] <- result
      df
    },
    error = function(e) {
      stop("Counting with bizdays failed: ", e$message)
    }
  )
}
