#' Rounds to the nearest desired multiple.
#'
#' This function behaves like Excel's `mround`.
#' @param x Number to be rounded
#' @param multiple Multiple to be rounded to
#' @export

mround <- function(x, multiple){
  multiple * janitor::round_half_up(x / multiple)
}

#' Rounds whole data frame to the nearest multiple  and suppresses any values below 5 a limit
#'
#' This function take a data frame, rounds every value to the nearest given multiple,
#' then converts any values that round to zero to a given suppression value.
#' @param data A data frame to be rounded and suppressed
#' @param multiple Multiple to which to round table
#' @param suppression_value Value to convert suppressed values to. Defaults to -1
#' for ease of output to Excel.
#' @export

round_and_suppress <- function(data, multiple = 5, suppression_value = -1){

  data %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::contains("Percentage"),
        ~ {janitor::round_half_up(., 2)
          }
        )
      ) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric) &
          !tidyselect::contains("Percentage") &
          !tidyselect::contains("Median"),
        ~ {dplyr::case_when(
          . < 1 ~ janitor::round_half_up(., 2),
          . == 0 ~ 0,
          sssstats::mround(., multiple) == 0 ~ suppression_value,
          TRUE ~ sssstats::mround(., multiple)
          )
        }
      )
    )
}
