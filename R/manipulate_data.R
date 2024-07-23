#' Transposes a Data Frame
#'
#' This function take a data frame or tibble and transposes it, turning columns to
#' rows and rows to columns
#' @param data A data frame to transpose.
#' @param pivot_column Column to be turned into column headings. Defaults to the
#' first column in the data frame.
#' @param names_to A character vector specifying the name for the column created
#' from the column names.
#' @export

transpose_data <- function(data, pivot_column = 1, names_to = "name"){
  data %>%
    tidyr::pivot_longer(-pivot_column, names_to = names_to) %>%
    tidyr::pivot_wider(names_from = pivot_column)
}

#' Adds a Coloumn of Age Bands to a Data Frame
#'
#' This function takes data frame and adds a column of age bands based on a column
#' of ages, according to a given age band specification.
#' @param data A data frame
#' @param age_col Column containing age data
#' @param age_breaks A vector of breaks in age bands
#' @param na Value to convert nas to. Defaults to "Unknown".
#' @export

convert_to_age_band <- function(data, age_col, age_breaks, na = "Unknown"){

age_band_spec <-
  create_age_band_spec(age_breaks) %>%
  dplyr::mutate(
    cond = glue::glue('{age_col} >= "{low}" & {age_col} < "{high}" ~ "{name}"'),
    cond = rlang::parse_exprs(.data$cond)
  )

data %>%
  dplyr::mutate(
    age_band = dplyr::case_when(!!!age_band_spec$cond)
  ) %>%
  dplyr::mutate(
    age_band = dplyr::case_when(
      is.na(age_band) ~ na,
      TRUE ~ age_band)
  )
}

#' Converts List of Ages to Age Band Specification
#'
#' @param ages Vector of breaks in age bands
#'
#'

create_age_band_spec <- function(ages){

  low_limit <-
    dplyr::tibble(name = paste("Under", ages[1]),
           low = 0,
           high = ages[1])

  high_limit <-
    dplyr::tibble(name = paste(ages[length(ages)], "and over"),
           low = ages[length(ages)],
           high = 150)

  limits <-
    dplyr::tibble(name = paste0(ages[-length(ages)], "-", ages[-1] - 1),
           low = ages[-length(ages)],
           high = ages[-1])

  dplyr::bind_rows(
    low_limit, limits, high_limit
  )

}
