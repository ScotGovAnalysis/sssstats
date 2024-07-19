#' Transposes a Data Frame
#'
#' This function take a data frame or tibble and transposes it, turning columns to
#'rows and rows to columns
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
