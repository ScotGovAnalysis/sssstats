#' This function takes raw input read from a publication file and extracts the
#' underlying data
#' @param data A data frame read from a publication file
#' @export

extract_table <- function(data) {
  if (ncol(data) < 2) {
    return()
  } else

  table <- data %>%
    dplyr::filter(dplyr::if_any(1, ~ !stringr::str_starts(., pattern = "\\[note"))) %>%
    dplyr::filter(!is.na(.data[["X2"]]) & !stringr::str_detect(.data[["X2"]], "Notes")) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                stringr::str_remove_all,
                                pattern = "\\[note \\d\\]|\\[note \\d\\d\\]")
    )

  col_names <- table %>%
    dplyr::slice(1) %>%
    purrr::as_vector() %>%
    make.names(unique = TRUE) %>%
    stringr::str_replace_all("\\."," ")

  table %>%
    rlang::set_names(col_names) %>%
    dplyr::slice(-1)

}

#' This function takes an excel file of publication tables and reads in the data
#' (including titles, heading and notes). Useful for debugging if `import tables()`
#' is not working as expected.
#' @param filepath Excel file containing publication tables
#' @export

import_raw_tables <- function(filepath) {
  filepath %>%
    readxl::excel_sheets() %>%
    rlang::set_names() %>%
    purrr::map(openxlsx::read.xlsx, xlsxFile = filepath, startRow =0, colNames = FALSE, fillMergedCells = TRUE) %>%
    purrr::map(dplyr::as_tibble)
}

#' This function takes an excel file of publication tables, reads in the data and
#' extracts the data tables.
#' @param filepath Excel file containing publication tables
#' @export

import_tables <- function(filepath) {
  filepath %>%
    readxl::excel_sheets() %>%
    rlang::set_names() %>%
    purrr::map(openxlsx::read.xlsx, xlsxFile = filepath, startRow =0, colNames = FALSE, fillMergedCells = TRUE) %>%
    purrr::map(dplyr::as_tibble) %>%
    purrr::map(sssstats::extract_table)
}
