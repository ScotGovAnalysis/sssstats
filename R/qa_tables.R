#' Take the Difference of a Matching Column in Two Tables
#'
#' This function returns a vector of differences corresponding to a specified column
#' in two data frames. It will only consider rows present in both data
#' sets based on the first column.
#' @param table_1 First table
#' @param table_2 Second table
#' @param column column to consider
#' @export


diff_column <- function(table_1, table_2, column) {
  this_time <-
    table_2 %>%
    dplyr::filter(dplyr::if_any(1, ~ . %in% table_1[[1]])) %>%
    dplyr::filter(dplyr::if_any(1, ~ !stringr::str_detect(., "Total"))) %>%
    dplyr::filter(dplyr::if_any(1, ~ !stringr::str_detect(., "Financial"))) %>%
    dplyr::pull(table_2[column] %>% colnames())

  last_time <-
    table_1 %>%
    dplyr::filter(dplyr::if_any(1, ~ . %in% table_2[[1]])) %>%
    dplyr::filter(dplyr::if_any(1, ~ !stringr::str_detect(., "Total"))) %>%
    dplyr::filter(dplyr::if_any(1, ~ !stringr::str_detect(., "Financial"))) %>%
    dplyr::pull(table_1[column] %>% colnames())

  last_time - this_time

}

#' Take the Difference of All Matching Columns in Two Tables
#'
#' This function returns a table of differences for any matching numeric columns
#' in two dataframes. It will only consider rows and columns present in both data
#' sets (row selection is based on the first column).
#' @param table_1 First table
#' @param table_2 Second table
#' @export
diff_tables <- function(table_1, table_2){

  is_col_numeric <- table_1 %>% purrr::map_lgl(is.numeric)

  numeric_cols <- colnames(table_1)[is_col_numeric]

  other_cols <- colnames(table_1)[!is_col_numeric]

  diffs <- purrr::map(numeric_cols, ~ sssstats::diff_column(
    dplyr::ungroup(table_1), dplyr::ungroup(table_2), .))

  filtered_table <- table_1 %>%
    dplyr::filter(dplyr::if_any(1, ~ . %in% table_2[[1]])) %>%
    dplyr::filter(dplyr::if_any(1, ~ !stringr::str_detect(., "Total"))) %>%
    dplyr::filter(dplyr::if_any(1, ~ !stringr::str_detect(., "Financial")))

   stats::setNames(diffs, numeric_cols) %>%
     dplyr::as_tibble() %>%
     dplyr::bind_cols(filtered_table[other_cols]) %>%
     dplyr::relocate(tidyselect::all_of(other_cols),
                     .before = tidyselect::everything())

}
