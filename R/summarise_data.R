#' Buckets All Non-Allowed Values to "Other"
#'
#' This function mutates a chosen column to substitute a generic value
#' if the current value is not in a given vector. Defaults to substituting
#' "Other". Useful for grouping infrequent or non-permitted values into a single
#' bucket.
#' @param data A data frame
#' @param column Columns to be converted
#' @param allowed_values Vector of allowed values. These will not be converted.
#' @param other Value to be substituted for non-allowed values. Defaults to "Other"
#' @export

bucket_other <- function(data, column, allowed_values, other = "Other"){
   data %>%
    dplyr::mutate({{column}} := dplyr::case_when(
      .data[[column]] %in% allowed_values ~
        .data[[column]],
      .default = other))
}

#' Adorns Sum by Financial Year to a Table
#'
#' This function behaves like [janitor::adorn_totals()], but adds totals by
#' financial year.
#' @param data A data frame
#' @param month Column containing months
#' @param financial_years Column containing financial years
#' @export

adorn_financial_years <- function(data, month, financial_years){

  data %>%
    dplyr::group_by({{financial_years}}) %>%
    dplyr::group_modify(~ dplyr::bind_rows(., dplyr::summarise(., dplyr::across(tidyselect::where(is.numeric),
                                                    ~sum(.,na.rm=TRUE))))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate({{month}} := dplyr::coalesce({{month}}, paste("Financial year", {{financial_years}}))) %>%
    dplyr::filter({{month}} != "Financial year -") %>%
    dplyr::filter({{month}} != "Financial year NA-NA") %>%
    dplyr::mutate(is_fin_year = stringr::str_detect({{month}},  "Financial year")) %>%
    dplyr::arrange(.data$is_fin_year) %>%
    dplyr::select(-c(.data$is_fin_year))
}
