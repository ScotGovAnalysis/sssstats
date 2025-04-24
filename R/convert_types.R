#' Converts Suppressed Value to -1
#'
#' This function take a value and if it is suppressed converts it to the string
#' "-1".
#' Defaults to converting the string "\[c\]".
#' This is useful for preparing suppressed tables for excel output and
#' manipulating numeric columns with suppression.
#' @param value Value to be converted
#' @param convert String indicating suppression, to be converted
#' @export

make_supp_negative <- function(value, convert = "\\[c\\]"){
  if(is.character(value)) {
    value %>%
    stringr::str_replace(convert, "-1")
    }
  else value
}

#' Converts Low Value to -100
#'
#' This function take a value and if it is labelled low converts it to the string
#' "-100".
#' Defaults to converting the string "\[low\]".
#' This is useful for preparing suppressed tables for excel output and
#' manipulating numeric columns with suppression.
#' @param value Value to be converted
#' @param convert String indicating low value, to be converted
#' @export

make_low_negative <- function(value, convert = "\\[low\\]"){
  if(is.character(value)) {
    value %>%
      stringr::str_replace(convert, "-100")
  }
  else value
}


#' Converts Suppressed Values to -1 Over a Table
#'
#' This function applies [make_supp_negative()] across a whole table.
#' Defaults to converting the string "\[c\]".
#' @param table A data frame
#' @param convert String indicating suppression, to be converted
#' @param across Columns to convert across. Takes tidyselect specification.
#' Defaults to `everything()`
#' @export

make_all_supp_negative <- function(table, convert = "\\[c\\]", across = tidyselect::everything()){

  table %>%
    dplyr::mutate(
      dplyr::across(
        {{across}},
        ~ purrr::modify(.x, ~ make_supp_negative(.x, convert = convert))))
}

#' Converts Low Values to -100 Over a Table
#'
#' This function applies [make_low_negative()] across a whole table.
#' Defaults to converting the string "\[low\]".
#' @param table A data frame
#' @param convert String indicating low value, to be converted
#' @param across Columns to convert across. Takes tidyselect specification.
#' Defaults to `everything()`
#' @export

make_all_low_negative <- function(table, convert = "\\[low\\]", across = tidyselect::everything()){

  table %>%
    dplyr::mutate(
      dplyr::across(
        {{across}},
        ~ purrr::modify(.x, ~ make_low_negative(.x, convert = convert))))
}


#' Converts String to Numeric If Possible
#'
#' This function takes a string and, if it can be parsed as a number, returns
#' a numeric. Otherwise it returns the string.
#' @param value Value to be converted
#' @export

make_string_numeric <- function(value){

  x <- suppressWarnings(type.convert(value))

  if(is.numeric(x)) x
  else value
}


#' Converts String to Numeric If Possible Over a Table
#'
#' This function applies [make_string_numeric()] across a whole table.
#' @param table A data frame
#' @param across Columns to convert across. Takes tidyselect specification.
#' Defaults to `everything()`
#' @export

make_all_string_numeric <- function(table, across = tidyselect::everything()) {
  tryCatch(
    expr = {
      table %>%
      dplyr::mutate(
        dplyr::across(
          {{across}},
          ~ purrr::map_vec(.x, ~ make_string_numeric(.x))))},
    error = function(cond) {
      message("Table may contain suppressed columns. Try running make_all_supp_negative first.")
      message("Original error message:")
      message(conditionMessage(cond))}
  )

}

#' Converts Number to Percent
#'
#' This function takes a string and, if it can be parsed as a number, divides it
#' by 100 to return a percentage. Otherwise it returns the string.
#' @param value Value to be converted
#' @export

make_number_percent <- function(value){

  if(is.numeric(value)) value/100
  else value
}


#' Converts Number to Percent Over a Table
#'
#' This function applies [make_number_percent()] across a whole table.
#' @param table A data frame
#' @param across Columns to convert across. Takes tidyselect specification.
#' Defaults to `contains("Percentage")`
#' @export

make_all_number_percent <- function(table, across = tidyselect::contains("Percentage")){

  table %>%
    dplyr::mutate(
      dplyr::across(
        {{across}},
        ~ purrr::map_vec(.x, ~ make_number_percent(.x))))
}

#' Replace NA Over a Table
#'
#' This function applies [replace_na()] across a whole table.
#' @param table A data frame
#' @param replace Value to replace NA with
#' @param across Columns to convert across. Takes tidyselect specification.
#' Defaults to `everything()`
#' @export

replace_all_na <- function(table, replace, across = tidyselect::everything()) {
  table %>%
    dplyr::mutate(
      dplyr::across(
        {{across}},
        ~tidyr::replace_na(.x, replace)))

}
