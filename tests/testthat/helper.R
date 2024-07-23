#' Creates Test Data Set
#'
#' Helper function to create test data set
#' @export

create_test_data <- function(cols = tidyselect::everything()){
  dplyr::tibble(integer = c(1, 2, 3),
         double = c(0.35, 2.68, 95.3),
         percentage = c(6, 9, 4),
         string = c("a", "b", "c"),
         boolean = c(TRUE, TRUE, FALSE),
         na = c("a", NA, "c"),
         suppressed = c("[c]", 3, "[c]"),
         dates = c("2024-11", "2024-12", "2025-01"),
         number_string = c("4", "5", "6")) %>%
    dplyr::select(all_of(cols))
}

#' Creates Test Data Set 2
#'
#' Helper function to create test data set 2
#' @export
create_test_data_2 <- function(cols = tidyselect::everything()){
  dplyr::tibble(place = c("Aberdeen", "Glasgow", "Edinburgh", "Edinburgh", "Ayr", "Perth", "Stirling", "Inverness", "Glasgow", "Edinburgh"),
                month = c("January 2024", "February 2023", "September 2025", "October 2024", "March 2023", "February 2022", "September 2022", "December 2024", "November 2024", "April 2022"),
                count = c(4, 5, 7, 3, 6, 2, 7, 3, 4, 9),
                value = c(450, 399, 233, 736, 182, 433, 469, 932, 102, 377),
                age = count * 5 + 1) %>%
    dplyr::select(all_of(cols))
}
