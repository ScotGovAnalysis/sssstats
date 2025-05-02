#' get_datazone_lookup
#' @description Function using SPARQLendpoint at https://statistics.gov.scot/sparql
#'  to get the correct datazone file. The function first gets the filename from
#'  sparql then reads it in using read_csv.
#' @param census_year A specific Census year ("2011" or "2022")in which the Scottish data zones are derived from.
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr case_when
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @return A data frame.
#' @export
get_datazone_lookup <- function(census_year) {
  # Checks parameter is valid
  if (!census_year %in% c("2022", "2011")) {
    stop("Census_year not recognised for the Scottish data zone lookup.
         Only '2011' and '2022' are valid for this function.")
  }

  # builds the query
  query_2011 <- "SELECT *
    WHERE {
    <http://statistics.gov.scot/data/data-zone-lookup>
    <http://publishmydata.com/def/dataset#downloadURL>
    ?o
    }
    LIMIT 10"

  query_2022 <- "SELECT *
    WHERE {
    <http://statistics.gov.scot/data/data-zone-lookup-2022>
    <http://publishmydata.com/def/dataset#downloadURL>
    ?o
    }
    LIMIT 10"

  query <- dplyr::case_when(
    census_year == "2022" ~ query_2022,
    census_year == "2011" ~ query_2011,
  )

  # Executes the query
  response <- httr::POST(
    url = "http://statistics.gov.scot/sparql.json",
    body = list(query = query)
  )

  # Process the results
  json_text <- response |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  bindings <- json_text$results$bindings

  url <- bindings$o$value

  # download the file
  datazone_lookup <- readr::read_csv(url, show_col_types = FALSE) |>
    janitor::clean_names()

}
