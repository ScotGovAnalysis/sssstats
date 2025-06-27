#' get_sspl_lookup Read in the Scottish Statistics Postcode Lookup
#' @description
#' Currently, the Scottish Statistics Postcode Lookup is available on this
#'  webpage: https://www.nrscotland.gov.uk/publications/scottish-statistics-postcode-lookup/
#'  -- future plans may include making this lookup available on https://statistics.gov.scot/home.
#'
#' The Scottish Statistics Postcode Lookup should be used for all official
#' statistical production to ensure geographic consistency across all official
#' statistics and to support the implementation of the Government Statistical
#' Service Geography Policy.
#' @param folder-path the path to the folder containing the lookup.
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @return A data frame.
#' @export

get_sspl_lookup<- function(folder_path){
  the_file <- "SingleRecord.csv"
  file_path <- paste0(folder_path, "/", the_file)

  if(!file.exists(file_path)){
    stop("File 'SingleRecord.csv' not found in folder: ", folder_path)
  }

  sspl <- readr::read_csv(file_path) |>
    janitor::clean_names(case = "snake")
}
