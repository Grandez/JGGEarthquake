library(tidyr,quietly=T, warn.conflicts=F)
library(data.table,quietly=T, warn.conflicts=F)
library(lubridate, quietly=T, warn.conflicts=F)


if(getRversion() >= "3.0.0") {
    utils::globalVariables(c("YEAR","MONTH","DAY","LONGITUDE","LATITUDE",
                             "LOCATION_NAME","COUNTRY","DEATHS","RITCHER",
                             "INJURIES", "EQ_PRIMARY"))
}

#' @title       eq_get_data
#' @description load the file
#'
#' @param filename Path to earthquakes file if already downloaded
#'                 when missing or NULL the file will be downloaded from NOAA
#'
#' @return The dataframe generated
#'         Error if file doesn't exist
#'
#' @importFrom utils data download.file
#' @export

eq_get_data <- function(filename=NULL) {
  if (is.null(filename)) {
    filename = tempfile()
    fileUrl = "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
    download.file(fileUrl, destfile=filename, method="curl", quiet=T)
  }
  data.table::fread(filename)
}

#' @title       eq_clean_data
#' @description Clean and prepare the dataframe for future uses
#'              Cleaning involves:
#'              1.- remove unused columns
#'              2.- Convert latitude and longitude to numeric
#'              3.- create the DATE field
#'              4.- Adjust LOCATION_NAME with eq_location_clean
#'
#' @param df Data frame
#'
#' @return The dataframe/datatable cleaned
#'
#' @examples
#' \dontrun{
#' dt = eq_clean_data(dataframe)
#' }


#' @export
eq_clean_data <- function(df) {
  dt <- eq_location_clean(df) %>%
        dplyr::select(-dplyr::starts_with("TOTAL"),
                      -dplyr::contains("DESCRIPTION"),
                      -dplyr::contains("HOUSES")) %>%     # Remove some columns
        tidyr::drop_na(YEAR, MONTH, DAY, EQ_PRIMARY) %>%                                            # Remove NA values
        subset(YEAR > 0) %>%                                                                 # Remove negative years
        dplyr::mutate(DATE = as.Date(sprintf("%04d-%02d-%02d",YEAR, MONTH, DAY), format="%Y-%m-%d"),
               RITCHER = EQ_PRIMARY
              ) %>%
        dplyr::select(DATE, COUNTRY, LOCATION_NAME, RITCHER, DEATHS, INJURIES, LATITUDE, LONGITUDE)

  # Set data types
  dt$LATITUDE = as.numeric(dt$LATITUDE)
  dt$LONGITUDE = as.numeric(dt$LONGITUDE)

  return (dt)
}

#' @title       eq_location_clean
#' @description Create LOATION_NAME based on country
#'
#' @param df Data frame
#'
#' @return The dataframe/datatable modified
#'
#' @examples
#' \dontrun{
#' dt = eq_location_clean(dataframe)
#' }

#' @export

eq_location_clean <- function(df) {
    df %>%
        dplyr::mutate(LOCATION_NAME =
               stringr::str_to_title(base::gsub("[^;\n]+[:]","",LOCATION_NAME)))
}

