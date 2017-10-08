require(ggplot2)

#' @title       eq_load_data
#' @description dowload and clean the earthquakes file from NOAA
#'              This is a wrapper for eq_get_data and eq_clean_data
#'
#' @param filename Path to earthquakes file if already downloaded
#'                 when missing the file will be downloaded from NOAA
#'
#' @return The dataframe generated with columns described here:
#'         \url{https://www.ngdc.noaa.gov/nndc/struts/results?&t=101650&s=225&d=225}
#'         Error if file doesn't exist
#' @export
eq_load_data <- function(filename=NULL) {
    dt <- eq_get_data(filename) %>% eq_clean_data()
}

#' eq_geom_timeline
#'
#' A wrapper function to simplify use of geom_timeline
#'
#' @param df A data table containing NOAA Earthquake processed
#' @param nmin minimum year as numeric
#' @param nmax maximum year as numeric
#' @param countries Vector of countries to filter
#'
#'
#' @return A ggplot2 graphical object displaying timeline of earthquakes data
#' @export
#'
#' @examples
#' \dontrun{
#' eq_geom_timeline(df)
#' eq_geom_timeline(df,nmin=2000,nmax=2015, countries=c("USA","IRAN"))
#' }
eq_geom_timeline <- function(df, nmin=1,
                                 nmax=lubridate::year(Sys.Date()),
                                 countries = "") {

    type = jgg_CheckDataType(df)

    if (type < 0) {
        stop("Data must be either a data.frame or the name of a file")
    }

    if (class(nmin) != "numeric" || class(nmax) != "numeric") {
        stop("nmin and nmax must be and integer positive")
    }
    if (nmin < 0 || nmax < 1) {
        stop("nmin and nmax must be and integer positive")
    }

    dmin <- as.Date(sprintf("%04d-%02d-%02d", nmin,  1,  1), format="%Y-%m-%d")
    dmax <- as.Date(sprintf("%04d-%02d-%02d", nmax, 12, 31), format="%Y-%m-%d")

    if (!(countries[1] == "")) {
        dplyr::filter(df, COUNTRY %in% countries)  %>%
            ggplot2::ggplot() +
            geom_timeline(ggplot2::aes(x=DATE,
                              y=COUNTRY,
                              colour=DEATHS,
                              size=RITCHER,
                              fill=DEATHS,
                              xmin = dmin,
                              xmax = dmax)
                         ) +
            ggplot2::scale_size_continuous(name = "Richter scale values") +
            ggplot2::scale_fill_continuous(name = "# Deaths") +
            ggplot2::scale_colour_continuous(name = "# Deaths") +
            ggplot2::scale_alpha_continuous(name = "# Deaths")  +
            theme_timeline +
            ggplot2::xlab("DATE")
    } else {
        ggplot2::ggplot(df) +
            geom_timeline(ggplot2::aes(x=DATE,
                              colour=DEATHS,
                              size=RITCHER,
                              fill=DEATHS,
                              xmin = dmin,
                              xmax = dmax)
                          ) +
            ggplot2::scale_size_continuous(name = "Richter scale values") +
            ggplot2::scale_fill_continuous(name = "# Deaths") +
            ggplot2::scale_colour_continuous(name = "# Deaths") +
            ggplot2::scale_alpha_continuous(name = "# Deaths") +
           theme_timeline +
           ggplot2::xlab("DATE")
    }
}

#' eq_geom_timeline_label
#'
#' A wrapper function to simplify use of geom_timeline
#'
#' @param df A data table containing NOAA Earthquake processed
#' @param nmin minimum year as numeric
#' @param nmax maximum year as numeric
#' @param countries Vector of countries to filter
#' @param n_max Integer value to control number of labels per group to show
#'
#'
#' @return A ggplot2 graphical object displaying timeline of earthquakes data
#' @export
#'
#' @examples
#' \dontrun{
#' eq_geom_timeline_label(df, n_max=5, xmin=2000, xmax=2015, countries=c("USA","CHINA"))
#' }
eq_geom_timeline_label <- function(df, nmin=1,
                                       nmax=lubridate::year(Sys.Date()),
                                       n_max=5,
                                       countries = "") {

    type = jgg_CheckDataType(df)

    if (type < 0) {
        stop("Data must be either a data.frame or the name of a file")
    }

    if (class(nmin) != "numeric" || class(nmax) != "numeric") {
        stop("nmin and nmax must be and integer positive")
    }
    if (nmin < 0 || nmax < 1) {
        stop("nmin and nmax must be and integer positive")
    }
    if (class(n_max) != "numeric" || n_max < 1) {
        stop("n_max must be and integer positive")
    }

    dmin <- as.Date(sprintf("%04d-%02d-%02d", nmin,  1,  1), format="%Y-%m-%d")
    dmax <- as.Date(sprintf("%04d-%02d-%02d", nmax, 12, 31), format="%Y-%m-%d")


    data = if (!(countries[1] == "")) filter(df, COUNTRY %in% countries) else df

    ggplot2::ggplot(df) +
            geom_timeline_label(ggplot2::aes(x=DATE,
                                    y=COUNTRY,
                                    colour=DEATHS,
                                    size=RITCHER,
                                    fill=DEATHS,
                                    xmin = dmin,
                                    xmax = dmax,
                                    n_max = n_max)
            ) +
            ggplot2::scale_size_continuous(name = "Richter scale values") +
            ggplot2::scale_fill_continuous(name = "# Deaths") +
            ggplot2::scale_colour_continuous(name = "# Deaths") +
            ggplot2::scale_alpha_continuous(name = "# Deaths")  +
            theme_timeline +
            ggplot2::xlab("DATE")
}


#############################################################################
#  PRIVATE FUNCTIONS
#############################################################################

#' jgg_CheckDataType
#' @description Internal function to validate input parameters
#' @usage NULL
#' @param data The object which to want check de data type
jgg_CheckDataType <- function(data) {
    if (is.null(data))               return (1)  # No data
    if (class(data) == "data.frame") return (0)  # Is a data.frame
    if (class(data) == "character")  return (2)  # Is a file name
    return (-1)
}
