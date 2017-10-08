require(ggplot2)
require(grid)
require(dplyr)

#############################################################################
#  PUBLIC FUNCTIONS
#############################################################################


if(getRversion() >= "3.0.0") {
  utils::globalVariables(c("YEAR","MONTH","DAY","LONGITUDE","LATITUDE",
                           "LOCATION_NAME","COUNTRY","DEATHS","RITCHER",
                           "EQ_PRIMARY", "DATE"))
}

#' geom_timeline
#'
#' A ggplot2 graphical function to plot a timeline of earthquakes from cleaned data.
#' The plot indicates the magnitude of each earthquake and number of deaths.
#'
#' @section Aesthetics:
#' \code{geom_timeline} understands the following aesthetics:
#' \itemize{
#'   \item \code{x} DATE
#'   \item \code{y} Countries
#'   \item \code{xmin} minimum date for earthquakes
#'   \item \code{xmax} maximum date for earthquakes
#'   \item \code{size} used to size shape based on magnitude of earthquake eg EQ_PRIMARY
#'   \item \code{fill} used to colour shape based on number of deaths eg DEATHS
#'   \item \code{colour} used to colour shape based on number of deaths eg DEATHS
##' }
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @return ggplot2 graphical object
#' @export
#'
#' @examples
#'\dontrun{
#' library(ggplot2)
#' dmin = as.Date("2000-01-01", format="%Y-%m-%d")
#' dmax = as.Date("2017-01-01", format="%Y-%m-%d")
#' ggplot(df) +
#' geom_timeline(aes(x=DATE,
#'                   colour=DEATHS,
#'                   size=RITCHER,
#'                   fill=DEATHS,
#'                   xmin=dmin,
#'                   xmax=dmax)
#'              )
#'}
#'
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTimeline, geom = GeomTimeline, mapping = mapping,
    data = data,  position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' geom_timeline_label
#'
#' A ggplot2 graphical function that adds labels to earthquakes visualised.
#' There is an option to select the "n" largest earthquakes by magnitude to which to apply the labels.
#' Best used with `eq_location_clean`.
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @section Aesthetics:
#' \code{geom_timeline_label} understands the following aesthetics:
#' \itemize{
#'   \item \code{x} date
#'   \item \code{y} (optional) aes can be used to group output eg by COUNTRY
#'   \item \code{location} aes used to selection labels eg LOCATION_NAME
#'   \item \code{xmin} minimum date for earthquakes
#'   \item \code{xmax} maximum date for earthquakes
#'   \item \code{size} aes used to indicate size eg EQ_PRIMARY
#'   \item \code{n_max} the top n number of labels to show based on size aes, defaults to n = 5
#' }
#'
#' @return A ggplot2 graphical object for labelling plots generated with geom_timeline.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'     ggplot(df) +
#'     geom_timeline_label(aes(x = DATE,
#'                             xmin = as.Date("2000-01-01", format="%Y-%m-%d"),
#'                             xmax = as.Date("2017-01-01", format="%Y-%m-%d"),
#'                             size=RITCHER,
#'                             n_max=5))
#'}
#'
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        stat = StatTimeline, geom = GeomTimelineLabel, mapping = mapping,
        data = data,  position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )

    #  ggplot2::layer(
    #geom = geomTimelineLabel, stat = StatTimeline, mapping = mapping,
    #data = data,  position = position,
    #show.legend = show.legend, inherit.aes = inherit.aes,
    #params = list(na.rm = na.rm, ...)
  #)
}


#' eq_map
#'
#' A function to generate an interactive map showing earthquakes.
#' The user specifies a column from the data which the earthquake is to be annotated by eg DATE.
#'
#' @param dt A data table containing NOAA Earthquake data cleaned
#' @param annot_col A column found in \code{eq_data} to annotate earthquake marker
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @return An interactive map displaying earthquate location for a given country with user defined popup.
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(lubridate)
#' dt <- eq_clean_data %>% filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000)
#' eq_map(dt, annot_col="DATE")
#'}
eq_map <- function(dt, annot_col) {

    leaflet() %>% addTiles() %>%
                  addCircleMarkers(data=dt, lng = ~ LONGITUDE, lat = ~ LATITUDE,
                                   radius = ~RITCHER, popup = annot_col)
}

#' eq_create_label
#'
#' A function to generate a custom popup box for a selected earthquake showing location,
#' magnitude and total deaths.
#'
#' @param dt A data table containing NOAA Earthquake data cleaned
#'
#' @return An interactive map displaying earthquate location for a given country with custom popup.
#' @export
#'
#' @examples
#' \dontrun{
#'  eq_create_label(dt)) %>% eq_map(annot_col="DATE")
#'}
#'
eq_create_label <- function(dt) {
    loc   <- paste("<b>Location:</b>", dt$LOCATION_NAME, "<br />")
    mag   <- paste("<b>Magnitude:</b>", dt$EQ_PRIMARY, "<br />")
    death <- paste("<b>Total deaths:</b>", dt$TOTAL_DEATHS, "<br />")
    out <- paste(loc,mag,death)
    return(out)
}


GeomTimeline <- ggproto("GeomTimeline", GeomPoint,
                        required_aes = c("x"),
                        optional_aes = c("y", "xmin","xmax", "colour", "fill", "size"),
                        default_aes = aes(shape = 21
                                          ,size = 5
                                          ,colour = "blue"
                                          ,fill = "blue"
                                          ,alpha = 0.5
                                          ,stroke = 1
                                          ,y = 0.5),

                        draw_key = draw_key_point,
                        draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                            coords <- coord$transform(data, panel_params)
                            c1 <- pointsGrob(
                                coords$x, coords$y,
                                pch = coords$shape,
                                gp = gpar(
                                    col = alpha(coords$colour, coords$alpha),
                                    fill = alpha(coords$fill, coords$alpha),
                                    # Stroke is added around the outside of the point
                                    fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                    lwd = coords$stroke * .stroke / 2
                                )
                            )

                            c2 <- segmentsGrob(
                                x0 = unit(coords$xmin,"native"),
                                x1 = unit(coords$xmax,"native"),
                                y0 = unit(coords$y,"native"),
                                y1 = unit(coords$y,"native"),
                                gp = gpar(col = "grey", alpha = 0.25)
                            )

                            grobTree(c2, c1)
                        }
)

#' geomTimelineLabel
#' @rdname Earthquake-ggproto
#' @format NULL
#' @usage NULL
# @importFrom grid segmentGrob
# @importFrom grid textGrob
# @importFrom grid gTree
# @importFrom grid gList
GeomTimelineLabel <- ggproto("GeomTimelineLabel", GeomPoint,
                        required_aes = c("x"),
                        optional_aes = c("y", "xmin","xmax", "n_max", "colour", "fill", "size"),
                        default_aes = aes(shape = 21
                                          ,size = 5
                                          ,colour = "blue"
                                          ,fill = "blue"
                                          ,alpha = 0.5
                                          ,stroke = 1
                                          ,y = 0.5),

                        draw_key = draw_key_point,
                        draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                            data
                            if ("n_max" %in% names(data)) {
                                nm <-  data$n_max[1]
                            } else {
                                nm <- 0
                            }
                            if ("n_max" %in% names(data) & nrow(data) > nm) {
                                data <- data %>%
                                    group_by(y) %>%
                                    top_n(n = data$n_max[1], wt = size)
                                data
                            }

                            coords <- coord$transform(data, panel_params)
                            c1 <- pointsGrob(
                                coords$x, coords$y,
                                pch = coords$shape,
                                gp = gpar(
                                    col = alpha(coords$colour, coords$alpha),
                                    fill = alpha(coords$fill, coords$alpha),
                                    # Stroke is added around the outside of the point
                                    fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                    lwd = coords$stroke * .stroke / 2
                                )
                            )

                            c2 <- segmentsGrob(
                                x0 = unit(coords$xmin,"native"),
                                x1 = unit(coords$xmax,"native"),
                                y0 = unit(coords$y,"native"),
                                y1 = unit(coords$y,"native"),
                                gp = gpar(col = "grey", alpha = 0.25)
                            )

                            # SegmentGrob to draw lines where we will plot our earthquake points
                            c3 <- segmentsGrob(
                                x0 = unit(coords$x,"native"),
                                x1 = unit(coords$x,"native"),
                                y0 = unit(coords$y,"native"),
                                y1 = unit(coords$y + 0.1,"native"),
                                gp = gpar(col = "grey", alpha = 0.75)
                            )

                            # textGrob to print location
                            c4 <- textGrob(
                                  label = coords$location,
                                  x = unit(coords$x,"native"),
                                  y = unit(coords$y + 0.12,"native"),
                                  rot = 45,
                                  just = "left",
                                  gp = gpar(fontsize = 8)
                            )

                            grobTree(c1, c2, c3, c4)
                        }
)

geomTimelineLabel2 <- ggproto("geomTimelineLabel2", Geom,
                              required_aes = c("x"),
                              optional_aes = c("y", "xmin","xmax", "n_max", "colour", "fill", "size"),
                              default_aes = aes(size =0, y = 0.5, fontsize = 8, alpha = 0.75, colour = "blue", fill = "blue"),
                              draw_key = draw_key_blank,
                              draw_panel = function(data, panel_scales, coord) {
                                        data
                                        if ("n_max" %in% names(data)) {
                                          nm <-  data$n_max[1]
                                        } else {
                                          nm <- 0
                                        }
                                        if ("n_max" %in% names(data) & nrow(data) > nm) {
                                          data <- data %>%
                                            group_by(y) %>%
                                            top_n(n = data$n_max[1], wt = size)
                                          data
                                        }

                                        coords <- coord$transform(data, panel_scales)

                                        c1 <- pointsGrob(
                                            coords$x, coords$y,
                                            pch = coords$shape,
                                            gp = gpar(
                                                col = alpha(coords$colour, coords$alpha),
                                                fill = alpha(coords$fill, coords$alpha),
                                                # Stroke is added around the outside of the point
                                         #       fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                lwd = coords$stroke * .stroke / 2
                                            )
                                        )

                                        # SegmentGrob to draw lines where we will plot our earthquake points
                                        c2 <- segmentsGrob(
                                          x0 = unit(coords$x,"native"),
                                          x1 = unit(coords$x,"native"),
                                          y0 = unit(coords$y,"native"),
                                          y1 = unit(coords$y + 0.05,"native"),
                                          gp = gpar(col = "grey", alpha = 0.75)
                                        )
                                        # textGrob to print location
                                        #c3 <- textGrob(
                                        #  label = coords$location,
                                        #  x = unit(coords$x,"native"),
                                        #  y = unit(coords$y + 0.06,"native"),
                                        #  rot = 45,
                                        #  just = "left",
                                        #  gp = gpar(fontsize = 8)
                                        #)
                                        # group our grobs together for output
                                        gTree(children = gList(c1,c2))
                                      })


#' StatTimeline
#' @rdname Earthquake-ggproto
#' @format NULL
#' @usage NULL
StatTimeline <- ggproto("StatTimeline", Stat
                        ,required_aes = c("x")
                        ,optional_aes=c("xmin", "xmax")
                        # ,default_aes = aes(xmin=NA, xmax=NA)
                        ,setup_params = function(data, params) {
                            xmin = if ("xmin" %in% names(data))  data$xmin else min(data$x)
                            xmax = if ("xmax" %in% names(data))  data$xmax else max(data$x)
                            list(
                                min = xmin,
                                max = xmax,
                                na.rm = TRUE
                            )
                        }
                        ,compute_group = function(data, scales, xmin, xmax) {
                            data %>% filter(data$x >= xmin & data$x <= xmax)
                        }
)


#' theme_timeline
#' @rdname Earthquake-theme
#' @format NULL
#' @usage NULL
#' @export
theme_timeline <- theme_classic() +
                  theme(axis.title.x = element_text(face = "bold")
                       ,axis.line.y =  element_blank()
                       ,axis.ticks.y = element_blank()
                       ,axis.title.y = element_blank()
                       ,legend.box = "horizontal"
                       ,legend.direction = "horizontal"
                       ,legend.position = "bottom"
                  )
