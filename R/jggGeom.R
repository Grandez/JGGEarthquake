library(ggplot2)
library(grid)

#############################################################################
#  PUBLIC FUNCTIONS
#############################################################################


#if(getRversion() >= "3.0.0") {
#  utils::globalVariables(c("YEAR","MONTH","DAY","LONGITUDE","LATITUDE",
#                           "LOCATION_NAME","COUNTRY","DEATHS","EQ_PRIMARY",
#                           "datetime","year","date_s1","bcyear"))
#}

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
#' library(dplyr)
#' library(ggplot2)
#'     ggplot(df) +
#'     geom_timeline(aes(x = DATE,
#'                       y = COUNTRY,
#'                       colour = DEATHS,
#'                       size = RITCHER,
#'                       fill = DEATHS,
#'                       xmin = 2000,
#'                       xmax = 2010)
#'
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
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
#' library(ggplot2)
#'     ggplot(df) +
#'     geom_timeline_label(aes(x = DATE,
#'                             location = LOCATION_NAME,
#'                             xmin = 2000,
#'                             xmax = 2010,
#'                             size=RITCHER,
#'                             n_max=5,
#'                             y=COUNTRY))
#'
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    geom = geomTimelineLabel, stat = StatTimeline, mapping = mapping,
    data = data,  position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#' geomTimelineLabel
#' @rdname Earthquake-ggproto
#' @format NULL
#' @usage NULL
# @importFrom grid segmentGrob
# @importFrom grid textGrob
# @importFrom grid gTree
# @importFrom grid gList
geomTimelineLabel <- ggproto("geomTimelineLable", Geom,
                              required_aes = c("x","location"),
                              optional_aes = c("y","n_max"),
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

                                        # SegmentGrob to draw lines where we will plot our earthquake points
                                        seg_grob <- grid::segmentsGrob(
                                          x0 = unit(coords$x,"native"),
                                          x1 = unit(coords$x,"native"),
                                          y0 = unit(coords$y,"native"),
                                          y1 = unit(coords$y + 0.05,"native"),
                                          gp = gpar(col = "grey", alpha = 0.75)
                                        )
                                        # textGrob to print location
                                        text_grob <- textGrob(
                                          label = coords$location,
                                          x = unit(coords$x,"native"),
                                          y = unit(coords$y + 0.06,"native"),
                                          rot = 45,
                                          just = "left",
                                          gp = gpar(fontsize = 8)
                                        )
                                        # group our grobs together for output
                                        gTree(children = gList(seg_grob,text_grob))
                                      })


#' StatTimeline
#' @rdname Earthquake-ggproto
#' @format NULL
#' @usage NULL
# @importFrom dplyr filter
StatTimeline <- ggproto("StatTimeline", Stat
                        ,required_aes = c("x")
                        ,optional_aes=c("xmin", "xmax")
                        # ,default_aes = aes(xmin=NA, xmax=NA)
                        ,setup_params = function(data, params) {
                            dmin = if ("xmin" %in% names(data))  data$xmin else min(data$x)
                            dmax = if ("xmax" %in% names(data))  data$xmax else max(data$x)
                            list(
                                min = dmin,
                                max = dmax,
                                na.rm = params$na.rm
                            )
                        }
                        ,compute_group = function(data, scales, min, max) {
                            data %>% filter(data$x >= min & data$x <= max)
                        }
)

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

                        #non_missing_aes = c("size", "shape", "colour"),
                        #default_aes = aes(y=0.5,
                        #                  shape = 21, colour = "gray", fill=NA, size = 1.5,
                        #                  alpha = NA, stroke = 0.5
                        #),
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

                            c2 <- linesGrob(
                                y=unit(c(coords$y, coords$y),"npc"),
                                gp = gpar(col="black", lwd = 2)
                            )
                            grobTree(c2, c1)
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