#' Facilitated plot of brazilian maps
#'
#' A wrapper in order to facilitate the plot
#' of the maps from this package. The function
#' returns a ggplot object so it can be edited
#' easily.
#'
#' @usage plot_brmap(map, data_to_join = data.frame(), join_by = NULL,
#'   var = "values", theme = theme_map())
#' @param map An object of class 'sf', 'SpatialPolygonsDataFrame' or 'data.frame'
#' @param data_to_join A data frame containing values to plot on the map.
#' @param join_by A character vector of variables to join by.
#' @param var The name of the column that contains the values of the field to
#'   be plotted. The default is \code{"value"}.
#' @param theme The theme that should be used for plotting the map. The default
#'   is \code{\link[ggthemes]{theme_map}}.
#' @return A \code{\link[ggplot2]{ggplot}} object that contains a basic
#'   brazilian map with the described parameters. Since the result is a \code{ggplot}
#'   object, it can be extended with more \code{geom} layers, scales, labels,
#'   themes, etc.
#'
#' @seealso \code{\link{get_brmap}}, \code{\link[ggplot2]{theme}}
#'
#' @examples
#' ## Plotting population estimates (2017) of the South Region
#' data("pop2017")
#' map_sul <- get_brmap(geo = "City", geo.filter = list(Region = 4))
#' mapa1 <- plot_brmap(map_sul,
#'                     data_to_join = pop2017,
#'                     join_by = c("City" = "mun"),
#'                     var = "pop2017")
#' mapa1
#'
#' # Output is ggplot object so it can be extended
#' # with any number of ggplot layers
#' library(ggplot2)
#' mapa1 +
#'   labs(title = "População Municipal 2017 - Região Sul")
#'
#'
#' # Only displaying the microregions of the state of Sao Paulo
#' map_sp_micro <- get_brmap(geo = "MicroRegion",
#'                           geo.filter = list(State = 35),
#'                           class = "SpatialPolygonsDataFrame")
#' plot_brmap(map_sp_micro)
#'
#' @export

plot_brmap <- function(map,
                       data_to_join = data.frame(),
                       join_by = NULL,
                       var = "values",
                       theme = theme_map()) {


  if (nrow(data_to_join) != 0) {

    map <- join_data(map = map, data = data_to_join, by = join_by)

  if (any(class(map) == "sf")) {

    map_ggplot <- as(map, "Spatial")
    map_ggplot@data$id = row.names(map_ggplot@data)

    map_ggplot_df <- suppressMessages( ggplot2::fortify(map_ggplot) )
    map_ggplot_df <- dplyr::left_join(map_ggplot_df, map_ggplot@data, by = "id")

  } else if (class(map) == "SpatialPolygonsDataFrame") {

    map_ggplot <- map
    map_ggplot@data$id = row.names(map_ggplot@data)

    map_ggplot_df <- suppressMessages( ggplot2::fortify(map_ggplot) )
    map_ggplot_df <- dplyr::left_join(map_ggplot_df, map_ggplot@data, by = "id")

  }

    ggplot2::ggplot() +
      ggplot2::geom_polygon(ggplot2::aes(fill = map_ggplot_df[[var]], x = map_ggplot_df$long, y = map_ggplot_df$lat, group = map_ggplot_df$group),
                            data = map_ggplot_df,
                            color = "black",
                            size = 0.2) +
      ggplot2::coord_fixed() +
      ggplot2::labs(fill = var) +
      theme_map()

  } else {

    if (any(class(map) == "sf")) map <- as(map, "Spatial") %>% ggplot2::fortify()
    if (class(map) == "SpatialPolygonsDataFrame") map <- map %>% ggplot2::fortify()

    ggplot2::ggplot() +
      ggplot2::geom_polygon(ggplot2::aes(x = map$long, y = map$lat, group = map$group),
                            data = map,
                            color = "black",
                            size = 0.2,
                            fill = "white") +
      ggplot2::coord_fixed() +
      theme_map()

  }

}

#' This creates a nice map theme for use in plot_usmap.
#' It is borrowed from the usmap package that borrowed from the
#' ggthemes package located at this repository:
#'    https://github.com/jrnold/ggthemes
#'
#' This function was manually rewritten here to avoid the need for
#'  another package import.
#'
#' All theme functions (i.e. theme_bw, theme, element_blank, %+replace%)
#'  come from ggplot2.
#'
#' @keywords internal
theme_map <- function(base_size = 9, base_family = "") {
  elementBlank = ggplot2::element_blank()
  `%+replace%` <- ggplot2::`%+replace%`
  unit <- ggplot2::unit

  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.line = elementBlank,
                   axis.text = elementBlank,
                   axis.ticks = elementBlank,
                   axis.title = elementBlank,
                   panel.background = elementBlank,
                   panel.border = elementBlank,
                   panel.grid = elementBlank,
                   panel.spacing = unit(0, "lines"),
                   plot.background = elementBlank,
                   legend.justification = c(0, 0),
                   legend.position = c(0, 0))
}

