#' Join external data
#'
#' A wrapper around dplyr's join
#' in order to facilitate the analysis
#' on the maps from this package
#'
#' @usage join_data(map, data, by = NULL)
#' @param map An object of class 'sf', 'SpatialPolygonsDataFrame' or 'data.frame'
#' @param data A data.frame object with the data to join
#' @param by A character vector of variables to join by. If NULL, the default, will do
#' a natural join, using all variables with common names across the two tables. See
#' dplyr's join to more information.
#' @return The function returns a 'sf', 'SpatialPolygonsDataFrame' or 'data.frame'
#'   object depending of the class of the map argument informed
#' @author Renato Prado Siqueira \email{<rpradosiqueira@@gmail.com>}
#' @seealso \code{\link{get_brmap}}
#' @examples
#' # Joining population estimates data to the year of 2017
#' data("pop2017")
#' municipios <- get_brmap(geo = "City", geo.filter = list(Region = 5),
#'                         class = "SpatialPolygonsDataFrame")
#'
#' municipios <- join_data(municipios, pop2017, by = c("City" = "mun"))
#'
#' @keywords IBGE shapefile geographic levels spatial
#' @importFrom magrittr %>%
#' @export

join_data <- function(map, data, by = NULL) {

  if (any(class(map) == "SpatialPolygonsDataFrame")) {

    map@data <- dplyr::left_join(map@data, data, by = by)

    map

  } else {

    data <- dplyr::left_join(map, data, by = by) %>%
      sf::st_as_sf()

    data

  }

}
