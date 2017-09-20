#' Get Brazilian maps from different geographic levels
#'
#' Turn available to manipulation
#' brazilian maps in various type
#' of geographic levels. The maps
#' are from IBGE (Instituto Brasileiro
#' de Geografia e Estatística) and
#' refers to the administrative
#' configurations of 2016.
#'
#' @usage get_brmap(geo = c("Brazil","Region","State","MesoRegion","MicroRegion","City"),
#'                  geo.filter = NULL,
#'                  class = c("sf", "SpatialPolygonsDataFrame", "data.frame"))
#' @param geo A string value with geographic levels of interest
#' @param geo.filter A named list object with the specific item of the
#'   geographic level or all itens of a determined higher geografic level
#' @param class The class of the object to be returned
#' @details
#' The \code{geo} argument can be one of "Brazil", "Region", "State",
#' "MesoRegion", "MicroRegion" and "City".
#' 'geo.filter' lists must be named with the same characters.
#' @return The function returns a 'sf', 'SpatialPolygonsDataFrame' or 'data.frame'
#'   object depending of the 'class' argument informed
#' @author Renato Prado Siqueira \email{<rpradosiqueira@@gmail.com>}
#' @seealso \code{\link{join_data}}
#' @examples
#' # Retrieving the map from the State of Rio de Janeiro
#' rio_map <- get_brmap(geo = "State",
#'                      geo.filter = list(State = 33),
#'                      class = "sf")
#' plot(rio_map)
#'
#' # Obtaining the municipalities maps from Midwest Region
#' cities_map <- get_brmap(geo = "City",
#'                         geo.filter = list(Region = 5),
#'                         class = "sf")
#' plot(cities_map)
#'
#' @keywords IBGE shapefile geographic levels spatial
#' @importFrom methods as
#' @importFrom magrittr %>%
#' @export

get_brmap <- function(geo = c("Brazil","Region","State","MesoRegion","MicroRegion","City"),
                      geo.filter = NULL,
                      class = c("sf", "SpatialPolygonsDataFrame", "data.frame")) {

  geo <- match.arg(geo)
  class <- match.arg(class)

  # geo.filter
  geo.filter.df <- data.frame(description = as.character(c("Brazil","Region","State","MesoRegion","MicroRegion","City")),
                              level = c(0, 1, 2, 4, 5, 7))

  geo1 <- merge(geo.filter.df, data.frame(description = names(geo.filter)))
  geo2 <- geo.filter.df[which(geo.filter.df$description == geo), ]

  if ( any(geo1$level > geo2$level) ) stop("The 'geo.filter' argument is misspecified")

  brmap <- base::readRDS(system.file("maps", paste0(geo, ".rds"), package = "brazilmaps"))

  if ( (!is.null(geo.filter)) & (geo %in% c("Region", "Brazil")) ) {

    warning("'geo.filter' argument will be assign to NULL when geo is 'Region' or 'Brazil'")
    geo.filter <- NULL

  } else if (!is.null(geo.filter)) {

    brmap_list <- list()

    for (i in 1:nrow(geo1)) {

      brmap_list[[i]] <- brmap %>%
        dplyr::filter( brmap[[(names(geo.filter)[i])]] %in% geo.filter[[i]] ) %>%
        sf::st_as_sf()

    }

    brmap <- do.call("rbind", brmap_list)

  }

  if (class == "SpatialPolygonsDataFrame") brmap <- as(brmap, "Spatial")
  if (class == "data.frame") brmap <- as(brmap, "Spatial") %>% ggplot2::fortify(region = geo)

  brmap

}


